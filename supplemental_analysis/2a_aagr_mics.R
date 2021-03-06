library(data.table)
library(openxlsx)
library(tidyverse)
library(foreign)

# settings --------------------------------------------------------------

if(!exists("set.globals") || !set.globals) {
  curr.dir <- getwd()
  this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(this.dir)
  source("settings.R")
  source("functions.R")
}

cat("\nComputing metric 2a, Average Annual Growth Rates by Alternative: MICs\n")
out.file.nm <- settings$aagr_mics$out.file.nm 


# general -----------------------------------------------------------------

years.col <- paste0("yr", fcast.yrs)

act.yrs <- c(2000, 2010, 2017)
act.yrs.cols <- paste0("act", act.yrs)
cols1 <- c(tail(act.yrs.cols, -1), act.yrs.cols[length(act.yrs.cols)])
cols2 <- c(head(act.yrs.cols, -1), act.yrs.cols[1])

attributes <- c("employment", "population")
ind.extension <- ".csv" 


# functions ----------------------------------------------------------

calc.actuals.delta <- function() {
  lu <- read.xlsx(file.path(data.dir, "usim_gis_lu_centers.xlsx")) %>% as.data.table
  emp <- read.actual.emp("MIC")
  edt <- merge(emp, lu, by.x = "Jurisdiction", by.y = "gis_center")
  cols <- c("growth_center_id", "name", colnames(edt)[str_which(colnames(edt), "^act|^county")])
  dt <- edt[, ..cols]
  deltanames <- paste0("delta_", cols1, "-", cols2)
  dtcalc <- dt[, (deltanames) := mapply(function(x, y) (.SD[[x]]-.SD[[y]]), cols1, cols2, SIMPLIFY = F)]
}

calc.actuals.by.mics <- function() {
  dt <- calc.actuals.delta()
  aagrnames <- paste0("aagr_", cols1, "-", cols2)
  start.year.col <- cols2
  end.year.col <- cols1
  start.year.num <- str_extract(start.year.col, "\\d+") %>% lapply(as.numeric) %>% unlist
  end.year.num <- str_extract(end.year.col, "\\d+") %>% lapply(as.numeric) %>% unlist
  dtaa <- dt[, (aagrnames) := mapply(function(a, b, c, d) ((.SD[[b]]/.SD[[a]])^(1/(d-c))-1), 
                                         start.year.col, end.year.col, start.year.num, end.year.num, SIMPLIFY =  F)]
  dtaa[, `:=` (county_juris = NULL, county_code = NULL)]
  setnames(dtaa, c("growth_center_id"), c("name_id"))
  dtaa[order(name_id)]
}

calc.actuals.by.cnty <- function() {
  dt <- calc.actuals.delta()
  deltanames <- paste0("delta_", cols1, "-", cols2)
  sdcols <- c(act.yrs.cols, deltanames)
  aagrnames <- paste0("aagr_", cols1, "-", cols2)
  start.year.col <- cols2
  end.year.col <- cols1
  start.year.num <- str_extract(start.year.col, "\\d+") %>% lapply(as.numeric) %>% unlist
  end.year.num <- str_extract(end.year.col, "\\d+") %>% lapply(as.numeric) %>% unlist
  dtaa <- dt[, lapply(.SD, sum), .SDcols = sdcols, by = .(county_name, county_juris, county_code)
          ][, (aagrnames) := mapply(function(a, b, c, d) ((.SD[[b]]/.SD[[a]])^(1/(d-c))-1), 
                                    start.year.col, end.year.col, start.year.num, end.year.num, SIMPLIFY =  F)]
  setnames(dtaa, c("county_code", "county_juris"), c("name_id", "name"))
  dtaa[order(name_id)]
}

calc.delta <- function() {
  lu <- fread(file.path(data.dir, "growth_centers.csv"))[growth_center_id >= 600, .(city_id, growth_center_id, name)]
  jlu <- read.xlsx(file.path(data.dir, "cities.xlsx")) %>% as.data.table
  
  dt <- compile.tbl("growth_center", allruns, run.dir, attributes, ind.extension)
  cols <- c("name_id", years.col, "indicator", "run")
  dtm <- dt[indicator == "employment" & name_id >= 600, ..cols]
  dtm[lu, on = c("name_id" = "growth_center_id"), `:=` (city_id = i.city_id, name = i.name)]
  dtm[jlu, on = c("city_id"), county_id := i.county_id]
  dtm[, county_id := as.character(county_id)
      ][, county_name := switch(county_id, "33" = "King", "35" = "Kitsap", "53" = "Pierce", "61" = "Snohomish"), by = county_id]
  dtm[name == "Frederickson", `:=`(county_name = "Pierce", county_id = "53")]
  
  dtm[, delta := get(eval(years.col[2]))-get(eval(years.col[1]))]
}

calc.by.mic <- function() {
  dt <- calc.delta()
  dt[, aagr := eval(calc.aagr(years.col[1], years.col[2], fcast.yrs[1], fcast.yrs[2]))
     ][is.nan(aagr), aagr := 0
       ]
  cols <- c("run", "name_id", "county_name", "name", "indicator", years.col[1], years.col[2], "delta", "aagr")
  t <- dt[, ..cols]
}

calc.by.cnty <- function() {
  dt <- calc.delta()
  sdcols <- c(years.col, "delta")
  cdt <- dt[, lapply(.SD, sum), .SDcols = sdcols, by = .(county_id, indicator, run, county_name)]
  cdt[, aagr := eval(calc.aagr(years.col[1], years.col[2], fcast.yrs[1], fcast.yrs[2]))
     ][is.nan(aagr), aagr := 0
       ]
  cdt[, name := switch(county_name, "King" = "King County", 
                       "Kitsap" = "Kitsap County", "Pierce" = "Pierce County", "Snohomish" = "Snohomish County"), by = county_name
      ][, name_id := switch(county_name,  "King" = "53033", "Kitsap" = "53035", "Pierce" = "53053", "Snohomish" = "53061"), by = county_name]
  cols <- c("run", "name_id", "county_name", "name", "indicator", years.col[1], years.col[2], "delta", "aagr")
  t <- cdt[, ..cols][order(county_name)]
}

compile.final.table <- function() {
  act.m <- calc.actuals.by.mics()
  act.c <- calc.actuals.by.cnty()
  act <- rbindlist(list(act.c, act.m), use.names = T)
  setcolorder(act, c("name_id", "county_name", "name", colnames(act)[str_which(colnames(act), "^act|^delta|^aagr")]))
  
  mdt <- calc.by.mic()
  cdt <- calc.by.cnty()
  dt <- rbindlist(list(cdt, mdt), use.names = T)
  setnames(dt, "indicator", "attribute")
  
  t <- act[dt, on = c("name_id", "county_name", "name")]
 
  cols <- c("run", "attribute", colnames(t)[!(colnames(t) %in% c("run", "attribute"))])
  setcolorder(t, cols)
  t[order(run, name_id)]
}

dt <- compile.final.table()

# loop through each run
dlist <- NULL
for (r in 1:length(run.dir)) {
  t <- NULL
  t <- dt[run == run.dir[r], ]
  dlist[[names(run.dir[r])]] <- t
}


# export ------------------------------------------------------------------

write.xlsx(dlist, file.path(out.dir, paste0(out.file.nm, "_", Sys.Date(), ".xlsx")))

setwd(curr.dir)