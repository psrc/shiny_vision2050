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

attributes <- c("employment", "population")
ind.extension <- ".csv" 


# functions ----------------------------------------------------------

calc.delta <- function() {
  lu <- fread(file.path(data.dir, "growth_centers.csv"))[growth_center_id >= 600, .(city_id, growth_center_id, name)]
  jlu <- read.xlsx(file.path(data.dir, "cities.xlsx")) %>% as.data.table
  
  dt <- compile.tbl("growth_center", allruns, run.dir, attributes, ind.extension)
  cols <- c("name_id", years.col, "indicator", "run")
  dtm <- dt[name_id >= 600, ..cols]
  dtm[lu, on = c("name_id" = "growth_center_id"), `:=` (city_id = i.city_id, name = i.name)]
  dtm[jlu, on = c("city_id"), county_id := i.county_id]
  dtm[, county_id := as.character(county_id)
      ][, county_name := switch(county_id, "33" = "King", "35" = "Kitsap", "53" = "Pierce", "61" = "Snohomish"), by = county_id]
  dtm[name == "Frederickson", `:=`(county_name = "Pierce", county_id = "53")]
  
  dtm[, delta := get(eval(years.col[2]))-get(eval(years.col[1]))]
}

calc.by.mic <- function() {
  dt <- calc.delta()
  dt[, aagr := (((get(eval(years.col[2]))/get(eval(years.col[1])))^(1/(fcast.yrs[2]-fcast.yrs[1]))) - 1)
     ][is.nan(aagr), aagr := 0
       ]
  cols <- c("run", "name_id", "county_name", "name", "indicator", years.col[1], years.col[2], "delta", "aagr")
  t <- dt[, ..cols]
}

calc.by.cnty <- function() {
  dt <- calc.delta()
  sdcols <- c(years.col, "delta")
  cdt <- dt[, lapply(.SD, sum), .SDcols = sdcols, by = .(county_id, indicator, run, county_name)]
  cdt[, aagr := (((get(eval(years.col[2]))/get(eval(years.col[1])))^(1/(fcast.yrs[2]-fcast.yrs[1]))) - 1)
     ][is.nan(aagr), aagr := 0
       ]
  cdt[, name := switch(county_name, "King" = "King County", 
                       "Kitsap" = "Kitsap County", "Pierce" = "Pierce County", "Snohomish" = "Snohomish County"), by = county_name
      ][, name_id := switch(county_name,  "King" = "53033", "Kitsap" = "53035", "Pierce" = "53053", "Snohomish" = "53061"), by = county_name]
  cols <- c("run", "name_id", "county_name", "name", "indicator", years.col[1], years.col[2], "delta", "aagr")
  t <- cdt[, ..cols][order(county_name)]
}

mdt <- calc.by.mic()
cdt <- calc.by.cnty()
dt <- rbindlist(list(cdt, mdt), use.names = T)
setnames(dt, "indicator", "attribute")

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