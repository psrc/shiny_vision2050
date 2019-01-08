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

cat("\nComputing metric 5, Growth by Centers & Station Areas (formerly known as TOD areas) by Jurisdiction, by type/mode\n")
out.file.nm <- settings$aagr_rgc_bsa_type_by_juris$out.file.nm 


# general -----------------------------------------------------------------

years.col <- paste0("yr", fcast.yrs)
ind.extension <- ".csv" 


# functions ----------------------------------------------------------

# compile forecast files 
file.regexp <- "city.*[population|employment]_tod_\\d\\.csv"
compile.tbl.supp.tod <- function(file.regexp, allruns, run.dir, ind.extension) {
  df <- NULL
  for (r in 1:length(run.dir)) { # for each run
    base.dir <- purrr::pluck(allruns, run.dir[r])
    filenames <- list.files(file.path(base.dir, "indicators"), pattern = file.regexp)
    for (afile in filenames) {
      datatable <- dt <- NULL
      datatable <- fread(file.path(base.dir, "indicators", afile), header = TRUE, sep = ",")
      colnames(datatable)[1] <- str_replace(colnames(datatable)[1], '\\w+_', 'name_')
      dt <- melt.data.table(datatable,
                            id.vars = "name_id",
                            measure.vars = colnames(datatable)[2: ncol(datatable)],
                            variable.name = "variable", value.name = "estimate")
      dt[, `:=` (run = run.dir[r], 
                 year = str_extract(variable, "\\d+$"), 
                 attribute = str_extract(variable, "^\\w+(?=_tod)"),
                 tod_id = str_extract(variable, "(?<=tod_)\\d{1}"))]
      df <- rbindlist(list(df, dt), use.names = TRUE, fill = TRUE)
    }
  }
  df[, variable := NULL]
  return(df)
}

calc.delta <- function() {
  jlu <- read.xlsx(file.path(data.dir, "cities.xlsx")) %>% as.data.table
  lu <- jlu[, .(city_id, county_id, city_name)]
  dt <- compile.tbl.supp.tod(file.regexp, allruns, run.dir, ind.extension)
  dtc <- dcast.data.table(dt, name_id + run + attribute + tod_id ~ paste0("yr", year), value.var = "estimate")   # cast year
  dtc[, delta := get(eval(years.col[2]))-get(eval(years.col[1]))]
  dtc[lu, on = c("name_id" = "city_id"), `:=` (county_id = i.county_id, city_name = i.city_name)]
}

calc.by.juris <- function() {
  dt <- calc.delta()
  dt[, aagr := (((get(eval(years.col[2]))/get(eval(years.col[1])))^(1/(fcast.yrs[2]-fcast.yrs[1]))) - 1)
        ][is.nan(aagr), aagr := 0
          ][, county_id := as.character(county_id)]
  setnames(dt, "city_name", "jurisdiction")
  dt[, county_name := switch(county_id, "33" = "King", "35" = "Kitsap", "53" = "Pierce", "61" = "Snohomish"), by = county_id]
  cols <- c("run", "name_id", "county_name", "jurisdiction", "attribute", "tod_id", years.col[1], years.col[2], "delta", "aagr")
  t <- dt[, ..cols][order(attribute, county_name, jurisdiction)]
}

calc.by.cnty <- function() {
  dt <- calc.delta()
  sdcols <- c(years.col, "delta")
  cdt <- dt[, lapply(.SD, sum), .SDcols = sdcols, by = .(run, county_id, attribute, tod_id)]
  cdt[, aagr := (((get(eval(years.col[2]))/get(eval(years.col[1])))^(1/(fcast.yrs[2]-fcast.yrs[1]))) - 1)
      ][is.nan(aagr), aagr := 0
        ][, county_id := as.character(county_id)]
  cdt[, jurisdiction := switch(county_id, "33" = "King County", "35" = "Kitsap County", "53" = "Pierce County", "61" = "Snohomish County"), by = county_id]
  cdt[, county_name := switch(county_id, "33" = "King", "35" = "Kitsap", "53" = "Pierce", "61" = "Snohomish"), by = county_id]
  cdt[, name_id := switch(county_name,  "King" = "53033", "Kitsap" = "53035", "Pierce" = "53053", "Snohomish" = "53061"), by = county_name]
  cols <- c("run", "name_id", "county_name", "jurisdiction", "attribute", "tod_id", years.col[1], years.col[2], "delta", "aagr")
  t <- cdt[, ..cols][order(attribute, county_name)] 
  
}

jdt <- calc.by.juris()
cdt <- calc.by.cnty()
dt <- rbindlist(list(cdt, jdt), use.names = T)

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