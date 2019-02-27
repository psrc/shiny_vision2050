# This script will read-in actual 2017 population estimates and 2050 scenario job and housing unit estimates

library(data.table)
library(openxlsx)
library(tidyverse)

# settings --------------------------------------------------------------

if(!exists("set.globals") || !set.globals) {
  curr.dir <- getwd()
  this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(this.dir)
  source("settings.R")
  source("functions.R")
}

cat("\nComputing jobs and housing data by city\n")
out.file.nm <- settings$jobs_hu_by_city$out.file.nm 


# general -----------------------------------------------------------------

years.col <- paste0("yr", fcast.yrs)
act.yrs <- c(2017)
# act.yrs.cols <- paste0("act", act.yrs)
# cols1 <- c(tail(act.yrs.cols, -1), act.yrs.cols[length(act.yrs.cols)])
# cols2 <- c(head(act.yrs.cols, -1), act.yrs.cols[1])
# allcols1 <- c(cols1, years.col[2])
# allcols2 <- c(cols2, years.col[1])

counties <- c("King", "Kitsap", "Pierce", "Snohomish")
names(counties) <- c("33", "35", "53", "61")

attributes <- c("employment", "residential_units")
ind.extension <- ".csv" 


# read actuals ------------------------------------------------------------

# actuals by city-block
hu.actuals.juris <- function() {
  lu <- read.xlsx(file.path(data.dir, "block_splits_juris_lu.xlsx")) %>% as.data.table # read cities lookup
  bs <- read.xlsx(file.path(data.dir, "block_splits_est2017.xlsx")) %>% as.data.table
  bs[, splitblkHU := get(eval(paste0("HU", act.yrs)))*rxHHpop]
  bsagg <- bs[, lapply(.SD, sum), .SDcols = "splitblkHU", by = c("bCOUNTY", "bSecField")]
  bsagg[, county_id := switch(bCOUNTY, "King" = 33, "Kitsap" = 35, "Pierce" = 53, "Snohomish" = 61), by = bCOUNTY]
  t <- merge(lu, bsagg, by.x = c("block_splits_jurisdiction", "county_id"), by.y = c("bSecField", "county_id"), all.x = TRUE)
  t[is.na(splitblkHU), splitblkHU := 0]
}


# forecasts ---------------------------------------------------------------

# scenarios
forecasts <- function() {
  mdt <- query.military(enlist.mil.file.nm, "city_id", 2050)[, .(city_id, enlist_estimate)]
  dt <- compile.tbl("city", allruns, run.dir, attributes, ind.extension)
  cols <- c("name_id", "indicator", "run", paste0("yr", fcast.yrs[2]))
  t <- dt[, ..cols]
  tc <- dcast.data.table(t, name_id + run ~ indicator, value.var = paste0("yr", fcast.yrs[2]))
  tm <- merge(tc, mdt, by.x =c("name_id"), by.y = c("city_id"), all.x = TRUE)
  tm[is.na(enlist_estimate), enlist_estimate := 0]
}

act <- hu.actuals.juris()[, .(city_id, county_id, county = bCOUNTY, rg_proposed, rg_existing, city_name, residential_units_act = splitblkHU)]
ft <- forecasts()

dt <- merge(act, ft, by.x = "city_id", by.y = "name_id")
setnames(dt, 
         c("residential_units_act", attributes, "enlist_estimate"), 
         c(paste0("residential_units_", act.yrs), paste0(attributes, "_", fcast.yrs[2]), paste0("enlisted_", fcast.yrs[2])))


dlist <- NULL
for (r in 1:length(run.dir)) {
  t <- NULL
  t <- dt[run == run.dir[r],]
  t[, scenario := names(run.dir[r])][, county := NULL]
  setcolorder(t,
              c(colnames(dt)[str_which(colnames(dt), "_id$")],
              # "county",
              colnames(dt)[str_which(colnames(dt), "^rg_")],
              "city_name", "run", "scenario",
              paste0("residential_units_", act.yrs), paste0(rev(attributes), "_", fcast.yrs[2]), paste0("enlisted_", fcast.yrs[2])))
  dlist[[names(run.dir[r])]] <- t
}

# export ------------------------------------------------------------------

write.xlsx(dlist, file.path(out.dir, paste0(out.file.nm, "_", Sys.Date(), ".xlsx")))

setwd(curr.dir)
