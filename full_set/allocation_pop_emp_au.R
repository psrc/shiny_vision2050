library(data.table)
library(openxlsx)

if(!exists("set.globals") || !set.globals) {
  script.dir <- "/Users/hana/R/vision2050indicators/full_set" 
  setwd(script.dir)
  source("settings.R")
  source("functions.R")
}
source("all_runs.R")

years <- c(2050) # only one year

out.file.nm <- "16_allocation_pop_emp_au"

geos <- c("fips_rgs", "grid")
geos <- c("fips_rgs")
attributes <- c("population", "employment", "activity_units")
ind.extension <- ".csv"

cities <- read.xlsx(file.path(data.dir, "cities_rgs.xlsx"))
file.name.actual <- list(fips_rgs = "/Volumes/DataTeam/Projects/V2050/SEIS/Data_Support/script_input/Control-Totals-STC.xlsx")

read.actual.fips_rgs <- function(file) {
  inds <- list(POP = c("population", "2017"), JOBS = c("employment", "2016"))
  alldf <- NULL
  for(ind in names(inds)) {
    df <- read.xlsx(file, sheet = ind, startRow = 2)
    df <- df[, c("city_id", inds[[ind]][2])] %>% merge(cities[, c("city_id", "fips_rgs_id")]) %>% data.table 
    setnames(df, inds[[ind]][2], "yr2017")
    df <- df[, .(estimate = sum(yr2017)), by = .(fips_rgs_id)][, indicator := inds[[ind]][1]][, year := "yr2017"]
    alldf <- rbind(alldf, df)
  }
  au <- alldf[, .(estimate = sum(estimate)), by = .(fips_rgs_id)]
  alldf <- rbind(alldf, au[, indicator := "activity_units"][, year := "yr2017"])
  return(alldf)
}


for(geo in geos) {
  alldata <- compile.tbl(geo)
  actual <- do.call(paste0("read.actual.", geo), list(file.name.actual[[geo]]))
  dfm <- data.table::melt(alldata,
                         id.vars = c("name_id", "run", "indicator"),
                         measure.vars = grep("yr", colnames(alldata), value = TRUE),
                         variable.name = "year", value.name = "estimate")
  dfm <- dfm[year %in% paste0("yr", years)]
  mil <- get.military(paste0(geo, "_id"))
  # TODO: replace 2017 modeled b y actual and add military
  #dfm[mil , .(estimate_m = estimate + i.enlist_estimate), on = c(geo, "year")]
  #for (r in 1:length(run.dir)) {
    
  #}
}



set.globals <- FALSE
