# Indicator 16: Allocation of population and employment

library(data.table)
library(openxlsx)

if(!exists("set.globals") || !set.globals) {
  script.dir <- "/Users/hana/R/vision2050indicators/full_set" 
  setwd(script.dir)
  source("settings.R")
  source("functions.R")
}

out.file.nm <- settings$alloc$out.file.nm

geos <- c("city", "grid")
#geos <- "grid"
attributes <- c("population", "employment", "activity_units")
ind.extension <- ".csv"

cities <- read.xlsx(file.path(data.dir, "cities_rgs.xlsx"))
file.name.actual <- list(fips_rgs = file.path(data.dir, "Control-Totals-STC.xlsx"))
file.name.actual$city <- file.name.actual$fips_rgs

read.actual.fips_rgs <- function(file) {
  inds <- list(POP = c("population", "2017"), JOBS = c("employment", "2016"))
  alldf <- NULL
  for(ind in names(inds)) {
    df <- read.xlsx(file, sheet = ind, startRow = 2)
    df <- df[, c("city_id", inds[[ind]][2])] %>% merge(cities[, c("city_id", "fips_rgs_proposed_id")]) %>% data.table 
    setnames(df, inds[[ind]][2], "base_year")
    df <- df[, .(estimate = sum(base_year)), by = .(fips_rgs_id)][, indicator := inds[[ind]][1]][, year := byr.col]
    alldf <- rbind(alldf, df)
  }
  au <- alldf[, .(estimate = sum(estimate)), by = .(fips_rgs_id)]
  alldf <- rbind(alldf, au[, indicator := "activity_units"][, year := byr.col])
  setnames(alldf, "fips_rgs_id", "name_id")
  return(alldf)
}

read.actual.city <- function(file) {
  inds <- list(POP = c("population", "2017"), JOBS = c("employment", "2016"))
  alldf <- NULL
  for(ind in names(inds)) {
    df <- read.xlsx(file, sheet = ind, startRow = 2)
    df <- df[, c("city_id", inds[[ind]][2])] %>% data.table 
    setnames(df, inds[[ind]][2], "base_year")
    df <- df[, .(estimate = sum(base_year)), by = .(city_id)][, indicator := inds[[ind]][1]][, year := byr.col]
    alldf <- rbind(alldf, df)
  }
  au <- alldf[, .(estimate = sum(estimate)), by = .(city_id)]
  alldf <- rbind(alldf, au[, indicator := "activity_units"][, year := byr.col])
  setnames(alldf, "city_id", "name_id")
  return(alldf)
}

export.allocation.fips_rgs <- function(data, ...) {
  write.xlsx(data, file.path(out.dir, paste0(out.file.nm, "_fips_rgs_", Sys.Date(), ".xlsx")))
}

export.allocation.city <- function(data, ...) {
  juris <- fread(file.path(data.dir, "Juris_Reporting.csv"))
  setnames(juris, "CityID", "name_id")
  tbls <- NULL
  for(scenario in names(data)) {
    df <- copy(data[[scenario]])
    df <- merge(df, juris[, .(name_id, county, RG_Proposed)], by = "name_id")
    tbl <- dcast(df[,.(delta, county, RG_Proposed, indicator)], county + indicator ~ RG_Proposed, 
                 value.var = "delta", fun.aggregate = sum)
    tbl[ , Total := CitiesTowns + Core + HCT + Metro + Rural + UU]
    tbls[[scenario]] <- tbl
  }
  write.xlsx(tbls, file.path(out.dir, paste0(out.file.nm, "_city_fips_", Sys.Date(), ".xlsx")))
}

export.allocation.grid <- function(data, geo.id) {
  for(scenario in names(data)) {
    df <- copy(data[[scenario]])
    df[, run := NULL]
    df <- dcast(df, name_id ~ indicator, value.var = "delta" )
    setnames(df, "name_id", geo.id)
    write.csv(df, file.path(out.dir.maps, paste0(out.file.nm, "_grid_", scenario, "_", Sys.Date(), ".csv")),
              row.names = FALSE)
  }
}


for(geo in geos) {
  cat("\nComputing indicator 16 for ", geo, " geography")
  geo.id <- paste0(geo, "_id")
  alldata <- compile.tbl(geo, allruns, run.dir, attributes, ind.extension)
  dfm <- data.table::melt(alldata,
                          id.vars = c("name_id", "run", "indicator"),
                          measure.vars = grep("yr", colnames(alldata), value = TRUE),
                          variable.name = "year", value.name = "estimate")
  
  if(geo != "grid") { # replace modelled with actual
    actual <- do.call(paste0("read.actual.", geo), list(file.name.actual[[geo]]))
    dfm <- dfm[year != byr.col,] # remove modelled current year
    # replace with actual for all runs
    for(r in unique(dfm$run)) 
      dfm <- rbind(dfm, actual[, run := r])
  }
  dfm <- dfm[year %in% years.to.keep]
  
  # add military and GQ
  milgq <- compile.mil.gq(geo.id)
  dfmmgq <- dfm[milgq, estimate := estimate + i.estimate, on = c("name_id", "indicator", "year")]
  
  # compute difference between end year and 2017
  dfmmgq[dfmmgq[year == byr.col], delta := round(estimate - i.estimate), on = c("name_id", "run", "indicator")]
  dfmmgq <- dfmmgq[year == years.col][, year := NULL][, estimate := NULL]
  
  # loop through each run
  dlist <- NULL
  for (r in 1:length(run.dir)) {
    t <- dfmmgq[run == run.dir[r], ]
    setcolorder(t, c("indicator", "run", "delta"))
    dlist[[names(run.dir[r])]] <- t
  }
  # export
  do.call(paste0("export.allocation.", geo), list(dlist, geo.id))
  cat("\n")
}



