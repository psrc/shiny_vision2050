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

cat("\nComputing metric 2, Average Annual Growth Rates by Alternative: RGCs and Jurisdictions with CSAs\n")
out.file.nm <- settings$aagr_rgc_bsa$out.file.nm 


# general -----------------------------------------------------------------

years.col <- paste0("yr", fcast.yrs)
act.yrs <- c(2000, 2010, 2017)
act.yrs.cols <- paste0("act", act.yrs)
cols1 <- c(tail(act.yrs.cols, -1), act.yrs.cols[length(act.yrs.cols)])
cols2 <- c(head(act.yrs.cols, -1), act.yrs.cols[1])
allcols1 <- c(cols1, years.col[2])
allcols2 <- c(cols2, years.col[1])

counties <- c("King", "Kitsap", "Pierce", "Snohomish")
names(counties) <- c("33", "35", "53", "61")

attributes <- c("employment", "population")
ind.extension <- ".csv" 


# functions ----------------------------------------------------------


# CSAs (juris) ------------------------------------------------------------


compile.pop.actuals.juris <- function() {
  totpop <- query.ofm(ofm.file.nm, "POP", c("2000", "2010", "2017"))[, geoid10 := NULL]
  clu <- read.cities.lu()
  bj.lu<- read.dbf(file.path(data.dir, "HCT_blck_40_Juris.dbf")) %>% as.data.table # read block-juris dbf
  bj.lu[, GEOID10 := as.character(GEOID10)]
  totpop[bj.lu, on = c("GEOID10"), `:=` (county_name = i.CNTYNAME, city_id = i.CityID)] # join ofm + lu #gis_juris = i.JURIS, 
  pjdt <- totpop[!is.na(city_id)][, lapply(.SD, sum), .SDcols = "estimate", by = .(countyfp10, county_name, city_id, year, indicator = attribute)] #gis_juris, 
  pjdt[clu, on = "city_id", `:=` (county_id = i.county_id, city_name = i.city_name, county_juris = i.county_juris, county_code = i.county_code)]
  pjdt[indicator == "POP", `:=` (indicator = "population", Type = "Jurisdiction")]
  pjdtc <- dcast.data.table(pjdt, 
                            county_name + county_juris + county_code + city_id + city_name + indicator + Type ~ paste0("act", year), 
                            value.var = "estimate")
}

compile.emp.actuals.juris <- function() {
  dt <- read.actual.emp("Jurisdiction")
  lu <- read.xlsx(file.path(data.dir, "usim_gis_lu_cities.xlsx")) %>% as.data.table # join to usim cities lu
  dt[lu, on = c("Jurisdiction" = "gis_jurisdiction", "County" = "county_name"), `:=` (city_id = i.city_id, city_name = i.city_name, county_name = i.county_name, 
                                                            county_juris = i.county_juris, county_code = i.county_code)]
  cols <- c(colnames(dt)[str_which(colnames(dt), "^county|^city|^act")], "indicator", "Type")
  dt[, ..cols]
}

compile.fcast.emp.tod <- function() { # by juris
  lu <- read.cities.lu()
  lu.cols <- colnames(lu)[str_which(colnames(lu), "^[city|county]")] 
  clu <- lu[, ..lu.cols]
  
  file.regexp <-"city.*employment_tod_\\d\\.csv" 
  dt <- compile.tbl.supp.tod(file.regexp, allruns, run.dir, ind.extension)
  cdt <- dt[, lapply(.SD, sum), .SDcols = "estimate", by = .(name_id, run, year, attribute)]
  dtc <- dcast.data.table(cdt, name_id + run + attribute ~ paste0("yr", year), value.var = "estimate")
  dtc[clu, on = c("name_id" = "city_id"), `:=` (city_name = i.city_name, 
                                                county_name = i.county_name, county_juris = i.county_juris, county_code = i.county_code)]
}

compile.fcast.pop.tod <- function() { # by juris
  # cities lu
  lu <- read.cities.lu()
  lu.cols <- colnames(lu)[str_which(colnames(lu), "^[city|county]")] 
  clu <- lu[, ..lu.cols]
  # block-juris lu
  bj.lu<- read.dbf(file.path(data.dir, "HCT_blck_40_Juris.dbf")) %>% as.data.table # read block-juris dbf
  bj.lu[, GEOID10 := as.character(GEOID10)]
  # need to add gq
  gqdt <- query.gq(gq.file.nm, c("hct_block_id", "census_block_id"), c("2017", "2050"))[hct_block_id == 1, ]
  gq <- dcast.data.table(gqdt, census_block_id ~ paste0("yr", year), value.var = "gq")
  gqbj <- gq[bj.lu, on = c("census_block_id" = "GEOID10")]
  gqm <- melt.data.table(gqbj, id.vars = c("CityID"), measure.vars = years.col, variable.name = "year", value.name = "gq")
  dtgq <- gqm[is.na(gq), gq := 0][, year := str_extract(year, "\\d+")][, lapply(.SD, sum), .SDcols = "gq", by = .(city_id = CityID, year)]
  
  file.regexp <-"city.*population_tod_\\d\\_hct_block.csv" 
  dt <- compile.tbl.supp.tod(file.regexp, allruns, run.dir, ind.extension)
  cdt <- dt[, lapply(.SD, sum), .SDcols = "estimate", by = .(name_id, run, year, attribute)]
  cdt[dtgq, on = c("name_id" = "city_id", "year" = "year"), gq := i.gq]
  cdt[is.na(gq), gq := 0][, base_plus := estimate + gq]
  t <- dcast.data.table(cdt, name_id + run + attribute ~ paste0("yr", year), value.var = "base_plus")
  t[clu, on = c("name_id" = "city_id"), `:=` (city_name = i.city_name, county_name = i.county_name, county_juris = i.county_juris, county_code = i.county_code)]
}

compile.juris.calc.delta <- function() {
  csa <- fread(file.path(data.dir, "cities_with_csas.csv"))
  act <- rbindlist(list(compile.pop.actuals.juris(), compile.emp.actuals.juris()), use.names = T)
  fcast <- rbindlist(list(compile.fcast.pop.tod(), compile.fcast.emp.tod()), use.names = T)
  jdt <- act[fcast, on = c("city_id" = "name_id", "indicator" = "attribute", "city_name", "county_name", "county_juris", "county_code")]
  jcols <- colnames(jdt)[str_which(colnames(jdt), "^act|^yr")]
  dt <- melt.data.table(jdt, id.vars = colnames(jdt)[!(colnames(jdt) %in% jcols)], measure.vars = jcols, variable.name = "year", value.name = "estimate")
  dt[is.na(estimate), estimate := 0]
  t <- dt[Type == "Jurisdiction"]
  cdt <- dcast.data.table(t, county_name + county_juris + county_code + city_id + city_name + indicator + Type + run ~ year, value.var = "estimate")
  
  deltanames <- paste0("delta_", allcols1, "-", allcols2)
  dtcalc <- cdt[, (deltanames) := mapply(function(x, y) (.SD[[x]]-.SD[[y]]), allcols1, allcols2, SIMPLIFY = F)]
  t <- dtcalc[city_name %in% csa$usim_jurisdiction,] # filter for only csa jurisdictions
}

compile.juris.calc.aagr.by.juris <- function() {
  t <- calc.aggr.by.subgeog(compile.juris.calc.delta())
  t[, `:=` (county_juris = NULL, county_code = NULL)]
  setnames(t, c("city_id", "city_name"), c("name_id", "name"))
  t[order(indicator, name)]
  return(t)
}

compile.juris.calc.aggr.by.cnty <- function() {
  t <- calc.aggr.by.cnty(compile.juris.calc.delta())
  setnames(t, c("county_juris", "county_code"), c("name", "name_id"))
  t[order(indicator, name)]
  return(t)
}



# RGCs --------------------------------------------------------------------


compile.pop.actuals.rgc <- function() {
  # centers lu
  clu <- read.xlsx(file.path(data.dir, "usim_gis_lu_centers.xlsx")) %>% as.data.table # join to usim cities lu#
  ctr.lu<- clu[growth_center_id >= 500 & growth_center_id < 600,]
  # blk to rgc corresp
  lu <- read.dbf(file.path(data.dir, "hctblock_rgc.dbf")) %>% as.data.table
  rlu <- lu[, .(GEOID10, NAME)][, lapply(.SD, as.character), .SDcols = c("GEOID10", "NAME")]
  totpop <- query.ofm(ofm.file.nm, "POP", c("2000", "2010", "2017"))[, geoid10 := NULL]
  totpop[rlu, on = c("GEOID10"), NAME := i.NAME]
  dt <- totpop[!is.na(NAME)
               ][, lapply(.SD, sum), .SDcols = c("estimate"), by = .(NAME, year, indicator = attribute)
                 ][indicator == "POP", `:=` (indicator = "population", Type = "RGC")]
  t <- dcast.data.table(dt, NAME + indicator + Type ~ paste0("act", year), value.var = "estimate") # cast years
  rt <- merge(t, ctr.lu, by.x = "NAME", by.y = "gis_center")
  rt[, NAME := NULL]
  setnames(rt, "growth_center_id", "name_id")
}

compile.emp.actuals.rgc <- function() {
  # need centers lu
  dt <- read.actual.emp("RGC")[, County := NULL]
  lu <- read.xlsx(file.path(data.dir, "usim_gis_lu_centers.xlsx")) %>% as.data.table # join to usim centers lu
  dt[lu, on = c("Jurisdiction" = "gis_center"), `:=` (name_id = i.growth_center_id, name = i.name, county_name = i.county_name, 
                                                                                      county_juris = i.county_juris, county_code = i.county_code)
     ][, Jurisdiction := NULL]

}

compile.fcast.pop.rgc <- function() {
  lu <- read.centers.lu()[growth_center_id >= 500 & growth_center_id < 600, .(growth_center_id, name, county_name, county_juris, county_code)]
  
  file.regexp <- "growth_center.*population_tod_\\d\\_hct_block.csv"
  dt <- compile.tbl.supp.tod(file.regexp, allruns, run.dir, ind.extension)
  rdt <- dt[name_id >= 500 & name_id < 600, lapply(.SD, sum), .SDcols = "estimate", by = .(name_id, run, year, attribute)] # roll up to rgc
  t <- lu[rdt, on = c("growth_center_id" = "name_id")]
  # add gq
  gqdt <- query.gq(gq.file.nm, c("growth_center_id"), c("2017", "2050"))[growth_center_id >= 500 & growth_center_id < 600]
  t[gqdt, on = c("growth_center_id", "year"), gq := i.gq]
  t[, base_plus := estimate + gq]
  rt <- dcast.data.table(t, growth_center_id + name + county_name + county_code + county_juris + run + attribute ~ paste0("yr", year), value.var = "base_plus")
  setnames(rt, c("growth_center_id", "attribute"), c("name_id", "indicator"))
}

compile.fcast.emp.rgc <- function() {
  dt <- compile.tbl("growth_center", allruns, run.dir, attributes, ind.extension)
  lu <- read.centers.lu()[, .(growth_center_id, name, county_name, county_juris, county_code)]
  cols <- c("run", "name_id", "indicator", years.col)
  cdt <- dt[indicator == "employment" & (name_id  >= 500 & name_id < 600), ..cols]
  cdt[lu, on = c("name_id" = "growth_center_id"), `:=` (name = i.name, 
                                                        county_name = i.county_name, county_juris = i.county_juris, county_code = i.county_code)]
}

compile.rgc.calc.delta <- function() {
  act <- rbindlist(list(compile.pop.actuals.rgc(), compile.emp.actuals.rgc()), use.names = T)
  fcast <- rbindlist(list(compile.fcast.pop.rgc(), compile.fcast.emp.rgc()), use.names = T)[, Type := "RGC"] # add Type == RGC
  rdt <- act[fcast, on = c("name_id", "indicator", "name", "county_name", "county_juris", "county_code", "Type")]
  rcols <- colnames(rdt)[str_which(colnames(rdt), "^act|^yr")]
  deltanames <- paste0("delta_", allcols1, "-", allcols2)
  dtcalc <- rdt[, (deltanames) := mapply(function(x, y) (.SD[[x]]-.SD[[y]]), allcols1, allcols2, SIMPLIFY = F)]
}

compile.rgc.calc.aagr.by.rgc <- function() {
  t <- calc.aggr.by.subgeog(compile.rgc.calc.delta())
  t[, `:=` (county_juris = NULL, county_code = NULL)]
  t[order(indicator, name)]
}

compile.rgc.calc.aggr.by.cnty <- function() {
  t <- calc.aggr.by.cnty(compile.rgc.calc.delta())
  setnames(t, c("county_juris", "county_code"), c("name", "name_id"))
  t[order(indicator, name)]
}


# Assembly ----------------------------------------------------------------

dt <- rbindlist(list(compile.rgc.calc.aggr.by.cnty(),
                     compile.rgc.calc.aagr.by.rgc(), 
                     compile.juris.calc.aggr.by.cnty(), 
                     compile.juris.calc.aagr.by.juris()), 
          use.names = T)

for (j in names(dt)[str_which(names(dt), "^aagr")]) set(dt, which(is.nan(dt[[j]])),j,0)
dt[Type == "Jurisdiction", Type := "CSA"]

# loop through each run
dlist <- NULL

for (r in 1:length(run.dir)) {
  t <- NULL
  t <- dt[run == run.dir[r], ]
  dlist[[names(run.dir[r])]] <- t
}


# export ------------------------------------------------------------------

write.xlsx(dlist, file.path(out.dir, paste0(out.file.nm, "_", Sys.Date(), ".xlsx")))


# Prep data for vizualization ---------------------------------------------

viz.prep.metric.2 <- function(dt) {
  csa <- fread(file.path(data.dir, "cities_with_csas.csv"))
  mcols <- colnames(dt)[str_which(colnames(dt), "^act|^yr|^delta|^aagr")]
  idcols <- setdiff(colnames(dt), mcols)
  calccols <- colnames(dt)[str_which(colnames(dt), "^delta|^aagr")]
  mdt <- melt.data.table(dt, id.vars = idcols, measure.vars = mcols, variable.name = "colname", value.name = "value")
  mdt[, `:=` (time_period = str_replace_all(colname, "[[:alpha:]]+|_", "") %>% str_trim(),
              value_type = str_extract(colname, "^[[:alpha:]]+"),
              dataset_type = str_extract(colname, "\\w+$") %>% str_extract("[[:alpha:]]+"))]
  mdt[, dataset_type := switch(dataset_type, "act" = "actual", "yr" = "forecast"), by = .(dataset_type)]
  mdt[, value_type := switch(value_type, "act" = "nominal", "yr" = "nominal"), by = .(value_type)]
  mdt$alternative[mdt$run] <- names(run.dir)
  t <- mdt[, .(Type, indicator, county_name, name_id, name, run, alternative, colname, time_period, value_type, dataset_type, value)]
}

dtviz <- viz.prep.metric.2(dt)
# write.csv(dtviz, file.path(out.dir, "tidy_format", paste0(out.file.nm, "_viz_", Sys.Date(), ".csv")), row.names = F)

setwd(curr.dir)
