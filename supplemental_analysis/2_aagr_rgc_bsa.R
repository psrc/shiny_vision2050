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

cat("\nComputing metric 2, Average Annual Growth Rates by Alternative: RGCs and Buffered Station Areas\n")
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
  # fcast.emp.j <- compile.fcast.emp.tod()
  # fcast.pop.j <- compile.fcast.pop.tod()
  # act.pop.j <- compile.pop.actuals.juris()
  # act.emp.j <- compile.emp.actuals.juris()
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
}

compile.juris.calc.aagr.by.juris <- function() {
  dt <- compile.juris.calc.delta()
  aagrnames <- paste0("aagr_", allcols1, "-", allcols2)
  start.year.col <- allcols2
  end.year.col <- allcols1
  start.year.num <- str_extract(start.year.col, "\\d+") %>% lapply(as.numeric) %>% unlist
  end.year.num <- str_extract(end.year.col, "\\d+") %>% lapply(as.numeric) %>% unlist
  dtaa <- dt[, (aagrnames) := mapply(function(a, b, c, d) ((.SD[[b]]/.SD[[a]])^(1/(d-c))-1), 
                                     start.year.col, end.year.col, start.year.num, end.year.num, SIMPLIFY =  F)]
}

compile.juris.calc.aggr.by.cnty <- function() {
  dt <- compile.juris.calc.delta()
  sdcols <- c(colnames(dt)[str_which(colnames(dt), "^act|^yr|^delta")])
  bycols <- c(colnames(dt)[str_which(colnames(dt), "^county")], "indicator", "Type", "run")
  t <- dt[, lapply(.SD, sum), .SDcols = sdcols, by = bycols][order(run, indicator)]
  aagrnames <- paste0("aagr_", allcols1, "-", allcols2)
  start.year.col <- allcols2
  end.year.col <- allcols1
  start.year.num <- str_extract(start.year.col, "\\d+") %>% lapply(as.numeric) %>% unlist
  end.year.num <- str_extract(end.year.col, "\\d+") %>% lapply(as.numeric) %>% unlist
  dtaa <- t[, (aagrnames) := mapply(function(a, b, c, d) ((.SD[[b]]/.SD[[a]])^(1/(d-c))-1), 
                                     start.year.col, end.year.col, start.year.num, end.year.num, SIMPLIFY =  F)]
}

compile.pop.actuals.rgc <- function() {
  # need blk to rgc corresp
}

compile.emp.actuals.rgc <- function() {
  # need blk to rgc corresp
}

compile.fcast.pop.rgc <- function() {
  # add gq
}

compile.fcast.emp.rgc <- function() {
  dt <- compile.tbl("growth_center", allruns, run.dir, attributes, ind.extension)
  lu <- read.centers.lu()[, .(growth_center_id, name, county_name, county_juris, county_code)]
  cols <- c("run", "name_id", "indicator", years.col)
  cdt <- dt[indicator == "employment" & (name_id  >= 500 & name_id < 600), ..cols]
  cdt[lu, on = c("name_id" = "growth_center_id"), `:=` (name = i.name, 
                                                        county_name = i.county_name, county_juris = i.county_juris, county_code = i.county_code)]
}

# test <- compile.juris.calc.aagr.by.juris()
test <- compile.juris.calc.aggr.by.cnty()