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

cat("\nComputing indicator 30, jobs-pop in tod areas\n")
out.file.nm <- settings$pjta$out.file.nm 


# general -----------------------------------------------------------------

years.col <- paste0("yr", c(byr, years))

# format run names for table
runs <- lapply(run.dir, function(x) unlist(strsplit(x,"[.]"))[[1]]) %>% unlist

counties <- c("King", "Kitsap", "Pierce", "Snohomish")
names(counties) <- c("33", "35", "53", "61")

attributes <- c("employment", "population")
ind.extension <- ".csv" 


# functions ----------------------------------------------------------

compile.control.totals <- function(workbook) {
  wb <- loadWorkbook(file.path(data.dir, workbook))
  scenarios <- names(wb)
  dt <- NULL
  for (ascenario in scenarios) {
    t <- NULL
    t <- read.xlsx(wb, sheet = ascenario) %>% as.data.table
    t[, scenario := ascenario]
    ifelse(is.null(dt), dt <- t, dt <- rbind(dt, t))
  }
  dtm <- melt.data.table(dt, id.vars = c("County", "scenario"), measure.vars = colnames(dt)[grep("17|50", colnames(dt))], value.name = "estimate")
  dtm[, `:=` (geog = "ct", indicator = str_extract(variable, "[[:alpha:]]+"), year = str_extract(variable, "(\\d+-)?\\d+$"))
      ][, indicator := switch(indicator, "Pop" = "population", "Emp" = "employment"), by = indicator
        ][, year := switch(year, "17" = "2017", "50" = "2050", "17-50" = "2017-2050"), by = year]
  df <- dtm[, Countychr := as.character(County)]
  df[, Countyname := switch(Countychr, "33" = "King", "35" = "Kitsap", "53" = "Pierce", "61" = "Snohomish", "99" = "Region"), by = Countychr]
  df[, `:=` (variable = NULL, Countychr = NULL)]
  d <- dcast.data.table(df, Countyname + County + scenario + indicator ~ geog + year, value.var = "estimate")
}

compile.tod.baseyear.actuals <- function(popfilename, empfilename) {
  pop <- read.xlsx(file.path(data.dir, popfilename)) %>% as.data.table
  pop.cnty <- pop[bSecField > 0, lapply(.SD, sum), .SDcols = c("splitblkTotpop"), by = .(County = bCOUNTY)
                  ][, .(County, estimate = splitblkTotpop)]
  pop.cnty <- rbind(pop.cnty, cbind("County" = "Region Total", pop.cnty[, lapply(.SD, sum), .SDcols = "estimate"]))
  pop.cnty[, indicator := "population"]
  
  emp <- read.xlsx(file.path(data.dir, empfilename)) %>% as.data.table
  emp.cnty <- emp[, .(County, estimate = Total.Jobs.in.TOD.Areas)][, indicator := "employment"]
  
  df <- rbindlist(list(pop.cnty, emp.cnty), use.names = TRUE)
  df[County == "Region Total", County := "Region"]
} 

query.military <- function(milfilename, geos, yrs) {
  mil <- fread(file.path(data.dir, milfilename))
  mil <- mil[!is.na(ParcelID)]
  enlist.lu <- as.data.table(enlist.lu)
  milgeo <- mil[enlist.lu, on = c("Base", "Zone", "ParcelID" = "parcel_id")]
  df <- melt.data.table(milgeo, id.vars = geos, measure.vars = grep("^\\d+", colnames(milgeo)), variable.name = "year", value.name = "enlist_estimate")
  df <- df[year %in% yrs, ][, lapply(.SD, sum), .SDcols = "enlist_estimate", by = c(geos, "year")]
}

query.gq <- function(gqfilename, geos, yrs) {
  gq <- read.xlsx(file.path(data.dir, gqfilename)) %>% as.data.table
  df <- melt.data.table(gq, id.vars = geos, measure.vars = grep("^\\d+", colnames(gq)), variable.name = "year", value.name = "gq")
  df <- df[year %in% yrs, ][, lapply(.SD, sum), .SDcols = "gq", by = c(geos, "year")][!is.na(get(eval(geos[1]))),]
}

gq <- query.gq("group_quarters_geo.xlsx", c("tod_id"), c("2017", "2050"))
mil.df <- query.military(enlist.mil.file.nm, c("tod_id"), c("2017", "2050"))

# actuals
ct <- compile.control.totals("2017_actuals_2050_controls.xlsx")
todbya <- compile.tod.baseyear.actuals("tod_est2017.xlsx", "tod_employment_2017_23.xlsx")
ct[todbya, on = c("Countyname" = "County", "indicator"), ("byr_tod_actual") := i.estimate]
ct <- ct[scenario %in% names(run.dir), ][, (paste0("byr_share_in_tod_actual")) := byr_tod_actual/get(paste0("ct_", byr))]
ct$run <- run.dir[ct$scenario]
ctdt <- ct[Countyname == "Region", ]


# transform county forecast -----------------------------------------------


compile.tod.by.county.files <- function(geog, allruns, run.dir, attributes, ind.extension) {
  counties <- c("kin", "kit", "pie", "sno")
  df <- NULL
  for (r in 1:length(run.dir)) { # for each run
    base.dir <- purrr::pluck(allruns, run.dir[r])
    for (c in 1:length(counties)) {
      for (a in 1:length(attributes)) { # for each attribute
        filename <- paste0(geog,'__',"table",'__', counties[c],'_', attributes[a], ind.extension)
        datatable <- fread(file.path(base.dir, "indicators", filename), header = TRUE, sep = ",")
        colnames(datatable)[1] <- str_replace(colnames(datatable)[1], '\\w+_', 'name_')
        dt <- melt.data.table(datatable, 
                              id.vars = "name_id",
                              measure.vars = colnames(datatable)[2: ncol(datatable)], 
                              variable.name = "variable", value.name = "estimate")
        dt[, `:=`(county = str_extract(variable, "^\\w{3}"),
                  year = str_extract(variable, "[[:digit:]]+"),
                  indicator = attributes[a],
                  run = run.dir[r])]
        dt[, countyname := switch(county, "kin" = "King", "kit" = "Kitsap", "pie" = "Pierce", "sno" = "Snohomish"), by = county]
        dt[, county := switch(county, "kin" = "33", "kit" = "35", "pie" = "53", "sno" = "61"), by = county]
        dt[, variable := NULL]
        df <- rbindlist(list(df, dt), use.names = TRUE, fill = TRUE)
      }
    }
  }
  return(df)
} 

create.tod.by.county.table <- function() {
  gq.cnty <- query.gq("group_quarters_geo.xlsx", c("county_id", "tod_id"), c("2017", "2050"))
  gq.cnty[, countyname := switch(as.character(county_id), "33" = "King", "35" = "Kitsap", "53" = "Pierce", "61" = "Snohomish"), by = county_id
          ][, indicator := "population"]
  mil.df.cnty <- query.military(enlist.mil.file.nm, c("county_id", "tod_id"), c("2017", "2050"))
  mil.df.cnty[, countyname := switch(as.character(county_id), "33" = "King", "35" = "Kitsap", "53" = "Pierce", "61" = "Snohomish"), by = county_id
              ][, indicator := "employment"]
  
  # county base
  gq.cnty2 <- gq.cnty[, lapply(.SD, sum), .SDcols = c("gq"), by = .(year, countyname, indicator)]
  mil.df.cnty2 <- mil.df.cnty[, lapply(.SD, sum), .SDcols = c("enlist_estimate"), by = .(year, countyname, indicator)]
  
  df.cnty <- compile.tbl('county', allruns, run.dir, attributes, ind.extension)
  base <- melt.data.table(df.cnty, 
                          id.vars = c("name_id", "indicator", "run"), 
                          measure.vars = grep("^yr\\d+", colnames(df.cnty)), variable.name = "year", value.name = "base")
  df.base <- base[year %in% years.col,
                  ][, year := str_extract(year, "\\d+")
                    ][, countyname := switch(as.character(name_id), "33" = "King", "35" = "Kitsap", "53" = "Pierce", "61" = "Snohomish"), by = name_id]
  df.base[gq.cnty2, on = c("countyname", "year", "indicator"), gq := i.gq][is.na(gq), gq := 0]
  df.base[mil.df.cnty2, on = c("countyname", "year", "indicator"), enlist := i.enlist_estimate][is.na(enlist), enlist := 0]
  df.base[, base_plus := base + gq + enlist]
  dt.base <- df.base[, .(countyname, indicator, run, year, base = base_plus)]
  
  # county tod
  df <- compile.tod.by.county.files('tod', allruns, run.dir, attributes, ind.extension)
  dt <- df[year %in% c(byr, years)]
  dt[gq.cnty, on = c("name_id" = "tod_id", "countyname", "year", "indicator"), gq := i.gq][is.na(gq), gq := 0]
  dt[mil.df.cnty, on = c("name_id" = "tod_id", "countyname", "year", "indicator"), enlist := i.enlist_estimate][is.na(enlist), enlist := 0]
  dt[, estimate_plus := estimate + gq + enlist]
  dt.tod <- dt[name_id != 0, 
               ][, .(name_id, county, countyname, year, run, indicator, estimate = estimate_plus)
                 ][, lapply(.SD, sum), .SDcols = c("estimate"), by = .(county, countyname, year, run, indicator)]
  dt.tod[dt.base, on = c("indicator", "run", "year", "countyname"), base := i.base]
  dt.tod.cast <- dcast.data.table(dt.tod, countyname + run + indicator ~ year, value.var = c("estimate", "base"))
  delta.expr <- parse(text = paste0("delta_tod := estimate_", years, " - estimate_", byr))
  deltareg.expr <- parse(text = paste0("delta_base := base_", years, " - base_", byr))
  dt.tod.cast[, eval(delta.expr)][, eval(deltareg.expr)][, delta_share_in_tod := delta_tod/delta_base]
  setnames(dt.tod.cast, paste0("estimate_", byr), "byr_tod")
  setnames(dt.tod.cast, paste0("base_", byr), "byr_base")
  setnames(dt.tod.cast, paste0("estimate_", years), paste0("yr", years, "_tod"))
  setnames(dt.tod.cast, paste0("base_", years), paste0("yr", years, "_base"))
  ct.cnty <- ct[Countyname != "Region",]
  dt.tod.cnty <- ct.cnty[dt.tod.cast, on = c("Countyname" = "countyname", "run", "indicator")]
  dt.tod.cnty[, (paste0(years.col[2], "_share_in_tod")) := (byr_tod_actual + delta_tod)/(get(paste0("ct_", byr)) + delta_base)]
}


# Regional Totals ---------------------------------------------------------

mil.df2 <- mil.df[, .(enlist_estimate = sum(enlist_estimate)), by = year]
gq2 <- gq[, .(gq = sum(gq)), by = year]

# include enlisted personnel and GQ
df.cnty <- compile.tbl('county', allruns, run.dir, attributes, ind.extension)
region.fcast <- df.cnty[, lapply(.SD, sum), .SDcols = years.col, by = c("indicator", "run")]
region <- melt(region.fcast, id.vars = c("indicator", "run"), variable.name = "year", value.name = "estimate")
region$year <- gsub("yr", "", region$year)
region[gq2, gq := i.gq, on = "year"]
region[mil.df2, enlist := i.enlist_estimate, on = "year"]
region[indicator == "population", region := estimate + gq]
region[indicator == "employment", region := estimate + enlist]


# transform regional forecast ---------------------------------------------

alldata <- compile.tbl('tod', allruns, run.dir, attributes, ind.extension)

df2 <- melt.data.table(alldata,
                       id.vars = c("name_id", "run", "indicator"),
                       measure.vars = grep("yr", colnames(alldata), value = TRUE),
                       variable.name = "year", value.name = "estimate")
df3 <- df2[year %in% years.col]
df3$year <- gsub("yr", "", df3$year)

# add GQ to forecast population df
df4.gq <- df3 %>%
  left_join(gq, by = c("name_id" = "tod_id", "year")) %>%
  replace_na(list(gq = 0)) %>%
  left_join(mil.df, by = c("name_id" = "tod_id", "year")) %>% 
  replace_na(list(enlist_estimate = 0)) %>%
  as.data.table()
df4.gq2 <- copy(df4.gq)
df4.gq2 <- df4.gq2[name_id > 0] # select only TODs 
df4.gq2[indicator == "population", estimate_w_milgq := estimate + gq]
df4.gq2[indicator == "employment", estimate_w_milgq := estimate + enlist_estimate]
df4.gq2 <- df4.gq2[, .(estimate_w_milgq = sum(estimate_w_milgq)), by = .(run, indicator, year)]
df4.gq2[region, base := i.region, on = c("indicator", "year", "run")]

setnames(df4.gq2, "estimate_w_milgq", "estimate")
df6 <- dcast(df4.gq2, run + indicator ~ year, value.var = c("estimate", "base"))
delta.expr <- parse(text = paste0("delta_tod := estimate_", years, " - estimate_", byr))
deltareg.expr <- parse(text = paste0("delta_base := base_", years, " - base_", byr))
df6[, eval(delta.expr)][, eval(deltareg.expr)]

sdcols <- c(2017, years)

df7.cast <- copy(df6)
df7.cast[, delta_share_in_tod := delta_tod/delta_base]
# rename column to be compatible with the previous version
setnames(df7.cast, paste0("estimate_", byr), "byr_tod")
setnames(df7.cast, paste0("base_", byr), "byr_base")
setnames(df7.cast, paste0("estimate_", years), paste0("yr", years, "_tod"))
setnames(df7.cast, paste0("base_", years), paste0("yr", years, "_base"))

# join ctdt & modeled dt; calc share_in_tod_2050
dt <- ctdt[df7.cast, on = c("run", "indicator")]
dt[, (paste0(years.col[2], "_share_in_tod")) := (byr_tod_actual + delta_tod)/(get(paste0("ct_", byr)) + delta_base)]

dt.cnty <- create.tod.by.county.table() # county geographies
dt.all <- rbindlist(list(dt.cnty, dt), use.names = TRUE) # bind all geographies

# loop through each run
dlist <- NULL
fincols <- c("tod", "base")
for (r in 1:length(run.dir)) {
  t <- NULL
  t <- dt.all[run == run.dir[r], ][order(-indicator)]
  setcolorder(t, c("Countyname",
                   "County",
                   "run",
                   "scenario",
                   "indicator",
                   grep("^ct", colnames(t), value = TRUE),
                   grep("tod_actual$", colnames(t), value = TRUE),
                   paste0("byr_", fincols),
                   paste0("yr", years, "_", fincols),
                   paste0("yr", years, "_share_in_tod"),
                   paste0("delta_", fincols), "delta_share_in_tod"))
  colnames(t)[grep("byr", colnames(t))] <- str_replace_all(colnames(t)[grep("byr", colnames(t))], "byr", paste0("yr", byr))
  dlist[[names(run.dir[r])]] <- t
}


# export ------------------------------------------------------------------

write.xlsx(dlist, file.path(out.dir, paste0(out.file.nm, "_", Sys.Date(), ".xlsx")))

setwd(curr.dir)
# set.globals <- FALSE