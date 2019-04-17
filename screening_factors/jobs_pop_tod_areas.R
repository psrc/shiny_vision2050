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
  source("transform_actual_pop_emp.R")
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

compile.hct.baseyear.actuals <- function() {
  dtlist <- NULL
  attrs <- c("pop", "emp") 
  for (i in 1:length(attrs)) {
    t <- prep.actuals.hct.by.juris("county", attrs[i])
    tcnty <- t[year == 2017 & code != 0, lapply(.SD, sum), .SDcols = "estimate", by = .(County, indicator = attribute)
      ][, County := switch(County, "33" = "King", "35" = "Kitsap", "53" = "Pierce", "61" = "Snohomish"), by = "County"] 
    tsum <- tcnty[, lapply(.SD, sum), .SDcols = "estimate", by = .(indicator)][, County := "Region"] 
    tt <- rbindlist(list(tcnty, tsum), use.names = T)
    dtlist[[attrs[i]]] <- tt
  }
  
  dt <- rbindlist(dtlist, use.names = T)
  dt[, indicator := switch(indicator, "pop" = "population", "emp" = "employment"), by = "indicator"]
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

# todbya <- compile.tod.baseyear.actuals("tod_est2017.xlsx", "tod_employment_2017_23.xlsx") # original
todbya <- compile.hct.baseyear.actuals() # new hct (as the crow flies)

ct[todbya, on = c("Countyname" = "County", "indicator"), ("byr_tod_actual") := i.estimate]
# ct <- ct[scenario %in% names(run.dir), ][, (paste0("byr_share_in_tod_actual")) := byr_tod_actual/get(paste0("ct_", byr))] # original
ct <- ct[indicator != "AU" & scenario %in% names(run.dir), ][, (paste0("byr_share_in_tod_actual")) := byr_tod_actual/get(paste0("ct_", byr))]
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


# equity ------------------------------------------------------------------


compile.modeled.equity.base <- function() {
  # compile modeled equity data from region
  file.regexp <- paste0("(^minority|^poverty).*(employment|population)\\.csv")
  edf <- compile.tbl.equity(file.regexp, allruns, run.dir, ind.extension)
  edf[, equity := switch(name_id, `1` = paste0("non-", generic_equity), `2` = generic_equity), by = name_id
      ][, indicator := str_extract(variable, "^[[:alpha:]]+")] # create new field equity: 1 = non-minority/poverty, 2 = minority/poverty
  edt <- edf[year %in% c(byr, years)]
  # transform gq mil grouping by equity categories and add to forecast
  # mil
  emil <- query.military(enlist.mil.file.nm, c("minority_id", "poverty_id"), c("2017", "2050"))
  emil[, `:=` (minority_id = as.character(minority_id), poverty_id = as.character(poverty_id))]
  emil[, minority := switch(minority_id, "0" = "non-minority", "1" = "minority"), by = minority_id
       ][, poverty := switch(poverty_id, "0" = "non-poverty", "1" = "poverty"), by = poverty_id
         ][, indicator := "employment"]
  emildt <- melt.data.table(emil, 
                            id.vars = c("indicator", "enlist_estimate", "year"), 
                            measure.vars = c("minority", "poverty"), variable.name = "generic_equity", value.name = "equity")
  mildt <- emildt[, lapply(.SD, sum), .SDcols = c("enlist_estimate"), by = .(indicator, equity, year)]
  
  # gq
  egq <- query.gq("group_quarters_geo.xlsx", c("minority_id", "poverty_id"), c("2017", "2050"))
  egq[, `:=` (minority_id = as.character(minority_id), poverty_id = as.character(poverty_id))]
  egq[, minority := switch(minority_id, "0" = "non-minority", "1" = "minority"), by = minority_id
       ][, poverty := switch(poverty_id, "0" = "non-poverty", "1" = "poverty"), by = poverty_id
         ][, indicator := "population"]
  egqdt <- melt.data.table(egq, 
                            id.vars = c("indicator", "gq", "year"), 
                            measure.vars = c("minority", "poverty"), variable.name = "generic_equity", value.name = "equity")
  gqdt <- egqdt[, lapply(.SD, sum), .SDcols = c("gq"), by = .(indicator, equity, year)]
  
  edt[mildt, on = c("equity", "indicator", "year"), enlist := i.enlist_estimate][is.na(enlist), enlist := 0]
  edt[gqdt, on = c("equity", "indicator", "year"), gq := i.gq][is.na(gq), gq := 0]
  edt[, estimate_plus := estimate + enlist + gq]
  dt <- edt[, lapply(.SD, sum), .SDcols = c("estimate_plus"), by = .(indicator, equity, run, year)]
  setnames(dt, "estimate_plus", "base")
  return(dt)
} 

compile.tod.actuals.equity <- function() {
  # 2017 in tod actuals
  eqlu <- read.equity.lu()
  popfilename <- "tod_est2017.xlsx"
  empfilename <- "Tracts_TOD_Employment.xlsx"
  # pop
  pop <- read.xlsx(file.path(data.dir, popfilename)) %>% as.data.table
  pop[, `:=` (tGEOID10 = as.character(str_sub(GEOID10, start = 1, end = 11)), indicator = "population")]
  p <- pop[bSecField > 0, ][, lapply(.SD, sum), .SDcols = c("splitblkTotpop"), by = .(GEOID10 = tGEOID10, indicator)]
  setnames(p, "splitblkTotpop", "estimate")
  p[eqlu, on = c("GEOID10"), `:=` (minority = i.minority, poverty = i.poverty)]
  p2 <- melt.data.table(p, 
                        id.vars = c("GEOID10", "estimate", "indicator"), 
                        measure.vars = c("minority", "poverty"), variable.name = "generic_equity", value.name = "equity")
  pdt <- p2[, lapply(.SD, sum), .SDcols = c("estimate"), by = .(equity, indicator)]
  # emp (no mil in tods)
  emp <- read.xlsx(file.path(data.dir, empfilename)) %>% as.data.table
  e <- emp[, .(GEOID10, estimate = SumOfAllJobs17, In_TOD)
           ][In_TOD == "Yes", lapply(.SD, sum), .SDcols = "estimate", by = GEOID10
             ][, indicator := "employment"]
  e[eqlu, on = c("GEOID10"), `:=` (minority = i.minority, poverty = i.poverty)]
  e2 <- melt.data.table(e, 
                        id.vars = c("GEOID10", "estimate", "indicator"), 
                        measure.vars = c("minority", "poverty"), variable.name = "generic_equity", value.name = "equity")
  edt <- e2[, lapply(.SD, sum), .SDcols = c("estimate"), by = .(equity, indicator)]

  dt <- rbindlist(list(pdt, edt), use.names = TRUE)
  setnames(dt, "estimate", "byr_tod_actual")
}

compile.control.totals.equity <- function() {
  # create ct_byr
  pop <- "tract_population_households.csv"
  emp <- "Tract2017_AllJobs.xlsx"
  
  p <- fread(file.path(data.dir, pop))
  p2 <- p[, GEOID10 := as.character(GEOID10)][, .(GEOID10, population = POP2017)] # contains gq
  
  e <- read.xlsx(file.path(data.dir, emp), startRow = 2) %>% as.data.table
  e2 <- e[, `:=` (GEOID10 = as.character(GEOID10), employment = Employment + Uniformed.Military)][, .(GEOID10, employment)] # contains mil
  p2[e2, on = "GEOID10", employment := i.employment][is.na(employment), employment := 0]
  
  eqlu <- read.equity.lu() %>% as.data.table # join with min/pov lookup
  dt <- p2[eqlu, on = "GEOID10"][, .(GEOID10, population, employment, minority, poverty)]
  d <- melt.data.table(dt, 
                       id.vars = c("GEOID10", "population", "employment"), 
                       measure.vars = c("minority", "poverty"), variable.name = "generic_equity", value.name = "equity")
  d2 <- melt.data.table(d, 
                        id.vars = c("GEOID10", "equity"), 
                        measure.vars = c("population", "employment"), variable.name = "indicator", value.name = "ct_byr")
  d3 <- d2[, lapply(.SD, sum), .SDcols = c("ct_byr"), by = .(equity, indicator)]
}

create.tod.equity.table <- function() {
  eact <- compile.tod.actuals.equity() # 2017 actuals
  
  # create control totals 
  #ct_2017, use the 2017 census tract actual estimates of pop (SAEP) and jobs (tract file)
  #ct_2017-2050 use the modeled delta, i.e. the same value that gets reported in the 'delta_base' field
  #ct_2050, add ct_2017 and ct_2017-2050
  ect <- compile.control.totals.equity()
  
  ebase <- compile.modeled.equity.base() # modeled equity base

  # modeled in tod by equity
  # gq
  egq <- query.gq("group_quarters_geo.xlsx", c("tod_id", "minority_id", "poverty_id"), c("2017", "2050"))
  egq[, `:=` (minority_id = as.character(minority_id), poverty_id = as.character(poverty_id))]
  egq[, minority := switch(minority_id, "0" = "non-minority", "1" = "minority"), by = minority_id
      ][, poverty := switch(poverty_id, "0" = "non-poverty", "1" = "poverty"), by = poverty_id
        ][, indicator := "population"]
  egqdt <- melt.data.table(egq, 
                           id.vars = c("tod_id", "indicator", "gq", "year"), 
                           measure.vars = c("minority", "poverty"), variable.name = "generic_equity", value.name = "equity")
  gqdt <- egqdt[, lapply(.SD, sum), .SDcols = "gq", by = .(tod_id, indicator, year, equity)]
  
  # mil
  emil <- query.military(enlist.mil.file.nm, c("tod_id", "minority_id", "poverty_id"), c("2017", "2050"))
  emil[, `:=` (minority_id = as.character(minority_id), poverty_id = as.character(poverty_id))]
  emil[, minority := switch(minority_id, "0" = "non-minority", "1" = "minority"), by = minority_id
       ][, poverty := switch(poverty_id, "0" = "non-poverty", "1" = "poverty"), by = poverty_id
         ][, indicator := "employment"]
  emildt <- melt.data.table(emil, 
                            id.vars = c("tod_id", "indicator", "enlist_estimate", "year"), 
                            measure.vars = c("minority", "poverty"), variable.name = "generic_equity", value.name = "equity")
  mildt <- emildt[, lapply(.SD, sum), .SDcols = "enlist_estimate", by = .(tod_id, indicator, year, equity)]
  # modeled data
  file.regxpr <- "(^tod).*(population|employment)\\.csv"
  edf <- compile.tbl.equity(file.regxpr, allruns, run.dir, ind.extension)
  edf[, equity := str_extract(variable, "^\\w+(?=_\\w+_\\d+)")
     ][, `:=` (equity = str_replace(equity, "_", "-"), indicator = str_extract(variable, "[[:alpha:]]+(?=_\\d+)"))]
  edt <- edf[year %in% c(byr, years)]
  
  edt[mildt, on = c("name_id" = "tod_id", "indicator", "equity", "year"), enlist := i.enlist_estimate][is.na(enlist), enlist := 0]
  edt[gqdt, on = c("name_id" = "tod_id", "indicator", "equity", "year"), gq := i.gq][is.na(gq), gq := 0]
  edt[, estimate_plus := estimate + enlist + gq]
  edt.tod <- edt[name_id != 0, 
                 ][, .(equity, year, run, indicator, estimate = estimate_plus)
                   ][, lapply(.SD, sum), .SDcols = c("estimate"), by = .(equity, year, run, indicator)]
  edt.tod[ebase, on = c("equity", "indicator", "run", "year"), base := i.base]

  edt.tod.cast <- dcast.data.table(edt.tod, equity + run + indicator ~ year, value.var = c("estimate", "base"))
  delta.expr <- parse(text = paste0("delta_tod := estimate_", years, " - estimate_", byr))
  deltareg.expr <- parse(text = paste0("delta_base := base_", years, " - base_", byr))
  edt.tod.cast[, eval(delta.expr)][, eval(deltareg.expr)][, delta_share_in_tod := delta_tod/delta_base]
  setnames(edt.tod.cast, paste0("estimate_", byr), "byr_tod")
  setnames(edt.tod.cast, paste0("base_", byr), "byr_base")
  setnames(edt.tod.cast, paste0("estimate_", years), paste0("yr", years, "_tod"))
  setnames(edt.tod.cast, paste0("base_", years), paste0("yr", years, "_base"))
  
  # assemble
  edt.tod.cast[eact, on = c("equity", "indicator"), byr_tod_actual := i.byr_tod_actual] # join actual tod byr
  edt.tod.cast[ect, on = c("equity", "indicator"), (paste0("ct_", byr)) := i.ct_byr] # join ct byr
  edt.tod.cast[, (paste0("byr_share_in_tod_actual")) := byr_tod_actual/get(paste0("ct_", byr))]
  edt.tod.cast[, (paste0("ct_", byr, "-", years)) := delta_base][, (paste0("ct_", years)) := (get(paste0("ct_", byr)) + delta_base)]
  edt.tod.cast[, (paste0(years.col[2], "_share_in_tod")) := (byr_tod_actual + delta_tod)/(get(paste0("ct_", byr)) + delta_base)]
  edt.tod.cast[, `:=` (County = "88")]
  edt.tod.cast$scenario <- names(run.dir)[match(edt.tod.cast$run, run.dir)]
  setnames(edt.tod.cast, "equity", "Countyname")
}


# Regional Totals (base) ---------------------------------------------------------

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
dt.equity <- create.tod.equity.table() # equity geographies
dt.all <- rbindlist(list(dt.cnty, dt.equity, dt), use.names = TRUE) # bind all geographies
countyname.sort <- c(counties, "poverty", "non-poverty", "minority", "non-minority", "Region")

# loop through each run
dlist <- NULL
fincols <- c("tod", "base")
for (r in 1:length(run.dir)) {
  t <- NULL
  t <- dt.all[run == run.dir[r], ][, Countyname.sort := factor(Countyname, levels = countyname.sort)][order(Countyname.sort, -indicator)][, Countyname.sort := NULL]
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