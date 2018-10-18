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


out.file.nm <- settings$goa$out.file.nm 

years.cols <- paste0("yr", c(byr, years))

# base year actuals -------------------------------------------------------

# total population and households (ofm)
# ofm.file.nm <- "J:/OtherData/OFM/SAEP/SAEP Extract_2017_10October03/requests/v2050/tract_population_households.csv"
ofm.file.nm <- file.path(data.dir, "tract_population_households.csv")
byro.df <- read.csv(ofm.file.nm, stringsAsFactors = F) %>%
  mutate_at("GEOID10", as.character) %>%
  select(ends_with("10"), population_byr = contains("POP"), households_byr = contains("OHU"))

# employment
# emp.file.nm <- "Census_Tract_jobs.dbf"
# empcol <- "Jobs_2017"
# 
# byre.df0 <- read.dbf(file.path(data.dir, emp.file.nm)) %>%
#   select_(.dots = c("GEOID10", setNames(empcol, "emp_no_enlist"))) %>%
#   mutate_at("GEOID10", as.character)
emp.file.nm <- "Tract2017_AllJobs.xlsx"
empcol <- "Employment"

byre.df0 <- read.xlsx(file.path(data.dir, emp.file.nm), startRow = 2, colNames = TRUE) %>%
  select_(.dots = c("GEOID10", setNames(empcol, "emp_no_enlist"))) %>%
  mutate_at("GEOID10", as.character)


# enlisted personnel ------------------------------------------------------

mil <- read.csv(file.path(data.dir, enlist.mil.file.nm), stringsAsFactors = FALSE) %>%
  drop_na(everything())
colnames(mil)[grep("^X\\d+", colnames(mil))] <- gsub("X", "yr", colnames(mil)[grep("^X\\d+", colnames(mil))])

mil.df <- mil %>% 
  left_join(enlist.lu, by = c("Base", "Zone", "ParcelID" = "parcel_id")) %>%
  gather(contains("yr"), key = "year", value = "estimate") %>%
  filter(year %in% paste0("yr", c(2017, years))) %>%
  group_by(census_tract_id, year) %>%
  summarise(enlist_estimate = sum(estimate))

# add enlisted to main employment df
byre.mil.df <- mil.df %>% filter(year == "yr2017") %>% select(census_tract_id, enlist_estimate)

byre.df <- byre.df0 %>%
  left_join(byre.mil.df, by = c("GEOID10" = "census_tract_id")) %>%
  replace_na(list(enlist_estimate = 0)) %>%
  mutate(employment_byr = emp_no_enlist + enlist_estimate)

# assemble base year data  
byr.df <- byro.df %>%
  left_join(byre.df, by = "GEOID10") %>%
  select(GEOID10, contains("_byr")) %>%
  as.data.table()

colnames(byr.df)[grep("byr", colnames(byr.df))] <- str_extract(colnames(byr.df)[grep("byr", colnames(byr.df))], "\\w+(?=_)")
bdf <- melt.data.table(byr.df, 
                       id.vars = "GEOID10", 
                       measure.vars = colnames(byr.df)[2:4], 
                       variable.name = "indicator", 
                       value.name = "actual_byr")
bdf[is.na(actual_byr), actual_byr := 0]


# GQ population -----------------------------------------------------------

# read GQ pop (incorporate to 2050 data)
colnames(gq.file)[grep("^\\d+", colnames(gq.file))] <- paste0("yr", colnames(gq.file)[grep("^\\d+", colnames(gq.file))])
gq <- gq.file %>%
  gather(contains("yr"), key = "year", value = "estimate") %>%
  filter(year %in% years.cols) %>%
  group_by(census_tract_id, year) %>%
  summarise(gq_estimate = sum(estimate)) %>%
  mutate(indicator = "population") %>%
  as.data.table

# general -----------------------------------------------------------------

# og.comp.ind <- "Y:/VISION 2050/Data/Opportunity Mapping Update/Finalopportunity_analysis - Opportunity_Index_06062012_Region_.csv"
og.comp.ind <- file.path(data.dir, "Finalopportunity_analysis - Opportunity_Index_06062012_Region_.csv")

tract.lu <- read.csv(file.path(data.dir, "tract_opportunity_lookup.csv"), stringsAsFactors = FALSE)

years.col <- paste0("yr", years)

attributes <- c("population", "employment", "households")
ind.extension <- ".csv"


# transform data ----------------------------------------------------------

opp.levels <- c("Very Low Opportunity", "Low Opportunity", "Moderate Opportunity", "High Opportunity", "Very High Opportunity", "Moderate to Very High Opportunity Areas")

alldata <- compile.tbl("census_tract", allruns, run.dir, attributes, ind.extension)
df2 <- melt.data.table(alldata,
                       id.vars = c("name_id", "run", "indicator"),
                       measure.vars = grep("yr", colnames(alldata), value = TRUE),
                       variable.name = "year", value.name = "estimate")
df3 <- df2[year %in% years.cols
           ][tract.lu, on = c("name_id" = "census_tract_id")
             ][, GEOID10 := as.character(geoid10)
               ][, .(name_id, GEOID10, run, Comp.Index, indicator, year, estimate)]

# add enlisted to forecast employment df
fcast.mil.df <- mil.df %>% 
  filter(year %in% years.cols) %>% 
  select(census_tract_id, year, enlist_estimate) %>% 
  mutate(indicator = "employment") %>%
  as.data.table

df3[fcast.mil.df, on = c("GEOID10" = "census_tract_id", "year", "indicator"), enlist := i.enlist_estimate][is.na(enlist), enlist := 0]
df3[gq, on = c("GEOID10" = "census_tract_id", "year", "indicator"), gq := i.gq_estimate][is.na(gq), gq := 0]
df3[bdf, on = c("GEOID10", "indicator"), actual_byr := i.actual_byr]
df3[, estimate_plus := estimate + enlist + gq]
df4 <- df3[, .(name_id, GEOID10, run, Comp.Index, indicator, year, actual_byr, estimate = estimate_plus)]

df5 <- dcast.data.table(df4, name_id + GEOID10 + run + Comp.Index + indicator + actual_byr ~ year, value.var = "estimate")
setnames(df5, years.cols[1], "byr")

sdcols <- c("actual_byr", "byr", years.col)
sumcols <- paste0("sum_", sdcols)
sharecols <- paste0("share_", sdcols)

calc.by.geog <- function(geog, atable) {
  delta.expr <- parse(text = paste0("delta := ", years.col, " - byr"))
  share.expr1 <- parse(text = paste0(sharecols[1], ":=", sdcols[1], "/", sumcols[1]))
  share.expr2 <- parse(text = paste0(sharecols[2], ":=", sdcols[2], "/", sumcols[2]))
  share.expr3 <- parse(text = paste0(sharecols[3], ":=", sdcols[3], "/", sumcols[3]))
  
  group.by.cols <- c("county", "indicator", "Comp.Index", "run")
  group.by.cols2 <- c("county", "indicator", "run")
  
  df <- copy(atable)
  if (geog == "county") {
    df <- atable[, countycode := as.character(str_sub(GEOID10, 4, 5))]
    df[, county := switch(countycode, "33" = "King", "35" = "Kitsap", "53" = "Pierce", "61" = "Snohomish"), by = countycode]
  } else if (geog == "region") {
    df[, county := "Region"]
  }
  d <- df[!is.na(Comp.Index), lapply(.SD, sum), .SDcols = c(sdcols), by = group.by.cols][, eval(delta.expr)]
  
  # calculate sums
  delta.sums <- d[, .(sum_delta = sum(delta)), by = group.by.cols2]
  geog.sums <- d[, lapply(.SD, sum), .SDcols = sdcols, by = group.by.cols2]
  setnames(geog.sums, sdcols, sumcols)
  
  # calculate Mod to Very High opp areas
  mod.high.opp <- d[Comp.Index %in% opp.levels[3:5], lapply(.SD, sum), .SDcols = c(sdcols, "delta"), by = group.by.cols2
                    ][, Comp.Index := eval(opp.levels[6])]
  # bind to main df
  dall <- rbindlist(list(d, mod.high.opp), use.names = TRUE)
  dfin <- dall[geog.sums, on = group.by.cols2
               ][delta.sums, on = group.by.cols2
                 ][, eval(share.expr1)
                   ][, eval(share.expr2)
                     ][, eval(share.expr3)
                       ][, share_delta := delta/sum_delta
                         ][, Comp.Index.sort := factor(Comp.Index, levels = opp.levels)
                           ][, indicator.sort := factor(indicator, levels = c("population", "households", "employment"))
                             ][order(indicator.sort, Comp.Index.sort)
                               ][, `:=` (indicator.sort = NULL, Comp.Index.sort = NULL)
                                 ]
}

df.reg <- calc.by.geog("region", df5)
df.cnty <- calc.by.geog("county", df5)
df9 <- rbindlist(list(df.cnty, df.reg), use.names = TRUE)

# loop through each run
dlist <- NULL
for (r in 1:length(run.dir)) {
  t <- NULL
  t <- df9[run == run.dir[r], ][, scenario := names(run.dir[r])]
  setcolorder(t, c("county", "indicator", "Comp.Index", "run", "scenario", sdcols, sumcols, sharecols, grep("delta", colnames(t), value = TRUE)))
  setnames(t, c("Comp.Index"), c("index"))
  colnames(t)[grep("byr", colnames(t))] <- str_replace_all(colnames(t)[grep("byr", colnames(t))], "byr", paste0("yr", byr))
  setnames(t, colnames(t)[grep("actual", colnames(t))], c(paste0(c("", "sum_", "share_"),  paste0(years.cols[1], "_actual"))))
  t[, (paste0("share_", years.cols[1])) := NULL][] # exclude share modeled byr; only shares for actual byr needed
  dlist[[names(run.dir[r])]] <- t
}


# export ------------------------------------------------------------------

write.xlsx(dlist, file.path(out.dir, paste0(out.file.nm, "_", Sys.Date(), ".xlsx")))

setwd(curr.dir)

