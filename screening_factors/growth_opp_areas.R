library(data.table)
library(openxlsx)
library(tidyverse)
library(foreign)

if(!exists("set.globals") || !set.globals) {
  source("settings.R")
  # setwd(script.dir)
  source("functions.R")
}

# settings --------------------------------------------------------------

curr.dir <- getwd()
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)
source("all_runs.R")

out.file.nm <- settings$goa$out.file.nm 


# base year actuals -------------------------------------------------------

# total population and households (ofm)
ofm.file.nm <- "J:/OtherData/OFM/SAEP/SAEP Extract_2017_10October03/requests/v2050/tract_population_households.csv"
byro.df <- read.csv(ofm.file.nm, stringsAsFactors = F) %>%
  mutate_at("GEOID10", as.character) %>%
  select(ends_with("10"), population_byr = contains("POP"), households_byr = contains("OHU"))

# employment
emp.file.nm <- "Census_Tract_jobs.dbf"
empcol <- "Jobs_2017"

byre.df0 <- read.dbf(file.path(data.dir, emp.file.nm)) %>%
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


# GQ population -----------------------------------------------------------

# read GQ pop (incorporate to 2050 data)

gq.cols <- c("census_tract_id", setNames(paste0("`", years, "`"), "gq"))
gq <- gq.file %>%
  select_(.dots = gq.cols) %>%
  group_by_(.dots = gq.cols[1]) %>%
  summarise_(.dots = setNames("sum(gq)", "gq"))

# general -----------------------------------------------------------------

og.comp.ind <- "Y:/VISION 2050/Data/Opportunity Mapping Update/Finalopportunity_analysis - Opportunity_Index_06062012_Region_.csv"

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
df3 <- df2[year %in% years.col][tract.lu, on = c("name_id" = "census_tract_id")][, GEOID10 := as.character(geoid10)]
df4 <- dcast.data.table(df3, name_id + GEOID10 + run + Comp.Index ~ indicator + year, value.var = "estimate")

# add enlisted to forecast employment df
fcast.mil.df <- mil.df %>% filter(year %in% years.col) %>% select(census_tract_id, enlist_estimate) %>% as.data.table

df5 <- df4 %>% 
  left_join(fcast.mil.df, by = c("GEOID10" = "census_tract_id")) %>%
  replace_na(list(enlist_estimate = 0)) %>%
  as.data.table()

fcast.expr <- parse(text = paste0(paste0("employment_w_enlist_", years.col), ":=", paste0("employment_", years.col), "+", "enlist_estimate"))
fcast.sel.col <- c(colnames(df5)[1:4], paste(c("employment_w_enlist", "households", "population"), years.col, sep = "_"))
df6 <- df5[, eval(fcast.expr)][, ..fcast.sel.col]
setnames(df6, paste0("employment_w_enlist_", years.col), paste0("employment_", years.col))

# add GQ population to forecast pop df
df6.gq <- df6 %>%
  left_join(gq, by = c("GEOID10" = "census_tract_id")) %>%
  replace_na(list(gq = 0)) %>%
  as.data.table()

gq.expr <- parse(text = paste0(paste0("population_w_gq_", years.col), ":= ", paste0("population_", years.col), "+ gq"))
gq.sel.col <- c(colnames(df6.gq)[1:6], paste0("population_w_gq_", years.col))
df6.gq2 <- df6.gq[, eval(gq.expr)][, ..gq.sel.col]
setnames(df6.gq2, paste0("population_w_gq_", years.col), paste0("population_", years.col))

# join with base year df
df7 <- df6.gq2[byr.df, on = "GEOID10"]

# calculate
df7.melt <- melt.data.table(df7, id.vars = colnames(df7)[1:4], measure.vars = colnames(df7)[5:10], variable.name = "attr", value.name = "estimate")
df7.sep <- df7.melt %>%
  replace_na(list(estimate = 0)) %>%
  separate(attr, into = c("indicator", "year"), sep = "_") %>%
  spread(year, estimate) %>%
  replace_na(list(estimate = 0)) %>%
  as.data.table

delta.expr <- parse(text = paste0("delta := ", years.col, " - byr"))
sdcols <- c("byr", years.col)
df8 <- df7.sep[!is.na(Comp.Index), lapply(.SD, sum), .SDcols = sdcols, by = list(indicator, Comp.Index, run)][, eval(delta.expr)]

delta.sums <- df8[, .(sum_delta = sum(delta)), by = list(indicator, run)]

# calculate Mod to Very High opp areas
mod.high.opp <- df8[Comp.Index %in% opp.levels[3:5], lapply(.SD, sum), .SDcols = c(sdcols, "delta"), by = list(indicator, run)
                    ][, Comp.Index := eval(opp.levels[6])]

df8.bind <- rbindlist(list(df8, mod.high.opp), use.names = TRUE)

df9 <- df8.bind[delta.sums, on = c("indicator", "run")
           ][, share_delta := delta/sum_delta
             ][, Comp.Index.sort := factor(Comp.Index, levels = opp.levels)
               ][order(indicator, Comp.Index.sort)
                 ][, Comp.Index.sort := NULL]

# loop through each run
dlist <- NULL
for (r in 1:length(run.dir)) {
  t <- NULL
  t <- df9[run == run.dir[r], ][, scenario := names(run.dir[r])]
  setcolorder(t, c("indicator", "Comp.Index", "run", "scenario", "byr", years.col, "delta", "sum_delta", "share_delta"))
  setnames(t, c("Comp.Index", "byr"), c("index", paste0("byr", byr))) 
  dlist[[names(run.dir[r])]] <- t
}


# export ------------------------------------------------------------------

write.xlsx(dlist, file.path(out.dir, paste0(out.file.nm, "_", Sys.Date(), ".xlsx")))

setwd(curr.dir)
set.globals <- FALSE
