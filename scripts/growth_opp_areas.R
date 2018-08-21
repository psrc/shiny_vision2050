library(data.table)
library(openxlsx)
library(tidyverse)
library(foreign)

# Aws-model04 - iSTC - \\aws-model04\e$\opusgit\urbansim_data\data\psrc_parcel\runs\run_3.run_2018_08_17_13_06
# Aws-model07 - STC - \\aws-model07\E$\opusgit\urbansim_data\data\psrc_parcel\runs\run_2.run_2018_08_15_13_45\ 
# Aws-model03 - DUG - \\aws-model03\e$\opusgit\urbansim_data\data\psrc_parcel\runs\run_22.run_2018_08_10_21_05
# Aws-model04 - H2O2 - \\aws-model04\e$\opusgit\urbansim_data\data\psrc_parcel\runs\run_1.run_2018_08_10_21_05
# Aws-model05 - TOD - \\aws-model05\e$\opusgit\urbansim_data\data\psrc_parcel\runs\run_3.run_2018_08_10_21_04


# user input --------------------------------------------------------------

this.dir <- "C:/Users/CLam/Desktop/shiny_vision2050/scripts"
# this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
source("all_runs.R")

indicator.dirnm <- "indicators"
run.dir <- c("STC" = "run_2.run_2018_08_15_13_45", 
             "DUG" = "run_22.run_2018_08_10_21_05", 
             "H2O2" = "run_1.run_2018_08_10_21_05", 
             "TOD" = "run_3.run_2018_08_10_21_04") 
years <- c(2050) # only one year
byr <- 2017

data.dir <- "../data"
out.dir <- "../scripts_results"
dsa.dir <- "X:/DSA/Vision2050/land_use_tables"

out.file.nm <- "dist_growth_opp_areas" 


# base year actuals -------------------------------------------------------

# population and housing (ofm) #### May need to use HHP
ofm.file.nm <- "J:/OtherData/OFM/SAEP/SAEP Extract_2017_10October03/requests/v2050/tract_population_households.csv"
byro.df <- read.csv(ofm.file.nm, stringsAsFactors = F) %>%
  mutate_at("GEOID10", as.character) %>%
  select(ends_with("10"), population_byr = contains("POP"), households_byr = contains("OHU"))

# employment
emp.dir <- "J:/Confid/Emp/Internal_Support/Growth_Mgmt/V2050/EIS/Tracts"
emp.file.nm <- "Census_Tract_jobs.dbf"
empcol <- "Jobs_2017"

byre.df0 <- read.dbf(file.path(emp.dir, emp.file.nm)) %>%
  select_(.dots = c("GEOID10", setNames(empcol, "emp_no_enlist"))) %>%
  mutate_at("GEOID10", as.character)


# enlisted personnel ------------------------------------------------------

enlist.lu <- read.xlsx(file.path(data.dir, "enlisted_personnel_geo.xlsx"))

enlist.mil.file.nm <- "enlisted_personnel_SoundCast_08202018.csv"
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

# read GQ pop, incorporate to 2050 data


# general -----------------------------------------------------------------

og.comp.ind <- "Y:/VISION 2050/Data/Opportunity Mapping Update/Finalopportunity_analysis - Opportunity_Index_06062012_Region_.csv"

tract.lu <- read.csv(file.path("../data", "tract_opportunity_lookup.csv"), stringsAsFactors = FALSE)

years.col <- paste0("yr", years)

# format run names for table
runs <- lapply(run.dir, function(x) unlist(strsplit(x,"[.]"))[[1]]) %>% unlist

attributes <- c("population", "employment", "households")
ind.extension <- ".csv"

df <- NULL


# functions ---------------------------------------------------------------

compile.tbl <- function(geog) {
  df <- NULL
  for (r in 1:length(run.dir)) { # for each run
    base.dir <- purrr::pluck(allruns, run.dir[r]) 
    for (a in 1:length(attributes)) { # for each attribute
      filename <- paste0(geog,'__',"table",'__',attributes[a], ind.extension)
      datatable <- read.csv(file.path(base.dir, indicator.dirnm, filename), header = TRUE, sep = ",")
      colnames(datatable)[2: ncol(datatable)] <- str_replace(colnames(datatable)[2: ncol(datatable)], '\\w+_', 'yr') # rename columns
      colnames(datatable)[1] <- str_replace(colnames(datatable)[1], '\\w+_', 'name_')
      datatable$indicator <- attributes[a]
      datatable$run <- run.dir[r]
      df <- rbindlist(list(df, datatable), use.names = TRUE, fill = TRUE)
    }
  }
  return(df)
}


# transform data ----------------------------------------------------------

opp.levels <- c("Very Low Opportunity", "Low Opportunity", "Moderate Opportunity", "High Opportunity", "Very High Opportunity")
alldata <- compile.tbl("census_tract")

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



# join with base year df
df7 <- df6[byr.df, on = "GEOID10"]

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
# all.sums <- df8[, lapply(.SD, sum), .SDcols = c(sdcols, "delta"), by = list(indicator, run)][, Comp.Index := "All"]
delta.sums <- df8[, .(sum_delta = sum(delta)), by = list(indicator, run)]

df9 <- df8[delta.sums, on = c("indicator", "run")
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

# write.xlsx(dlist, file.path(dsa.dir, paste0(out.file.nm, "_", Sys.Date(), ".xlsx")))

