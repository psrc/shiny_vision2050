library(data.table)
library(openxlsx)
library(tidyverse)

# Aws-model04 - iSTC - \\aws-model04\e$\opusgit\urbansim_data\data\psrc_parcel\runs\run_3.run_2018_08_17_13_06
# Aws-model03 - iDUG - \\aws-model03\e$\opusgit\urbansim_data\data\psrc_parcel\runs\run_1.run_2018_08_17_15_45
# Aws-model05 - iH2O2 - \\aws-model04\e$\opusgit\urbansim_data\data\psrc_parcel\runs\run_4.run_2018_08_17_16_15

# Aws-model07 - STC - \\aws-model07\E$\opusgit\urbansim_data\data\psrc_parcel\runs\run_2.run_2018_08_15_13_45
# Aws-model03 - DUG - \\aws-model03\e$\opusgit\urbansim_data\data\psrc_parcel\runs\run_22.run_2018_08_10_21_05
# Aws-model04 - H2O2 - \\aws-model04\e$\opusgit\urbansim_data\data\psrc_parcel\runs\run_1.run_2018_08_10_21_05
# Aws-model05 - TOD - \\aws-model05\e$\opusgit\urbansim_data\data\psrc_parcel\runs\run_3.run_2018_08_10_21_04


# user input --------------------------------------------------------------

#this.dir <- "C:/Users/CLam/Desktop/shiny_vision2050/scripts" 
this.dir <- "/Users/hana/R/vision2050indicators/full_set" 
# this.dir <-  dirname(parent.frame(2)$ofile)
setwd(this.dir)
source("all_runs.R")

# urbansim
indicator.dirnm <- "indicators"
run.dir <- c("iSTC" = "run_3.run_2018_08_17_13_06",
             "iDUG" = "run_1.run_2018_08_17_15_45", 
             "iH2O2" = "run_4.run_2018_08_17_16_15", 
             "TOD" = "run_3.run_2018_08_10_21_04") 
run.dir <- c("iSTC" = "run_3.run_2018_08_10_21_04",
             "iDUG" = "run_3.run_2018_08_10_21_04", 
             "iH2O2" = "run_3.run_2018_08_10_21_04", 
             "TOD" = "run_3.run_2018_08_10_21_04") 
years <- c(2050) # only one year
byr <- 2017

# output directory options
#out.dir <- "../scripts_results"
#dsa.dir <- "X:/DSA/Vision2050/land_use_tables"
dsa.dir <- "/Volumes/DataTeam/Projects/V2050/SEIS/Data_Support/Model_Output"
data.dir <- file.path("../data")

out.file.nm <- "15_jobs_housing_ratio" 


# general -----------------------------------------------------------------

subarea.cnty.lu <- read.xlsx(file.path("../data", "subarea.xlsx"))

years.col <- paste0("yr", years)
kin.cols <- c("East King","Sea-Shore", "South King")
geo.cols <- c("King", kin.cols, "Kitsap", "Pierce", "Snohomish")

attributes <- c("employment", "households")
new.attributes <- c("employment", "housing_unit")
ind.extension <- ".csv"

df.subarea <- NULL


# base year actuals -------------------------------------------------------

file.nm <- file.path(data.dir, "jobs_hsg_ratio_2017_15rev.xlsx")

# housing units ofm (april 1 postcensal)
byro.df <- read.xlsx(file.nm, sheet = "hsg_psrc") %>%
  select(geography, housing_unit_byr = hu) %>%
  filter(geography != "Region")

byre.df0 <- read.xlsx(file.nm, sheet = "emp_psrc") %>%
  select(geography = Subarea, emp_no_enlist = `Total.Jobs.(2017)`) %>%
  filter(geography != "Total")


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
  group_by(subarea_id, year) %>%
  summarise(enlist_estimate = sum(estimate)) %>%
  left_join(subarea.cnty.lu, by = "subarea_id")

# add enlisted to main employment df
byre.mil.df <- mil.df %>% filter(year == "yr2017") %>% select(subarea_name, enlist_estimate)

byre.df <- byre.df0 %>%
  left_join(byre.mil.df, by = c("geography" = "subarea_name")) %>%
  replace_na(list(enlist_estimate = 0)) %>%
  mutate(employment_byr = emp_no_enlist + enlist_estimate) %>%
  select(-subarea_id)

# assemble base year data  
byr.df <- byre.df %>%
  left_join(byro.df, by = "geography") %>%
  select(geography, contains("_byr")) %>%
  as.data.table()


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


# subarea forecast data -----------------------------------------------------

df.subarea <- compile.tbl('subarea')

df2 <- melt.data.table(df.subarea,
                       id.vars = c("name_id", "run", "indicator"),
                       measure.vars = grep("yr", colnames(df.subarea), value = TRUE),
                       variable.name = "year", value.name = "estimate")
df3 <- df2[year %in% years.col][subarea.cnty.lu, on = c("name_id" = "subarea_id")]

# sum KC, append to main forecast df
kc <- df3[cnty_id == 33, lapply(.SD, sum), .SDcols = "estimate", by =.(run, indicator, year, cnty_id, cnty_name)
          ][, `:=` (name_id = 7, subarea_name = "King")]
df4 <- rbindlist(list(df3, kc), use.names = TRUE, fill = TRUE)
df.cast <- dcast.data.table(df4, cnty_name + subarea_name + run  ~ indicator + year, value.var = "estimate")

# add enlisted to employment in forecast df
fcast.mil.df <- mil.df %>% filter(year %in% years.col) %>% select(subarea_name, enlist_estimate) %>% as.data.table

df5 <- df.cast %>%
  left_join(fcast.mil.df, by = "subarea_name") %>%
  replace_na(list(enlist_estimate = 0)) %>%
  select(-subarea_id) %>%
  as.data.table()

fcast.expr <- parse(text = paste0(paste0("employment_w_enlist_", years.col), ":=", paste0("employment_", years.col), "+", "enlist_estimate"))
fcast.expr2 <- parse(text = paste0(paste0("households_to_units_", years.col), ":=", paste0("households_", years.col), "/", ".95"))
fcast.sel.col <- c(colnames(df5)[1:3], paste(c("employment_w_enlist", "households_to_units"), years.col, sep = "_"))
df6 <- df5[, eval(fcast.expr)][, eval(fcast.expr2)][, ..fcast.sel.col]
setnames(df6, 
         c(paste0("employment_w_enlist_", years.col), paste0("households_to_units_", years.col)), 
         c(paste0("employment_", years.col), paste0("housing_unit_", years.col)))

d <- df6[byr.df, on = c("subarea_name" = "geography")]

# sum region, append to main forecast df
sdcols <- c(grep("employment|housing_unit", colnames(d), value = T))
reg <- d[!(subarea_name %in% kin.cols), lapply(.SD, sum), .SDcols = sdcols, by = run][, `:=` (subarea_name = "Region", cnty_name = "Region")]
d2 <- rbindlist(list(d, reg), use.names = T)

# calculate ratios
byr.col <-  c(grep("byr", colnames(d2), value = T))
fcast.col <-  c(grep("_yr", colnames(d2), value = T))

fcast.expr <- parse(text = paste0(paste0("ehratio_", years.col), ":=", fcast.col[1], "/", fcast.col[2]))
byr.expr <- parse(text = paste0("ehratio_byr :=", byr.col[1], "/", byr.col[2]))

d3 <- d2[, eval(fcast.expr)][, eval(byr.expr)]

# calculate index and store each table (by run) in list
dlist <- NULL
for (r in 1:length(run.dir)) {
  t <- NULL
  ind.expr <- parse(text = paste0(paste0("ehindex_", years.col), ":=", paste0("ehratio_", years.col), "/t[subarea_name == 'Region', ", paste0("ehratio_", years.col), "]"))
  t <- d3[run == run.dir[r],]
  t[, `:=` (ehindex_byr = (ehratio_byr/(t[subarea_name == "Region", ehratio_byr])))][, eval(ind.expr)][, scenario := names(run.dir[r])]
  roworder <- c("King", geo.cols[2:length(geo.cols)], "Region")
  t2 <- t[match(roworder, t$subarea_name)]
  setcolorder(t2, c("cnty_name", "subarea_name", "run", "scenario", paste0(new.attributes, "_byr"), paste0(new.attributes, "_", years.col), paste0("ehratio_", c("byr", years.col)), grep("ehindex", colnames(t2), value = T)))
  dlist[[names(run.dir[r])]] <- t2
}


# export ------------------------------------------------------------------

write.xlsx(dlist, file.path(dsa.dir, paste0(out.file.nm, "_", Sys.Date(), ".xlsx")))

