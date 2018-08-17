library(data.table)
library(openxlsx)
library(tidyverse)

# Aws-model07 - STC - \\aws-model07\E$\opusgit\urbansim_data\data\psrc_parcel\runs\run_2.run_2018_08_15_13_45\
# Aws-model03 - DUG - \\aws-model03\e$\opusgit\urbansim_data\data\psrc_parcel\runs\run_22.run_2018_08_10_21_05
# Aws-model04 - H2O2 - \\aws-model04\e$\opusgit\urbansim_data\data\psrc_parcel\runs\run_1.run_2018_08_10_21_05
# Aws-model05 - TOD - \\aws-model05\e$\opusgit\urbansim_data\data\psrc_parcel\runs\run_3.run_2018_08_10_21_04


# user input --------------------------------------------------------------

this.dir <- "C:/Users/CLam/Desktop/shiny_vision2050/scripts" 
# this.dir <-  dirname(parent.frame(2)$ofile)
setwd(this.dir)
source("all_runs.R")

# urbansim
indicator.dirnm <- "indicators"
run.dir <- c("STC" = "run_2.run_2018_08_15_13_45", "DUG" = "run_22.run_2018_08_10_21_05", "H2O2" = "run_1.run_2018_08_10_21_05", "TOD" = "run_3.run_2018_08_10_21_04") 
years <- c(2050) # only one year

# output directory options
out.dir <- "../scripts_results"
dsa.dir <- "X:/DSA/Vision2050/land_use_tables"
dsa.dir2 <- "J:/Projects/V2050/SEIS/Data_Support/Tables"

out.file.nm <- "jobs_housing_ratio" 


# base year actuals -------------------------------------------------------

act.yr <- "2017"
seis.dir <- "J:/Projects/V2050/SEIS/Data_Support/Tables/Working"

kin.cols <- c("East King", "Sea-Shore", "South King")
geo.cols <- c("King", kin.cols, "Kitsap", "Pierce", "Snohomish")

# households
byrh.df <-read.csv(file.path(seis.dir, "15_subareas_housing_2017.csv")) %>% 
  select(geography, households_byr = hhs)

# employment
byre.df0 <- read.xlsx(file.path(seis.dir, "15_subareas_employment_2017.xlsx")) %>%
  select(geography = Subarea, employment_byr = Including.Military) %>%
  filter(geography %in% geo.cols)

kin.tot <- byre.df0 %>%
  filter(geography %in% kin.cols) %>%
  summarise(employment_byr = sum(employment_byr)) %>%
  mutate(geography = "King")

byre.df <- byre.df0 %>%
  bind_rows(kin.tot)

by.df <- byre.df  %>%
  left_join(byrh.df, by = "geography") %>%
  mutate(geography = ifelse(geography == "King", "All King County", geography))
  

# general -----------------------------------------------------------------

subarea.cnty.lu <- read.xlsx(file.path("../data", "subarea.xlsx"))
years.col <- paste0("yr", years)

# format run names for table
# runs <- lapply(run.dir, function(x) unlist(strsplit(x,"[.]"))[[1]]) %>% unlist

attributes <- c("employment", "households")
ind.extension <- ".csv"

# df.cnty <- NULL
df.subarea <- NULL


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

# sum KC
kc <- df3[cnty_id == 33, lapply(.SD, sum), .SDcols = "estimate", by =.(run, indicator, year, cnty_id, cnty_name)
          ][, `:=` (name_id = 7, subarea_name = "All King County")]

df4 <- rbindlist(list(df3, kc), use.names = TRUE, fill = TRUE)
df.cast <- dcast.data.table(df4, cnty_name + subarea_name + run  ~ indicator + year, value.var = "estimate")
d <- df.cast[by.df, on = c("subarea_name" = "geography")]

# sum region 
sdcols <- c(grep("employment|households", colnames(d), value = T))
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
  roworder <- c("All King County", geo.cols[2:length(geo.cols)], "Region")
  t2 <- t[match(roworder, t$subarea_name)]
  setcolorder(t2, c("cnty_name", "subarea_name", "run", "scenario", paste0(attributes, "_byr"), paste0(attributes, "_", years.col), paste0("ehratio_", c("byr", years.col)), grep("ehindex", colnames(t2), value = T)))
  dlist[[names(run.dir[r])]] <- t2
}


# export ------------------------------------------------------------------

write.xlsx(dlist, file.path(dsa.dir, paste0(out.file.nm, "_", Sys.Date(), ".xlsx")))

