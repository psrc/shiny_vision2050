library(data.table)
library(openxlsx)
library(tidyverse)

if(!exists("set.globals") || !set.globals) {
  curr.dir <- getwd()
  this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(this.dir)
  source("settings.R")
  # setwd(script.dir)
  source("functions.R")
}

# settings --------------------------------------------------------------

# curr.dir <- getwd()
# this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
# setwd(this.dir)
source("all_runs.R")

out.file.nm <- settings$jhr$out.file.nm


# general -----------------------------------------------------------------

subarea.cnty.lu <- read.xlsx(file.path(data.dir, "subarea.xlsx"))

years.col <- paste0("yr", years)
kin.cols <- c("East King","Sea-Shore", "South King")
geo.cols <- c("King", kin.cols, "Kitsap", "Pierce", "Snohomish")

attributes <- c("employment", "households")
new.attributes <- c("employment", "housing_unit")
ind.extension <- ".csv"


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


# subarea forecast data -----------------------------------------------------

df.subarea <- compile.tbl('subarea', allruns, run.dir, attributes, ind.extension)

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

write.xlsx(dlist, file.path(out.dir, paste0(out.file.nm, "_", Sys.Date(), ".xlsx")))

setwd(curr.dir)
