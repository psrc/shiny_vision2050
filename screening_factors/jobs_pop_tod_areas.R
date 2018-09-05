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


out.file.nm <- settings$pjta$out.file.nm 


# general -----------------------------------------------------------------

years.col <- paste0("yr", years)

# format run names for table
runs <- lapply(run.dir, function(x) unlist(strsplit(x,"[.]"))[[1]]) %>% unlist

counties <- c("King", "Kitsap", "Pierce", "Snohomish")
names(counties) <- c("33", "35", "53", "61")

attributes <- c("employment", "population")
ind.extension <- ".csv" 


# base year actuals -------------------------------------------------------

# total population (tod2 blocksplits, does not sum to regional total)
# pop.file.nm <- "J:/Projects/Population/OFMPopHsgData/OFMSAEP/Custom_Ests/HHPop_est_Block_Spilt/tod2_V2050/est2017/tod_est2017.xlsx"
pop.file.nm <- file.path(data.dir, "tod_est2017.xlsx")
byro.df <- read.xlsx(pop.file.nm) %>%
  select(tod_id = bSecField, population_byr = splitblkTotpop) %>%
  group_by(tod_id) %>%
  summarise(population_byr = sum(population_byr))

# add remaining population to tod_id = 0
# ofm.file.nm <- "J:/OtherData/OFM/SAEP/SAEP Extract_2017_10October03/requests/v2050/county.xlsx"
ofm.file.nm <- file.path(data.dir, "county.xlsx")
ofm <- read.xlsx(ofm.file.nm)
ofm.cnty <- ofm %>% select(starts_with("POP"))
ofm.region <- ofm.cnty %>% summarise_(.dots = setNames(paste0("sum(", paste0("POP", byr), ")"), "population_ofm"))

tot.byro.df <- byro.df %>% summarise(population_byr = sum(population_byr))
todid0 <- ofm.region$population_ofm - tot.byro.df$population_byr # add to 0
upd.todid0 <- byro.df$population_byr[byro.df$tod_id == 0] + todid0

byro.df2 <- byro.df %>%
  mutate(population_byr = replace(population_byr, tod_id == 0, upd.todid0))
  
# employment
# emp.dir <- "J:/Confid/Emp/Internal_Support/Growth_Mgmt/V2050/EIS/TOD"
emp.file.nm <- "TOD_Jobs17.dbf"
empcol <- "Jobs_2017"
todcol <- "TOD_Area5"

# employment: find tod = 0 values
tot.emp.df <- read.xlsx(file.path(data.dir, "tod_employment_2017_23.xlsx"))
non.tod.emp <- tot.emp.df %>%
  filter(County == "Region Total") %>%
  mutate(tod_id = 0, employment_byr = Total.Jobs - Total.Jobs.in.TOD.Areas) %>%
  select(tod_id, employment_byr)

# employment: assemble (enlisted not included)
byre.df0 <- read.dbf(file.path(data.dir, emp.file.nm)) %>%
  select_(.dots = c(setNames(todcol, "tod_id"), setNames(empcol, "employment_byr"))) %>%
  group_by(tod_id) %>%
  summarise(employment_byr = sum(employment_byr)) %>%
  bind_rows(non.tod.emp) %>%
  mutate_at("tod_id", as.character)

# assemble base year data (enlisted not included)
byr.df <- byro.df2 %>%
  left_join(byre.df0, by = "tod_id") %>%
  select(tod_id, contains("_byr")) %>%
  replace_na(list(employment_byr = 0)) %>%
  as.data.table()


# enlisted personnel ------------------------------------------------------

mil <- read.csv(file.path(data.dir, enlist.mil.file.nm), stringsAsFactors = FALSE) %>%
  drop_na(everything())
colnames(mil)[grep("^X\\d+", colnames(mil))] <- gsub("X", "yr", colnames(mil)[grep("^X\\d+", colnames(mil))])

mil.df <- mil %>% 
  left_join(enlist.lu, by = c("Base", "Zone", "ParcelID" = "parcel_id")) %>%
  gather(contains("yr"), key = "year", value = "estimate") %>%
  filter(year %in% paste0("yr", c(2017, years))) %>%
  group_by(tod_id, year) %>%
  summarise(enlist_estimate = sum(estimate))


# GQ population -----------------------------------------------------------

# read GQ pop (incorporate to 2050 data)
gq.cols <- c("tod_id", setNames(paste0("`", years, "`"), "gq"))
gq <- gq.file %>%
  select_(.dots = gq.cols) %>%
  group_by_(.dots = gq.cols[1]) %>%
  summarise_(.dots = setNames("sum(gq)", "gq"))


# Regional Totals ---------------------------------------------------------

regcols <- grep("pop|emp", colnames(byr.df), value = TRUE)
region.byr <- byr.df[, lapply(.SD, sum), .SDcols = regcols]

mil.df2 <- mil.df %>% as.data.table

# include enlisted personnel and GQ
df.cnty <- compile.tbl('county', allruns, run.dir, attributes, ind.extension)
region.fcast <- df.cnty[, lapply(.SD, sum), .SDcols = years.col, by = c("indicator", "run")]
region <- dcast.data.table(region.fcast, run ~ indicator, value.var = years.col)
region[, `:=` (employment_byr = region.byr$employment_byr, 
               population_byr = region.byr$population_byr,
               gq = sum(gq$gq),
               enlist_byr = mil.df2[year != years.col, enlist_estimate],
               enlist = mil.df2[year == years.col, enlist_estimate])
       ]
region.expr <- parse(text = paste0(paste0("employment_", years.col), ":=", "employment + enlist"))
region.expr2 <- parse(text = paste0("employment_w_enlist := employment_byr + enlist_byr"))
region.expr3 <- parse(text = paste0(paste0("population_", years.col), ":=", "population + gq"))
region[, eval(region.expr)][, eval(region.expr2)][, eval(region.expr3 )]

sel.reg.cols <- c("run", grep(years.col, colnames(region), value = TRUE), "employment_w_enlist", "population_byr")
tot.region <- region[, ..sel.reg.cols][, name_id := "region"]
setnames(tot.region, "employment_w_enlist", "employment_byr")


# transform data ----------------------------------------------------------

alldata <- compile.tbl('tod', allruns, run.dir, attributes, ind.extension)

df2 <- melt.data.table(alldata,
                       id.vars = c("name_id", "run", "indicator"),
                       measure.vars = grep("yr", colnames(alldata), value = TRUE),
                       variable.name = "year", value.name = "estimate")
df3 <- df2[year %in% years.col]
df4 <- dcast.data.table(df3, name_id + run ~ indicator + year, value.var = "estimate")

# add GQ to forecast population df
df4.gq <- df4 %>%
  left_join(gq, by = c("name_id" = "tod_id")) %>%
  replace_na(list(gq = 0)) %>%
  as.data.table()

gq.expr <- parse(text = paste0(paste0("population_w_gq_", years.col), ":= ", paste0("population_", years.col), "+ gq"))
gq.sel.col <- c(colnames(df4.gq)[1:3], paste0("population_w_gq_", years.col))
df4.gq2 <- df4.gq[, eval(gq.expr)][, ..gq.sel.col][, name_id := as.character(name_id)]
setnames(df4.gq2, paste0("population_w_gq_", years.col), paste0("population_", years.col))

# join with base year df (all 0-6)
regcols2 <- grep("pop|emp", colnames(df4.gq2), value = TRUE)
# filter for TOD areas and sum
df5 <- df4.gq2[byr.df, on = c("name_id" = "tod_id")][name_id != 0, lapply(.SD, sum), .SDcols = c(regcols, regcols2), by = run][, name_id := "tod"] 

df6 <- rbindlist(list(df5, tot.region), use.names = TRUE)
df6.melt <- melt.data.table(df6, id.vars = c("name_id", "run"), measure.vars = c(regcols, regcols2), variable.name = "attr", value.name = "estimate")

df6.sep <- df6.melt %>%
  separate(attr, into = c("indicator", "year"), sep = "_") %>%
  spread(year, estimate) %>%
  replace_na(list(estimate = 0)) %>%
  as.data.table

delta.expr <- parse(text = paste0("delta := ", years.col, " - byr"))
sdcols <- c("byr", years.col)

df7 <- df6.sep[, eval(delta.expr)]

df7.cast <- dcast.data.table(df7, run + indicator ~ name_id, value.var = c("byr", "yr2050", "delta"))
df7.cast[, share_delta_in_tod := delta_tod/delta_region]

# loop through each run
dlist <- NULL
fincols <- c("tod", "region")
for (r in 1:length(run.dir)) {
  t <- NULL
  t <- df7.cast[run == run.dir[r], ][, scenario := names(run.dir[r])]
  setcolorder(t, c("run", "scenario", "indicator", paste0("byr_", fincols), paste0(years.col, "_", fincols), paste0("delta_", fincols), "share_delta_in_tod"))
  dlist[[names(run.dir[r])]] <- t
}

# export ------------------------------------------------------------------

write.xlsx(dlist, file.path(out.dir, paste0(out.file.nm, "_", Sys.Date(), ".xlsx")))

setwd(curr.dir)
# set.globals <- FALSE