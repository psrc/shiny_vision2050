library(data.table)
library(openxlsx)
library(tidyverse)
library(foreign)

if(!exists("set.globals") || !set.globals) {
  curr.dir <- getwd()
  this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(this.dir)
  source("settings.R")
  source("functions.R")
}

# settings --------------------------------------------------------------

# curr.dir <- getwd()
# this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
# setwd(this.dir)
# source("all_runs.R")

out.file.nm <- "23_baseyear_pop_tod_areas"

# base year actuals -------------------------------------------------------

# total population (tod2 blocksplits, does not sum to regional total)
pop.file.nm <- "J:/Projects/Population/OFMPopHsgData/OFMSAEP/Custom_Ests/HHPop_est_Block_Spilt/tod2_V2050/est2017/tod_est2017.xlsx"
byro.df <- read.xlsx(pop.file.nm) %>%
  select(county = bCOUNTY, tod_id = bSecField, population_byr = splitblkTotpop) %>%
  filter(tod_id != 0) %>%
  group_by(county) %>%
  summarise(population_byr = sum(population_byr))

# add remaining population to tod_id = 0
ofm.file.nm <- "J:/OtherData/OFM/SAEP/SAEP Extract_2017_10October03/requests/v2050/county.xlsx"
ofm <- read.xlsx(ofm.file.nm) %>% mutate(county = recode(COUNTYFP10, "033" = "King", "035" = "Kitsap", "053" = "Pierce", "061" = "Snohomish"))
ofm.cnty <- ofm %>% select(county, starts_with("POP"))

tot.byro.df <- byro.df %>% summarise(population_byr = sum(population_byr))
todid0 <- ofm.region$population_ofm - tot.byro.df$population_byr # add to 0
upd.todid0 <- byro.df$population_byr[byro.df$tod_id == 0] + todid0
byro.df2 <- byro.df %>%
  mutate(population_byr = replace(population_byr, tod_id == 0, upd.todid0))

byro.df.sum <- byro.df %>% 
  group_by(county) %>% 
  summarise(population_byr = sum(population_byr)) 
calc.byro <- ofm.cnty %>% left_join(byro.df.sum, by = "county") %>% mutate(diff = POP2017 - population_byr)
byro.todid0 <- byro.df %>% filter(tod_id == 0)


write.xlsx(, file.path(out.dir, paste0(out.file.nm, "_", Sys.Date(), ".xlsx")))

setwd(curr.dir)