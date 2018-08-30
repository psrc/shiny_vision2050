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

out.file.nm <- "23_baseyear_pop_tod_areas"

# base year actuals -------------------------------------------------------
pop.file.nm <- "J:/Projects/Population/OFMPopHsgData/OFMSAEP/Custom_Ests/HHPop_est_Block_Spilt/tod2_V2050/est2017/tod_est2017.xlsx"
ofm.file.nm <- "J:/OtherData/OFM/SAEP/SAEP Extract_2017_10October03/requests/v2050/county.xlsx"

# total population in tod (tod2 blocksplits, does not sum to regional total)
byro.df <- read.xlsx(pop.file.nm) %>%
  select(county = bCOUNTY, tod_id = bSecField, population_byr = splitblkTotpop) %>%
  filter(tod_id != 0) %>%
  group_by(county) %>%
  summarise(population_byr = sum(population_byr))

# ofm
ofm <- read.xlsx(ofm.file.nm) %>% mutate(county = recode(COUNTYFP10, "033" = "King", "035" = "Kitsap", "053" = "Pierce", "061" = "Snohomish"))
ofm.cnty <- ofm %>% select(county, population_ofm = starts_with("POP"))

df <- ofm.cnty %>% 
  left_join(byro.df, by = "county") %>%
  mutate()

write.xlsx(, file.path(out.dir, paste0(out.file.nm, "_", Sys.Date(), ".xlsx")))

setwd(curr.dir)