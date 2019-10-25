library(data.table)
library(openxlsx)
library(tidyverse)
library(foreign)

if(!exists("set.globals") || !set.globals) {
  curr.dir <- getwd()
  this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(this.dir)
  # source("settings.R")
  # source("functions.R")
}

# out.dir <- "J:/Projects/V2050/SEIS/Data_Support/Tables/Working"
out.dir <- "C:/Users/clam/Desktop/shiny_vision2050/scripts_results" # Test

out.file.nm <- "23_tod_population_2017"

# base year actuals -------------------------------------------------------
pop.file.nm <- "J:/Projects/Population/OFMPopHsgData/OFMSAEP/Custom_Ests/HHPop_est_Block_Spilt/tod3_V2050/est2017/tod_est2017.xlsx"
ofm.file.nm <- "J:/OtherData/OFM/SAEP/SAEP Extract_2017_10October03/requests/v2050/county.xlsx"

# total population in tod (tod2 blocksplits, does not sum to regional total)
byro.df <- read.xlsx(pop.file.nm) %>%
  select(geography = bCOUNTY, tod_id = bSecField, population_byr = splitblkTotpop) %>%
  filter(tod_id != 0) %>%
  group_by(geography) %>%
  summarise(population_tod = sum(population_byr))

# ofm
ofm <- read.xlsx(ofm.file.nm) %>% mutate(geography = recode(COUNTYFP10, "033" = "King", "035" = "Kitsap", "053" = "Pierce", "061" = "Snohomish"))
ofm.cnty <- ofm %>% select(geography, population_total = starts_with("POP"))

df <- ofm.cnty %>% 
  left_join(byro.df, by = "geography") 

df.reg <- df %>% summarise_if(is.numeric, sum) %>% mutate(geography = "Region")

df <- df %>% bind_rows(df.reg) %>% mutate(share_in_tod = population_tod/population_total)

write.xlsx(df, file.path(out.dir, paste0(out.file.nm, "_", Sys.Date(), ".xlsx")))

# setwd(curr.dir)