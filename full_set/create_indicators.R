# main file for generating all indicators

wrk.dir <- "/Users/hana/R/vision2050indicators/full_set"
curr.dir <- getwd()
setwd(wrk.dir)

# global settings
set.globals <- FALSE
source("../screening_factors/settings.R")
source("settings.R")

# run scripts
#source("../screening_factors/jobs_housing_ratio.R") # 15, 18
source("jobs_housing_ratio.R") # 15, 18
source("housing_type_mix.R") # 17
source("growth_opp_areas.R") # 79
source("allocation_pop_emp_au.R") # 16

setwd(curr.dir)
