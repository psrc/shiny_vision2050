# main file for generating all indicators

wrk.dir <- "/Users/hana/R/vision2050indicators/full_set"
curr.dir <- getwd()
setwd(wrk.dir)

# global settings
source("settings.R")
set.globals <- TRUE

# run scripts
source("functions.R")
#source("../screening_factors/jobs_housing_ratio.R") # 15, 18
#source("../screening_factors/housing_type_mix.R") # 17
#source("../screening_factors/growth_opp_areas.R") # 79
#source("../screening_factors/jobs_pop_tod_areas.R") # 30
source("allocation_pop_emp_au.R") # 16
source("pop_emp_density.R") # 22 & 29
rm(set.globals)

setwd(curr.dir)
