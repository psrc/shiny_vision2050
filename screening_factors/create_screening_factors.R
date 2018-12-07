# generate screening factors

wrk.dir <- "C:/Users/CLam/Desktop/shiny_vision2050/screening_factors"
curr.dir <- getwd()
setwd(wrk.dir)

source("jobs_housing_ratio.R")
source("housing_type_mix.R")
source("growth_opp_areas.R") 
source("jobs_pop_tod_areas.R")
source("risk_disp_areas.R")