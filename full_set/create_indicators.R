# main file for generating all indicators

wrk.dir <- "/Users/hana/R/vision2050indicators/full_set"
# wrk.dir <- "C:/Users/CLam/Desktop/shiny_vision2050/full_set"
curr.dir <- getwd()
setwd(wrk.dir)

run.screening.factors <- TRUE

# global settings
source("settings.R")
set.globals <- TRUE

# run scripts
source("functions.R")
if(run.screening.factors) {
  source("../screening_factors/jobs_housing_ratio.R") # 15, 18
  setwd(wrk.dir)
  source("../screening_factors/housing_type_mix.R") # 17
  setwd(wrk.dir)
  source("../screening_factors/growth_opp_areas.R") # 79
  setwd(wrk.dir)
  source("../screening_factors/jobs_pop_tod_areas.R") # 30
  setwd(wrk.dir)
  source("../screening_factors/risk_disp_areas.R") # 80
  setwd(wrk.dir)
}
#source("allocation_pop_emp_au.R") # 16; gridcell indicator (does not need to be run every time)
#source("pop_emp_density.R") # 22 & 29; gridcell indicators (do not need to be run every time)
source("growth_proximity.R") # 28a & 28b, 31, 64
source("redev_infill.R") # 32
source("imper_surface.R") # 55

# cleanup
rm(set.globals)
setwd(curr.dir)
