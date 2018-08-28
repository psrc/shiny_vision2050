library(data.table)

if(!exists("set.globals") || !set.globals) {
  source("settings.R")
  setwd(script.dir)
  source("functions.R")
}
source("all_runs.R")

years <- c(2050) # only one year

out.file.nm <- "16_allocation_pop_emp_au"

geos <- c("grid", "fips_rgs")
attributes <- c("population", "employment", "households")
ind.extension <- ".csv"

for (r in 1:length(run.dir)) {
  for(geo in geos) {
    alldata <- compile.tbl(geo)
  }
}