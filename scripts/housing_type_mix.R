library(data.table)

# hit 'source'
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
source("all_runs.R")

# user input --------------------------------------------------------------

run.dir <- c("run_2.run_2018_07_17_15_22")
out.dir <- "../scripts_results"
data.dir <- file.path("../data")
years <- c(2050)

# lookup table
constraints <- fread(file.path(data.dir, "development_constraints.csv"))


# transform data ----------------------------------------------------------

# the density brackets
density.split <- list(res = c(10, 45), nonres = c(1, 3))

# load parcels - this will be an indicator file
base.dir <- purrr::pluck(allruns, run.dir)
filename <- "parcel__dataset_table__households_jobs__2050.tab"
pcl <- fread(file.path(base.dir, "indicators", filename))

# get the maximum density
constr <- constraints[, .(max_dens = max(maximum)), by = .(plan_type_id, constraint_type)]

# derive max residential and non-res density
pcl[constr[constraint_type == "units_per_acre"], max_res_density := i.max_dens, on = "plan_type_id"]
# pcl[constr[constraint_type == "far"], max_nonres_density := i.max_dens, on = "plan_type_id"]

# categorize into three groups
pcl[, res_density_type := factor(ifelse(max_res_density < density.split$res[1], "low",
                                        ifelse(max_res_density >= density.split$res[2], "high", "med")),
                                 levels = c("low", "med", "high"))]
# pcl[, nonres_density_type := factor(ifelse(max_nonres_density < density.split$nonres[1], "low",
#                                            ifelse(max_nonres_density >= density.split$nonres[2], "high", "med")),
#                                     levels = c("low", "med", "high"))]

cols <- c("residential_units", "residential_units_base")
df <- pcl[!is.na(res_density_type), lapply(.SD, sum), .SDcols = cols, by = "res_density_type"][order(res_density_type)] 
df[, delta := residential_units - residential_units_base][, sum_delta := sum(delta)][, share_delta := delta/sum_delta]


# export ------------------------------------------------------------------

# write.xlsx(d2, file.path(out.dir, "housing_type_mix.xlsx"))
write.csv(df, file.path(out.dir, "housing_type_mix.csv"), row.names = FALSE)
write.csv(df, file.path("X:/DSA/Vision2050/land_use_tables", "housing_type_mix.csv"), row.names = FALSE)
