library(data.table)

# Aws-model04 - iSTC - \\aws-model04\e$\opusgit\urbansim_data\data\psrc_parcel\runs\run_3.run_2018_08_17_13_06
# Aws-model03 - iDUG - \\aws-model03\e$\opusgit\urbansim_data\data\psrc_parcel\runs\run_1.run_2018_08_17_15_45
# Aws-model05 - iH2O2 - \\aws-model04\e$\opusgit\urbansim_data\data\psrc_parcel\runs\run_4.run_2018_08_17_16_15

# Aws-model07 - STC - \\aws-model07\E$\opusgit\urbansim_data\data\psrc_parcel\runs\run_2.run_2018_08_15_13_45
# Aws-model03 - DUG - \\aws-model03\e$\opusgit\urbansim_data\data\psrc_parcel\runs\run_22.run_2018_08_10_21_05
# Aws-model04 - H2O2 - \\aws-model04\e$\opusgit\urbansim_data\data\psrc_parcel\runs\run_1.run_2018_08_10_21_05
# Aws-model05 - TOD - \\aws-model05\e$\opusgit\urbansim_data\data\psrc_parcel\runs\run_3.run_2018_08_10_21_04


# user input --------------------------------------------------------------

this.dir <- "C:/Users/CLam/Desktop/shiny_vision2050/scripts" 
# this.dir <-  dirname(parent.frame(2)$ofile)
setwd(this.dir)
source("all_runs.R")

# urbansim
run.dir <- c("iSTC" = "run_3.run_2018_08_17_13_06",
             "iDUG" = "run_1.run_2018_08_17_15_45", 
             "iH2O2" = "run_4.run_2018_08_17_16_15", 
             "TOD" = "run_3.run_2018_08_10_21_04") 
years <- c(2050) # only one year

out.dir <- "../scripts_results"
dsa.dir <- "X:/DSA/Vision2050/land_use_tables"
data.dir <- file.path("../data")

out.file.nm <- "housing_type_mix"

# lookup table
constraints <- fread(file.path(data.dir, "development_constraints.csv"))


# transform data ----------------------------------------------------------

# the density brackets
density.split <- list(res = c(12, 50), nonres = c(1, 3))
filename <- "parcel__dataset_table__households_jobs__2050.tab" 

dlist <- NULL
for (r in 1:length(run.dir)) { # for each run
  pcl <- df <- NULL
  base.dir <- purrr::pluck(allruns, run.dir[r]) 
  
  # load parcels - this will be an indicator file
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
  df[, delta := residential_units - residential_units_base
     ][, sum_delta := sum(delta)
       ][, share_delta := delta/sum_delta
         ][, `:=` (run = run.dir[r], scenario = names(run.dir[r]))]
  setcolorder(df, c("run", "scenario", "res_density_type", cols[2], cols[1], "delta", "sum_delta", "share_delta"))
  setnames(df, cols[1], paste0(cols[1], "_yr", years))
  dlist[[names(run.dir[r])]] <- df
}


# export ------------------------------------------------------------------

write.xlsx(dlist, file.path(dsa.dir, paste0(out.file.nm, "_", Sys.Date(), ".xlsx")))
