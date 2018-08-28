library(data.table)

if(!exists("set.globals") || !set.globals) {
  source("settings.R")
  # setwd(script.dir)
  source("functions.R")
}

# settings --------------------------------------------------------------

curr.dir <- getwd()
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)
# source("settings.R")
# source("functions.R")
source("all_runs.R")

run.dir <- settings$global$run.dir
years <- settings$global$years
data.dir <-settings$global$data.dir
out.dir <- settings$global$out.dir
out.file.nm <- settings$htm$out.file.nm

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

write.xlsx(dlist, file.path(out.dir, paste0(out.file.nm, "_", Sys.Date(), ".xlsx")))

setwd(curr.dir)
set.globals <- FALSE