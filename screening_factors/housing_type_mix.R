library(data.table)
library(openxlsx)

# settings --------------------------------------------------------------

if(!exists("set.globals") || !set.globals) {
  curr.dir <- getwd()
  this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(this.dir)
  source("settings.R")
  source("functions.R")
}

source("all_runs.R")

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
  tot.cols <- paste0("sum_", cols)
  share.cols <- paste0("share_", cols)
  share.expr1 <- parse(text = paste0(share.cols[2], ":=", cols[2], "/", tot.cols[2]))
  share.expr2 <- parse(text = paste0(share.cols[1], ":=", cols[1], "/", tot.cols[1]))

  # region
  df <- pcl[!is.na(res_density_type), lapply(.SD, sum), .SDcols = cols, by = "res_density_type"][order(res_density_type)]
  tot.reg <- df[, lapply(.SD, sum), .SDcols = cols][, res_density_type := "total"]
  df[, (tot.cols[2]) := tot.reg$residential_units_base
     ][,(tot.cols[1]) := tot.reg$residential_units
       ][, delta := residential_units - residential_units_base
         ][, sum_delta := sum(delta)
           ][, geography := "Region"]

  # county
  df.cnty <- pcl[!is.na(res_density_type), lapply(.SD, sum), .SDcols = cols, by = .(county_id, res_density_type)][order(county_id, res_density_type)]
  tot.cnty <- df.cnty[, lapply(.SD, sum), .SDcols = cols, by = county_id][, `:=` (res_density_type = "total")]
  
  tot.cnty2 <- tot.cnty[, 1:3]
  setnames(tot.cnty2, cols, tot.cols)
  
  df.cnty2 <- df.cnty[tot.cnty2, on = "county_id"
                       ][, delta := residential_units - residential_units_base]
  df.cnty2.sum.delta <- df.cnty2[, lapply(.SD, sum), .SDcols = "delta", by = county_id]
  setnames(df.cnty2.sum.delta, "delta", "sum_delta")
  df.cnty3 <- df.cnty2[df.cnty2.sum.delta, on = "county_id"
                       ]
  # [county_id == 33, geography := "King"
  #                        ][county_id == 35, geography := "Kitsap"
  #                          ][county_id == 53, geography := "Pierce"
  #                            ][county_id == 61, geography := "Snohomish"
  #                              ][, county_id := NULL]
  
  df.cnty3[, geography := switch(as.character(county_id), "33" = "King", "35" = "Kitsap", "53" = "Pierce", "61" = "Snohomish"), by = county_id][, county_id := NULL]
  
  df.all <- rbindlist(list(df.cnty3, df), use.names = TRUE)
  
  df.all[, share_delta := delta/sum_delta
             ][, `:=` (run = run.dir[r], scenario = names(run.dir[r]))
               ][, eval(share.expr1)
                 ][, eval(share.expr2)]

  setcolorder(df.all, c("geography", "run", "scenario", "res_density_type", cols[2], cols[1], rev(tot.cols), rev(share.cols), "delta", "sum_delta", "share_delta"))
  
  
  dlist[[names(run.dir[r])]] <- df.all
}


# export ------------------------------------------------------------------

write.xlsx(dlist, file.path(out.dir, paste0(out.file.nm, "_", Sys.Date(), ".xlsx")))

setwd(curr.dir)
