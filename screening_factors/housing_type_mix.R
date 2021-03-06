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

cat("\nComputing indicator 17, housing type mix\n")
out.file.nm <- settings$htm$out.file.nm

# lookup table
constraints <- fread(file.path(data.dir, "development_constraints.csv"))
#capacity <- fread(file.path(data.dir, "CapacityIndicatorPcl_res50.csv"))


# transform data ----------------------------------------------------------

# the density brackets
density.split <- list(res = c(12, 50), nonres = c(1, 3))
filename <- "parcel__dataset_table__households_jobs__2050.tab" 
filename.byr <- "parcel__dataset_table__households_jobs__2017.tab" 

dlist <- NULL
for (r in 1:length(run.dir)) { # for each run
  pcl <- df <- NULL
  base.dir <- purrr::pluck(allruns, run.dir[r])
  
  # load parcels - this will be an indicator file
  pcl <- fread(file.path(base.dir, "indicators", filename))
  pcl.byr <- fread(file.path(base.dir, "indicators", filename.byr))
  
  # get the maximum density
  constr <- constraints[, .(max_dens = max(maximum)), by = .(plan_type_id, constraint_type)]
  
  # derive max residential and non-res density, remove original residential_units_base
  pcl[constr[constraint_type == "units_per_acre"], max_res_density := i.max_dens, on = "plan_type_id"][, residential_units_base := NULL]
  # pcl[constr[constraint_type == "far"], max_nonres_density := i.max_dens, on = "plan_type_id"]
  
  # categorize into three groups
  #cap <- capacity[, .(parcel_id, DUbase)]
  pcl[pcl.byr, residential_units_base := i.residential_units, on = "parcel_id"][is.na(residential_units_base), residential_units_base := 0]
  pcl[, `:=` (real_residential_units = pmax(residential_units, households),
              res_density_type = factor(ifelse(max_res_density < density.split$res[1], "low",
                                               ifelse(max_res_density >= density.split$res[2], "high", "med")), levels = c("low", "med", "high")))]
  # pcl[, nonres_density_type := factor(ifelse(max_nonres_density < density.split$nonres[1], "low",
  #                                            ifelse(max_nonres_density >= density.split$nonres[2], "high", "med")),
  #                                     levels = c("low", "med", "high"))]
  
  cols <- c("real_residential_units", "residential_units_base")
  tot.cols <- paste0("sum_", cols)
  share.cols <- paste0("share_", cols)
  share.expr1 <- parse(text = paste0(share.cols[2], ":=", cols[2], "/", tot.cols[2]))
  share.expr2 <- parse(text = paste0(share.cols[1], ":=", cols[1], "/", tot.cols[1]))
  
  # region
  compile.region <- function(table) {
    df <- table[!is.na(res_density_type), lapply(.SD, sum), .SDcols = cols, by = "res_density_type"][order(res_density_type)]
    tot <- df[, lapply(.SD, sum), .SDcols = cols][, res_density_type := "total"]
    df[, (tot.cols[2]) := tot$residential_units_base
       ][,(tot.cols[1]) := tot$real_residential_units
         ][, delta := real_residential_units - residential_units_base
           ][, sum_delta := sum(delta)
             ][, geography := "Region"]
  }
  
  # county
  compile.cnty <- function(table) {
    df <- table[!is.na(res_density_type), lapply(.SD, sum), .SDcols = cols, by = .(county_id, res_density_type)][order(county_id, res_density_type)]
    tot <- df[, lapply(.SD, sum), .SDcols = cols, by = county_id]
    setnames(tot, cols, tot.cols)
    df2 <- df[tot, on = "county_id"][, delta := real_residential_units - residential_units_base]
    df2.sum.delta <- df2[, lapply(.SD, sum), .SDcols = "delta", by = county_id]
    setnames(df2.sum.delta, "delta", "sum_delta")
    df3 <- df2[df2.sum.delta, on = "county_id"
               ][, geography := switch(as.character(county_id), "33" = "King", "35" = "Kitsap", "53" = "Pierce", "61" = "Snohomish"), by = county_id
                 ][, county_id := NULL]
  }
  
  # poverty 
  compile.poverty <- function(table) {
    df <- table[!is.na(res_density_type), lapply(.SD, sum), .SDcols = cols, by = .(poverty_id, res_density_type)][order(poverty_id, res_density_type)]
    tot <- df[, lapply(.SD, sum), .SDcols = cols, by = poverty_id]
    setnames(tot, cols, tot.cols)
    df2 <- df[tot, on = "poverty_id"][, delta := real_residential_units - residential_units_base]
    df2.sum.delta <- df2[, lapply(.SD, sum), .SDcols = "delta", by = poverty_id]
    setnames(df2.sum.delta, "delta", "sum_delta")
    df3 <- df2[df2.sum.delta, on = "poverty_id"
               ][, geography := switch(as.character(poverty_id), "2" = "poverty", "1" = "non-poverty"), by = poverty_id
                 ][, poverty_id := NULL]
    
  }  
  
  # minority
  compile.minority <- function(table) {
    df <- table[!is.na(res_density_type), lapply(.SD, sum), .SDcols = cols, by = .(minority_id, res_density_type)][order(minority_id, res_density_type)]
    tot <- df[, lapply(.SD, sum), .SDcols = cols, by = minority_id]
    setnames(tot, cols, tot.cols)
    df2 <- df[tot, on = "minority_id"][, delta := real_residential_units - residential_units_base]
    df2.sum.delta <- df2[, lapply(.SD, sum), .SDcols = "delta", by = minority_id]
    setnames(df2.sum.delta, "delta", "sum_delta")
    df3 <- df2[df2.sum.delta, on = "minority_id"
               ][, geography := switch(as.character(minority_id), "2" = "minority" , "1"= "non-minority"), by = minority_id
                 ][, minority_id := NULL]
  }
  
  df.cnty <-compile.cnty(pcl)
  df.reg <- compile.region(pcl)
  df.poverty <-compile.poverty(pcl)
  df.minority <-compile.minority(pcl)
  
  df.all <- rbindlist(list(df.cnty, df.reg, df.poverty, df.minority), use.names = TRUE)
  
  df.all[, share_delta := delta/sum_delta
         ][, `:=` (run = run.dir[r], scenario = names(run.dir[r]))
           ][, eval(share.expr1)
             ][, eval(share.expr2)]
  
  setcolorder(df.all, 
              c("geography", "run", "scenario", "res_density_type", cols[2], cols[1], rev(tot.cols), rev(share.cols), "delta", "sum_delta", "share_delta"))
  colnames(df.all)[grep("residential_units$", colnames(df.all))] <- str_replace_all(colnames(df.all)[grep("residential_units$", colnames(df.all))], 
                                                                                    "real_residential_units$", 
                                                                                    paste0("residential_units_yr", years))
  counties <- c("King", "Kitsap", "Pierce", "Snohomish")
  countyname.sort <- c(counties, "poverty", "non-poverty", "minority", "non-minority", "Region")
  dt.all <- df.all[, county.sort := factor(geography, levels = countyname.sort)
                   ][order(county.sort)
                     ][, `:=`(county.sort = NULL)]
  
  dlist[[names(run.dir[r])]] <- dt.all
}


# export ------------------------------------------------------------------

write.xlsx(dlist, file.path(out.dir, paste0(out.file.nm, "_", Sys.Date(), ".xlsx")))

setwd(curr.dir)
