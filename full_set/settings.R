library(openxlsx)
library(magrittr)
library(dplyr)
library(data.table)

# base indicator directory (L drive)
# rund <- "/Volumes/Model\ Data/vision2050/opusgit/urbansim_data/data/psrc_parcel/runs"
# rund <- "L:/vision2050/opusgit/urbansim_data/data/psrc_parcel/runs"
# rund <- "N:/vision2050/opusgit/urbansim_data/data/psrc_parcel/runs"
# base <- list(Aws01 = file.path(rund, "awsmodel01"),
#              Aws03 = file.path(rund, "awsmodel03"),
#              Aws04 = file.path(rund, "awsmodel04"),
#              Aws05 = file.path(rund, "awsmodel05"),
#              Aws06 = file.path(rund, "awsmodel06"),
#              Aws07 = file.path(rund, "awsmodel07"),
#              Aws08 = file.path(rund, "awsmodel08")
#             )
script.dir <- "/Users/hana/R/vision2050indicators/full_set"

# base indicator directories (L and N drives)
rund <- list("L:/vision2050/opusgit/urbansim_data/data/psrc_parcel/runs", 
             "N:/vision2050/opusgit/urbansim_data/data/psrc_parcel/runs")
#rund <- list(#"/Volumes/Model\ Data/vision2050/opusgit/urbansim_data/data/psrc_parcel/runs", 
#             "/Volumes/Model\ Data\ 2/vision2050/opusgit/urbansim_data/data/psrc_parcel/runs")
base <- c()
for (r in 1:length(rund)) {
  aws <- paste0("awsmodel0", c(1, 3:8))
  for (a in 1:length(aws)) {
    base <- append(base, file.path(rund[[r]], aws[[a]]), length(base))
  }
}

run.dir <- c("DeepSix" = "run_44.run_2019_10_18_15_35",
             "WildBill" = "run_39.run_2019_10_18_15_36"
             #"Breaker" = "run_12.run_2019_10_04_12_54",
             #"Clutch" = "run_19.run_2019_10_04_13_12",
             #"Flash" = "run_43.run_2019_10_04_13_22",
             #"Grunt" = "run_38.run_2019_10_04_13_28",
             #"Duke" ="run_2.run_2019_05_22_12_21",
             #"Shipwreck" = "run_14.run_2019_05_15_20_08",
             #"Roadblock"= "run_14.run_2019_05_17_13_46",
             #"StormShadow" = "run_11.run_2019_09_20_15_31",
             #"Flint" = "run_18.run_2019_09_20_15_49",
             #"QuickKick" = "run_1.run_2019_05_18_07_28",
             #"Tomax" = "run_10.run_2019_09_13_13_47"
             #"Tripwire" = "run_14.run_2019_08_25_15_03",
             #"Firefly" = "run_8.run_2019_08_23_14_49",
             #"Copperhead" = "run_13.run_2019_08_22_19_09"
             #"SgtSlaughter" = "run_7.run_2019_07_04_11_37"#, # N drive
             #"NemesisEnforcer" = "run_4.run_2019_06_29_07_47", # N drive
             #"LiftTicket" = "run_3.run_2019_06_13_14_36"#,
             #"STC" = "KEEP DSEIS STC run_6.run_2018_10_23_11_15",
             #"RUG" = "KEEP DSEIS RUG run_5.run_2018_10_25_09_07",
             #"TFG" = "KEEP DSEIS TFG run_8.run_2018_10_29_15_01"#,
             # "DUG" = "run_4.run_2018_10_02_11_57", 
             # "H2O2" = "run_6.run_2018_10_02_12_01",
             # "TOD" = "run_3.run_2018_10_02_14_30",
             # "H2O2-JH" = "run_2.run_2018_10_05_14_50",
             # "DUG-JH" = "run_1.run_2018_10_01_20_37",
             # "TOD-JH" = "run_12.run_2018_10_05_15_04",
             #"DW" = "run_3.run_2018_10_11_13_14",
             #"DW-JH" = "run_13.run_2018_10_11_13_14"
             ) 

indicator.dirnm <- "indicators"

# base directory used for various inputs and outputs
# bdir <- "/Volumes/DataTeam/Projects/V2050/SEIS/Data_Support"
#bdir <- "~/DataTeam/Projects/V2050/SEIS/Data_Support"
bdir <- "J:/Projects/V2050/SEIS/Data_Support"

data.dir <- file.path(bdir, "script_input")
# out.dir <- file.path(bdir, "Model_Output/Working")
out.dir <- "J:/Projects/V2050/PUGS-PA/Model_Output"
# out.dir <- "C:/Users/clam/Desktop/shiny_vision2050/scripts_results" # Test
# out.dir <- "/Volumes/DataTeam/Projects/V2050/PUGS-PA/Model_Output"
# out.dir <- file.path(bdir, "Model_Output") # for final outputs
out.dir.maps <- file.path(out.dir, "Maps")
# out.dir.maps <- file.path(bdir, "Model_Output", "Maps")

years <- c(2050)
byr <- 2017
years.col <- paste0("yr", years)
byr.col <- paste0("yr", byr)
years.to.keep <- c(byr.col, years.col)

# These settings are needed because the ones above can get overwritten by the screening factors scripts
fs.years <- years
fs.byr <- byr
fs.years.col <- years.col
fs.byr.col <- byr.col
fs.years.to.keep <- years.to.keep
fs.years.to.keep.int <- c(byr, years)

# military employment
enlist.lu.nm <- "enlisted_personnel_geo.xlsx"
enlist.lu <- read.xlsx(file.path(data.dir, enlist.lu.nm))

enlist.mil.file.nm <- "enlisted_personnel_SoundCast_08202018.csv"

# group quarters
process.colnames <- function(df){
  colnames(df)[grep("^X\\d+", colnames(df))] <- gsub("X", "yr", colnames(df)[grep("^X\\d+", colnames(df))])
  df
}
# gq.file.nm <- "group-quarters_original.xlsx" # does not have buffer ids -> will not work with 28a,b
gq.file.nm <- "group_quarters_geo.xlsx"
gq.file.nm.sup <- "group_quarters_geo.xlsx" # contains additional geographies

gq.file <- read.xlsx(file.path(data.dir, gq.file.nm)) # for Christy's scripts
gq.file2 <- read.xlsx(file.path(data.dir, gq.file.nm), check.names = TRUE) # for Hana's scripts
gq.file2 <- process.colnames(gq.file2)

# # add more geographies to the original gq file
# gq.file.sup <- read.xlsx(file.path(data.dir, gq.file.nm.sup), check.names = TRUE)
# gq.file.sup <- process.colnames(gq.file.sup)
# miscols <- setdiff(colnames(gq.file.sup), colnames(gq.file2))
# gq.file2 <- merge(gq.file2, gq.file.sup[, c("record", miscols)], by = "record")

# for Christy's scripts
eqlu.file <- read.xlsx(file.path(data.dir, "2015-and-2016-5yr-ACS-Equity-Populations-20181009.xlsx"), sheet = "acs5yr_2016") %>% as.data.table

# Script-specific settings
settings <- list(goa = list(out.file.nm = "79_dist_growth_opp_areas"),
                 htm = list(out.file.nm = "17_housing_type_mix"),
                 pjta = list(out.file.nm = "30_jobs_pop_tod_areas"),
                 jhr = list(out.file.nm = "18_jobs_housing_ratio"),
                 alloc = list(out.file.nm = "16_allocation_pop_emp_au"),
                 epden = list(out.file.nm.byr = "22_pop_emp_au_density",
                              out.file.nm = "29_pop_emp_au_density"),
                 gpro = list(out.file.nm.a = "28a_transit_proximity",
                             out.file.nm.b = "28b_uga_proximity", 
                             include.equity = c(TRUE, FALSE)),
		            park = list(out.file.nm = "64_buffered_parks", 
		                        include.actuals = TRUE, include.equity = TRUE,
		                        total.suffix = "_inside_ugb",
		                        milgq.filter = list(park_buffer = list( quo(`==`(!!sym("park_buffer_id"), 1))),
		                                            total = list( quo(`==`(!!sym("inside_ugb"), 1)))),
		                        eq.milgq.filter = list(park_buffer = list( quo(`==`(!!sym("park_buffer_id"), 1))),
		                                            total = list( quo(`==`(!!sym("inside_ugb"), 1))))
		                      ),
		            gamn = list(out.file.nm = "31_growth_amentities", include.equity = TRUE),
		            redevinf = list(out.file.nm = "32_redevelopment_infill"),
		            impersurf = list(out.file.nm = "55_impervious_surface"),
		            sewr = list(out.file.nm = "58_sewer_proximity"),
		            ard = list(out.file.nm = "80_areas_risk_displacement")
            )

# assigns directories to runs
source("../screening_factors/all_runs.R")

