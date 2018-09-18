library(openxlsx)

# base indicator directory (L drive)
rund <- "/Volumes/Model\ Data/vision2050/opusgit/urbansim_data/data/psrc_parcel/runs"
# rund <- "L:/vision2050/opusgit/urbansim_data/data/psrc_parcel/runs"
base <- list(Aws01 = file.path(rund, "awsmodel01"),
             Aws03 = file.path(rund, "awsmodel03"),
             Aws04 = file.path(rund, "awsmodel04"),
             Aws05 = file.path(rund, "awsmodel05"),
             Aws06 = file.path(rund, "awsmodel06"),
             Aws07 = file.path(rund, "awsmodel07")
            )
script.dir <- "/Users/hana/R/vision2050indicators/full_set"
run.dir <- c("STC" = "run_3.run_2018_08_17_13_06",
             "DUG" = "run_1.run_2018_08_17_15_45", 
             "H2O2" = "run_4.run_2018_08_17_16_15",
             "TODtd" = "run_9.run_2018_08_30_22_08"
             #"TODprelim" = "run_3.run_2018_08_10_21_04"
             ) 

indicator.dirnm <- "indicators"

# base directory used for various inputs and outputs
bdir <- "/Volumes/DataTeam/Projects/V2050/SEIS/Data_Support"
#bdir <- "J:/Projects/V2050/SEIS/Data_Support"

data.dir <- file.path(bdir, "script_input")
out.dir <- file.path(bdir, "Model_Output/Working")
#out.dir <- file.path(bdir, "Model_Output") # for final outputs 
out.dir.maps <- file.path(out.dir, "Maps")

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

# military employment
enlist.lu.nm <- "enlisted_personnel_geo.xlsx"
enlist.lu <- read.xlsx(file.path(data.dir, enlist.lu.nm))

enlist.mil.file.nm <- "enlisted_personnel_SoundCast_08202018.csv"

# group quarters
process.colnames <- function(df){
  colnames(df)[grep("^X\\d+", colnames(df))] <- gsub("X", "yr", colnames(df)[grep("^X\\d+", colnames(df))])
  df
}
gq.file.nm <- "group-quarters_original.xlsx" # does not have buffer ids -> will not work with 28a,b
gq.file.nm.sup <- "group_quarters_geo.xlsx" # contains additional geographies

gq.file <- read.xlsx(file.path(data.dir, gq.file.nm)) # for Christy's scripts
gq.file2 <- read.xlsx(file.path(data.dir, gq.file.nm), check.names = TRUE) # for Hana's scripts
gq.file2 <- process.colnames(gq.file2)

# add more geographies to the original gq file
gq.file.sup <- read.xlsx(file.path(data.dir, gq.file.nm.sup), check.names = TRUE)
gq.file.sup <- process.colnames(gq.file.sup)
miscols <- setdiff(colnames(gq.file.sup), colnames(gq.file2))
gq.file2 <- merge(gq.file2, gq.file.sup[, c("record", miscols)], by = "record")

# Script-specific settings
settings <- list(goa = list(out.file.nm = "79_dist_growth_opp_areas"),
                 htm = list(out.file.nm = "17_housing_type_mix"),
                 pjta = list(out.file.nm = "30_jobs_pop_tod_areas"),
                 jhr = list(out.file.nm = "18_jobs_housing_ratio"),
                 alloc = list(out.file.nm = "16_allocation_pop_emp_au"),
                 epden = list(out.file.nm.byr = "22_pop_emp_au_density",
                              out.file.nm = "29_pop_emp_au_density"),
                 gpro = list(out.file.nm.a = "28a_transit_proximity",
                             out.file.nm.b = "28b_uga_proximity"),
		            park = list(out.file.nm = "64_buffered_parks"),
		            gamn = list(out.file.nm = "31_growth_amentities")
            )

# assigns directories to runs
source("../screening_factors/all_runs.R")

