library(openxlsx)
library(magrittr)
library(dplyr)


# general settings --------------------------------------------------------


rund <- "L:/vision2050/opusgit/urbansim_data/data/psrc_parcel/runs" # base indicator directory (L drive)
bdir <- "J:/Projects/V2050/SEIS/Data_Support" # base directory used for various inputs 
odir <- "J:/Projects/V2050/Supp_Alt_Analysis" # ..and outputs

# Hana's settings
# rund <- "/Volumes/Model\ Data/vision2050/opusgit/urbansim_data/data/psrc_parcel/runs"
# bdir <- "/Volumes/DataTeam/Projects/V2050/SEIS/Data_Support"
# bdir <- "~/DataTeam/Projects/V2050/SEIS/Data_Support"
# indicator.dirnm <- "indicators"

base <- list(Aws01 = file.path(rund, "awsmodel01"),
             Aws03 = file.path(rund, "awsmodel03"),
             Aws04 = file.path(rund, "awsmodel04"),
             Aws05 = file.path(rund, "awsmodel05"),
             Aws06 = file.path(rund, "awsmodel06"),
             Aws07 = file.path(rund, "awsmodel07"),
             Aws08 = file.path(rund, "awsmodel08")
)

run.dir <- c("STC" = "run_6.run_2018_10_23_11_15",
             "RUG" = "run_5.run_2018_10_25_09_07",
             "TFG" = "run_8.run_2018_10_29_15_01") 

data.dir <- file.path(bdir, "script_input")

out.dir <- file.path(odir, "Model_Output")

fcast.yrs <- c(2017, 2050)

# read data ---------------------------------------------------------------

ofm.file.nm <- "J:/OtherData/OFM/SAEP/elmer/ofm_saep_2018.csv"
enlist.lu.nm <- "enlisted_personnel_geo.xlsx"
enlist.lu <- read.xlsx(file.path(data.dir, enlist.lu.nm))

enlist.mil.file.nm <- "enlisted_personnel_SoundCast_08202018.csv"
gq.file.nm <- "group_quarters_geo.xlsx"


settings <- list(aagr_rgc_bsa = list(out.file.nm = "2_aagr_rgc_bsa"),
                 aagr_rgc_bsa_type_by_juris = list(out.file.nm = "5_aagr_rgc_bsa_by_type_juris")
                 ) 

source("../screening_factors/all_runs.R")