rund <- "/Volumes/Model\ Data/vision2050/opusgit/urbansim_data/data/psrc_parcel/runs"
base <- list(Aws01 = file.path(rund, "awsmodel01"),
             Aws03 = file.path(rund, "awsmodel03"),
             Aws04 = file.path(rund, "awsmodel04"),
             Aws05 = file.path(rund, "awsmodel05"),
             Aws06 = file.path(rund, "awsmodel06"),
             Aws07 = file.path(rund, "awsmodel07")
)
script.dir <- "/Users/hana/R/vision2050indicators/full_set" 

run.dir <- c("iSTC" = "run_3.run_2018_08_17_13_06"#,
             #"iDUG" = "run_1.run_2018_08_17_15_45", 
             #"iH2O2" = "run_4.run_2018_08_17_16_15"#, 
             #"TOD" = "run_3.run_2018_08_10_21_04"
             )  
indicator.dirnm <- "indicators"
data.dir <- "../data"

set.globals <- TRUE


