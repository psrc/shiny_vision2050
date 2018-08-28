rund <- "/Volumes/Model\ Data/vision2050/opusgit/urbansim_data/data/psrc_parcel/runs"
base <- list(Aws01 = file.path(rund, "awsmodel01"),
             Aws05 = file.path(rund, "awsmodel05"),
             Aws06 = file.path(rund, "awsmodel06"),
             Aws07 = file.path(rund, "awsmodel07")
)
script.dir <- "/Users/hana/R/vision2050indicators/full_set" 

run.dir <- c("iSTC" = "run_3.run_2018_08_10_21_04",
             "iDUG" = "run_3.run_2018_08_10_21_04", 
             "iH2O2" = "run_3.run_2018_08_10_21_04", 
             "TOD" = "run_3.run_2018_08_10_21_04")  

set.globals <- TRUE


