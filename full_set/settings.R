rund <- "/Volumes/Model\ Data/vision2050/opusgit/urbansim_data/data/psrc_parcel/runs"
base <- list(Aws01 = file.path(rund, "awsmodel01"),
             Aws03 = file.path(rund, "awsmodel03"),
             Aws04 = file.path(rund, "awsmodel04"),
             Aws05 = file.path(rund, "awsmodel05"),
             Aws06 = file.path(rund, "awsmodel06"),
             Aws07 = file.path(rund, "awsmodel07")
)
script.dir <- "/Users/hana/R/vision2050indicators/full_set" 

run.dir <- c("iSTC" = "run_3.run_2018_08_17_13_06",
             "iDUG" = "run_1.run_2018_08_17_15_45", 
             "iH2O2" = "run_4.run_2018_08_17_16_15"#, 
             #"TOD" = "run_3.run_2018_08_10_21_04"
             )  
indicator.dirnm <- "indicators"
#data.dir <- "../data"
data.dir <- "/Volumes/DataTeam/Projects/V2050/SEIS/Data_Support/script_input"
out.dir <- "/Volumes/DataTeam/Projects/V2050/SEIS/Data_Support/Model_Output"

years <- c(2050)
byr <- 2017

enlist.lu.nm <- "enlisted_personnel_geo.xlsx"
enlist.lu <- read.xlsx(file.path(data.dir, enlist.lu.nm))

enlist.mil.file.nm <- "enlisted_personnel_SoundCast_08202018.csv"

gq.file.nm <- "group-quarters.xlsx"
gq.file <- read.xlsx(file.path(data.dir, "group-quarters.xlsx"), check.names = TRUE)
colnames(gq.file)[grep("^X\\d+", colnames(gq.file))] <- gsub("X", "yr", colnames(gq.file)[grep("^X\\d+", colnames(gq.file))])

settings <- list(goa = list(out.file.nm = "79_dist_growth_opp_areas"),
                 htm = list(out.file.nm = "17_housing_type_mix"),
                 pjta = list(out.file.nm = "30_jobs_pop_tod_areas"),
                 jhr = list(out.file.nm = "18_jobs_housing_ratio"),
                 alloc = list(out.file.nm = "16_allocation_pop_emp_au"),
                 epden = list(out.file.nm.byr = "22_pop_emp_au_density",
                               out.file.nm <- "29_pop_emp_au_density")
)
