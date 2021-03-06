rund <- list("L:/vision2050/opusgit/urbansim_data/data/psrc_parcel/runs", "N:/vision2050/opusgit/urbansim_data/data/psrc_parcel/runs")
# rund <- "L:/vision2050/opusgit/urbansim_data/data/psrc_parcel/runs"

base <- c()
for (r in 1:length(rund)) {
  aws <- paste0("awsmodel0", c(1, 3:8))
  for (a in 1:length(aws)) {
    base <- append(base, file.path(rund[[r]], aws[[a]]), length(base))
  }
}

# base <- list(Aws01 = file.path(rund, "awsmodel01"),
#              Aws03 = file.path(rund, "awsmodel03"),
#              Aws04 = file.path(rund, "awsmodel04"),
#              Aws05 = file.path(rund, "awsmodel05"),
#              Aws06 = file.path(rund, "awsmodel06"),
#              Aws07 = file.path(rund, "awsmodel07"),
#              Aws08 = file.path(rund, "awsmodel08"))
             
# base <- list(Aws01 = "//aws-model01/e$/opusgit/urbansim_data/data/psrc_parcel/runs",
#              Aws02 = "//aws-model02/e$/opusgit/urbansim_data/data/psrc_parcel/runs",
#              Aws03 = "//aws-model03/e$/opusgit/urbansim_data/data/psrc_parcel/runs",
#              Aws04 = "//aws-model04/e$/opusgit/urbansim_data/data/psrc_parcel/runs",
#              Aws05 = "//aws-model05/e$/opusgit/urbansim_data/data/psrc_parcel/runs",
#              Aws06 = "//aws-model06/e$/opusgit/urbansim_data/data/psrc_parcel/runs",
#              Aws07 = "//aws-model07/e$/opusgit/urbansim_data/data/psrc_parcel/runs",
#              Aws08 = "//aws-model08/e$/opusgit/urbansim_data/data/psrc_parcel/runs",
#              Aws09 = "//aws-model09/e$/opusgit/urbansim_data/data/psrc_parcel/runs",
#              Modelsrv5 = "//modelsrv5/d$/opusgit/urbansim_data/data/psrc_parcel/runs",
#              Modelsrv6 = "//modelsrv6/d$/opusgit/urbansim_data/data/psrc_parcel/runs",
#              Modelsrv8 = "//MODELSRV8/d$/opusgit/urbansim_data/data/psrc_parcel/runs",
#              Modelsrv3 = "//modelsrv3/e$/opusgit/urbansim_data/data/psrc_parcel/runs"
# )
ppa.list <- c() # add only name of run (e.g. "WildBill") if it is a PPA, it should also be included in run.dir. This is for indicator #30 (jobs_pop_tod_areas.R)
run.dir <- c(#"ShortFuse" = "run_13.run_2019_10_31_14_10",
             # "GrandSlam" = "run_20.run_2019_10_31_14_17",
             # "Torpedo" = "run_40.run_2019_10_31_14_31"
             "RockNRoll" = "run_45.run_2019_10_31_14_30"
             # "DeepSix" = "run_44.run_2019_10_18_15_35",
             # "WildBill" = "run_39.run_2019_10_18_15_36"#,
             # "Breaker" = "run_12.run_2019_10_04_12_54",
             # "Clutch" = "run_19.run_2019_10_04_13_12"#,
             # "SgtSlaughter" = "run_7.run_2019_07_04_11_37"#,
             # "NemesisEnforcer" = "run_4.run_2019_06_29_07_47" # N drive
             #"Serpentor" = "run_18.run_2019_06_06_19_53"#, # N drive
             # "Destro" = "run_16.run_2019_06_06_20_01"#, # N drive
             # "CobraCommander" = "run_17.run_2019_05_31_20_22"#, # L drive
             # "GungHo" = "run_12.run_2019_05_17_14_22",
             # "QuickKick" = "run_1.run_2019_05_18_07_28"#,
             #"Zartan" = "run_11.run_2019_05_09_12_51",
             #"DrMindbender" = "run_13.run_2019_05_09_12_57"#,
             # "PUG-TFG-DueDil" = "run_11.run_2019_05_06_14_39"#, 
             # "PPA-7575" = "run_12.run_2019_05_03_15_05",
             # "PPA-6575" = "run_9.run_2019_05_03_15_05"
             #"PUG-STC" = "run_10.run_2019_04_04_11_39", 
             # "PUG-RUG" = "run_6.run_2019_04_04_11_41",
             # "PUG-TFG" = "run_11.run_2019_04_18_12_42",
             # "PUG-RUG-boost-to-1" = "run_8.run_2019_04_22_20_49"
             # "PUG-TFG" = "run_10.run_2019_04_15_09_52"
             # "STC" = "KEEP DSEIS STC run_6.run_2018_10_23_11_15", #summer 2018/DEIS
             # "RUG" = "KEEP DSEIS RUG run_5.run_2018_10_25_09_07",#summer 2018/DEIS
             # "TFG" = "run_8.run_2018_10_29_15_01"#summer 2018/DEIS
             # "DUG" = "run_4.run_2018_10_02_11_57",
             # "DUG-JH" = "run_1.run_2018_10_01_20_37",
             # "H2O2" = "run_6.run_2018_10_02_12_01",
             # "H2O2-JH" = "run_2.run_2018_10_05_14_50",
             # "TOD" = "run_3.run_2018_10_02_14_30",
             # "TOD-JH" = "run_12.run_2018_10_05_15_04",
             # "DW" = "run_3.run_2018_10_11_13_14",
             # "DW-JH" = "run_13.run_2018_10_11_13_14"
             )

# out.dir <- "X:/DSA/Vision2050/land_use_tables"
# out.dir <- "J:/Projects/V2050/SEIS/Data_Support/Model_Output"
out.dir <- "J:/Projects/V2050/PUGS-PA/Model_Output"
# out.dir <- "C:/Users/CLam/Desktop/shiny_vision2050/scripts_results"
          
data.dir <- "J:/Projects/V2050/SEIS/Data_Support/script_input"
years <- c(2050)
byr <- 2017

enlist.lu.nm <- "enlisted_personnel_geo.xlsx"
enlist.lu <- read.xlsx(file.path(data.dir, enlist.lu.nm))

enlist.mil.file.nm <- "enlisted_personnel_SoundCast_08202018.csv"

gq.file.nm <- "group_quarters_geo.xlsx"
# gq.file.nm <- "group-quarters_original.xlsx"
gq.file <- read.xlsx(file.path(data.dir, gq.file.nm))

eqlu.file <- read.xlsx(file.path(data.dir, "2015-and-2016-5yr-ACS-Equity-Populations-20181009.xlsx"), sheet = "acs5yr_2016") %>% as.data.table

settings <- list(goa = list(out.file.nm = "79_dist_growth_opp_areas"),
                 htm = list(out.file.nm = "17_housing_type_mix"),
                 pjta = list(out.file.nm = "30_jobs_pop_tod_areas"),
                 jhr = list(out.file.nm = "18_jobs_housing_ratio"),
                 ard = list(out.file.nm = "80_areas_risk_displacement")) 

source("all_runs.R")


