# Aws-model04 - iSTC - \\aws-model04\e$\opusgit\urbansim_data\data\psrc_parcel\runs\run_3.run_2018_08_17_13_06
# Aws-model03 - iDUG - \\aws-model03\e$\opusgit\urbansim_data\data\psrc_parcel\runs\run_1.run_2018_08_17_15_45
# Aws-model05 - iH2O2 - \\aws-model04\e$\opusgit\urbansim_data\data\psrc_parcel\runs\run_4.run_2018_08_17_16_15
# Aws-model05 - TOD - \\aws-model05\e$\opusgit\urbansim_data\data\psrc_parcel\runs\run_3.run_2018_08_10_21_04

base <- list(Aws01 = "//aws-model01/e$/opusgit/urbansim_data/data/psrc_parcel/runs",
             Aws02 = "//aws-model02/e$/opusgit/urbansim_data/data/psrc_parcel/runs",
             Aws03 = "//aws-model03/e$/opusgit/urbansim_data/data/psrc_parcel/runs",
             Aws04 = "//aws-model04/e$/opusgit/urbansim_data/data/psrc_parcel/runs",
             Aws05 = "//aws-model05/e$/opusgit/urbansim_data/data/psrc_parcel/runs",
             Aws06 = "//aws-model06/e$/opusgit/urbansim_data/data/psrc_parcel/runs",
             Aws07 = "//aws-model07/e$/opusgit/urbansim_data/data/psrc_parcel/runs",
             Aws08 = "//aws-model08/e$/opusgit/urbansim_data/data/psrc_parcel/runs",
             Aws09 = "//aws-model09/e$/opusgit/urbansim_data/data/psrc_parcel/runs",
             Modelsrv5 = "//modelsrv5/d$/opusgit/urbansim_data/data/psrc_parcel/runs",
             Modelsrv6 = "//modelsrv6/d$/opusgit/urbansim_data/data/psrc_parcel/runs",
             Modelsrv8 = "//MODELSRV8/d$/opusgit/urbansim_data/data/psrc_parcel/runs",
             Modelsrv3 = "//modelsrv3/e$/opusgit/urbansim_data/data/psrc_parcel/runs"
)

run.dir <- c("iSTC" = "run_3.run_2018_08_17_13_06",
             "iDUG" = "run_1.run_2018_08_17_15_45",
             "iH2O2" = "run_4.run_2018_08_17_16_15",
             "TOD" = "run_3.run_2018_08_10_21_04")

out.dir <- "X:/DSA/Vision2050/land_use_tables"
          
data.dir <- "J:/Projects/V2050/SEIS/Data_Support/script_input"
years <- c(2050)
byr <- 2017

enlist.lu.nm <- "enlisted_personnel_geo.xlsx"
enlist.lu <- read.xlsx(file.path(data.dir, enlist.lu.nm))

enlist.mil.file.nm <- "enlisted_personnel_SoundCast_08202018.csv"

gq.file.nm <- "group-quarters.xlsx"
gq.file <- read.xlsx(file.path(data.dir, gq.file.nm))

settings <- list(goa = list(out.file.nm = "79_dist_growth_opp_areas"),
                 htm = list(out.file.nm = "17_housing_type_mix"),
                 pjta = list(out.file.nm = "30_jobs_pop_tod_areas"),
                 jhr = list(out.file.nm = "18_jobs_housing_ratio")
                 ) 





