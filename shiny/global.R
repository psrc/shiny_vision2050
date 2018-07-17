library(shiny)
# library(DT)
library(plotly)
library(tidyverse)


# Modelsrv Directories ----------------------------------------------------


# Shiny Server
# base <- list(Modelsrv5 = "/media/modelsrv5d/opusgit/urbansim_data/data/psrc_parcel",
#              Modelsrv6 = "/media/modelsrv6d/opusgit/urbansim_data/data/psrc_parcel",
#              Modelsrv8 = "/media/modelsrv8d/opusgit/urbansim_data/data/psrc_parcel",
#              Modelsrv3 = "/media/modelsrv3e/opusgit/urbansim_data/data/psrc_parcel"
# )

# local
#base <- list(Modelsrv5 = "//modelsrv5/d$/opusgit/urbansim_data/data/psrc_parcel",
#             Modelsrv6 = "//modelsrv6/d$/opusgit/urbansim_data/data/psrc_parcel",
#             Modelsrv8 = "//MODELSRV8/d$/opusgit/urbansim_data/data/psrc_parcel",
#             Modelsrv3 = "//modelsrv3/e$/opusgit/urbansim_data/data/psrc_parcel"
#             )
base <- list(Modelsrv5 = "/Users/hana/d5$/opusgit/urbansim_data/data/psrc_parcel",
             Modelsrv6 = "/Users/hana/d6$/opusgit/urbansim_data/data/psrc_parcel"
             #Modelsrv8 = "//MODELSRV8/d$/opusgit/urbansim_data/data/psrc_parcel",
             #Modelsrv3 = "//modelsrv3/e$/opusgit/urbansim_data/data/psrc_parcel"
)

# indicator/output directories
lu.run <- file.path(base$Modelsrv6, "runs", "run_134.run_2018_05_12_13_11", "indicators")
sc.run <- file.path(base$Modelsrv5, "soundcast_output", "T2040_2014") 