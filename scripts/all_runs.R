library(tidyverse)

# base <- list(Modelsrv5 = "/media/modelsrv5d/opusgit/urbansim_data/data/psrc_parcel/runs",
#              Modelsrv6 = "/media/modelsrv6d/opusgit/urbansim_data/data/psrc_parcel/runs",
#              Modelsrv8 = "/media/modelsrv8d/opusgit/urbansim_data/data/psrc_parcel/runs",
#              Modelsrv3 = "/media/modelsrv3e/opusgit/urbansim_data/data/psrc_parcel/runs"
# )

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

# scan for all directories in servers
# scan all modelservers for runs and setnames
allruns <- list()
for (b in 1:length(base)) {
  fdirlist <- list.dirs(base[[b]], full.names = TRUE, recursive = FALSE)
  ndirlist <- list.dirs(base[[b]], full.names = FALSE, recursive = FALSE)
  dirlist <- setNames(fdirlist, ndirlist)
  ifelse(is.null(allruns), allruns <- dirlist, allruns <- append(allruns, dirlist))
}

