library(tidyverse)

# scan for all directories in servers
# scan all modelservers for runs and setnames
allruns <- list()
for (b in 1:length(base)) {
  fdirlist <- list.dirs(base[[b]], full.names = TRUE, recursive = FALSE)
  ndirlist <- list.dirs(base[[b]], full.names = FALSE, recursive = FALSE)
  dirlist <- setNames(fdirlist, ndirlist)
  ifelse(is.null(allruns), allruns <- dirlist, allruns <- append(allruns, dirlist))
}

