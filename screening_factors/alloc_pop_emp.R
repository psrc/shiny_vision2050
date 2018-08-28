# This script fulfills #16 on the long-list.
# Apply the following:
# - allocations
# - enlisted personnel to actual and forecast emp
# - GQ to forecast pop

library(data.table)
library(openxlsx)
library(tidyverse)
library(foreign)


# user input --------------------------------------------------------------

this.dir <- "C:/Users/CLam/Desktop/shiny_vision2050/scripts"
# this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
source("all_runs.R")

indicator.dirnm <- "indicators"
run.dir <- c("iSTC" = "run_3.run_2018_08_17_13_06",
             "DUG" = "run_22.run_2018_08_10_21_05", 
             "H2O2" = "run_1.run_2018_08_10_21_05", 
             "TOD" = "run_3.run_2018_08_10_21_04") 
years <- c(2050) # only one year
byr <- 2017

data.dir <- "../data"
out.dir <- "../scripts_results"
dsa.dir <- "X:/DSA/Vision2050/land_use_tables"

out.file.nm <- "alloc_pop_emp"

# general -----------------------------------------------------------------



years.col <- paste0("yr", years)

attributes <- c("employment", "population")
ind.extension <- ".csv"



# base year actuals -------------------------------------------------------

# enlisted personnel ------------------------------------------------------

# functions ---------------------------------------------------------------

compile.tbl <- function(geog) {
  df <- NULL
  for (r in 1:length(run.dir)) { # for each run
    base.dir <- purrr::pluck(allruns, run.dir[r]) 
    for (a in 1:length(attributes)) { # for each attribute
      filename <- paste0(geog,'__',"table",'__',attributes[a], ind.extension)
      datatable <- read.csv(file.path(base.dir, indicator.dirnm, filename), header = TRUE, sep = ",")
      colnames(datatable)[2: ncol(datatable)] <- str_replace(colnames(datatable)[2: ncol(datatable)], '\\w+_', 'yr') # rename columns
      colnames(datatable)[1] <- str_replace(colnames(datatable)[1], '\\w+_', 'name_')
      datatable$indicator <- attributes[a]
      datatable$run <- run.dir[r]
      df <- rbindlist(list(df, datatable), use.names = TRUE, fill = TRUE)
    }
  }
  return(df)
}

# forecast data -----------------------------------------------------



# export ------------------------------------------------------------------

write.xlsx(dlist, file.path(dsa.dir, paste0(out.file.nm, "_", Sys.Date(), ".xlsx")))
