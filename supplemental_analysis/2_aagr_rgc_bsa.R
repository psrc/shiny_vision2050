library(data.table)
library(openxlsx)
library(tidyverse)
library(foreign)

# settings --------------------------------------------------------------

if(!exists("set.globals") || !set.globals) {
  curr.dir <- getwd()
  this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(this.dir)
  source("settings.R")
  source("functions.R")
}

cat("\nComputing metric 2, Average Annual Growth Rates by Alternative: RGCs and Buffered Station Areas\n")
out.file.nm <- settings$aagr_rgc_bsa$out.file.nm 


# general -----------------------------------------------------------------

years.col <- paste0("yr", fcast.yrs)

counties <- c("King", "Kitsap", "Pierce", "Snohomish")
names(counties) <- c("33", "35", "53", "61")

attributes <- c("employment", "population")
ind.extension <- ".csv" 


# functions ----------------------------------------------------------

compile.baseyear.actuals <- function() {
  pop <- query.ofm(ofm.file.nm, "POP", c("2000", "2017"))
}
