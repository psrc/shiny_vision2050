library(data.table)
library(openxlsx)

# hit 'source'
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
source("all_runs.R")


# user input --------------------------------------------------------------

run.dir <- c("run_2.run_2018_07_17_15_22") # can have multiple runs
out.dir <- "../scripts_results"
dsa.dir <- "X:/DSA/Vision2050/land_use_tables"

out.file.nm <- "jobs_housing_ratio" 
years <- c(2014, 2050)
indicator.dirnm <- "indicators_full"

# general -----------------------------------------------------------------

subarea.cnty.lu <- read.xlsx(file.path("../data", "subarea.xlsx"))
years.col <- paste0("yr", years)

# format run names for table
runs <- lapply(run.dir, function(x) unlist(strsplit(x,"[.]"))[[1]]) %>% unlist

# counties <- c("King", "Kitsap", "Pierce", "Snohomish")
# names(counties) <- c("33", "35", "53", "61")

attributes <- c("employment", "households")
ind.extension <- ".csv"

df.cnty <- NULL
df.subarea <- NULL


# functions ---------------------------------------------------------------

compile.tbl <- function(geography) {
  df <- NULL
  for (r in 1:length(run.dir)) { # for each run
    base.dir <- purrr::pluck(allruns, run.dir[r])
    for (a in 1:length(attributes)) { # for each attribute
      filename <- paste0(geography,'__',"table",'__',attributes[a], ind.extension)
      datatable <- read.csv(file.path(base.dir, indicator.dirnm, filename), header = TRUE, sep = ",")
      colnames(datatable)[2: ncol(datatable)] <- str_replace(colnames(datatable)[2: ncol(datatable)], '\\w+_', 'yr') # rename columns
      colnames(datatable)[1] <- str_replace(colnames(datatable)[1], '\\w+_', 'name_')
      datatable$indicator <- attributes[a]
      datatable$run <- runs[r]
      df <- rbindlist(list(df, datatable), use.names = TRUE, fill = TRUE)
    }
  }
  return(df)
}

export.file <- function(data, extension, directory) {
  if (extension == "csv") {
    write.csv(data, file.path(directory, paste0(out.file.nm, ".", extension)), row.names = FALSE)
  } else if (extension == "xlsx") {
    write.xlsx(data, file.path(directory, paste0(out.file.nm, ".", extension)), row.names = FALSE)
  }
}

# transform subarea data -----------------------------------------------------

df.subarea <- compile.tbl('subarea')

df2 <- melt.data.table(df.subarea,
                       id.vars = c("name_id", "run", "indicator"),
                       measure.vars = grep("yr", colnames(df.subarea), value = TRUE),
                       variable.name = "year", value.name = "estimate")
df3 <- df2[year %in% years.col][subarea.cnty.lu, on = c("name_id" = "subarea_id")]

# sum KC
kc <- df3[cnty_id == 33, lapply(.SD, sum), .SDcols = "estimate", by =.(run, indicator, year, cnty_id, cnty_name)
          ][, `:=` (name_id = 7, subarea_name = "All King County")]

df4 <- rbindlist(list(df3, kc), use.names = TRUE, fill = TRUE)
df.cast <- dcast.data.table(df4, cnty_name + subarea_name + run + year ~ indicator, value.var = "estimate")
d <- df.cast[, ehratio := employment/households]

# format d
d2 <- dcast.data.table(d, cnty_name + subarea_name + run ~ year, value.var = c("employment", "households", "ehratio"))
setnames(d2, c(paste0("ehratio_", years.col)), c(paste0("baseyr", years[1]), paste0("naa", years[2])))


# qc ----------------------------------------------------------------------

# df.subarea.agg <- df.subarea[, lapply(.SD, sum), .SDcols = years.col, by = .(name_id, indicator)]
# df.subarea.agg[subarea.cnty.lu, on = c("name_id" = "subarea_id")][, lapply(.SD, sum), .SDcols = years.col, by = .(cnty_name, indicator)][order(indicator, cnty_name)]
# reg.sum.subarea <- df.subarea[, lapply(.SD, sum), .SDcols = years.col, by = .(indicator)]

# transform county data

# df.cnty <- compile.tbl('county')
# 
# df.cnty[, lapply(.SD, sum), .SDcols = years.col, by = .(name_id, indicator)]
# reg.sum.cnty <- df.cnty[, lapply(.SD, sum), .SDcols = years.col, by = .(indicator)]


# export ------------------------------------------------------------------

export.file(d2, "csv", dsa.dir)



