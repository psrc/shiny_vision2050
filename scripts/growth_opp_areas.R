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

out.file.nm <- "dist_growth_opp_areas" 
years <- c(2014, 2050)
indicator.dirnm <- "indicators_full"


# general -----------------------------------------------------------------

og.comp.ind <- "Y:/VISION 2050/Data/Opportunity Mapping Update/Finalopportunity_analysis - Opportunity_Index_06062012_Region_.csv"

tract.lu <- read.csv(file.path("../data", "tract_opportunity_lookup.csv"), stringsAsFactors = FALSE)

years.col <- paste0("yr", years)

# format run names for table
runs <- lapply(run.dir, function(x) unlist(strsplit(x,"[.]"))[[1]]) %>% unlist

attributes <- c("population", "employment", "households")
ind.extension <- ".csv"

df <- NULL


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


# transform data ----------------------------------------------------------

opp.levels <- c("Very Low Opportunity", "Low Opportunity", "Moderate Opportunity", "High Opportunity", "Very High Opportunity")
alldata <- compile.tbl("census_tract")
df2 <- melt.data.table(alldata,
                       id.vars = c("name_id", "run", "indicator"),
                       measure.vars = grep("yr", colnames(alldata), value = TRUE),
                       variable.name = "year", value.name = "estimate")
df3 <- df2[year %in% years.col][tract.lu, on = c("name_id" = "census_tract_id")]
df4 <- dcast.data.table(df3, name_id + run + indicator + Comp.Index ~ year, value.var = "estimate")

df5 <- df4[Comp.Index != "NA", lapply(.SD, sum), .SDcols = years.col, by = list(indicator, Comp.Index, run)
           ][, delta := get(eval(years.col[2])) - get(eval(years.col[1]))]

sums <- df5[, .(sum_delta = sum(delta)), by = indicator]

df6 <- df5[sums, on = "indicator"
           ][, share_delta := delta/sum_delta
                 ][, Comp.Index.sort := factor(Comp.Index, levels = opp.levels)
                   ][order(indicator, Comp.Index.sort)
                     ][, Comp.Index.sort := NULL]

export.file(df6, "csv", dsa.dir)
