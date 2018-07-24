library(data.table)

# hit 'source'
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
source("all_runs.R")


# user input --------------------------------------------------------------

run.dir <- c("run_2.run_2018_07_17_15_22") # can have multiple runs
out.dir <- "../scripts_results"
years <- c(2014, 2050)


# general -----------------------------------------------------------------

years.col <- paste0("yr", years)

# format run names for table
runs <- lapply(run.dir, function(x) unlist(strsplit(x,"[.]"))[[1]]) %>% unlist

counties <- c("King", "Kitsap", "Pierce", "Snohomish")
names(counties) <- c("33", "35", "53", "61")

attributes <- c("employment", "households")
extension <- ".csv"

df <- NULL


# transform city data -----------------------------------------------------



# transform county data ---------------------------------------------------

for (r in 1:length(run.dir)) { # for each run
  base.dir <- purrr::pluck(allruns, run.dir[r])
  for (a in 1:length(attributes)) { # for each attribute
    filename <- paste0('county','__',"table",'__',attributes[a], extension)
    datatable <- read.csv(file.path(base.dir, "indicators", filename), header = TRUE, sep = ",")
    colnames(datatable)[2: ncol(datatable)] <- str_replace(colnames(datatable)[2: ncol(datatable)], '\\w+_', 'yr') # rename columns
    colnames(datatable)[1] <- str_replace(colnames(datatable)[1], '\\w+_', 'name_')
    datatable$indicator <- attributes[a]
    datatable$run <- runs[r]
    df <- rbindlist(list(df, datatable), use.names = TRUE, fill = TRUE)
  }
}

df$name_id <- as.character(df$name_id)
df$cntyname <- counties[df$name_id]

df2 <- melt.data.table(df, 
                       id.vars = c("name_id", "cntyname", "run", "indicator"), 
                       measure.vars = grep("yr", colnames(df), value = TRUE), 
                       variable.name = "year", value.name = "estimate")
df3 <- df2[year %in% years.col] 
df.cast <- dcast.data.table(df3, name_id + cntyname + run + year ~ indicator, value.var = "estimate")
d <- df.cast[, ehratio := employment/households][order(run)]

# format d
d2 <- dcast.data.table(d, cntyname + run ~ year, value.var = c("employment", "households", "ehratio"))
# d2[order(run)]
setnames(d2, c(paste0("ehratio_", years.col)), c(paste0("baseyr", years[1]), paste0("naa", years[2])))


# export ------------------------------------------------------------------

# write.xlsx(d2, file.path(out.dir, "jobs_housing_ratio.xlsx"))
write.csv(d2, file.path(out.dir, "jobs_housing_ratio.csv"), row.names = FALSE)


