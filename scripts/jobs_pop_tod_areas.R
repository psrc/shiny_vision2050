library(data.table)

# hit 'source'
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
source("all_runs.R")


# user input --------------------------------------------------------------

run.dir <- c("run_2.run_2018_07_17_15_22")
out.dir <- "../scripts_results"
years <- c(2014, 2050)


# general -----------------------------------------------------------------

years.col <- paste0("yr", years)

# format run names for table
runs <- lapply(run.dir, function(x) unlist(strsplit(x,"[.]"))[[1]]) %>% unlist

counties <- c("King", "Kitsap", "Pierce", "Snohomish")
names(counties) <- c("33", "35", "53", "61")

attributes <- c("employment", "population")
extension <- ".csv"

df <- NULL
df.cnty <- NULL

# transform county data ---------------------------------------------------

for (r in 1:length(run.dir)) { # for each run
  base.dir <- purrr::pluck(allruns, run.dir[r])
  for (a in 1:length(attributes)) { # for each attribute
    filename <- paste0('tod','__',"table",'__',attributes[a], extension)
    datatable <- read.csv(file.path(base.dir, "indicators", filename), header = TRUE, sep = ",")
    colnames(datatable)[2: ncol(datatable)] <- str_replace(colnames(datatable)[2: ncol(datatable)], '\\w+_', 'yr') # rename columns
    colnames(datatable)[1] <- str_replace(colnames(datatable)[1], '\\w+_', 'name_')
    datatable$indicator <- attributes[a]
    datatable$run <- runs[r]
    df <- rbindlist(list(df, datatable), use.names = TRUE, fill = TRUE)
    
    cnty.filename <- paste0('county','__',"table",'__',attributes[a], extension)
    cnty.datatable <- read.csv(file.path(base.dir, "indicators", cnty.filename), header = TRUE, sep = ",")
    colnames(cnty.datatable)[2: ncol(cnty.datatable)] <- str_replace(colnames(cnty.datatable)[2: ncol(cnty.datatable)], '\\w+_', 'yr') # rename columns
    colnames(cnty.datatable)[1] <- str_replace(colnames(cnty.datatable)[1], '\\w+_', 'name_')
    cnty.datatable$indicator <- attributes[a]
    cnty.datatable$run <- runs[r]
    df.cnty <- rbindlist(list(df.cnty, cnty.datatable), use.names = TRUE, fill = TRUE)
  }
}

df.sum <- df[name_id != 0, lapply(.SD, sum), .SDcols = years.col, by = c("indicator")]
region <- df.cnty[, lapply(.SD, sum), .SDcols = years.col, by = c("indicator")]

d <- df.sum[region, on = 'indicator']
setnames(d, colnames(d)[(ncol(d)-1):ncol(d)], paste0("tot" , years))
d2 <- d[, `:=` (share1 = get(paste0(years.col[1]))/get(paste0("tot", years[1])),
          share2 = get(paste0(years.col[2]))/get(paste0("tot", years[2])))
  ]
setnames(d2, c("share1", "share2"), c(paste0("baseyr", years[1]), paste0("naa", years[2])))


# # export ------------------------------------------------------------------

# # write.xlsx(d2, file.path(out.dir, "jobs_housing_ratio.xlsx"))
write.csv(d2, file.path(out.dir, "jobs_pop_tod_areas.csv"), row.names = FALSE)
write.csv(d2, file.path("X:/DSA/Vision2050/land_use_tables", "jobs_pop_tod_areas.csv"), row.names = FALSE)

