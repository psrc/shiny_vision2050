# Indicator 22 & 29: population and employment density per acre (for maps)

library(data.table)

if(!exists("set.globals") || !set.globals) {
  script.dir <- "/Users/hana/R/vision2050indicators/full_set" 
  setwd(script.dir)
  source("settings.R")
  source("functions.R")
}

out.file.nm.byr <- settings$epden$out.file.nm.byr # "22_pop_emp_au_density"
out.file.nm <- settings$epden$out.file.nm # "29_pop_emp_au_density"

geos <- "grid"

attributes <- c("population", "employment", "activity_units")
ind.extension <- ".csv"

cat("\nComputing indicator 22 & 29 for ", geo, " geography")

geo.id <- paste0(geo, "_id")
alldata <- compile.tbl(geo, allruns, run.dir, attributes, ind.extension)
dfm <- data.table::melt(alldata,
                        id.vars = c("name_id", "run", "indicator"),
                        measure.vars = grep("yr", colnames(alldata), value = TRUE),
                        variable.name = "year", value.name = "estimate")
dfm <- dfm[year %in% years.to.keep]
# add military and GQ
milgq <- compile.mil.gq(geo.id)
dfmmgq <- dfm[milgq, estimate := estimate + i.estimate, on = c("name_id", "indicator", "year")]
dfmmgq[, estimate := estimate / 5.56] # gridcell has 5.56 acres

# loop through each run
dlist <- NULL
for (r in 1:length(run.dir)) {
  t <- dfmmgq[run == run.dir[r], ]
  setcolorder(t, c("indicator", "run", "estimate"))
  dlist[[names(run.dir[r])]] <- t
}

# export
for(scenario in names(dlist)) {
  df <- copy(dlist[[scenario]])
  df[, run := NULL]
  df <- dcast(df, name_id + year ~ indicator, value.var = "estimate" )
  setnames(df, "name_id", geo.id)
  write.csv(df[year == byr.col][,year := NULL], file.path(out.dir.maps, paste0(out.file.nm.byr, "_grid_", scenario, "_", Sys.Date(), ".csv")),
              row.names = FALSE)
  write.csv(df[year == years.col][,year := NULL], file.path(out.dir.maps, paste0(out.file.nm, "_grid_", scenario, "_", Sys.Date(), ".csv")),
              row.names = FALSE)
}

cat("\n")
