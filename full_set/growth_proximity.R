# Indicator 28a & 28b: Population and employment growth proximity

library(data.table)

if(!exists("set.globals") || !set.globals) {
  script.dir <- "/Users/hana/R/vision2050indicators/full_set" 
  setwd(script.dir)
  source("settings.R")
  source("functions.R")
}

ind.types <- c("transit_buffer", "uga_buffer")
attributes <- c("population", "employment", "activity_units")
ind.extension <- ".csv"

years.col <- paste0("yr", years)
years.to.keep <- paste0("yr", c(byr, years))

out.file.nm <- list(transit_buffer = settings$gpro$out.file.nm.a, # "28a_transit_proximity"
                    uga_buffer = settings$gpro$out.file.nm.b) # "28b_uga_proximity"

geo <- "county"
geo.id <- paste0(geo, "_id")

for(itype in ind.types) {
  cat("\nComputing indicator 28 for ", itype)
  alldata <- compile.tbl(geo, allruns, run.dir, paste(attributes, itype, sep = "_"), ind.extension)
  dfm <- data.table::melt(alldata,
                          id.vars = c("name_id", "run", "indicator"),
                          measure.vars = grep("yr", colnames(alldata), value = TRUE),
                          variable.name = "year", value.name = "estimate")
  dfm <- dfm[year %in% years.to.keep & name_id > 0]
  
  # add military and GQ
  # uncomment the next two lines after Peter adds these ids into the military and GQ files
  filter <- list( quo(`==`(!!sym(paste0(itype, "_id")), 1))) # filter records with buffer_id being one
  milgq <- compile.mil.gq(geo.id, mil.filter = filter, gq.filter = filter)
  dfmmgq <- dfm[milgq, estimate := estimate + i.estimate, on = c("name_id", "indicator", "year")]
  #dfmmgq <- dfm # delete this line after military is ready
  
  # loop through each run and gather results
  dlist <- NULL
  for (r in 1:length(run.dir)) {
    t <- dfmmgq[run == run.dir[r], ]
    t[, run := NULL][, name_id := NULL]
    t[, year := gsub("yr", "", year)]
    t <- dcast(t, year ~ indicator, value.var = "estimate")
    dlist[[names(run.dir[r])]] <- t
  }
  # export
  write.xlsx(dlist, file.path(out.dir, paste0(out.file.nm[[geo]], Sys.Date(), ".xlsx")))

}