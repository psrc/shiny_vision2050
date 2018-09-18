# Indicator 28a & 28b: Population and employment growth proximity
# Indicators 31 & 64: Growth amenities and park buffer

library(data.table)

if(!exists("set.globals") || !set.globals) {
  script.dir <- "/Users/hana/R/vision2050indicators/full_set" 
  setwd(script.dir)
  source("settings.R")
  source("functions.R")
}

ind.extension <- ".csv"

juris <- fread(file.path(data.dir, "Juris_Reporting.csv"))
counties <- unique(juris[, .(countyID, county)])


out.file.nm <- list(transit_buffer = settings$gpro$out.file.nm.a, # "28a_transit_proximity"
                    uga_buffer = settings$gpro$out.file.nm.b, # "28b_uga_proximity"
		                park_buffer = settings$park$out.file.nm,  # "64_buffered_parks"
		                growth_amenities = settings$gamn$out.file.nm # "31_growth_amentities"
		                )
all_attrs <- list(transit_buffer = c("population", "employment", "activity_units"),
	     	          uga_buffer = c("population", "employment", "activity_units"),
		              park_buffer = c("population", "employment"),
		              growth_amenities = "population"
		              )
ind.types <- names(all_attrs)
#ind.types <- "uga_buffer"

geo <- "county"
geo.id <- paste0(geo, "_id")

for(itype in ind.types) {
  cat("\nComputing indicator 28, 31 & 64 for ", itype)
  attributes <- all_attrs[[itype]]
  alldata <- compile.tbl(geo, allruns, run.dir, paste(attributes, itype, sep = "_"), ind.extension)
  dfm <- data.table::melt(alldata,
                          id.vars = c("name_id", "run", "indicator"),
                          measure.vars = grep("yr", colnames(alldata), value = TRUE),
                          variable.name = "year", value.name = "estimate")
  dfm <- dfm[year %in% fs.years.to.keep & name_id > 0]
  
  # add military and GQ
  filter <- list( quo(`==`(!!sym(paste0(itype, "_id")), 1))) # filter records with buffer_id being one
  milgq <- compile.mil.gq(geo.id, mil.filter = filter, gq.filter = filter)
  dfmmgq <- dfm[milgq, estimate := estimate + i.estimate, on = c("name_id", "indicator", "year")]
  
  dfmmgq <- merge(dfmmgq, counties, by.x = "name_id", by.y = "countyID")
  
  # loop through each run and gather results
  dlist <- NULL
  for (r in 1:length(run.dir)) {
    t <- dfmmgq[run == run.dir[r], ]
    t[, run := NULL][, name_id := NULL]
    t[, year := gsub("yr", "", year)]
    t[, indicator := gsub(paste0("_", itype), "", indicator)]
    t <- dcast(t, county  ~ indicator + year, value.var = "estimate")
    # add regional total
    t <- rbind(t, cbind(county = "Region", t[, lapply(.SD, sum), .SDcols = which(sapply(t, is.numeric))]))
    dlist[[names(run.dir[r])]] <- t
  }
  # export
  write.xlsx(dlist, file.path(out.dir, paste0(out.file.nm[[itype]], "_", Sys.Date(), ".xlsx")))

}
cat("\n")
