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
		              park_buffer = c("population", "employment", "activity_units"),
		              growth_amenities = "population"
		              )

share_attr <- list(transit_buffer = "activity_units",
                   uga_buffer = "activity_units", 
                   park_buffer = "activity_units", 
                   growth_amenities = "population")

ind.types <- names(all_attrs)
#ind.types <- "uga_buffer"

geo <- "county"
geo.id <- paste0(geo, "_id")

for(itype in ind.types) {
  cat("\nComputing indicator 28, 31 & 64 for ", itype)
  attributes <- all_attrs[[itype]]
  buffer.df <- NULL
  for(buffer in c(itype, "total")) {
    these.attr <- if(buffer == itype) paste(attributes,  itype, sep = "_") else attributes
    alldata <- compile.tbl(geo, allruns, run.dir, these.attr, ind.extension)
    dfm <- data.table::melt(alldata,
                          id.vars = c("name_id", "run", "indicator"),
                          measure.vars = grep("yr", colnames(alldata), value = TRUE),
                          variable.name = "year", value.name = "estimate")
    dfm <- dfm[year %in% fs.years.to.keep & name_id > 0]
  
    # add military and GQ
    filter <- list( quo(`==`(!!sym(paste0(itype, "_id")), 1))) # filter records with buffer_id being one
    milgq <- compile.mil.gq(geo.id, mil.filter = filter, gq.filter = filter)
    dfmmgq <- dfm[milgq, estimate := estimate + i.estimate, on = c("name_id", "indicator", "year")]
    dfmmgq[, year := gsub("yr", "", year)]
    dfmmgq[, indicator := gsub(paste0("_", itype), "", indicator)]
    #dfmmgq[, is_total := buffer == "total"]
    #if(buffer == "total")
    #  dfmmgq[, indicator := paste(indicator, "total", sep = "_")]
    buffer.df[[buffer]] <- dfmmgq
  }
  # merge buffer and totals  
  resdf <- merge(buffer.df[[itype]], buffer.df[["total"]], by = c("year", "indicator", "run", "name_id"))
  setnames(resdf, "estimate.x", "estimate")
  setnames(resdf, "estimate.y", "total")
  # compute deltas
  resdf[resdf[year == 2017], `:=`(base = i.estimate, base_total = i.total), on = .(indicator, run, name_id)]
  resdf[, `:=`(delta = estimate - base, delta_total = total - base_total)]
  # compute shares
  resdf[, share := estimate / total * 100]
  resdf[, delta_share := delta / delta_total * 100]
  resdf <- merge(resdf, counties, by.x = "name_id", by.y = "countyID")

  # which columns to keep
  repyears <- rep(c(fs.byr, fs.years), length(attributes))
  keep.cols <- paste(attributes, repyears, sep = "_")
  keep.cols <- c(keep.cols, paste0(keep.cols, "_total"))
  keep.cols <- c(keep.cols, paste("share", share_attr[[itype]], fs.byr, sep = "_"), 
                 paste("delta", share_attr[[itype]], fs.years, sep = "_"),
                 paste("delta", share_attr[[itype]], fs.years, "total", sep = "_"),
                 paste("delta_share", share_attr[[itype]], fs.years, sep = "_"))
  keep.cols <- c("county", keep.cols)
  # loop through each run and gather results
  dlist <- NULL
  for (r in 1:length(run.dir)) {
    t <- resdf[run == run.dir[r], ]
    t[, run := NULL][, name_id := NULL]
    t <- dcast(t, county  ~ indicator + year, value.var = c("estimate", "total", "delta", "delta_total", "share", "delta_share"))
    colnames(t) <- gsub("estimate_", "", colnames(t)) # 
    replace <- paste("total", attributes, repyears , sep="_")
    replace.with <- paste(attributes,  repyears, "total", sep="_")
    for(iattr in 1:length(replace)) 
      colnames(t) <- gsub(replace[iattr], replace.with[iattr], colnames(t))
    t <- t[, colnames(t) %in% keep.cols, with = FALSE]
    # add regional total
    t <- rbind(t, cbind(county = "Region", t[, lapply(.SD, sum), 
                                             .SDcols = which(sapply(t, is.numeric))]))
    t[county == "Region", grep("share_", colnames(t))] <- NA
    dlist[[names(run.dir[r])]] <- t
  }
  # export
  write.xlsx(dlist, file.path(out.dir, paste0(out.file.nm[[itype]], "_", Sys.Date(), ".xlsx")))

}
cat("\n")
