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

names.conversion <- list(transit_buffer = "gpro", uga_buffer = "gpro", park_buffer = "park", 
                         growth_amenities = "gamn", sewer_buffer = "sewr")

out.file.nm <- list(transit_buffer = settings$gpro$out.file.nm.a, # "28a_transit_proximity"
                    uga_buffer = settings$gpro$out.file.nm.b, # "28b_uga_proximity"
		                park_buffer = settings$park$out.file.nm,  # "64_buffered_parks"
		                growth_amenities = settings$gamn$out.file.nm, # "31_growth_amentities"
		                sewer_buffer = settings$sewr$out.file.nm # "58_sewer_proximity"
		                )
all_attrs <- list(transit_buffer = c("population", "employment", "activity_units"),
	     	          uga_buffer = c("population", "employment", "activity_units"),
		              park_buffer = c("population", "employment", "activity_units"),
		              growth_amenities = "population",
		              sewer_buffer = c("population", "employment", "activity_units")
		              )
# which attributes to use for computing shares
share_attr <- list(transit_buffer = "activity_units",
                   uga_buffer = "activity_units", 
                   park_buffer = "activity_units", 
                   growth_amenities = "population",
                   sewer_buffer = "activity_units")

ind.types <- names(all_attrs)
ind.types <- "park_buffer"

geo <- "county"
geo.id <- paste0(geo, "_id")

get.byr.actuals.park_buffer.county <- function() {
  act <- data.table(read.xlsx(file.path(data.dir, "64_parks_access_2017.xlsx"), sheet = "Sheet1"))
  colnames(act) <- c("county", "employment_2017_actual", "military", "population_2017_actual",
                     "activity_units_2017_actual", "employment_2017_actual_total", 
                     "population_2017_total_actual", "activity_units_2017_total_actual")
  act[, employment_2017_actual := employment_2017_actual + military]
  act[, military := NULL]
  act
}


for(itype in ind.types) {
  cat("\nComputing indicator 28, 31 & 64 for ", itype)
  attributes <- all_attrs[[itype]]
  buffer.df <- NULL
  include.actuals <- FALSE
  if(!is.null(settings[[names.conversion[[itype]]]]$include.actuals)) 
    include.actuals <- settings[[names.conversion[[itype]]]]$include.actuals
  for(buffer in c(itype, "total")) { # run twice, once for the specific buffer indicator, once for totals
    # get Opus indicators
    these.attr <- if(buffer == itype) paste(attributes,  itype, sep = "_") else attributes
    alldata <- compile.tbl(geo, allruns, run.dir, these.attr, ind.extension)
    
    # convert to long format
    dfm <- data.table::melt(alldata,
                          id.vars = c("name_id", "run", "indicator"),
                          measure.vars = grep("yr", colnames(alldata), value = TRUE),
                          variable.name = "year", value.name = "estimate")
    
    # remove irrelevant years
    dfm <- dfm[year %in% fs.years.to.keep & name_id > 0]
  
    # add military and GQ
    filter <- list( quo(`==`(!!sym(paste0(itype, "_id")), 1))) # filter records with buffer_id being one
    milgq <- compile.mil.gq(geo.id, mil.filter = filter, gq.filter = filter)
    dfmmgq <- dfm[milgq, estimate := estimate + i.estimate, on = c("name_id", "indicator", "year")]
    
    # remove "yr" from year values
    dfmmgq[, year := gsub("yr", "", year)]
    
    # remove the name of the buffer from the indicator, e.g. "population_park_buffer" is replaced with "population"
    dfmmgq[, indicator := gsub(paste0("_", itype), "", indicator)]
    
    # collect result
    buffer.df[[buffer]] <- dfmmgq
  }
  
  # merge buffer and totals  
  resdf <- merge(buffer.df[[itype]], buffer.df[["total"]], by = c("year", "indicator", "run", "name_id"))
  setnames(resdf, "estimate.x", "estimate")
  setnames(resdf, "estimate.y", "total")
  # compute deltas
  resdf[resdf[year == 2017], `:=`(base = i.estimate, base_total = i.total), on = .(indicator, run, name_id)]
  resdf[, `:=`(delta = estimate - base, delta_total = total - base_total)]
  # merge with counties
  resdf <- merge(resdf, counties, by.x = "name_id", by.y = "countyID")

  # names of columns to compute shares with
  share.col.byr <- paste("share", share_attr[[itype]], fs.byr, sep = "_")
  share.col.delta <- paste("delta_share", share_attr[[itype]], fs.years, sep = "_")
  numer.col.byr <- paste(share_attr[[itype]], fs.byr, sep = "_")
  denomin.col.byr <- paste0(numer.col.byr, "_total")
  if(include.actuals) { # if desired, compute the shares from actuals
    numer.col.byr <- paste0(numer.col.byr, "_actual")
    denomin.col.byr <- paste0(denomin.col.byr,"_actual")
  }
  numer.col.delta <- paste("delta", share_attr[[itype]], fs.years, sep = "_")
  denomin.col.delta <- paste("delta", share_attr[[itype]], fs.years, "total", sep = "_")
  
  # which columns to keep
  repyears <- rep(c(fs.byr, fs.years), length(attributes))
  keep.cols <- paste(attributes, repyears, sep = "_")
  keep.cols <- c(keep.cols, paste0(keep.cols, "_total"), numer.col.delta, denomin.col.delta, "county")

  # loop through each run and gather results
  dlist <- NULL
  for (r in 1:length(run.dir)) {
    t <- resdf[run == run.dir[r], ]
    t[, run := NULL][, name_id := NULL]
    # convert to wide format
    t <- dcast(t, county  ~ indicator + year, value.var = c("estimate", "total", "delta", "delta_total"
                                                            #,"share", "delta_share"
                                                            ))
    # remove the word "estimate_" from column names
    colnames(t) <- gsub("estimate_", "", colnames(t)) #
    # put the word "total" in column names at the end
    replace <- paste("total", attributes, repyears , sep="_")
    replace.with <- paste(attributes,  repyears, "total", sep="_")
    for(iattr in 1:length(replace)) 
      colnames(t) <- gsub(replace[iattr], replace.with[iattr], colnames(t))
    # keep only relevant columns 
    t <- t[, colnames(t) %in% keep.cols, with = FALSE]

    # add regional total
    t <- rbind(t, cbind(county = "Region", t[, lapply(.SD, sum), 
                                             .SDcols = which(sapply(t, is.numeric))]))
    # include 2017 actual if needed
    if(include.actuals) 
      t <- merge(t, do.call(paste("get.byr.actuals", itype, geo, sep = "."), list()), by = geo, sort = FALSE)

    # compute shares
    expr1 <- parse(text = paste0(share.col.byr, " := ", numer.col.byr, "/", denomin.col.byr))
    expr2 <- parse(text = paste0(share.col.delta, " := ", numer.col.delta, "/", denomin.col.delta))
    t[, eval(expr1)]
    t[, eval(expr2)]
    
    dlist[[names(run.dir[r])]] <- t
  }
  # export
  write.xlsx(dlist, file.path(out.dir, paste0(out.file.nm[[itype]], "_", Sys.Date(), ".xlsx")))

}
cat("\n")
