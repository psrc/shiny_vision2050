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
                         growth_amenities = "gamn"#, sewer_buffer = "sewr"
                         )

out.file.nm <- list(transit_buffer = settings$gpro$out.file.nm.a, # "28a_transit_proximity"
                    uga_buffer = settings$gpro$out.file.nm.b, # "28b_uga_proximity"
		                park_buffer = settings$park$out.file.nm,  # "64_buffered_parks"
		                growth_amenities = settings$gamn$out.file.nm#, # "31_growth_amentities"
		                # sewer_buffer = settings$sewr$out.file.nm # "58_sewer_proximity"
		                )
all_attrs <- list(transit_buffer = c("population", "employment", "activity_units"),
	     	          uga_buffer = c("population", "employment", "activity_units"),
		              park_buffer = c("population"#, "employment", "activity_units"
		                              ),
		              growth_amenities = "population"#,
		              # sewer_buffer = c("population", "employment", "activity_units")
		              )
# which attributes to use for computing shares
share_attr <- list(transit_buffer = "activity_units",
                   uga_buffer = "activity_units", 
                   park_buffer = "population", 
                   growth_amenities = "population"#,
                   # sewer_buffer = "activity_units"
                   )
include.equity <- list(transit_buffer = settings$gpro$include.equity[1],
                       uga_buffer =  settings$gpro$include.equity[2],
                       park_buffer = settings$park$include.equity,
                       growth_amenities = settings$gamn$include.equity)

ind.types <- names(all_attrs)
# ind.types <- "park_buffer"

geo <- "county"
geo.id <- paste0(geo, "_id")

get.byr.actuals.park_buffer.county <- function() {
  #act <- data.table(read.xlsx(file.path(data.dir, "64_parks_access_2017.xlsx"), sheet = "Sheet1"))
  #colnames(act) <- c("name_id", "employment_2017_actual", "military", "population_2017_actual",
  #                   "activity_units_2017_actual", "employment_2017_actual_total", 
  #                   "population_2017_total_actual", "activity_units_2017_total_actual")
  #act[, employment_2017_actual := employment_2017_actual + military]
  #act[, military := NULL]
  act <- data.table(read.xlsx(file.path(data.dir, "62_parks_access_2017.xlsx")))
  colnames(act) <- c("name_id", "population_2017_actual", "population_2017_total_actual")
  act[, population_2017_actual := round(population_2017_actual)]
  act[, population_2017_total_actual := round(population_2017_total_actual)]
  act
}

row.order.base <- c("King", "Kitsap", "Pierce", "Snohomish")

for(itype in ind.types) {
  cat("\nComputing indicator 28, 31 & 64 for ", itype)
  settngs <- settings[[names.conversion[[itype]]]]
  attributes <- all_attrs[[itype]]
  buffer.df <- eqbuffer.df <- NULL
  include.actuals <- FALSE
  if(!is.null(settngs$include.actuals)) 
    include.actuals <- settngs$include.actuals
  incl.eqty <- FALSE
  if(!is.null(include.equity[[itype]])) incl.eqty <- include.equity[[itype]]
  totsuffix <- ""
  if(!is.null(settngs$total.suffix))
    totsuffix <- settngs$total.suffix
  row.order <- row.order.base
  if(incl.eqty)
      row.order <- c(row.order, "poverty", "non-poverty", "minority", "non-minority")
  for(buffer in c(itype, "total")) { # run twice, once for the specific buffer indicator, once for totals
    # get Opus indicators
    these.attr <- if(buffer == itype) paste(attributes,  itype, sep = "_") else paste0(attributes, totsuffix)
    alldata <- compile.tbl(geo, allruns, run.dir, these.attr, ind.extension)
    
    # convert to long format
    dfm <- data.table::melt(alldata,
                          id.vars = c("name_id", "run", "indicator"),
                          measure.vars = grep("yr", colnames(alldata), value = TRUE),
                          variable.name = "year", value.name = "estimate")
    
    # remove irrelevant years
    dfm <- dfm[year %in% fs.years.to.keep & name_id > 0]
  
    # remove the name of the buffer from the indicator, e.g. "population_park_buffer" is replaced with "population"
    dfm[, indicator := gsub(paste0("_(", paste(c(names(names.conversion), "inside_ugb"), collapse = "|"), ")"), "", indicator)]
    
    # add military and GQ
    filter <- NULL
    if(!is.null(settngs$milgq.filter)) filter <- settngs$milgq.filter[[buffer]]
    else {
      # default filter
      if(buffer  != "total")
        filter <- list( quo(`==`(!!sym(paste0(itype, "_id")), 1))) # filter records with buffer_id being one
    }
    milgq <- compile.mil.gq(geo.id, mil.filter = filter, gq.filter = filter)
   
    # join the two datasets
    dfm[milgq, estimate := estimate + i.estimate, on = c("name_id", "indicator", "year")]
    
    # remove "yr" from year values
    dfm[, year := gsub("yr", "", year)]
    
    
    # get equity data
    if(incl.eqty) {
      file.regexp <- paste0("(^minority|^poverty).*(", paste(these.attr, collapse = "|"), ")\\.csv")
      edf <- compile.tbl.equity(file.regexp, allruns, run.dir, ind.extension)
      edf[, equity := switch(name_id, `1` = paste0("non-", generic_equity), `2` = generic_equity), by = name_id
          ][, indicator := as.character(variable)] # create new field equity: 1 = non-minority/poverty, 2 = minority/poverty
      edf[, indicator := substr(indicator, 1, nchar(indicator)-5)]
      edt <- edf[year %in% fs.years.to.keep.int]
      #if(buffer == "total") {
      #  edt[, estimate := sum(estimate), by = .(run, year, indicator, generic_equity)]
      #} 
	  edt <- edt[, `:=`(variable = NULL, name_id = NULL, generic_equity = NULL)]
	  setnames(edt, "equity", "name_id")
      edt[, indicator := gsub(paste0("(", paste(c("_inside_ugb", "minority_", "poverty_", 
                                                  paste0("_", names(names.conversion))), collapse = "|"), ")"), "", indicator)]
      
      # transform military df grouping by equity categories and add to forecast
      eqfilter1 <- eqfilter2 <- NULL
      if(!is.null(settngs$eq.milgq.filter)) eqfilter1 <- eqfilter2 <- settngs$eq.milgq.filter[[buffer]]
      else {
        if(buffer  != "total") {
            eqfilter1 <- eqfilter2 <- filter
        #  eqfilter1 <- list( quo(`==`(!!sym("minority_id"), 1)))
        #  eqfilter2 <- list( quo(`==`(!!sym("poverty_id"), 1)))
        }
      }
      eqmilgq1 <- compile.mil.gq("minority_id", mil.filter = eqfilter1, gq.filter = eqfilter1)
      eqmilgq1[, equity := ifelse(name_id == 0, "non-minority", "minority")]
      eqmilgq1[, name_id := NULL]
      setnames(eqmilgq1, "equity", "name_id")
      eqmilgq2 <- compile.mil.gq("poverty_id", mil.filter = eqfilter2, gq.filter = eqfilter2)
      eqmilgq2[, equity := ifelse(name_id == 0, "non-poverty", "poverty")]
      eqmilgq2[, name_id := NULL]
      setnames(eqmilgq2, "equity", "name_id")
      #if(buffer  == "total") {
      #  eqmilgq1 <- eqmilgq1[, estimate := sum(estimate), by = .(year, indicator)]
      #  eqmilgq2 <- eqmilgq2[, estimate := sum(estimate), by = .(year, indicator)]
      #}
      eqmilgq <- rbind(eqmilgq1, eqmilgq2)[, year := gsub("yr", "", year)]
      edt[eqmilgq, estimate := estimate + i.estimate, on = c("name_id", "indicator", "year")]
      
      eqbuffer.df[[buffer]] <- edt
    }
    
    # collect result
    buffer.df[[buffer]] <- dfm
  }
  
  # merge buffer and totals  
  resdf <- merge(buffer.df[[itype]], buffer.df[["total"]], by = c("year", "indicator", "run", "name_id"))
  # merge with counties
  resdf <- merge(resdf, counties, by.x = "name_id", by.y = "countyID")
  resdf[, name_id := county][, county := NULL]
  if(incl.eqty) {
    resdf <- rbind(resdf,
                 merge(eqbuffer.df[[itype]], eqbuffer.df[["total"]], by = c("year", "indicator", "run", "name_id")))
  }
  setnames(resdf, "estimate.x", "estimate")
  setnames(resdf, "estimate.y", "total")
  # compute deltas
  resdf[resdf[year == fs.byr], `:=`(base = i.estimate, base_total = i.total), on = .(indicator, run, name_id)]
  resdf[, `:=`(delta = estimate - base, delta_total = total - base_total)]
  

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
  repyears <- rep(fs.years.to.keep.int, length(attributes))
  keep.cols <- paste(attributes, repyears, sep = "_")
  keep.cols <- c(keep.cols, paste0(keep.cols, "_total"), numer.col.delta, denomin.col.delta, "name_id")

  # loop through each run and gather results
  dlist <- NULL
  for (r in 1:length(run.dir)) {
    t <- resdf[run == run.dir[r], ]
    t[, run := NULL]
    # convert to wide format
    t <- dcast(t, name_id  ~ indicator + year, value.var = c("estimate", "total", "delta", "delta_total"
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
    
    # put rows into a particular order
    t$name_id <- factor(t$name_id, levels = row.order)
    t <- t[order(name_id)]
    
    # add regional total
    t <- rbind(t, cbind(name_id = "Region", t[!name_id %in% c("minority", "non-minority", "poverty", "non-poverty"), 
                                              lapply(.SD, sum), 
                                             .SDcols = which(sapply(t, is.numeric))]))
    # include 2017 actual if needed
    if(include.actuals) {
      t <- merge(t, do.call(paste("get.byr.actuals", itype, geo, sep = "."), list()), by = "name_id", sort = FALSE, 
                 all.x = TRUE)
      t[name_id %in% c("minority", "non-minority", "poverty", "non-poverty"), (paste0("population_",fs.byr,"_actual")) := get(paste0("population_", fs.byr))
        ][name_id %in% c("minority", "non-minority", "poverty", "non-poverty"), (paste0("population_",fs.byr,"_total_actual")) := get(paste0("population_", fs.byr, "_total"))]
    } else {
      # create control totals (similar to #30 jobs_pop_tod_areas.R)
      #ct_2017, use the 2017 census tract actual estimates of pop (SAEP) and jobs (tract file)
      #ct_2017-2050 use the modeled delta, i.e. the same value that gets reported in the 'delta_base' aka 'delta_total' field
      #ct_2050, add ct_2017 and ct_2017-2050
      
      ct <- compile.control.totals("2017_actuals_2050_controls.xlsx")
      ct <- ct[indicator != "AU" & scenario %in% names(run.dir[r]), ]
      setnames(ct, 'Countyname', 'name_id')
      ct$run <- run.dir[ct$scenario]
      ct[, `:=` (County = NULL, scenario = NULL)]
      
      cteq <- compile.control.totals.equity() # baseyear only
      setnames(cteq, "equity", "name_id")
      ctdel <- resdf[name_id %in%  c("minority", "non-minority", "poverty", "non-poverty") &
                       year == fs.years & 
                       run == run.dir[r] &
                       indicator != 'activity_units', .(name_id, indicator, run, delta_total)]
      
      ctdel[cteq, on = c('name_id', 'indicator'), ct_byr := ct_byr][, (paste0('ct_', fs.years)) := ct_byr + delta_total]
      setnames(ctdel, c('ct_byr', 'delta_total'), c(paste0('ct_', fs.byr), paste0('ct_', fs.byr, '-', fs.years)))
      ct.tot <- rbindlist(list(ct, ctdel), use.names = T , fill = T)
      ct.tot <- dcast.data.table(ct.tot, name_id + run ~ indicator, value.var = str_subset(colnames(ct.tot), "^ct_"))

      if (itype == "growth_amenities") {
        ct.sort.cols <- c('name_id', str_subset(colnames(ct.tot), "pop.*$"))
      } else {
        ct.sort.cols <- c('name_id', str_subset(colnames(ct.tot), "pop.*$"), str_subset(colnames(ct.tot), "emp.*$"))
      }
      ct.tot <- ct.tot[ , ..ct.sort.cols] 
      t <- ct.tot[t, on = 'name_id']
    }
      
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
