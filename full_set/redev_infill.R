# Indicator 32: Growth through redevelopment and infill (Acres)

library(data.table)

if(!exists("set.globals") || !set.globals) {
  script.dir <- "/Users/hana/R/vision2050indicators/full_set"
  setwd(script.dir)
  source("settings.R")
  source("functions.R")
}

cat("\nComputing indicator 32, growth through redevelopment and infill\n")

out.file.nm <- settings$redevinf$out.file.nm

str.redev.infill <- "Acreage_by_built_res_density__.*\\.tab"
#str.redev.infill <- "Acreage by development category__.*\\.tab"

strtypes.sort <- c('Low'= 'Res or Mixed Use Low', 'Medium' = 'Res or Mixed Use Moderate','High' = 'Res or Mixed Use High', 'Nonres' = 'Non-Residential')
geog.sort <- c('King', 'Kitsap', 'Pierce', 'Snohomish', 'Poverty', 'Non-Poverty', 'Minority', 'Non-Minority', 'Region')

compile.dev.land.tbl <- function(allruns, run.dir, pattern) {
  df <- NULL
  for (r in 1:length(run.dir)) { # for each run
    base.dir <- purrr::pluck(allruns, run.dir[r])
    files <- list.files(file.path(base.dir, "indicators"), pattern = pattern)
    for (f in 1:length(files)) {
      datatable <- dtm <- NULL
      filename <- files[f]
      datatable <- fread(file.path(base.dir, "indicators", filename), header = TRUE)
      colnames(datatable)[1] <- "id"
      melt.cols <- colnames(datatable)[!(colnames(datatable) %in% "id")]
      dtm <- melt.data.table(datatable, id.vars = "id", measure.vars = melt.cols, variable.name = "attribute", value.name = "estimate")
      dtm[, `:=` (table = str_extract(files[f], "^[[:alpha:]]+(?=__)"),
                  year = str_extract(files[f], "(\\d+)(?=\\.)"),
                  strtype = str_extract(attribute, "^[[:alpha:]]+(?=_)"),
                  devtype = str_extract(attribute, "(?<=)[[:alpha:]]+$"),
                  run = run.dir[r])]
      df <- rbindlist(list(df, dtm), use.names = TRUE, fill = TRUE)
    }
  }
  return(df)
}

create.redev.infill.tbl <- function(table) {
  dtc <- dcast.data.table(table, run + id + table + strtype + devtype ~ paste0("yr", year), value.var = "estimate", subset = .(year %in% fs.years.to.keep.int))
  dtc.exist <- read.xlsx(file.path(data.dir, "32_redev_infill_baseyear_values.xlsx"))
  dtc.else <- dtc[devtype %in% c('redev', 'newdev'),][, delta := get(fs.years.to.keep[2]) - get(fs.years.to.keep[1])] 
  dtc.else.cast <- dcast.data.table(dtc.else, run + id + table + strtype ~ devtype, value.var = c(fs.years.to.keep[1], fs.years.to.keep[2], "delta"))
  dtj <- merge(dtc.exist, dtc.else.cast, by = c("id", "table", "strtype"))
  setDT(dtj)
  dtj[id %in% c(33, 35, 53, 61) & table == 'county', geography := switch(as.character(id), '33' = 'King', '35' = 'Kitsap', '53' = 'Pierce', '61' = 'Snohomish'), by = id]
  dtj[id %in% c(1) & table == 'alldata', geography := 'Region']
  dtj[id %in% c(1) & table %in% c('minority', 'poverty'), geography := str_to_title(paste0('non-', table))]
  dtj[id %in% c(2) & table %in% c('minority', 'poverty'), geography := str_to_title(table)]
  dtj[, `:=` (id = NULL, table = NULL)]
  dtj[, strtype := switch(strtype, 'Low'= 'Res or Mixed Use Low', 'Medium' = 'Res or Mixed Use Moderate','High' = 'Res or Mixed Use High', 'Nonres' = 'Non-Residential'), by = strtype]
  
  old.cols <- str_subset(colnames(dtj), "_")
  new.cols <- paste0(str_extract(old.cols, "(?<=_)[[:alpha:]]+"), "_", str_extract(old.cols, "^\\w+(?=_)"))
  setnames(dtj, old.cols, new.cols)

  dt.all <- dtj[, strtype.sort := factor(strtype, levels = unname(strtypes.sort))
                ][, geography.sort := factor(geography, levels = geog.sort)
                  ][order(geography.sort, strtype.sort)
                    ][, `:=` (strtype.sort = NULL, geography.sort = NULL)]
}

dt <- compile.dev.land.tbl(allruns, run.dir, str.redev.infill)
dt.all <- create.redev.infill.tbl(dt)

# loop through each run
dlist <- NULL
for (r in 1:length(run.dir)) {
  t <- NULL
  t <- dt.all[run == run.dir[r], ][, scenario := names(run.dir[r])]
  keep.cols <- c('geography', 'strtype', 'run', 'scenario', str_subset(colnames(t), "baseyear"), str_subset(colnames(t), "delta"))
  t <- t[, ..keep.cols]
  setnames(t, str_subset(colnames(t), "baseyear"), paste0(str_subset(colnames(t), "baseyear"), "_", fs.years.to.keep[1]))
  setnames(t,str_subset(colnames(t), "delta"), str_replace(str_subset(colnames(t), "delta"), "delta", paste0(fs.years.to.keep[1], "-", fs.years.to.keep[2])))
  dlist[[names(run.dir[r])]] <- t
}


# export ------------------------------------------------------------------

write.xlsx(dlist, file.path(out.dir, paste0(out.file.nm, "_", Sys.Date(), ".xlsx")))

setwd(curr.dir)


