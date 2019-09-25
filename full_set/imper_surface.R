# Indicator 55: Affective Impervious Surface

library(data.table)

if(!exists("set.globals") || !set.globals) {
  script.dir <- "/Users/hana/R/vision2050indicators/full_set"
  setwd(script.dir)
  source("settings.R")
  source("functions.R")
}

cat("\nComputing indicator 55, Impervious acreage by land category\n")

out.file.nm <- settings$impersurf$out.file.nm

str.dvpt <- "Impervious_acreage_by_dvpt_category_by_year__.*\\.tab"
str.cnty <- "county.*impervious_area.csv"

compile.imper.surf.tbls <- function(allruns, run.dir, pattern) {
  df <- NULL
  for (r in 1:length(run.dir)) { # for each run
    base.dir <- purrr::pluck(allruns, run.dir[r])
    files <- list.files(file.path(base.dir, "indicators"), pattern = pattern)
    for (f in 1:length(files)) {
      datatable <- NULL
      filename <- files[f]
      datatable <- fread(file.path(base.dir, "indicators", filename), header = TRUE)
      colnames(datatable)[1] <- "id"
      if (pattern == str.dvpt) {
        datatable[, `:=` (geography = 'Region',
                          year = str_extract(files[f], "(\\d+)(?=\\.)"),
                          run = run.dir[r])]
        df <- rbindlist(list(df, datatable), use.names = TRUE, fill = TRUE)
      } else if (pattern == str.cnty) {
        datatable[, geography := switch(as.character(id), '33' = 'King', '35' = 'Kitsap', '53' = 'Pierce', '61' = 'Snohomish'), by = id
                  ][, run := run.dir[r]]
        df <- rbindlist(list(df, datatable), use.names = TRUE, fill = TRUE)
      }

    }
  }
  return(df)
}

create.imper.surf.by.dvpt.tbl <- function() {
  dt <- compile.imper.surf.tbls(allruns, run.dir, str.dvpt)
  dtm <- melt.data.table(dt, 
                         id.vars = c("geography", "year", "run"), 
                         measure.vars = str_subset(colnames(dt), "older|newer"), 
                         variable.name = "attribute", value.name = "acres")
  dtc <- dcast.data.table(dtm, geography + run + attribute ~ paste0("acres_yr", year), value.var = "acres")
  dtc[, attribute := as.character(attribute)][, attribute := switch(attribute, "Existing_older" = "Built before 1996", 
                                                                    "Built_newer" = "Built 1996 and after", 
                                                                    "Redeveloped_older" = "Built before 1996, redeveloped"), by = attribute]
}

create.imper.surf.by.cnty.tbl <- function() {
  dt <- compile.imper.surf.tbls(allruns, run.dir, str.cnty)
  dt[, `:=` (attribute = "Total Impervious Area", id = NULL)]
  setnames(dt, str_subset(colnames(dt), "impervious_area"), paste0("acres_yr", str_extract(str_subset(colnames(dt), "impervious_area"), "\\d+$")))
  return(dt)
}

dt.all <- rbindlist(list(create.imper.surf.by.dvpt.tbl(), create.imper.surf.by.cnty.tbl()), use.names = TRUE, fill = TRUE)

# loop through each run
dlist <- NULL
for (r in 1:length(run.dir)) {
  t <- NULL
  t <- dt.all[run == run.dir[r], ][, scenario := names(run.dir[r])]
  setcolorder(t, c('geography', 'attribute', 'run', 'scenario', str_subset(colnames(t), 'acres')))
  dlist[[names(run.dir[r])]] <- t
}


# export ------------------------------------------------------------------

write.xlsx(dlist, file.path(out.dir, paste0(out.file.nm, "_", Sys.Date(), ".xlsx")))

setwd(curr.dir)
