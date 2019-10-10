# gather forecast data (non-equity related)
compile.tbl <- function(geog, allruns, run.dir, attributes, ind.extension) {
  df <- NULL
  for (r in 1:length(run.dir)) { # for each run
    base.dir <- purrr::pluck(allruns, run.dir[r])
    for (a in 1:length(attributes)) { # for each attribute
      filename <- paste0(geog,'__',"table",'__',attributes[a], ind.extension)
      datatable <- fread(file.path(base.dir, "indicators", filename), header = TRUE, sep = ",")
      colnames(datatable)[2: ncol(datatable)] <- str_replace(colnames(datatable)[2: ncol(datatable)], '\\w+_', 'yr') # rename columns
      colnames(datatable)[1] <- str_replace(colnames(datatable)[1], '\\w+_', 'name_')
      datatable$indicator <- attributes[a]
      datatable$run <- run.dir[r]
      df <- rbindlist(list(df, datatable), use.names = TRUE, fill = TRUE)
    }
  }
  return(df)
}

# read and transform tract level file flagged with respective equity category
read.equity.lu <- function() {
  t <- eqlu.file[, .(GEOID10, minority_geog, poverty_geog)
                 ][, `:=` (GEOID10 = as.character(GEOID10), minority_geog = as.character(minority_geog), poverty_geog = as.character(poverty_geog))]
  t[, minority := switch(minority_geog, "0" = "non-minority", "1" = "minority"), by = minority_geog]
  t[, poverty := switch(poverty_geog, "0" = "non-poverty", "1" = "poverty"), by = poverty_geog]
  return(t)
}

# gather equity forecast data
# requires a regular expression string to retrieve specific equity filenames
compile.tbl.equity <- function(file.regexp, allruns, run.dir, ind.extension) {
  df <- NULL
  for (r in 1:length(run.dir)) { # for each run
    base.dir <- purrr::pluck(allruns, run.dir[r])
    # select all equity files containing "minority" or "poverty"
    alleqfiles <- list.files(file.path(base.dir, "indicators"), pattern = ".*minority|poverty.*") 
    filenames <- grep(file.regexp, alleqfiles, value = TRUE) 
    for (afile in filenames) {
      datatable <- dt <- NULL
      datatable <- fread(file.path(base.dir, "indicators", afile), header = TRUE, sep = ",")
      colnames(datatable)[1] <- str_replace(colnames(datatable)[1], '\\w+_', 'name_')
      dt <- melt.data.table(datatable, 
                            id.vars = "name_id",
                            measure.vars = colnames(datatable)[2: ncol(datatable)], 
                            variable.name = "variable", value.name = "estimate")
      dt[, `:=` (run = run.dir[r], year = str_extract(variable, "[[:digit:]]+"), generic_equity = str_extract(afile, "minority|poverty"))] 
      df <- rbindlist(list(df, dt), use.names = TRUE, fill = TRUE)
    }
  }
  return(df)
}

# Transform pop and emp trend estimates
prep.actuals.hct.by.juris <- function(sumby = c("county", "region"), attribute = c("pop", "emp")) {
  hct.lu <- fread(file.path(data.dir, "HCTcode.csv"))
  # dt.sheet <- read.xlsx(file.path(data.dir, "Background_data_wave2.xlsx"), sheet = "data_trend_pop")
  dt.sheet <- read.xlsx(file.path(data.dir, "Background_data_wave2.xlsx"), sheet = paste0("data_trend_", attribute))
  setDT(dt.sheet)
  cols <- dt.sheet[c(1:3), ]
  paste.collapse <- partial(paste, collapse = "_")
  column.collapse <- compose(paste.collapse, unlist)
  new.cols <- map(cols, column.collapse) %>% unlist %>% unname
  new.cols <- str_replace_all(new.cols, "NA_+", "") %>% str_replace_all("<=\\d+", "2025") %>% str_replace_all(">\\d+", "2040")
  
  dt <- dt.sheet[c(4:nrow(dt.sheet))]
  setnames(dt, colnames(dt), new.cols)
  dt <- dt[!is.na(County)]
  for(col in names(dt)) set(dt, i=which(dt[[col]] == "n/a"), j = col, value = "0") # replace n/a
  num.cols <- str_subset(new.cols, paste0("^", str_to_title(attribute)))
  id.cols <- setdiff(colnames(dt), num.cols)
  dt <- dt[, (num.cols) := lapply(.SD, as.numeric), .SDcols = num.cols] # convert to numeric
  dtm <- melt.data.table(dt, id.vars = id.cols, measure.vars = num.cols, variable.name = "columnname", value.name = "estimate")
  dtm[, `:=` (attribute = attribute, 
              year = str_extract(columnname, "(?<=\\w{3})\\d+"),
              desc = str_extract(columnname, "(?<=\\w{3}\\d{4}_).+") %>% str_replace_all("_", " "))]
  dtm[hct.lu, on = c("desc" = "description2"), code := i.code][desc %like% "RGC", code := 9][desc %like% "Non", code := 0]
  
  if (sumby == "county") {
    t <- dtm[, lapply(.SD, sum), .SDcols = "estimate", by = .(County, code, attribute, year)]
  } else if (sumby == "region") {
    t <- dtm[, lapply(.SD, sum), .SDcols = "estimate", by = .(code, attribute, year)]
  } 
} 

compile.control.totals <- function(workbook) {
  wb <- loadWorkbook(file.path(data.dir, workbook))
  scenarios <- names(wb)
  dt <- NULL
  for (ascenario in scenarios) {
    t <- NULL
    t <- read.xlsx(wb, sheet = ascenario) %>% as.data.table
    t[, scenario := ascenario]
    ifelse(is.null(dt), dt <- t, dt <- rbind(dt, t))
  }
  dtm <- melt.data.table(dt, id.vars = c("County", "scenario"), measure.vars = colnames(dt)[grep("17|50", colnames(dt))], value.name = "estimate")
  dtm[, `:=` (geog = "ct", indicator = str_extract(variable, "[[:alpha:]]+"), year = str_extract(variable, "(\\d+-)?\\d+$"))
      ][, indicator := switch(indicator, "Pop" = "population", "Emp" = "employment"), by = indicator
        ][, year := switch(year, "17" = "2017", "50" = "2050", "17-50" = "2017-2050"), by = year]
  df <- dtm[, Countychr := as.character(County)]
  df[, Countyname := switch(Countychr, "33" = "King", "35" = "Kitsap", "53" = "Pierce", "61" = "Snohomish", "99" = "Region"), by = Countychr]
  df[, `:=` (variable = NULL, Countychr = NULL)]
  d <- dcast.data.table(df, Countyname + County + scenario + indicator ~ geog + year, value.var = "estimate")
}

compile.control.totals.equity <- function() {
  # create ct_byr
  pop <- "tract_population_households.csv"
  emp <- "Tract2017_AllJobs.xlsx"
  
  p <- fread(file.path(data.dir, pop))
  p2 <- p[, GEOID10 := as.character(GEOID10)][, .(GEOID10, population = POP2017)] # contains gq
  
  e <- read.xlsx(file.path(data.dir, emp), startRow = 2) %>% as.data.table
  e2 <- e[, `:=` (GEOID10 = as.character(GEOID10), employment = Employment + Uniformed.Military)][, .(GEOID10, employment)] # contains mil
  p2[e2, on = "GEOID10", employment := i.employment][is.na(employment), employment := 0]
  
  eqlu <- read.equity.lu() %>% as.data.table # join with min/pov lookup
  dt <- p2[eqlu, on = "GEOID10"][, .(GEOID10, population, employment, minority, poverty)]
  d <- melt.data.table(dt, 
                       id.vars = c("GEOID10", "population", "employment"), 
                       measure.vars = c("minority", "poverty"), variable.name = "generic_equity", value.name = "equity")
  d2 <- melt.data.table(d, 
                        id.vars = c("GEOID10", "equity"), 
                        measure.vars = c("population", "employment"), variable.name = "indicator", value.name = "ct_byr")
  d3 <- d2[, lapply(.SD, sum), .SDcols = c("ct_byr"), by = .(equity, indicator)]
}

