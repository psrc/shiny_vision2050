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
