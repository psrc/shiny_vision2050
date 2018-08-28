compile.tbl <- function(geog) {
  df <- NULL
  for (r in 1:length(run.dir)) { # for each run
    base.dir <- purrr::pluck(allruns, run.dir[r]) 
    for (a in 1:length(attributes)) { # for each attribute
      filename <- paste0(geog,'__',"table",'__',attributes[a], ind.extension)
      datatable <- read.csv(file.path(base.dir, indicator.dirnm, filename), header = TRUE, sep = ",")
      colnames(datatable)[2: ncol(datatable)] <- str_replace(colnames(datatable)[2: ncol(datatable)], '\\w+_', 'yr') # rename columns
      colnames(datatable)[1] <- str_replace(colnames(datatable)[1], '\\w+_', 'name_')
      datatable$indicator <- attributes[a]
      datatable$run <- run.dir[r]
      df <- rbindlist(list(df, datatable), use.names = TRUE, fill = TRUE)
    }
  }
  return(df)
}
