source("../screening_factors/functions.R")

# query ofm pop block-level
query.ofm <- function(ofmfile, attributes, yrs) {
  ofm <- fread(ofmfile)
  o <- ofm[year %in% yrs & attribute %in% attributes, ] # county as string?
  o[, GEOID10 := as.character(geoid10)]
} 

query.military <- function(milfilename, geos, yrs) {
  mil <- fread(file.path(data.dir, milfilename))
  mil <- mil[!is.na(ParcelID)]
  enlist.lu <- as.data.table(enlist.lu)
  milgeo <- mil[enlist.lu, on = c("Base", "Zone", "ParcelID" = "parcel_id")]
  df <- melt.data.table(milgeo, id.vars = geos, measure.vars = grep("^\\d+", colnames(milgeo)), variable.name = "year", value.name = "enlist_estimate")
  df <- df[year %in% yrs, ][, lapply(.SD, sum), .SDcols = "enlist_estimate", by = c(geos, "year")]
}

query.gq <- function(gqfilename, geos, yrs) {
  gq <- read.xlsx(file.path(data.dir, gqfilename)) %>% as.data.table
  df <- melt.data.table(gq, id.vars = geos, measure.vars = grep("^\\d+", colnames(gq)), variable.name = "year", value.name = "gq")
  df <- df[year %in% yrs, ][, lapply(.SD, sum), .SDcols = "gq", by = c(geos, "year")][!is.na(get(eval(geos[1]))),]
}

read.actual.emp <- function(geog) { # "Jurisdiction", "RGC", "MIC"
  emp <- read.xlsx(file.path(data.dir, "Centers_StationAreas_Employment.xlsx"), sheet = "all_geog") %>% as.data.table
  colnames(emp)[str_which(colnames(emp), "\\d{4}")] <- paste0("act", colnames(emp)[str_which(colnames(emp), "\\d{4}")])
  emp[, indicator := "employment"]
  emp[Type %in% geog, ]
}

read.cities.lu <- function() {
  jlu <- read.xlsx(file.path(data.dir, "cities.xlsx")) %>% as.data.table
  jlu[, county_id := as.character(county_id)
      ][, county_name := switch(county_id, "33" = "King", "35" = "Kitsap", "53" = "Pierce", "61" = "Snohomish"), by = county_id
        ][, county_juris := switch(county_id, "33" = "King County", "35" = "Kitsap County", "53" = "Pierce County", "61" = "Snohomish County"), by = county_id
          ][, county_code := switch(county_id,  "33" = "53033", "35" = "53035", "53" = "53053", "61" = "53061"), by = county_id]
}

read.centers.lu <- function() {
  lu <- fread(file.path(data.dir, "growth_centers.csv"))[growth_center_id >= 500]
  jlu <- read.cities.lu()
  lu[jlu, on = c("city_id"), `:=` (county_id = i.county_id, 
                                   city_name = i.city_name, 
                                   county_name = i.county_name, county_juris = i.county_juris, county_code = i.county_code)]
  lu[name == "Frederickson", `:=` (county_id = "53", county_name = "Pierce", county_juris = "Pierce County", county_code = "53053")]
}

compile.tbl.supp.tod <- function(file.regexp, allruns, run.dir, ind.extension) {
  df <- NULL
  for (r in 1:length(run.dir)) { # for each run
    base.dir <- purrr::pluck(allruns, run.dir[r])
    filenames <- list.files(file.path(base.dir, "indicators"), pattern = file.regexp)
    for (afile in filenames) {
      datatable <- dt <- NULL
      datatable <- fread(file.path(base.dir, "indicators", afile), header = TRUE, sep = ",")
      colnames(datatable)[1] <- str_replace(colnames(datatable)[1], '\\w+_', 'name_')
      dt <- melt.data.table(datatable,
                            id.vars = "name_id",
                            measure.vars = colnames(datatable)[2: ncol(datatable)],
                            variable.name = "variable", value.name = "estimate")
      dt[, `:=` (run = run.dir[r], 
                 year = str_extract(variable, "\\d+$"), 
                 attribute = str_extract(variable, "^\\w+(?=_tod)"),
                 tod_id = str_extract(variable, "(?<=tod_)\\d{1}"))]
      df <- rbindlist(list(df, dt), use.names = TRUE, fill = TRUE)
    }
  }
  df[, variable := NULL]
  return(df)
}

calc.aagr <- function(start.year.col, end.year.col, start.year.num, end.year.num) {
  x <- parse(text = paste0( "((", end.year.col,"/",start.year.col,")^(1/(",end.year.num,"-", start.year.num,"))) - 1" ))
  # (((get(eval(years.col[2]))/get(eval(years.col[1])))^(1/(fcast.yrs[2]-fcast.yrs[1]))) - 1)
}
