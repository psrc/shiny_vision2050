source("../screening_factors/functions.R")

# query ofm pop block-level
query.ofm <- function(ofmfile, attributes, yrs) {
  ofm <- fread(ofmfile)
  ofm[year %in% yrs & attribute %in% attributes, ] # county as string?
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
