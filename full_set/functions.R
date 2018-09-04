source("../screening_factors/functions.R")

get.military <- function(geog, mil.filter = NULL, ...) {
  mil <- read.csv(file.path(data.dir, enlist.mil.file.nm), stringsAsFactors = FALSE) %>%
            drop_na(everything())
  colnames(mil)[grep("^X\\d+", colnames(mil))] <- gsub("X", "yr", colnames(mil)[grep("^X\\d+", colnames(mil))])
  dots <- lapply(c(geog, "year"), as.symbol)
  mil.df <- mil %>% 
    left_join(enlist.lu, by = c("Base", "Zone", "ParcelID" = "parcel_id")) %>%
    gather(contains("yr"), key = "year", value = "estimate") %>%
    filter(year %in% paste0("yr", c(byr, years)))  %>%
    filter(!!!mil.filter) %>% 
    group_by_(.dots = dots) %>%
    summarise(estimate = sum(estimate))  
  return(mil.df)
}

get.gq <- function(geog, gq.filter = NULL, ...) {
  # GQ population -----------------------------------------------------------
  gq.cols <- lapply(c(geog, "year"), as.symbol)
  gq <- gq.file2 %>%
    gather(contains("yr"), key = "year", value = "estimate") %>% 
    filter(year %in% paste0("yr", c(byr, years))) %>%
    filter(!!!gq.filter) %>% 
    group_by_(.dots = gq.cols) %>%
    summarise(estimate = sum(estimate))
  return(gq)
}

compile.mil.gq <- function(geo.id, ...) {
  mil <- data.table(get.military(geo.id, ...))
  setnames(mil, geo.id, "name_id")
  mil[, indicator := "employment"]
  gq <- data.table(get.gq(geo.id, ...))
  setnames(gq, geo.id, "name_id")
  gq[, indicator := "population"]
  milgq <- rbind(mil, gq)
  milgq <- rbind(milgq, milgq[, .(estimate = sum(estimate)), by = .(name_id, year)][, indicator := "activity_units"])
  return(milgq)
}
