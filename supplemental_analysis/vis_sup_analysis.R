library(data.table)
library(tidyverse)
library(lemon)

dir <- "J:/Projects/V2050/Supp_Alt_Analysis/Model_Output/tidy_format"
fname <- "capacity_analysis_raw_data.csv"
type <- c("pop", "emp", "au")


# transform spreadsheet
transform.data <- function(dir, fname) {
  rdt <- fread(file.path(dir, fname))
  
  # convert chr cols to numeric
  idcols <- colnames(rdt)[str_which(colnames(rdt), "^rgid_|^city_|^county_")]
  cols <- setdiff(colnames(rdt), idcols)
  chrcols <- cols[which(rdt[, sapply(.SD, is.character), .SDcols = cols])]
  rdt[, (chrcols) := lapply(.SD, as.numeric), .SDcols = chrcols]
  
  # tidy
  rdts <- NULL
  scen <- c("stc", "tfg", "rug")
  for (t in 1:length(type)) {
    selcols <- c(idcols, colnames(rdt)[str_which(colnames(rdt), paste0(type[t], "$"))]) 
    new.selcols <- str_replace_all(selcols, paste0("_", type[t]), "")
    dt <- rdt[, ..selcols]
    setnames(dt, selcols, new.selcols)
    dt[, esttype := type[t]]
    tt <- melt.data.table(dt, id.vars = setdiff(c(new.selcols, "esttype"), scen), measure.vars = scen, variable.name = "scenario", value.name = "estimate")
    rdts[[type[t]]] <- tt
  } 
  
  alldata <- rbindlist(rdts, use.names = T)
}

## county/regional geography proposed
create.county.rgprop.dt <- function(transformed.dt) {
  type.labels <- c("Population", "Employment", "Activity Units")
  rgprop.labels <- c("Metro", "Core", "HCT", "C&T", "UU")
  dt <- transformed.dt[rgid_prop != 6, lapply(.SD, sum), .SDcols = c("blands", "usim_min", "usim_50", "usim_max", "estimate"), 
                       by = .(county_id, rgid_prop, esttype, scenario)]
  dt[, esttype := factor(esttype, levels = type, labels = type.labels)]
  dt[, rgid_prop := factor(rgid_prop, levels = 1:5, labels = rgprop.labels)]
}

create.county.rgprop.linetype.dt <- function(table) {
  dt <- melt.data.table(table,
                          id.vars = c("county_id", "rgid_prop", "esttype"),
                          measure.vars = c("blands", "usim_min", "usim_50", "usim_max"),
                          variable.name = "linetype",
                          value.name = "line_estimate")
  dtu <- unique(dt)
  dtu <- dtu[linetype != "usim_min",]
  exc <- dtu[(esttype == "Activity Units" & linetype == "usim_max"), ]
  ajdt <- dtu[!exc, on = c("county_id", "rgid_prop", "esttype", "linetype")]
  ajdt[, linetype := factor(linetype, levels = c("blands", "usim_max", "usim_50"), labels = c("Buildable Lands", "Urbansim Max", "Urbansim (norm)"))]
}

alldata <- transform.data(dir, fname)
cdt <- create.county.rgprop.dt(alldata)
ldt <- create.county.rgprop.linetype.dt(cdt)








