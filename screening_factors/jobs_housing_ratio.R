library(data.table)
library(openxlsx)
library(tidyverse)

# settings --------------------------------------------------------------

if(!exists("set.globals") || !set.globals) {
  curr.dir <- getwd()
  this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(this.dir)
  source("settings.R")
  source("functions.R")
}

cat("\nComputing indicator 18, jobs-housing ratio\n")
out.file.nm <- settings$jhr$out.file.nm


# general -----------------------------------------------------------------

subarea.cnty.lu <- read.xlsx(file.path(data.dir, "subarea.xlsx"))

years.col <- paste0("yr", years)
kin.cols <- c("East King","Sea-Shore", "South King")
geo.cols <- c("King", kin.cols, "Kitsap", "Pierce", "Snohomish")

attributes <- c("employment", "households")
new.attributes <- c("employment", "housing_unit")
ind.extension <- ".csv"


# base year actuals -------------------------------------------------------

file.nm <- file.path(data.dir, "jobs_hsg_ratio_2017_15rev.xlsx")

# housing units ofm (april 1 postcensal)
byro.df <- read.xlsx(file.nm, sheet = "hsg_psrc") %>%
  select(geography, housing_unit_byr = hu) %>%
  filter(geography != "Region")

byre.df0 <- read.xlsx(file.nm, sheet = "emp_psrc") %>%
  select(geography = Subarea, emp_no_enlist = `Total.Jobs.(2017)`) %>%
  filter(geography != "Total")


# enlisted personnel ------------------------------------------------------

mil <- read.csv(file.path(data.dir, enlist.mil.file.nm), stringsAsFactors = FALSE) %>%
  drop_na(everything())

colnames(mil)[grep("^X\\d+", colnames(mil))] <- gsub("X", "yr", colnames(mil)[grep("^X\\d+", colnames(mil))])

mil.df <- mil %>% 
  left_join(enlist.lu, by = c("Base", "Zone", "ParcelID" = "parcel_id")) %>%
  gather(contains("yr"), key = "year", value = "estimate") %>%
  filter(year %in% paste0("yr", c(2017, years))) %>%
  group_by(subarea_id, year) %>%
  summarise(enlist_estimate = sum(estimate)) %>%
  left_join(subarea.cnty.lu, by = "subarea_id")

# add enlisted to main employment df
byre.mil.df <- mil.df %>% filter(year == "yr2017") %>% select(subarea_name, enlist_estimate)

byre.df <- byre.df0 %>%
  left_join(byre.mil.df, by = c("geography" = "subarea_name")) %>%
  replace_na(list(enlist_estimate = 0)) %>%
  mutate(employment_byr = emp_no_enlist + enlist_estimate) %>%
  select(-subarea_id)

# assemble base year data  
byr.df <- byre.df %>%
  left_join(byro.df, by = "geography") %>%
  select(geography, contains("_byr")) %>%
  as.data.table()


# subarea forecast data -----------------------------------------------------

df.subarea <- compile.tbl('subarea', allruns, run.dir, attributes, ind.extension)

df2 <- melt.data.table(df.subarea,
                       id.vars = c("name_id", "run", "indicator"),
                       measure.vars = grep("yr", colnames(df.subarea), value = TRUE),
                       variable.name = "year", value.name = "estimate")
df3 <- df2[year %in% years.col][subarea.cnty.lu, on = c("name_id" = "subarea_id")]

# sum KC, append to main forecast df
kc <- df3[cnty_id == 33, lapply(.SD, sum), .SDcols = "estimate", by =.(run, indicator, year, cnty_id, cnty_name)
          ][, `:=` (name_id = 7, subarea_name = "King")]
df4 <- rbindlist(list(df3, kc), use.names = TRUE, fill = TRUE)
df.cast <- dcast.data.table(df4, cnty_name + subarea_name + run  ~ indicator + year, value.var = "estimate")

# add enlisted to employment in forecast df
fcast.mil.df <- mil.df %>% filter(year %in% years.col) %>% select(subarea_name, enlist_estimate) %>% as.data.table

df5 <- df.cast %>%
  left_join(fcast.mil.df, by = "subarea_name") %>%
  replace_na(list(enlist_estimate = 0)) %>%
  select(-subarea_id) %>%
  as.data.table()

fcast.expr <- parse(text = paste0(paste0("employment_w_enlist_", years.col), ":=", paste0("employment_", years.col), "+", "enlist_estimate"))
fcast.expr2 <- parse(text = paste0(paste0("households_to_units_", years.col), ":=", paste0("households_", years.col), "/", ".95"))
fcast.sel.col <- c(colnames(df5)[1:3], paste(c("employment_w_enlist", "households_to_units"), years.col, sep = "_"))
df6 <- df5[, eval(fcast.expr)][, eval(fcast.expr2)][, ..fcast.sel.col]
setnames(df6, 
         c(paste0("employment_w_enlist_", years.col), paste0("households_to_units_", years.col)), 
         c(paste0("employment_", years.col), paste0("housing_unit_", years.col)))

d <- df6[byr.df, on = c("subarea_name" = "geography")]

# sum region, append to main forecast df
sdcols <- c(grep("employment|housing_unit", colnames(d), value = T))
reg <- d[!(subarea_name %in% kin.cols), lapply(.SD, sum), .SDcols = sdcols, by = run][, `:=` (subarea_name = "Region", cnty_name = "Region")]
d2 <- rbindlist(list(d, reg), use.names = T)

# calculate ratios
byr.col <-  c(grep("byr", colnames(d2), value = T))
fcast.col <-  c(grep("_yr", colnames(d2), value = T))

fcast.expr3 <- parse(text = paste0(paste0("ehratio_", years.col), ":=", fcast.col[1], "/", fcast.col[2]))
byr.expr <- parse(text = paste0("ehratio_byr :=", byr.col[1], "/", byr.col[2]))

d3 <- d2[, eval(fcast.expr3)][, eval(byr.expr)]


# equity data -------------------------------------------------------------

# equity expression to retrieve regional equity tables
file.regexp <- paste0("(^minority|^poverty).*(employment|households)\\.csv")

compile.tract.actuals <- function(hu, emp) { # includes military
  h <- read.xlsx(file.path(data.dir, hu), sheet = "Total Housing Units", startRow = 11, rows = c(1:1469)) %>% as.data.table
  h2 <- h[County.Name %in% c("King", "Kitsap", "Pierce", "Snohomish")
     ][, .(GEOID10 = Census.Tract.Code.Complete, housing_unit = Estimated.Total.Housing.Units.2017)][, GEOID10 := as.character(GEOID10)]
  
  e <- read.xlsx(file.path(data.dir, emp), startRow = 2) %>% as.data.table
  e2 <- e[, `:=` (GEOID10 = as.character(GEOID10), total_employment = Employment + Uniformed.Military)][, .(GEOID10, employment = total_employment)]
  h2[e2, on = "GEOID10", emp := i.employment][is.na(emp), emp := 0]
  setnames(h2, "emp", "employment")
  
  eqlu <- read.equity.lu() %>% as.data.table # join with min/pov lookup
  dt <- h2[eqlu, on = "GEOID10"][, .(GEOID10, housing_unit, employment, minority, poverty)]
  d <- melt.data.table(dt, 
                       id.vars = c("GEOID10", "housing_unit", "employment"), 
                       measure.vars = c("minority", "poverty"), variable.name = "generic_equity", value.name = "equity")
  d2 <- d[, lapply(.SD, sum), .SDcols = c("housing_unit", "employment"), by = equity]
  setnames(d2, c("housing_unit", "employment"), paste0(c("housing_unit", "employment"), "_byr"))
}

query.military <- function(milfilename, geos, yrs) {
  mil <- fread(file.path(data.dir, milfilename))
  mil <- mil[!is.na(ParcelID)]
  enlist.lu <- as.data.table(enlist.lu)
  milgeo <- mil[enlist.lu, on = c("Base", "Zone", "ParcelID" = "parcel_id")]
  df <- melt.data.table(milgeo, id.vars = geos, measure.vars = grep("^\\d+", colnames(milgeo)), variable.name = "year", value.name = "enlist_estimate")
  df <- df[year %in% yrs, ][, lapply(.SD, sum), .SDcols = "enlist_estimate", by = c(geos, "year")]
}

# create equity table
calc.equity.table <- function() {
  eqact <- compile.tract.actuals("saep_tract10.xlsx", "Tract2017_AllJobs.xlsx")
  
  edf <- compile.tbl.equity(file.regexp, allruns, run.dir, ind.extension)
  edf[, equity := switch(name_id, `1` = paste0("non-", generic_equity), `2` = generic_equity), by = name_id
      ][, indicator := str_extract(variable, "^[[:alpha:]]+")] # create new field equity: 1 = non-minority/poverty, 2 = minority/poverty
  edt <- edf[year == years]
  # transform military df grouping by equity categories and add to forecast
  emil <- query.military(enlist.mil.file.nm, c("minority_id", "poverty_id"), "2050")
  emil[, `:=` (minority_id = as.character(minority_id), poverty_id = as.character(poverty_id))]
  emil[, minority := switch(minority_id, "0" = "non-minority", "1" = "minority"), by = minority_id
       ][, poverty := switch(poverty_id, "0" = "non-poverty", "1" = "poverty"), by = poverty_id
         ][, indicator := "employment"]
  emildt <- melt.data.table(emil, 
                            id.vars = c("indicator", "enlist_estimate"), 
                            measure.vars = c("minority", "poverty"), variable.name = "generic_equity", value.name = "equity")
  
  edt[emildt, on = c("generic_equity", "equity", "indicator"), enlist := i.enlist_estimate
      ][is.na(enlist), enlist := 0
        ][, estimate_w_enlist := estimate + enlist
          ][indicator == "households", estimate_plus := estimate_w_enlist/.95
            ][is.na(estimate_plus), estimate_plus := estimate_w_enlist
              ][, new_indicator := switch(indicator, "employment" = "employment", "households" = "housing_unit"), by = indicator]
  dt <- dcast.data.table(edt, run + equity ~ new_indicator + paste0("yr", year), value.var = "estimate_plus")
  dt[eqact, on = "equity", `:=` (housing_unit_byr = i.housing_unit_byr, employment_byr = i.employment_byr)
     ][, `:=`(cnty_name = "Region", subarea_name = equity)
       ][, equity := NULL
         ][, eval(fcast.expr3)][, eval(byr.expr)]
}


eqdf <- calc.equity.table()
# bind subareas & region with equity categories
dt <- rbindlist(list(d3, eqdf), use.names = TRUE)

# calculate index and store each table (by run) in list
dlist <- NULL
for (r in 1:length(run.dir)) {
  t <- NULL
  ind.expr <- parse(text = paste0(paste0("ehindex_", years.col), ":=", paste0("ehratio_", years.col), "/t[subarea_name == 'Region', ", paste0("ehratio_", years.col), "]"))
  t <- dt[run == run.dir[r],]
  t[, `:=` (ehindex_byr = (ehratio_byr/(t[subarea_name == "Region", ehratio_byr])))][, eval(ind.expr)][, scenario := names(run.dir[r])]
  roworder <- c("King", "Sea-Shore", "South King", "East King", "Kitsap", "Pierce", "Snohomish", 
                "minority", "non-minority", "poverty", "non-poverty", "Region")
  t2 <- t[match(roworder, t$subarea_name)]
  setcolorder(t2, c("cnty_name", 
                    "subarea_name", 
                    "run", 
                    "scenario", 
                    paste0(new.attributes, "_byr"), 
                    paste0(new.attributes, "_", years.col), 
                    paste0("ehratio_", c("byr", years.col)), 
                    grep("ehindex", colnames(t2), value = T)))
  colnames(t2)[grep("byr", colnames(t2))] <- str_replace_all(colnames(t2)[grep("byr", colnames(t2))], "byr", paste0("yr", byr))
  colnames(t2)[grep("^(em|h).*yr2017", colnames(t2))] <- paste0(colnames(t2)[grep("^(em|h).*yr2017", colnames(t2))], "_actual")
  dlist[[names(run.dir[r])]] <- t2
}


# export ------------------------------------------------------------------

write.xlsx(dlist, file.path(out.dir, paste0(out.file.nm, "_", Sys.Date(), ".xlsx")))

setwd(curr.dir)
