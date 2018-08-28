library(data.table)
library(openxlsx)
library(tidyverse)

# hit 'source'
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)


# user input --------------------------------------------------------------

out.dir <- ("../scripts_results")
attributes <- c("Pop", "Emp")
scen.type <- "STC" #"H2O" # 

lookup.file <- list(filepath = "Y:/VISION 2050/RGS/Regional Geographies/CitiesRG_county.xlsx", sheet = "Lookup4model") 
modimpt.file <- list(filepath = paste0("J:/Projects/V2050/STC_RGS/Script/RGS2050_", scen.type, "_modInput.xlsx"), sheet = c("CityPop0050", "CityEmp0050"))

lookup <- read.xlsx(lookup.file$filepath, sheet = lookup.file$sheet)

years <- c(2000, 2016, 2050) # RLM's 16 headers are actually 17
ext.years <- c(years[1], years[length(years)])
mid.years <- c(years[2], years[length(years)])
gro.ext.years.abbr <- lapply(ext.years %>% as.character, function(x) substr(x, 3, 4)) %>% unlist %>% paste0(., collapse = "")
gro.mid.years.abbr <- lapply(mid.years %>% as.character, function(x) substr(x, 3, 4)) %>% unlist %>% paste0(., collapse = "")
share.years.abbr <- c(gro.ext.years.abbr, gro.mid.years.abbr)


# loop --------------------------------------------------------------------

dfs <- NULL

for (a in 1:length(attributes)) {
  t <- read.xlsx(modimpt.file$filepath, sheet = grep(attributes[a], modimpt.file$sheet, value = TRUE))
  
  # counties (denominator)
  cnty <- t %>%
    select_(.dots = c("County", paste0(attributes[a], "Gro", gro.mid.years.abbr))) %>%
    group_by(County) %>%
    summarise_(.dots = setNames(paste0('sum(', attributes[a], "Gro", gro.mid.years.abbr, ")"), "CountyTotal"))
  
  # region (denominator)
  region <- t %>%
    select_(.dots = c("County", paste0(attributes[a], "Gro", gro.mid.years.abbr))) %>%
    summarise_(.dots = setNames(paste0('sum(', attributes[a], "Gro", gro.mid.years.abbr, ")"), "RegionTotal"))
  
  # cities to new rgs roll-up
  t2 <- t %>%
    left_join(lookup, by = c("CityID" = "city_id", "County" = "county_id")) %>%
    select_(.dots = c("fips_RG_Proposed_id", "County", grep(paste0("^", attributes[a]), colnames(.)), grep(paste0("^RG_"), colnames(.)))) %>%
    mutate_(.dots = setNames(paste0(attributes[a], rev(ext.years), collapse = "-"), paste0(paste0(attributes[a], "Gro"), gro.ext.years.abbr))) %>%
    select_(.dots = c("fips_RG_Proposed_id", "County", grep(paste0("^", attributes[a], "Gro"), colnames(.)), grep(paste0("^RG_"), colnames(.)))) %>%
    group_by(fips_RG_Proposed_id, County, RG_Proposed) %>% #
    summarise_if(is.numeric, sum) %>%
    mutate_(.dots = setNames(paste0("(", paste0(paste0(attributes[a], "Gro"), share.years.abbr, collapse = "-"), ")/", paste0(attributes[a], "Gro", gro.ext.years.abbr)), paste0(attributes[a], "Achieved"))) %>%
    left_join(cnty, by = "County") %>%
    mutate_(.dots = setNames(paste0(paste0(attributes[a], "Gro", gro.mid.years.abbr), "/CountyTotal"), paste0(paste0(attributes[a], "Gro", gro.mid.years.abbr), "_of_co"))) %>%
    select(-CountyTotal)
  
  # region by new rgs
  t4 <- t %>%
    left_join(lookup, by = c("CityID" = "city_id", "County" = "county_id")) %>%
    select_(.dots = c(grep(paste0("^", attributes[a]), colnames(.)), grep(paste0("^RG_"), colnames(.)))) %>%
    mutate_(.dots = setNames(paste0(attributes[a], rev(ext.years), collapse = "-"), paste0(paste0(attributes[a], "Gro"), gro.ext.years.abbr))) %>%
    select_(.dots = c(grep(paste0("^", attributes[a], "Gro"), colnames(.)), grep(paste0("^RG_"), colnames(.)))) %>%
    group_by(RG_Proposed) %>% #
    summarise_if(is.numeric, sum) %>%
    mutate_(.dots = setNames(paste0("(", paste0(paste0(attributes[a], "Gro"), share.years.abbr, collapse = "-"), ")/", paste0(attributes[a], "Gro", gro.ext.years.abbr)), paste0(attributes[a], "Achieved"))) %>%
    mutate(RegionTotal = region$RegionTotal) %>%
    mutate_(.dots = c(setNames(paste0(paste0(attributes[a], "Gro", gro.mid.years.abbr), "/RegionTotal"), paste0(paste0(attributes[a], "Gro", gro.mid.years.abbr), "_of_co")),
                      setNames(NA, "fips_RG_Proposed_id"),
                      setNames(NA, "County")
                      )) %>%
    select(-RegionTotal)
  
  # counties 
  t3 <- t %>%
    left_join(lookup, by = c("CityID" = "city_id", "County" = "county_id")) %>%
    select_(.dots = c("County", grep(paste0("^", attributes[a]), colnames(.)), grep(paste0("^RG_"), colnames(.)))) %>%
    mutate_(.dots = setNames(paste0(attributes[a], rev(ext.years), collapse = "-"), paste0(paste0(attributes[a], "Gro"), gro.ext.years.abbr))) %>%
    select_(.dots = c("County", grep(paste0("^", attributes[a], "Gro"), colnames(.)), grep(paste0("^RG_"), colnames(.)))) %>%
    group_by(County) %>% 
    summarise_if(is.numeric, sum) %>%
    mutate_(.dots = setNames(paste0("(", paste0(paste0(attributes[a], "Gro"), share.years.abbr, collapse = "-"), ")/", paste0(attributes[a], "Gro", gro.ext.years.abbr)), paste0(attributes[a], "Achieved"))) %>%
    mutate(RegionTotal = region$RegionTotal) %>%
    mutate_(.dots = c(setNames(paste0(paste0(attributes[a], "Gro", gro.mid.years.abbr), "/RegionTotal"), paste0(paste0(attributes[a], "Gro", gro.mid.years.abbr), "_of_co")),
                      setNames(NA, "fips_RG_Proposed_id"),
                      setNames("'County'", "RG_Proposed"))) %>%
    select(-RegionTotal)
  
  dfs[[paste0(attributes[a], "Region")]] <- t4
  dfs[[paste0(attributes[a], "CntyDenom")]] <- cnty
  dfs[[attributes[a]]] <- t2
  dfs[[paste0(attributes[a], "Cnty")]] <- t3
}

df1 <- merge(dfs$Pop, dfs$Emp, by = c("fips_RG_Proposed_id", "County", "RG_Proposed"))
df2 <- merge(dfs$PopCnty, dfs$EmpCnty, by = c("fips_RG_Proposed_id", "County", "RG_Proposed"))
df3 <- merge(dfs$PopRegion, dfs$EmpRegion, by = c("fips_RG_Proposed_id", "County", "RG_Proposed"))

df <- bind_rows(df1, df2, df3)

# RLM's 16 headers are actually 17
colnames(df)[grep(gro.mid.years.abbr, colnames(df))] <- gsub(substr(years[2], 3, 4), "17", colnames(df)[grep(gro.mid.years.abbr, colnames(df))])


# export ------------------------------------------------------------------

write.xlsx(df, file.path(out.dir, paste0("proposedRGS_", scen.type, "_", Sys.Date(), ".xlsx")))


