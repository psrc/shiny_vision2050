library(data.table)
library(openxlsx)

bdir <- "/Volumes/DataTeam/Projects/V2050/SEIS/Data_Support"

data.dir <- file.path(bdir, "script_input")
out.dir <- file.path(bdir, "Model_Output/Working")
out.file.prefix <- "80_CapacityIndicatorTract"

parcel.file <- file.path(data.dir, "CapacityIndicatorPcl_res50.csv")
tracts <- data.table(read.xlsx(file.path(data.dir, "census_tract.xlsx")))

parcel.geo <- fread(file.path(data.dir, "parcels_geos.csv"))

# read capacity file and merge with the census_tract_id
pcls <- fread(parcel.file)
pcl <- merge(pcls, parcel.geo[, .(parcel_id, census_tract_id)], by = "parcel_id")

# aggregate residential columns by census tract
aggr <- pcl[, .(DUbase = sum(DUbase), DUcapacity = round(sum(DUcapacity))), by = census_tract_id]
aggr[, DUdiff := DUcapacity - DUbase]

# merge with census_tract file
aggr.tr <- merge(tracts[, .(census_tract_id, geoid10)], aggr, by = "census_tract_id", all = TRUE)
aggr.tr[is.na(DUbase), `:=`(DUbase = 0, DUcapacity = 0, DUdiff = 0)]

fwrite(aggr.tr, file = file.path(out.dir, paste0(out.file.prefix, "_", Sys.Date(), ".csv")))
