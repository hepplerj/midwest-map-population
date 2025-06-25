#!/usr/bin/Rscript

## Load census data and aggregate population data. Create a single
## long table to be used in visualization `midwest-census.csv`.

library(tidyverse)
library(stringr)
library(sf)
library(foreign)
options(stringsAsFactors = FALSE)

sqm_sqmi <- function(sq_meters) {
  sq_meters / 2589988.110336;
}

###--------------------------------------------------
# Prepare dataframe
###--------------------------------------------------

# Read CSV files and create a list
fn <- Sys.glob("data/*.csv")
csv <- lapply(fn, read_csv)
names(csv) <- fn %>% str_replace("data/", "") %>% str_replace(".csv", "")
rm(fn)

# Read all DBF files into a list
fn <- Sys.glob("data/shp/*.dbf")
dbf <- lapply(fn, read.dbf, as.is = TRUE)
names(dbf) <-  fn %>% 
  str_replace(".dbf", "") %>%
  str_replace("data/shp/", "")

# Calculated data
# ----------------------------

# Each of the c_#### variables below calculates a set of data for Census 
# information from NHGIS shapefiles and NHGIS data tables.
derived_data <- function(df) {
  df %>% mutate(
    totalDens = round(totalPop / sqm_sqmi(area), 2)
  ) 
}

# 1790
c_1790 <- csv$nhgis0013_ds1_1790_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1790.simplified, by = "GISJOIN") %>%
  select(GISJOIN,
         totalPop = A00AA1790,
         freeAfAm = AAQ001,
         slavePop = AAQ002,
         totalWhite = AAQ003,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  mutate(totalAfAm = freeAfAm + slavePop,
         afAmPerc = round(100 * totalAfAm / totalPop, 2),
         whitePerc = round(100 * totalWhite / totalPop, 2)) %>% 
  derived_data()

# 1800
c_1800 <- csv$nhgis0013_ds2_1800_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1800.simplified, by = "GISJOIN") %>%
  select(GISJOIN,
         totalPop = A00AA1800,
         freeAfAm = AAY001,
         slavePop = AAY002,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  mutate(totalAfAm = freeAfAm + slavePop,
         afAmPerc = round(100 * totalAfAm / totalPop, 2),
         totalWhite = totalPop - totalAfAm,
         whitePerc = round(100 * totalWhite / totalPop, 2)) %>% 
  derived_data()

# 1810
c_1810 <- csv$nhgis0013_ds3_1810_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1810.simplified, by = "GISJOIN") %>%
  select(GISJOIN,
         totalPop = A00AA1810,
         freeAfAm = AA7001,
         slavePop = AA7002,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  mutate(totalAfAm = freeAfAm + slavePop,
         afAmPerc = round(100 * totalAfAm / totalPop, 2),
         totalWhite = totalPop - totalAfAm,
         whitePerc = round(100 * totalWhite / totalPop, 2)) %>% 
  derived_data()

# 1820
c_1820 <- csv$nhgis0013_ds4_1820_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1820.simplified, by = "GISJOIN") %>%
  mutate(freeAfAm = ABB005 + ABB006,
         slavePop = ABB003 + ABB004,
         totalAfAm = freeAfAm + slavePop,
         afAmPerc = round(100 * totalAfAm / A00AA1820, 2),
         totalWhite = A00AA1820 - totalAfAm,
         whitePerc = round(100 * totalWhite / A00AA1820, 2)) %>%
  select(GISJOIN,
         totalPop = A00AA1820,
         freeAfAm,
         slavePop,
         totalAfAm,
         afAmPerc,
         totalWhite,
         whitePerc,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  derived_data()

# 1830
c_1830 <- csv$nhgis0013_ds5_1830_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1830.simplified, by = "GISJOIN") %>%
  mutate(freeAfAm = ABO005 + ABO006,
         slavePop = ABO003 + ABO004,
         totalAfAm = freeAfAm + slavePop,
         afAmPerc = round(100 * totalAfAm / A00AA1830, 2),
         totalWhite = A00AA1830 - totalAfAm,
         whitePerc = round(100 * totalWhite / A00AA1830, 2)) %>%
  select(GISJOIN,
         totalPop = A00AA1830,
         freeAfAm,
         slavePop,
         totalAfAm,
         afAmPerc,
         totalWhite,
         whitePerc,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  derived_data()

# 1840
c_1840 <- csv$nhgis0013_ds7_1840_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1840.simplified, by = "GISJOIN") %>%
  mutate(totalAfAm = ACS002 + ACS003,
         afAmPerc = round(100 * totalAfAm / A00AA1840, 2),
         whitePerc = round(100 * ACS001 / A00AA1840, 2)) %>%
  select(GISJOIN,
         totalPop = A00AA1840,
         freeAfAm = ACS002,
         slavePop = ACS003,
         totalAfAm,
         afAmPerc,
         totalWhite = ACS001,
         whitePerc,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  derived_data()

# 1850
c_1850 <- csv$nhgis0013_ds10_1850_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1850.simplified, by = "GISJOIN") %>%
  mutate(totalAfAm = AE6002 + AE6003,
         afAmPerc = round(100 * totalAfAm / A00AA1850, 2),
         whitePerc = round(100 * AE6001 / A00AA1850, 2)) %>%
  select(GISJOIN,
         totalPop = A00AA1850,
         freeAfAm = AE6002,
         slavePop = AE6003,
         totalAfAm,
         afAmPerc,
         totalWhite = AE6001,
         whitePerc,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  derived_data()

# 1860
c_1860 <- csv$nhgis0016_ds14_1860_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1860.simplified, by = "GISJOIN") %>%
  mutate(freeAfAm = AH2003 + AH2004,
         slavePop = AH2005 + AH2006,
         totalAfAm = AH2003 + AH2004 + AH2005 + AH2006,
         afAmPerc = round(100 * totalAfAm / A00AA1860, 2),
         totalWhite = AH2001 + AH2002,
         whitePerc = round(100 * totalWhite / A00AA1860, 2)) %>%
  select(GISJOIN,
         totalPop = A00AA1860,
         freeAfAm,
         slavePop,
         totalAfAm,
         afAmPerc,
         totalWhite,
         whitePerc,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  derived_data()

# 1870
c_1870 <- csv$nhgis0031_ds17_1870_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1870.simplified, by = "GISJOIN") %>%
  select(GISJOIN,
         totalPop = A00AA1870,
         totalAfAm = AK3002,
         totalAsian = AK3003,
         totalIndian = AK3004,
         totalWhite = AK3001,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  mutate(afAmPerc = round(100 * totalAfAm / totalPop, 2),
         asianPerc = round(100 * totalAsian / totalPop, 2),
         indianPerc = round(100 * totalIndian / totalPop, 2),
         whitePerc = round(100 * totalWhite / totalPop, 2)
  ) %>% 
  derived_data()

# 1880
c_1880 <- csv$nhgis0031_ds23_1880_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1880.simplified, by = "GISJOIN") %>%
  select(GISJOIN,
         totalPop = A00AA1880,
         totalAfAm = APP002,
         totalAsian = APP003,
         totalIndian = APP004,
         totalWhite = APP001,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  mutate(afAmPerc = round(100 * totalAfAm / totalPop, 2),
         asianPerc = round(100 * totalAsian / totalPop, 2),
         indianPerc = round(100 * totalIndian / totalPop, 2),
         whitePerc = round(100 * totalWhite / totalPop, 2)
  ) %>% 
  derived_data()

# 1890
c_1890 <- csv$nhgis0032_ds27_1890_county %>%
  left_join(csv$nhgis0033_ds27_1890_county, by = "GISJOIN") %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1890.simplified, by = "GISJOIN") %>%
  select(GISJOIN,
         totalPop = A00AA1890,
         totalAfAm = AVF001,
         totalAsian = AVF004,
         totalIndian = AVF010,
         totalWhite = AV2001,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR.x) %>%
  mutate(afAmPerc = round(100 * totalAfAm / totalPop, 2),
         asianPerc = round(100 * totalAsian / totalPop, 2),
         indianPerc = round(100 * totalIndian / totalPop, 2),
         whitePerc = round(100 * totalWhite / totalPop, 2)
  ) %>% 
  derived_data()

# 1900
c_1900 <- csv$nhgis0033_ds31_1900_county %>%
  left_join(csv$nhgis0036_ds31_1900_county, by = "GISJOIN") %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1900.simplified, by = "GISJOIN") %>%
  mutate(totalAfAm = AZ3003 + AZ3004) %>%
  select(GISJOIN,
         totalPop = A00AA1900,
         totalAfAm,
         totalWhite = AZ2001,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR.x) %>%
  mutate(afAmPerc = round(100 * totalAfAm / totalPop, 2),
         whitePerc = round(100 * totalWhite / totalPop, 2)
  ) %>% 
  derived_data()

# 1910
c_1910 <- csv$nhgis0030_ds37_1910_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1910.simplified, by = "GISJOIN") %>%
  select(GISJOIN,
         totalPop = A00AA1910,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  derived_data()

# 1920
c_1920 <- csv$nhgis0030_ds43_1920_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1920.simplified, by = "GISJOIN") %>%
  select(GISJOIN,
         totalPop = A00AA1920,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  derived_data()

# 1930
c_1930 <- csv$nhgis0037_ds52_1930_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1930.simplified, by = "GISJOIN") %>%
  select(GISJOIN,
         totalPop = A00AA1930,
         totalAfAm = BC0001,
         totalWhite = BCZ001,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  mutate(afAmPerc = round(100 * totalAfAm / totalPop, 2),
         whitePerc = round(100 * totalWhite / totalPop, 2)
  ) %>% 
  derived_data()

# 1940
c_1940 <- csv$nhgis0037_ds78_1940_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1940.simplified, by = "GISJOIN") %>%
  select(GISJOIN,
         totalPop = A00AA1940,
         totalAfAm = BYA003,
         totalWhite = BWS001,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  mutate(afAmPerc = round(100 * totalAfAm / totalPop, 2),
         whitePerc = round(100 * totalWhite / totalPop, 2)
  ) %>% 
  derived_data()

# 1950
c_1950 <- csv$nhgis0038_ds83_1950_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1950.simplified, by = "GISJOIN") %>%
  select(GISJOIN,
         totalPop = A00AA1950,
         totalAfAm = B1T002,
         totalWhite = B1T001,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  mutate(afAmPerc = round(100 * totalAfAm / totalPop, 2),
         whitePerc = round(100 * totalWhite / totalPop, 2)
  ) %>% 
  derived_data()

# 1960
c_1960 <- csv$nhgis0038_ds89_1960_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1960.simplified, by = "GISJOIN") %>%
  select(GISJOIN,
         totalPop = A00AA1960,
         totalAfAm = B48002,
         totalWhite = B48001,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  mutate(afAmPerc = round(100 * totalAfAm / totalPop, 2),
         whitePerc = round(100 * totalWhite / totalPop, 2)
  ) %>% 
  derived_data()

# 1970
c_1970 <- csv$nhgis0038_ds94_1970_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1970.simplified, by = "GISJOIN") %>%
  select(GISJOIN,
         totalPop = A00AA1970,
         totalAfAm = CBW002,
         totalWhite = CBW001,
         totalIndian = CBW003,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  mutate(afAmPerc = round(100 * totalAfAm / totalPop, 2),
         indianPerc = round(100 * totalIndian / totalPop, 2),
         whitePerc = round(100 * totalWhite / totalPop, 2)
  ) %>% 
  derived_data()

# 1980
c_1980 <- csv$nhgis0038_ds104_1980_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1980.simplified, by = "GISJOIN") %>%
  mutate(
    totalIndian = C9D003 + C9D004, C9D005,
    totalAsian = C9D006 + C9D007 + C9D008 + C9D009 + C9D010 + C9D011 + C9D012 + C9D013 + C9D014
  ) %>%
  select(GISJOIN,
         totalPop = A00AA1980,
         totalAfAm = C9D002,
         totalWhite = C9D001,
         totalIndian,
         totalAsian,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  mutate(afAmPerc = round(100 * totalAfAm / totalPop, 2),
         asianPerc = round(100 * totalAsian / totalPop, 2),
         indianPerc = round(100 * totalIndian / totalPop, 2),
         whitePerc = round(100 * totalWhite / totalPop, 2)
  ) %>% 
  derived_data()

# 1990
c_1990 <- csv$nhgis0038_ds120_1990_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1990.simplified, by = "GISJOIN") %>%
  select(GISJOIN,
         totalPop = A00AA1990,
         totalAfAm = EUY002,
         totalWhite = EUY001,
         totalIndian = EUY003,
         totalAsian = EUY004,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  mutate(afAmPerc = round(100 * totalAfAm / totalPop, 2),
         asianPerc = round(100 * totalAsian / totalPop, 2),
         indianPerc = round(100 * totalIndian / totalPop, 2),
         whitePerc = round(100 * totalWhite / totalPop, 2)
  ) %>% 
  derived_data()

# 2000
c_2000 <- csv$nhgis0038_ds146_2000_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_2000.simplified, by = "GISJOIN") %>%
  select(GISJOIN,
         totalPop = A00AA2000,
         totalAfAm = FMR002,
         totalWhite = FMR001,
         totalIndian = FMR003,
         totalAsian = FMR004,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  mutate(afAmPerc = round(100 * totalAfAm / totalPop, 2),
         asianPerc = round(100 * totalAsian / totalPop, 2),
         indianPerc = round(100 * totalIndian / totalPop, 2),
         whitePerc = round(100 * totalWhite / totalPop, 2)
  ) %>% 
  derived_data()

# 2010
c_2010 <- csv$nhgis0038_ds176_20105_2010_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_2010.simplified, by = "GISJOIN") %>%
  mutate(year = 2010) %>% 
  select(GISJOIN,
         totalPop = A00AA2010,
         totalAfAm = JMBE003,
         totalWhite = JMBE002,
         totalIndian = JMBE004,
         totalAsian = JMBE005,
         state = STATE.x,
         county = COUNTY.x,
         year) %>% 
  mutate(afAmPerc = round(100 * totalAfAm / totalPop, 2),
         asianPerc = round(100 * totalAsian / totalPop, 2),
         indianPerc = round(100 * totalIndian / totalPop, 2),
         whitePerc = round(100 * totalWhite / totalPop, 2)
  )

# Join all the data together and output
## Using the rbind.fill from `plyr` since columns from the dataframes are 
## different sizes.
library(plyr)
all <- rbind.fill(c_1790, c_1800, c_1810, c_1820, c_1830, c_1840, c_1850, c_1860, c_1870, c_1880, c_1890, c_1900, c_1910, c_1920, c_1930, c_1940, c_1950, c_1960, c_1970, c_1980, c_1990, c_2000, c_2010)

# Select only midwestern cities
midwestStates <- c("Illinois", "Indiana", "Iowa", "Kansas", "Missouri", "Minnesota", "Michigan", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin")
midwestCensus <- all %>%
  filter(state %in% midwestStates)

# Write CSV
write_csv(midwestCensus, "midwest-census.csv", na = "")

###--------------------------------------------------
# Shapefile prep (read, convert projection, create DF)
###--------------------------------------------------

library(rgdal)
wgs.84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

shp_1810 <- read_sf("/Users/jheppler/Dropbox/research-data/nhgis-shapefiles/epsg4326/", "US_county_1810")
shp_md_1810 <- shp_1810[shp_1810$STATENAM %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Missouri", "Minnesota", "Michigan", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin"),] %>% 
  select(-STATE, -COUNTY)
merge_1810 <- merge(shp_md_1810, c_1810, by = "GISJOIN")
merge_1810 <- st_transform(merge_1810, st_crs(wgs.84))
write_sf(obj=merge_1810, dsn="~/Desktop", layer="merge_1810", driver="ESRI Shapefile")

shp_1820 <- read_sf("/Users/jheppler/Dropbox/research-data/nhgis-shapefiles/epsg4326/", "US_county_1820")
shp_md_1820 <- shp_1820[shp_1820$STATENAM %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Missouri", "Minnesota", "Michigan", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin"),] %>% 
  select(-STATE, -COUNTY)
merge_1820 <- merge(shp_md_1820, c_1820, by = "GISJOIN")
merge_1820 <- st_transform(merge_1820, st_crs(wgs.84))
write_sf(obj=merge_1820, dsn="~/Desktop", layer="merge_1820", driver="ESRI Shapefile")

shp_1830 <- read_sf("/Users/jheppler/Dropbox/research-data/nhgis-shapefiles/epsg4326/", "US_county_1830")
shp_md_1830 <- shp_1830[shp_1830$STATENAM %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Missouri", "Minnesota", "Michigan", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin"),] %>% 
  select(-STATE, -COUNTY)
merge_1830 <- merge(shp_md_1830, c_1830, by = "GISJOIN")
merge_1830 <- st_transform(merge_1830, st_crs(wgs.84))
write_sf(obj=merge_1830, dsn="~/Desktop", layer="merge_1830", driver="ESRI Shapefile")

shp_1840 <- read_sf("/Users/jheppler/Dropbox/research-data/nhgis-shapefiles/epsg4326/", "US_county_1840")
shp_md_1840 <- shp_1840[shp_1840$STATENAM %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Missouri", "Minnesota", "Michigan", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin"),] %>% 
  select(-STATE, -COUNTY)
merge_1840 <- merge(shp_md_1840, c_1840, by = "GISJOIN")
merge_1840 <- st_transform(merge_1840, st_crs(wgs.84))
write_sf(obj=merge_1840, dsn="~/Desktop", layer="merge_1840", driver="ESRI Shapefile")

shp_1850 <- read_sf("/Users/jheppler/Dropbox/research-data/nhgis-shapefiles/epsg4326/", "US_county_1850")
shp_md_1850 <- shp_1850[shp_1850$STATENAM %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Missouri", "Minnesota", "Michigan", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin"),] %>% 
  select(-STATE, -COUNTY)
merge_1850 <- merge(shp_md_1850, c_1850, by = "GISJOIN")
merge_1850 <- st_transform(merge_1850, st_crs(wgs.84))
write_sf(obj=merge_1850, dsn="~/Desktop", layer="merge_1850", driver="ESRI Shapefile")

shp_1860 <- read_sf("/Users/jheppler/Dropbox/research-data/nhgis-shapefiles/epsg4326/", "US_county_1860_conflated")
shp_md_1860 <- shp_1860[shp_1860$STATENAM %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Missouri", "Minnesota", "Michigan", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin"),] %>% 
  select(-STATE, -COUNTY)
merge_1860 <- merge(shp_md_1860, c_1860, by = "GISJOIN")
merge_1860 <- st_transform(merge_1860, st_crs(wgs.84))
write_sf(obj=merge_1860, dsn="~/Desktop", layer="merge_1860", driver="ESRI Shapefile")

shp_1870 <- read_sf("/Users/jheppler/Dropbox/research-data/nhgis-shapefiles/epsg4326/", "US_county_1870")
shp_md_1870 <- shp_1870[shp_1870$STATENAM %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Missouri", "Minnesota", "Michigan", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin"),] %>% 
  select(-STATE, -COUNTY)
merge_1870 <- merge(shp_md_1870, c_1870, by = "GISJOIN")
merge_1870 <- st_transform(merge_1870, st_crs(wgs.84))
write_sf(obj=merge_1870, dsn="~/Desktop", layer="merge_1870", driver="ESRI Shapefile")

shp_1880 <- read_sf("/Users/jheppler/Dropbox/research-data/nhgis-shapefiles/epsg4326/", "US_county_1880")
shp_md_1880 <- shp_1880[shp_1880$STATENAM %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Missouri", "Minnesota", "Michigan", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin"),] %>% 
  select(-STATE, -COUNTY)
merge_1880 <- merge(shp_md_1880, c_1880, by = "GISJOIN")
merge_1880 <- st_transform(merge_1880, st_crs(wgs.84))
write_sf(obj=merge_1880, dsn="~/Desktop", layer="merge_1880", driver="ESRI Shapefile")

shp_1890 <- read_sf("/Users/jheppler/Dropbox/research-data/nhgis-shapefiles/epsg4326/", "US_county_1890")
shp_md_1890 <- shp_1890[shp_1890$STATENAM %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Missouri", "Minnesota", "Michigan", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin"),] %>% 
  select(-STATE, -COUNTY)
merge_1890 <- merge(shp_md_1890, c_1890, by = "GISJOIN")
merge_1890 <- st_transform(merge_1890, st_crs(wgs.84))
write_sf(obj=merge_1890, dsn="~/Desktop", layer="merge_1890", driver="ESRI Shapefile")

shp_1900 <- read_sf("/Users/jheppler/Dropbox/research-data/nhgis-shapefiles/epsg4326/", "US_county_1900_conflated")
shp_md_1900 <- shp_1900[shp_1900$STATENAM %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Missouri", "Minnesota", "Michigan", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin"),] %>% 
  select(-STATE, -COUNTY)
merge_1900 <- merge(shp_md_1900, c_1900, by = "GISJOIN")
merge_1900 <- st_transform(merge_1900, st_crs(wgs.84))
write_sf(obj=merge_1900, dsn="~/Desktop", layer="merge_1900", driver="ESRI Shapefile")

shp_1910 <- read_sf("/Users/jheppler/Dropbox/research-data/nhgis-shapefiles/epsg4326/", "US_county_1910")
shp_md_1910 <- shp_1910[shp_1910$STATENAM %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Missouri", "Minnesota", "Michigan", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin"),] %>% 
  select(-STATE, -COUNTY)
merge_1910 <- merge(shp_md_1910, c_1910, by = "GISJOIN")
merge_1910 <- st_transform(merge_1910, st_crs(wgs.84))
write_sf(obj=merge_1910, dsn="~/Desktop", layer="merge_1910", driver="ESRI Shapefile")

shp_1920 <- read_sf("/Users/jheppler/Dropbox/research-data/nhgis-shapefiles/epsg4326/", "US_county_1920")
shp_md_1920 <- shp_1920[shp_1920$STATENAM %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Missouri", "Minnesota", "Michigan", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin"),] %>% 
  select(-STATE, -COUNTY)
merge_1920 <- merge(shp_md_1920, c_1920, by = "GISJOIN")
merge_1920 <- st_transform(merge_1920, st_crs(wgs.84))
write_sf(obj=merge_1920, dsn="~/Desktop", layer="merge_1920", driver="ESRI Shapefile")

shp_1930 <- read_sf("/Users/jheppler/Dropbox/research-data/nhgis-shapefiles/epsg4326/", "US_county_1930")
shp_md_1930 <- shp_1930[shp_1930$STATENAM %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Missouri", "Minnesota", "Michigan", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin"),] %>% 
  select(-STATE, -COUNTY)
merge_1930 <- merge(shp_md_1930, c_1930, by = "GISJOIN")
merge_1930 <- st_transform(merge_1930, st_crs(wgs.84))
write_sf(obj=merge_1930, dsn="~/Desktop", layer="merge_1930", driver="ESRI Shapefile")

shp_1940 <- read_sf("/Users/jheppler/Dropbox/research-data/nhgis-shapefiles/epsg4326/", "US_county_1940")
shp_md_1940 <- shp_1940[shp_1940$STATENAM %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Missouri", "Minnesota", "Michigan", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin"),] %>% 
  select(-STATE, -COUNTY)
merge_1940 <- merge(shp_md_1940, c_1940, by = "GISJOIN")
merge_1940 <- st_transform(merge_1940, st_crs(wgs.84))
write_sf(obj=merge_1940, dsn="~/Desktop", layer="merge_1940", driver="ESRI Shapefile")

shp_1950 <- read_sf("/Users/jheppler/Dropbox/research-data/nhgis-shapefiles/epsg4326/", "US_county_1950")
shp_md_1950 <- shp_1950[shp_1950$STATENAM %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Missouri", "Minnesota", "Michigan", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin"),] %>% 
  select(-STATE, -COUNTY)
merge_1950 <- merge(shp_md_1950, c_1950, by = "GISJOIN")
merge_1950 <- st_transform(merge_1950, st_crs(wgs.84))
write_sf(obj=merge_1950, dsn="~/Desktop", layer="merge_1950", driver="ESRI Shapefile")

shp_1960 <- read_sf("/Users/jheppler/Dropbox/research-data/nhgis-shapefiles/epsg4326/", "US_county_1960")
shp_md_1960 <- shp_1960[shp_1960$STATENAM %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Missouri", "Minnesota", "Michigan", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin"),] %>% 
  select(-STATE, -COUNTY)
merge_1960 <- merge(shp_md_1960, c_1960, by = "GISJOIN")
merge_1960 <- st_transform(merge_1960, st_crs(wgs.84))
write_sf(obj=merge_1960, dsn="~/Desktop", layer="merge_1960", driver="ESRI Shapefile")

shp_1970 <- read_sf("/Users/jheppler/Dropbox/research-data/nhgis-shapefiles/epsg4326/", "US_county_1970")
shp_md_1970 <- shp_1970[shp_1970$STATENAM %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Missouri", "Minnesota", "Michigan", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin"),] %>% 
  select(-STATE, -COUNTY)
merge_1970 <- merge(shp_md_1970, c_1970, by = "GISJOIN")
merge_1970 <- st_transform(merge_1970, st_crs(wgs.84))
write_sf(obj=merge_1970, dsn="~/Desktop", layer="merge_1970", driver="ESRI Shapefile")

shp_1980 <- read_sf("/Users/jheppler/Dropbox/research-data/nhgis-shapefiles/epsg4326/", "US_county_1980")
shp_md_1980 <- shp_1980[shp_1980$STATENAM %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Missouri", "Minnesota", "Michigan", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin"),] %>% 
  select(-STATE, -COUNTY)
merge_1980 <- merge(shp_md_1980, c_1980, by = "GISJOIN")
merge_1980 <- st_transform(merge_1980, st_crs(wgs.84))
write_sf(obj=merge_1980, dsn="~/Desktop", layer="merge_1980", driver="ESRI Shapefile")

shp_1990 <- read_sf("/Users/jheppler/Dropbox/research-data/nhgis-shapefiles/epsg4326/", "US_county_1990")
shp_md_1990 <- shp_1990[shp_1990$STATENAM %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Missouri", "Minnesota", "Michigan", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin"),] %>% 
  select(-STATE, -COUNTY)
merge_1990 <- merge(shp_md_1990, c_1990, by = "GISJOIN")
merge_1990 <- st_transform(merge_1990, st_crs(wgs.84))
write_sf(obj=merge_1990, dsn="~/Desktop", layer="merge_1990", driver="ESRI Shapefile")

shp_2000 <- read_sf("/Users/jheppler/Dropbox/research-data/nhgis-shapefiles/epsg4326/", "US_county_2000")
shp_md_2000 <- shp_2000[shp_2000$STATENAM %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Missouri", "Minnesota", "Michigan", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin"),] %>% 
  select(-STATE, -COUNTY)
merge_2000 <- merge(shp_md_2000, c_2000, by = "GISJOIN")
merge_2000 <- st_transform(merge_2000, st_crs(wgs.84))
write_sf(obj=merge_2000, dsn="~/Desktop", layer="merge_2000", driver="ESRI Shapefile")

shp_2010 <- read_sf("/Users/jheppler/Dropbox/research-data/nhgis-shapefiles/epsg4326/", "US_county_2010")
shp_md_2010 <- shp_2010[shp_2010$NAME10 %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Missouri", "Minnesota", "Michigan", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin"),]
merge_2010 <- merge(shp_md_2010, c_2010, by = "GISJOIN")
merge_2010 <- st_transform(merge_2010, st_crs(wgs.84))
write_sf(obj=merge_2010, dsn="~/Desktop", layer="merge_2010", driver="ESRI Shapefile")

# Clean up
rm(list=ls(pattern="shp_"))
rm(list=ls(pattern="c_"))