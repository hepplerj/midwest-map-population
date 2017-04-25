#!/usr/bin/Rscript

## Load census data and aggregate population data. Create a single
## long table to be used in visualization `midwest-census.csv`.

library(dplyr)
library(readr)
library(stringr)
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
    totalDensity = round(totalPopulation / sqm_sqmi(area), 2)
  ) 
}

# 1790
c_1790 <- csv$nhgis0013_ds1_1790_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1790.simplified, by = "GISJOIN") %>%
  select(GISJOIN,
         totalPopulation = A00AA1790,
         freeAfAmPopulation = AAQ001,
         slavePopulation = AAQ002,
         totalWhitePopulation = AAQ003,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  mutate(totalAfAmPopulation = freeAfAmPopulation + slavePopulation,
         totalAfAmPercentage = round(100 * totalAfAmPopulation / totalPopulation, 2),
         totalWhitePercentage = round(100 * totalWhitePopulation / totalPopulation, 2)) %>% 
  derived_data()

# 1800
c_1800 <- csv$nhgis0013_ds2_1800_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1800.simplified, by = "GISJOIN") %>%
  select(GISJOIN,
         totalPopulation = A00AA1800,
         freeAfAmPopulation = AAY001,
         slavePopulation = AAY002,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  mutate(totalAfAmPopulation = freeAfAmPopulation + slavePopulation,
         totalAfAmPercentage = round(100 * totalAfAmPopulation / totalPopulation, 2),
         totalWhitePopulation = totalPopulation - totalAfAmPopulation,
         totalWhitePercentage = round(100 * totalWhitePopulation / totalPopulation, 2)) %>% 
  derived_data()

# 1810
c_1810 <- csv$nhgis0013_ds3_1810_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1810.simplified, by = "GISJOIN") %>%
  select(GISJOIN,
         totalPopulation = A00AA1810,
         freeAfAmPopulation = AA7001,
         slavePopulation = AA7002,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  mutate(totalAfAmPopulation = freeAfAmPopulation + slavePopulation,
         totalAfAmPercentage = round(100 * totalAfAmPopulation / totalPopulation, 2),
         totalWhitePopulation = totalPopulation - totalAfAmPopulation,
         totalWhitePercentage = round(100 * totalWhitePopulation / totalPopulation, 2)) %>% 
  derived_data()

# 1820
c_1820 <- csv$nhgis0013_ds4_1820_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1820.simplified, by = "GISJOIN") %>%
  mutate(freeAfAmPopulation = ABB005 + ABB006,
         slavePopulation = ABB003 + ABB004,
         totalAfAmPopulation = freeAfAmPopulation + slavePopulation,
         totalAfAmPercentage = round(100 * totalAfAmPopulation / A00AA1820, 2),
         totalWhitePopulation = A00AA1820 - totalAfAmPopulation,
         totalWhitePercentage = round(100 * totalWhitePopulation / A00AA1820, 2)) %>%
  select(GISJOIN,
         totalPopulation = A00AA1820,
         freeAfAmPopulation,
         slavePopulation,
         totalAfAmPopulation,
         totalAfAmPercentage,
         totalWhitePopulation,
         totalWhitePercentage,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  derived_data()

# 1830
c_1830 <- csv$nhgis0013_ds5_1830_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1830.simplified, by = "GISJOIN") %>%
  mutate(freeAfAmPopulation = ABO005 + ABO006,
         slavePopulation = ABO003 + ABO004,
         totalAfAmPopulation = freeAfAmPopulation + slavePopulation,
         totalAfAmPercentage = round(100 * totalAfAmPopulation / A00AA1830, 2),
         totalWhitePopulation = A00AA1830 - totalAfAmPopulation,
         totalWhitePercentage = round(100 * totalWhitePopulation / A00AA1830, 2)) %>%
  select(GISJOIN,
         totalPopulation = A00AA1830,
         freeAfAmPopulation,
         slavePopulation,
         totalAfAmPopulation,
         totalAfAmPercentage,
         totalWhitePopulation,
         totalWhitePercentage,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  derived_data()

# 1840
c_1840 <- csv$nhgis0013_ds7_1840_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1840.simplified, by = "GISJOIN") %>%
  mutate(totalAfAmPopulation = ACS002 + ACS003,
         totalAfAmPercentage = round(100 * totalAfAmPopulation / A00AA1840, 2),
         totalWhitePercentage = round(100 * ACS001 / A00AA1840, 2)) %>%
  select(GISJOIN,
         totalPopulation = A00AA1840,
         freeAfAmPopulation = ACS002,
         slavePopulation = ACS003,
         totalAfAmPopulation,
         totalAfAmPercentage,
         totalWhitePopulation = ACS001,
         totalWhitePercentage,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  derived_data()

# 1850
c_1850 <- csv$nhgis0013_ds10_1850_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1850.simplified, by = "GISJOIN") %>%
  mutate(totalAfAmPopulation = AE6002 + AE6003,
         totalAfAmPercentage = round(100 * totalAfAmPopulation / A00AA1850, 2),
         totalWhitePercentage = round(100 * AE6001 / A00AA1850, 2)) %>%
  select(GISJOIN,
         totalPopulation = A00AA1850,
         freeAfAmPopulation = AE6002,
         slavePopulation = AE6003,
         totalAfAmPopulation,
         totalAfAmPercentage,
         totalWhitePopulation = AE6001,
         totalWhitePercentage,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  derived_data()

# 1860
c_1860 <- csv$nhgis0016_ds14_1860_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1860.simplified, by = "GISJOIN") %>%
  mutate(freeAfAmPopulation = AH2003 + AH2004,
         slavePopulation = AH2005 + AH2006,
         totalAfAmPopulation = AH2003 + AH2004 + AH2005 + AH2006,
         totalAfAmPercentage = round(100 * totalAfAmPopulation / A00AA1860, 2),
         totalWhitePopulation = AH2001 + AH2002,
         totalWhitePercentage = round(100 * totalWhitePopulation / A00AA1860, 2)) %>%
  select(GISJOIN,
         totalPopulation = A00AA1860,
         freeAfAmPopulation,
         slavePopulation,
         totalAfAmPopulation,
         totalAfAmPercentage,
         totalWhitePopulation,
         totalWhitePercentage,
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
         totalPopulation = A00AA1870,
         totalAfAmPopulation = AK3002,
         totalAsianPopulation = AK3003,
         totalIndianPopulation = AK3004,
         totalWhitePopulation = AK3001,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  mutate(totalAfAmPercentage = round(100 * totalAfAmPopulation / totalPopulation, 2),
         totalAsianPercentage = round(100 * totalAsianPopulation / totalPopulation, 2),
         totalIndianPercentage = round(100 * totalIndianPopulation / totalPopulation, 2),
         totalWhitePercentage = round(100 * totalWhitePopulation / totalPopulation, 2)
  ) %>% 
  derived_data()

# 1880
c_1880 <- csv$nhgis0031_ds23_1880_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1880.simplified, by = "GISJOIN") %>%
  select(GISJOIN,
         totalPopulation = A00AA1880,
         totalAfAmPopulation = APP002,
         totalAsianPopulation = APP003,
         totalIndianPopulation = APP004,
         totalWhitePopulation = APP001,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  mutate(totalAfAmPercentage = round(100 * totalAfAmPopulation / totalPopulation, 2),
         totalAsianPercentage = round(100 * totalAsianPopulation / totalPopulation, 2),
         totalIndianPercentage = round(100 * totalIndianPopulation / totalPopulation, 2),
         totalWhitePercentage = round(100 * totalWhitePopulation / totalPopulation, 2)
  ) %>% 
  derived_data()

# 1890
c_1890 <- csv$nhgis0032_ds27_1890_county %>%
  left_join(csv$nhgis0033_ds27_1890_county, by = "GISJOIN") %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1890.simplified, by = "GISJOIN") %>%
  select(GISJOIN,
         totalPopulation = A00AA1890,
         totalAfAmPopulation = AVF001,
         totalAsianPopulation = AVF004,
         totalIndianPopulation = AVF010,
         totalWhitePopulation = AV2001,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR.x) %>%
  mutate(totalAfAmPercentage = round(100 * totalAfAmPopulation / totalPopulation, 2),
         totalAsianPercentage = round(100 * totalAsianPopulation / totalPopulation, 2),
         totalIndianPercentage = round(100 * totalIndianPopulation / totalPopulation, 2),
         totalWhitePercentage = round(100 * totalWhitePopulation / totalPopulation, 2)
  ) %>% 
  derived_data()

# 1900
c_1900 <- csv$nhgis0033_ds31_1900_county %>%
  left_join(csv$nhgis0036_ds31_1900_county, by = "GISJOIN") %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1900.simplified, by = "GISJOIN") %>%
  mutate(totalAfAmPopulation = AZ3003 + AZ3004) %>%
  select(GISJOIN,
         totalPopulation = A00AA1900,
         totalAfAmPopulation,
         totalWhitePopulation = AZ2001,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR.x) %>%
  mutate(totalAfAmPercentage = round(100 * totalAfAmPopulation / totalPopulation, 2),
         totalWhitePercentage = round(100 * totalWhitePopulation / totalPopulation, 2)
  ) %>% 
  derived_data()

# 1910
c_1910 <- csv$nhgis0030_ds37_1910_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1910.simplified, by = "GISJOIN") %>%
  select(GISJOIN,
         totalPopulation = A00AA1910,
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
         totalPopulation = A00AA1920,
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
         totalPopulation = A00AA1930,
         totalAfAmPopulation = BC0001,
         totalWhitePopulation = BCZ001,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  mutate(totalAfAmPercentage = round(100 * totalAfAmPopulation / totalPopulation, 2),
         totalWhitePercentage = round(100 * totalWhitePopulation / totalPopulation, 2)
  ) %>% 
  derived_data()

# 1940
c_1940 <- csv$nhgis0037_ds78_1940_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1940.simplified, by = "GISJOIN") %>%
  select(GISJOIN,
         totalPopulation = A00AA1940,
         totalAfAmPopulation = BYA003,
         totalWhitePopulation = BWS001,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  mutate(totalAfAmPercentage = round(100 * totalAfAmPopulation / totalPopulation, 2),
         totalWhitePercentage = round(100 * totalWhitePopulation / totalPopulation, 2)
  ) %>% 
  derived_data()

# 1950
c_1950 <- csv$nhgis0038_ds83_1950_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1950.simplified, by = "GISJOIN") %>%
  select(GISJOIN,
         totalPopulation = A00AA1950,
         totalAfAmPopulation = B1T002,
         totalWhitePopulation = B1T001,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  mutate(totalAfAmPercentage = round(100 * totalAfAmPopulation / totalPopulation, 2),
         totalWhitePercentage = round(100 * totalWhitePopulation / totalPopulation, 2)
  ) %>% 
  derived_data()

# 1960
c_1960 <- csv$nhgis0038_ds89_1960_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1960.simplified, by = "GISJOIN") %>%
  select(GISJOIN,
         totalPopulation = A00AA1960,
         totalAfAmPopulation = B48002,
         totalWhitePopulation = B48001,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  mutate(totalAfAmPercentage = round(100 * totalAfAmPopulation / totalPopulation, 2),
         totalWhitePercentage = round(100 * totalWhitePopulation / totalPopulation, 2)
  ) %>% 
  derived_data()

# 1970
c_1970 <- csv$nhgis0038_ds94_1970_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1970.simplified, by = "GISJOIN") %>%
  select(GISJOIN,
         totalPopulation = A00AA1970,
         totalAfAmPopulation = CBW002,
         totalWhitePopulation = CBW001,
         totalIndianPopulation = CBW003,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  mutate(totalAfAmPercentage = round(100 * totalAfAmPopulation / totalPopulation, 2),
         totalIndianPercentage = round(100 * totalIndianPopulation / totalPopulation, 2),
         totalWhitePercentage = round(100 * totalWhitePopulation / totalPopulation, 2)
  ) %>% 
  derived_data()

# 1980
c_1980 <- csv$nhgis0038_ds104_1980_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1980.simplified, by = "GISJOIN") %>%
  mutate(
    totalIndianPopulation = C9D003 + C9D004, C9D005,
    totalAsianPopulation = C9D006 + C9D007 + C9D008 + C9D009 + C9D010 + C9D011 + C9D012 + C9D013 + C9D014
  ) %>%
  select(GISJOIN,
         totalPopulation = A00AA1980,
         totalAfAmPopulation = C9D002,
         totalWhitePopulation = C9D001,
         totalIndianPopulation,
         totalAsianPopulation,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  mutate(totalAfAmPercentage = round(100 * totalAfAmPopulation / totalPopulation, 2),
         totalAsianPercentage = round(100 * totalAsianPopulation / totalPopulation, 2),
         totalIndianPercentage = round(100 * totalIndianPopulation / totalPopulation, 2),
         totalWhitePercentage = round(100 * totalWhitePopulation / totalPopulation, 2)
  ) %>% 
  derived_data()

# 1990
c_1990 <- csv$nhgis0038_ds120_1990_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1990.simplified, by = "GISJOIN") %>%
  select(GISJOIN,
         totalPopulation = A00AA1990,
         totalAfAmPopulation = EUY002,
         totalWhitePopulation = EUY001,
         totalIndianPopulation = EUY003,
         totalAsianPopulation = EUY004,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  mutate(totalAfAmPercentage = round(100 * totalAfAmPopulation / totalPopulation, 2),
         totalAsianPercentage = round(100 * totalAsianPopulation / totalPopulation, 2),
         totalIndianPercentage = round(100 * totalIndianPopulation / totalPopulation, 2),
         totalWhitePercentage = round(100 * totalWhitePopulation / totalPopulation, 2)
  ) %>% 
  derived_data()

# 2000
c_2000 <- csv$nhgis0038_ds146_2000_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_2000.simplified, by = "GISJOIN") %>%
  select(GISJOIN,
         totalPopulation = A00AA2000,
         totalAfAmPopulation = FMR002,
         totalWhitePopulation = FMR001,
         totalIndianPopulation = FMR003,
         totalAsianPopulation = FMR004,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  mutate(totalAfAmPercentage = round(100 * totalAfAmPopulation / totalPopulation, 2),
         totalAsianPercentage = round(100 * totalAsianPopulation / totalPopulation, 2),
         totalIndianPercentage = round(100 * totalIndianPopulation / totalPopulation, 2),
         totalWhitePercentage = round(100 * totalWhitePopulation / totalPopulation, 2)
  ) %>% 
  derived_data()

# 2010
c_2010 <- csv$nhgis0038_ds176_20105_2010_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_2010.simplified, by = "GISJOIN") %>%
  mutate(year = 2010) %>% 
  select(GISJOIN,
         totalPopulation = A00AA2010,
         totalAfAmPopulation = JMBE003,
         totalWhitePopulation = JMBE002,
         totalIndianPopulation = JMBE004,
         totalAsianPopulation = JMBE005,
         state = STATE.x,
         county = COUNTY.x,
         year) %>% 
  mutate(totalAfAmPercentage = round(100 * totalAfAmPopulation / totalPopulation, 2),
         totalAsianPercentage = round(100 * totalAsianPopulation / totalPopulation, 2),
         totalIndianPercentage = round(100 * totalIndianPopulation / totalPopulation, 2),
         totalWhitePercentage = round(100 * totalWhitePopulation / totalPopulation, 2)
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

# CSV and RDS
write_rds(midwestCensus, "midwest-census.rds")
write_csv(midwestCensus, "midwest-census.csv", na = "")

###--------------------------------------------------
# Shapefile prep (read, convert projection, create DF)
###--------------------------------------------------

library(rgdal)
wgs.84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Need to download the 1970-1850 shapefiles from NHGIS

shp_1860 <- readOGR("/Users/jheppler/Dropbox/research-data/nhgis-shapefiles/epsg4326/", "US_county_1860_conflated")
shp_md_1860 <- shp_1860[shp_1860$STATENAM %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Missouri", "Minnesota", "Michigan", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin"),]
merge_1860 <- merge(shp_md_1860, c_1860, by = "GISJOIN")
merge_1860 <- spTransform(merge_1860, CRS(wgs.84))
writeOGR(obj=merge_1860, dsn="data", layer="merge_1860", driver="ESRI Shapefile")

shp_1870 <- readOGR("/Users/jheppler/Dropbox/research-data/nhgis-shapefiles/epsg4326/", "US_county_1870")
shp_md_1870 <- shp_1870[shp_1870$STATENAM %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Missouri", "Minnesota", "Michigan", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin"),]
merge_1870 <- merge(shp_md_1870, c_1870, by = "GISJOIN")
merge_1870 <- spTransform(merge_1870, CRS(wgs.84))
writeOGR(obj=merge_1870, dsn="data", layer="merge_1870", driver="ESRI Shapefile")

shp_1880 <- readOGR("/Users/jheppler/Dropbox/research-data/nhgis-shapefiles/epsg4326/", "US_county_1880")
shp_md_1880 <- shp_1880[shp_1880$STATENAM %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Missouri", "Minnesota", "Michigan", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin"),]
merge_1880 <- merge(shp_md_1880, c_1880, by = "GISJOIN")
merge_1880 <- spTransform(merge_1880, CRS(wgs.84))
writeOGR(obj=merge_1880, dsn="data", layer="merge_1880", driver="ESRI Shapefile")

shp_1890 <- readOGR("/Users/jheppler/Dropbox/research-data/nhgis-shapefiles/epsg4326/", "US_county_1890")
shp_md_1890 <- shp_1890[shp_1890$STATENAM %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Missouri", "Minnesota", "Michigan", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin"),]
merge_1890 <- merge(shp_md_1890, c_1890, by = "GISJOIN")
merge_1890 <- spTransform(merge_1890, CRS(wgs.84))
writeOGR(obj=merge_1890, dsn="data", layer="merge_1890", driver="ESRI Shapefile")

shp_1900 <- readOGR("/Users/jheppler/Dropbox/research-data/nhgis-shapefiles/epsg4326/", "US_county_1900_conflated")
shp_md_1900 <- shp_1900[shp_1900$STATENAM %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Missouri", "Minnesota", "Michigan", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin"),]
merge_1900 <- merge(shp_md_1900, c_1900, by = "GISJOIN")
merge_1900 <- spTransform(merge_1900, CRS(wgs.84))
writeOGR(obj=merge_1900, dsn="data", layer="merge_1900", driver="ESRI Shapefile")

shp_1910 <- readOGR("/Users/jheppler/Dropbox/research-data/nhgis-shapefiles/epsg4326/", "US_county_1910")
shp_md_1910 <- shp_1910[shp_1910$STATENAM %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Missouri", "Minnesota", "Michigan", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin"),]
merge_1910 <- merge(shp_md_1910, c_1910, by = "GISJOIN")
merge_1910 <- spTransform(merge_1910, CRS(wgs.84))
writeOGR(obj=merge_1910, dsn="data", layer="merge_1910", driver="ESRI Shapefile")

shp_1920 <- readOGR("/Users/jheppler/Dropbox/research-data/nhgis-shapefiles/epsg4326/", "US_county_1920")
shp_md_1920 <- shp_1920[shp_1920$STATENAM %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Missouri", "Minnesota", "Michigan", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin"),]
merge_1920 <- merge(shp_md_1920, c_1920, by = "GISJOIN")
merge_1920 <- spTransform(merge_1920, CRS(wgs.84))
writeOGR(obj=merge_1920, dsn="data", layer="merge_1920", driver="ESRI Shapefile")

shp_1930 <- readOGR("/Users/jheppler/Dropbox/research-data/nhgis-shapefiles/epsg4326/", "US_county_1930")
shp_md_1930 <- shp_1930[shp_1930$STATENAM %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Missouri", "Minnesota", "Michigan", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin"),]
merge_1930 <- merge(shp_md_1930, c_1930, by = "GISJOIN")
merge_1930 <- spTransform(merge_1930, CRS(wgs.84))
writeOGR(obj=merge_1930, dsn="data", layer="merge_1930", driver="ESRI Shapefile")

shp_1940 <- readOGR("/Users/jheppler/Dropbox/research-data/nhgis-shapefiles/epsg4326/", "US_county_1940")
shp_md_1940 <- shp_1940[shp_1940$STATENAM %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Missouri", "Minnesota", "Michigan", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin"),]
merge_1940 <- merge(shp_md_1940, c_1940, by = "GISJOIN")
merge_1940 <- spTransform(merge_1940, CRS(wgs.84))
writeOGR(obj=merge_1940, dsn="data", layer="merge_1940", driver="ESRI Shapefile")

shp_1950 <- readOGR("/Users/jheppler/Dropbox/research-data/nhgis-shapefiles/epsg4326/", "US_county_1950")
shp_md_1950 <- shp_1950[shp_1950$STATENAM %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Missouri", "Minnesota", "Michigan", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin"),]
merge_1950 <- merge(shp_md_1950, c_1950, by = "GISJOIN")
merge_1950 <- spTransform(merge_1950, CRS(wgs.84))
writeOGR(obj=merge_1950, dsn="data", layer="merge_1950", driver="ESRI Shapefile")

shp_1960 <- readOGR("/Users/jheppler/Dropbox/research-data/nhgis-shapefiles/epsg4326/", "US_county_1960")
shp_md_1960 <- shp_1960[shp_1960$STATENAM %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Missouri", "Minnesota", "Michigan", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin"),]
merge_1960 <- merge(shp_md_1960, c_1960, by = "GISJOIN")
merge_1960 <- spTransform(merge_1960, CRS(wgs.84))
writeOGR(obj=merge_1960, dsn="data", layer="merge_1960", driver="ESRI Shapefile")

shp_1970 <- readOGR("/Users/jheppler/Dropbox/research-data/nhgis-shapefiles/epsg4326/", "US_county_1970")
shp_md_1970 <- shp_1970[shp_1970$STATENAM %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Missouri", "Minnesota", "Michigan", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin"),]
merge_1970 <- merge(shp_md_1970, c_1970, by = "GISJOIN")
merge_1970 <- spTransform(merge_1970, CRS(wgs.84))
writeOGR(obj=merge_1970, dsn="data", layer="merge_1970", driver="ESRI Shapefile")

shp_1980 <- readOGR("/Users/jheppler/Dropbox/research-data/nhgis-shapefiles/epsg4326/", "US_county_1980")
shp_md_1980 <- shp_1980[shp_1980$STATENAM %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Missouri", "Minnesota", "Michigan", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin"),]
merge_1980 <- merge(shp_md_1980, c_1980, by = "GISJOIN")
merge_1980 <- spTransform(merge_1980, CRS(wgs.84))
writeOGR(obj=merge_1980, dsn="data", layer="merge_1980", driver="ESRI Shapefile")

shp_1990 <- readOGR("/Users/jheppler/Dropbox/research-data/nhgis-shapefiles/epsg4326/", "US_county_1990")
shp_md_1990 <- shp_1990[shp_1990$STATENAM %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Missouri", "Minnesota", "Michigan", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin"),]
merge_1990 <- merge(shp_md_1990, c_1990, by = "GISJOIN")
merge_1990 <- spTransform(merge_1990, CRS(wgs.84))
writeOGR(obj=merge_1990, dsn="data", layer="merge_1990", driver="ESRI Shapefile")

shp_2000 <- readOGR("/Users/jheppler/Dropbox/research-data/nhgis-shapefiles/epsg4326/", "US_county_2000")
shp_md_2000 <- shp_2000[shp_2000$STATENAM %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Missouri", "Minnesota", "Michigan", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin"),]
merge_2000 <- merge(shp_md_2000, c_2000, by = "GISJOIN")
merge_2000 <- spTransform(merge_2000, CRS(wgs.84))
writeOGR(obj=merge_2000, dsn="data", layer="merge_2000", driver="ESRI Shapefile")

shp_2010 <- readOGR("/Users/jheppler/Dropbox/research-data/nhgis-shapefiles/epsg4326/", "US_county_2010")
shp_md_2010 <- shp_2010[shp_2010$NAME10 %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Missouri", "Minnesota", "Michigan", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin"),]
merge_2010 <- merge(shp_md_2010, c_2010, by = "GISJOIN")
merge_2010 <- spTransform(merge_2010, CRS(wgs.84))
writeOGR(obj=merge_2010, dsn="data", layer="merge_2010", driver="ESRI Shapefile")

save(merge_1860, merge_1870, merge_1880, merge_1890, merge_1910, merge_1900, merge_1920, merge_1930, merge_1940, merge_1950, merge_1960, merge_1970, merge_1980, merge_1990, merge_2000, merge_2010, file = "midwest-counties.rds")


# DELETE
library(leaflet)
pal <- colorQuantile("Greens", NULL, n = 9)
# breaks:
# - under 2
# - 2.0-4.9
# - 5.0-9.9
# - 10.0-24.9
# - 25-49.9
# - 50-74.9
# -75+

popup <- paste0("<strong>", merge_1980$county, " County", ", ", merge_1980$state, "</strong>", 
                      "<br><strong>Population: </strong>", 
                      merge_1980$totalPopulation,
                      "<br><strong>White population: </strong>",
                      merge_1980$totalWhitePopulation, " (", merge_1980$totalWhitePercentage, "%)",
                      "<br><strong>African American population: </strong>",
                      merge_1980$totalAfAmPopulation , " (" , merge_1980$totalAfAmPercentage , "%)",
                      "<br><strong>Asian population: </strong>",
                      merge_1980$totalAsianPopulation, " (", merge_1980$totalAsianPercentage, "%)",
                      "<br><strong>Indian population: </strong>",
                      merge_1980$totalIndianPopulation, " (", merge_1980$totalIndianPercentage, "%)")

leaflet(data = merge_1980) %>%
  addProviderTiles('CartoDB.Positron') %>%
  addPolygons(fillColor = ~pal(merge_1980$totalAsianPercentage),
              fillOpacity = 0.8,
              color = "#BDBDC3",
              weight = 1,
              popup = popup) %>%
  addLegend(pal = pal,
            values = merge_1980$totalAsianPercentage,
            position = "bottomright",
            title = "Native population")

