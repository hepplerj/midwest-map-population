#!/usr/bin/Rscript

# Load census data and aggregate population data. Create a single
# long table to be used in visualization.

library(dplyr)
library(readr)
library(stringr)
library(foreign)
options(stringsAsFactors = FALSE)

sqm_sqmi <- function(sq_meters) {
  sq_meters / 2589988.110336;
}

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
  ) %>%
    select(-area)
}

# 1790
c_1790 <- csv$nhgis0013_ds1_1790_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1790.simplified, by = "GISJOIN") %>%
  select(GISJOIN,
         totalPopulation = A00AA1790,
         freeAfAmPopulation = AAQ001,
         slavePopulation = AAQ002,
         totalAfAmPopulation = AAQ001 + AAQ002,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  derived_data()

# 1800
c_1800 <- csv$nhgis0013_ds2_1800_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1800.simplified, by = "GISJOIN") %>%
  select(GISJOIN,
         totalPopulation = A00AA1800,
         freeAfAmPopulation = AAY001,
         slavePopulation = AAY002,
         totalAfAmPopulation = AAY001 + AAY002,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  derived_data()

# 1810
c_1810 <- csv$nhgis0013_ds3_1810_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1810.simplified, by = "GISJOIN") %>%
  select(GISJOIN,
         totalPopulation = A00AA1810,
         freeAfAmPopulation = AA7001,
         slavePopulation = AA7002,
         totalAfAmPopulation = AA7001 + AA7002,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  derived_data()

# 1820
c_1820 <- csv$nhgis0013_ds4_1820_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1820.simplified, by = "GISJOIN") %>%
  mutate(freeAfAmPopulation = ABB005 + ABB006,
         slavePopulation = ABB003 + ABB004,
         totalAfAmPopulation = freeAfAmPopulation + slavePopulation) %>%
  select(GISJOIN,
         totalPopulation = A00AA1820,
         freeAfAmPopulation,
         slavePopulation,
         totalAfAmPopulation,
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
         totalAfAmPopulation = freeAfAmPopulation + slavePopulation) %>%
  select(GISJOIN,
         totalPopulation = A00AA1830,
         freeAfAmPopulation,
         slavePopulation,
         totalAfAmPopulation,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  derived_data()

# 1840
c_1840 <- csv$nhgis0013_ds7_1840_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1840.simplified, by = "GISJOIN") %>%
  mutate(totalAfAmPopulation = ACS002 + ACS003) %>%
  select(GISJOIN,
         totalPopulation = A00AA1840,
         freeAfAmPopulation = ACS002,
         slavePopulation = ACS003,
         totalAfAmPopulation,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  derived_data()

# 1850
c_1850 <- csv$nhgis0013_ds10_1850_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1850.simplified, by = "GISJOIN") %>%
  mutate(totalAfAmPopulation = AE6002 + AE6003) %>%
  select(GISJOIN,
         totalPopulation = A00AA1850,
         freeAfAmPopulation = AE6002,
         slavePopulation = AE6003,
         totalAfAmPopulation,
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
         totalAfAmPopulation = AH2003 + AH2004 + AH2005 + AH2006) %>%
  select(GISJOIN,
         totalPopulation = A00AA1860,
         freeAfAmPopulation,
         slavePopulation,
         totalAfAmPopulation,
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
         totalAsiaPopulation = AK3003,
         totalIndianPopulation = AK3004,
         totalWhitePopulation = AK3001,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  derived_data()

# 1880
c_1880 <- csv$nhgis0031_ds23_1880_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1880.simplified, by = "GISJOIN") %>%
  select(GISJOIN,
         totalPopulation = A00AA1880,
         totalAfAmPopulation = APP002,
         totalAsiaPopulation = APP003,
         totalIndianPopulation = APP004,
         totalWhitePopulation = APP001,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
  derived_data()

# 1890
c_1890 <- csv$nhgis0032_ds27_1890_county %>%
  left_join(csv$nhgis0033_ds27_1890_county, by = "GISJOIN") %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1890.simplified, by = "GISJOIN") %>%
  select(GISJOIN,
         totalPopulation = A00AA1890,
         totalAfAmPopulation = AVF001,
         totalAsiaPopulation = AVF004,
         totalIndianPopulation = AVF010,
         totalWhitePopulation = AV2001,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR.x) %>%
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
  derived_data()

# 1950
c_1950 <- csv$nhgis0038_ds83_1950_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1950.simplified, by = "GISJOIN") %>%
  select(GISJOIN,
         totalPopulation = A00AA1950,
         totalAfAmPopulation = B1T002,
         totalWhitePoplulation = B1T001,
         state = STATE.x,
         county = COUNTY.x,
         area = SHAPE_AREA,
         year = YEAR) %>%
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
  derived_data()

# 2010
c_2010 <- csv$nhgis0038_ds176_20105_2010_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_2010.simplified, by = "GISJOIN") %>%
  select(GISJOIN,
         totalPopulation = A00AA2010,
         totalAfAmPopulation = JMBE003,
         totalWhitePopulation = JMBE002,
         totalIndianPopulation = JMBE004,
         totalAsianPopulation = JMBE005,
         state = STATE.x,
         county = COUNTY.x,
         year = YEAR)

# Join all the data together and output
## Using the rbind.fill from `plyr` since columns from the dataframes are 
## different sizes.
library(plyr)
all <- rbind.fill(c_1790, c_1800, c_1810, c_1820, c_1830, c_1840, c_1850, c_1860, c_1870, c_1880, c_1890, c_1900, c_1910, c_1920, c_1930, c_1940, c_1950, c_1960, c_1970, c_1980, c_1990, c_2000, c_2010)

# Select only midwestern cities
midwestStates <- c("Illinois", "Indiana", "Iowa", "Kansas", "Missouri", "Minnesota", "Missouri", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin")
midwestCensus <- all %>%
  filter(state %in% midwestStates)

write_csv(midwestCensus, "midwest-census.csv", na = "")
