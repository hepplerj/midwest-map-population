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

temp_census <- read_csv("data/nhgis0014_ts_county.csv")

# Read all DBF files into a list
fn <- Sys.glob("/Users/jasonheppler/Dropbox/research-data/nhgis-shapefiles/epsg4326/*.dbf")
dbf <- lapply(fn, read.dbf, as.is = TRUE)
names(dbf) <-  fn %>% 
  str_replace("/Users/jasonheppler/Dropbox/research-data/nhgis-shapefiles/epsg4326/", "") %>%
  str_replace(".dbf", "")

# Calculated data
# ----------------------------
derived_data <- function(df) {
  df %>% mutate(
    totalDensity = round(totalPopulation / sqm_sqmi(area), 2)
  ) %>%
    select(-area)
}

# 1860
c_1860 <- csv$nhgis0014_ts_county %>%
  left_join(csv$nhgis0014_ts_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1860_conflated, by = "GISJOIN") %>%
  select(GISJOIN,
         totalPopulation = A00AA1860,
         area = SHAPE_AREA,
         year = YEAR) %>%
  derived_data()

# 1870
c_1870 <- csv$nhgis0026_ds17_1870_county %>%
  left_join(csv$nhgis0026_ds17_1870_county, by = "GISJOIN") %>%
  left_join(dbf$US_county_1870, by = "GISJOIN") %>%
  select(GISJOIN,
         totalPopulation = (AK3001 + AK3002 + AK3003 + AK3004),
         area = SHAPE_AREA,
         year = YEAR) %>%
  derived_data()

# 1880


# 1890


# 1900


# 1910


# 1920


# 1930


# 1940


# 1950


# 1960


# 1970


# 1980


# 1990


# 2000


# 2010


# Join all the data together and output
all <- rbind(c_1860, c_1870, c_1880, c_1890, c_1900, c_1910, c_1920, c_1930, c_1940, c_1950, c_1960, c_1970, c_1980, c_1990, c_2000, c_2010)

# Select only midwestern cities
states <- c("Illinois", "Indiana", "Iowa", "Kansas", "Missouri", "Minnesota", "Missouri", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin")
midwest <- all %>%
  filter(STATE %in% states)

write_csv(midwest, "midwest-census.csv", na = "")
