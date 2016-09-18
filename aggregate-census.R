#!/usr/bin/Rscript

# Load census data and aggregate population data. Create a single
# long table to be used in visualization.

library(dplyr)
library(readr)
library(stringr)
options(stringsAsFactors = FALSE)

sqm_sqmi <- function(sq_meters) {
  sq_meters / 2589988.110336;
}

fn <- Sys.glob("data/*.csv")
csv <- lapply(fn, read_csv)
names(csv) <- fn %>% str_replace("data/", "") %>% str_replace(".csv", "")
rm(fn)

# Data prep and selection
# ----------------------------
csv <- read_csv("data/nhgis0023_ts_nominal_county.csv")

# Select only midwestern cities
states <- c("Illinois", "Indiana", "Iowa", "Kansas", "Missouri", "Minnesota", "Missouri", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin")
midwest <- csv %>%
  filter(STATE %in% states)

# Calculated data
# ----------------------------
derived_data <- function(df) {
  df %>% mutate(
    totalPopulation = totalPopulation,
    totalDensity = round(totalPopulation / sqm_sqmi(area), 2)
  ) %>%
    select(-area)
}

# 1850
c_1850 <- csv$nhgis0013_ds10_1850_county %>%
  left_join(csv$nhgis0013_ds10_1850_county, by = "GISJOIN") %>%
  left_join(dbf$county_1850, by = "GISJOIN") %>%
  select(GISJOIN,
         totalPopulation = A00A1123,
         area = SHAPE_AREA,
         year = YEAR) %>%
  derived_data()

# Join all the data together and output
all <- rbind(c_1790, c_1800, c_1810, c_1820, c_1830, c_1840, c_1850, c_1860)
write_csv(all, "census.csv", na = "")
