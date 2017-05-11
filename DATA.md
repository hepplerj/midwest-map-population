# Explanation of the Data

## Summary

The data contained in this repository was prepared by Jason A. Heppler to explore population and demographic change in the American Midwest between 1810 and 2010.

## Type, Format, Extent

The project strives to use widely-used and sharable data formats, where
possible. Most of the data is stored as `csv` files: `midwest-cities` is 1.3 MB in size, `midwest-census` is 895.6 KB in size.  Several shapefiles were also generated for use with Leaflet.

## Filenaming Conventions

The `csv` data files are prepended with `midwest-` to indicate that they are subset of larger data files (in this case, the CESTA city data and NHGIS data tables).

## Modifications

The data has undergone transformation from their original format. 

For the purposes of creating a look-up table for the demographic information, the data was joined with shapefiles by their GISJOIN unique identification and then output into a table containing only the necessary information.

See also the two scripts used for the data preparation:

- `prep-census.R`: This script uses NHGIS shapefiles and data tables to
  create demographic information about the Midwest. The output includes
  a shapefile for each decade, and `midwest-census.csv` file.
- `prep-data.R`: This script uses the CESTA cities population data to
  clean up lat/long information, filter the data to Midwestern states,
  and writes a `midwest-cities.csv` file.

## Methods and Tools

The data was prepared using the R language, relying on two scripts `prep-census.R` and `prep-data.R`. Explanations of these scripts can be found above in the **Modifications** section.

The visualization of the information is done in
[Shiny](https://shiny.rstudio.com/).

## Sources

City population data is from the U.S. Census Bureau and Erik Steiner, Spatial History Project, Center for Spatial and Textual Analysis, Stanford University. Data on GitHub. Census demographic data comes from the Minnesota Population Center, National Historical Geographic Information System: Version 2.0 (Minneapolis, MN: University of Minnesota, 2011), <http://nhgis.org>.

## Reuse License

The scripts in this repository are all available for use under the MIT license (see file `LICENSE`). The data may or not have been gathered by the project team. Data contained in this repository is included only if we thought doing so was within fair use or we have permission and may be reused only if credit is given to those cited under the **Sources** section above.
