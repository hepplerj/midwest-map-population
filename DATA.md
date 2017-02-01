# Explanation of the Data

## Summary

The data contained in this repository was prepared by Jason A. Heppler to explore population and demographic change in the American Midwest between 1790 and 2010.

## Type, Format, Extent

The data is stored as `csv`. The `midwest-cities` file is 1.3 MB in size, the `midwest-census` file is 895.6 KB in size.

## Filenaming Conventions

The data files are prepended with `midwest-` to indicate that they are subset of larger data files.

## Modifications

The data has undergone transformation from their original format. 

For the purposes of creating a look-up table for the demographic information, the data was joined with shapefiles by their GISJOIN unique identification and then output into a table containing only the necessary information.

See also the two scripts used for the data preparation:

- `prep-census.R`:
- `prep-data.R`:

## Methods and Tools

The data was prepared using the R language, relying on two scripts `prep-census.R` and `prep-data.R`. Explanations of these scripts can be found above in the **Modifications** section.

## Sources

City population data is from the U.S. Census Bureau and Erik Steiner, Spatial History Project, Center for Spatial and Textual Analysis, Stanford University. Data on GitHub. Census demographic data comes from the Minnesota Population Center, National Historical Geographic Information System: Version 2.0 (Minneapolis, MN: University of Minnesota, 2011), <http://nhgis.org>.

## Reuse License

The scripts in this repository are all available for use under the MIT license (see file `LICENSE`). The data may or not have been gathered by the project team. Data contained in this repository is included only if we thought doing so was within fair use or we have permission and may be reused only if credit is given to those cited under the **Sources** section above.
