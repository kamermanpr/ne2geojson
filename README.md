ne2geojson
================

[![Licence](https://img.shields.io/badge/licence-MIT+-lightgrey.svg)] [![minimal R version](https://img.shields.io/badge/R%3E%3D-3.3.0-6666ff.svg)](https://cran.r-project.org/) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/ne2geojson)](https://cran.r-project.org/package=ne2geojson) [![packageversion](https://img.shields.io/badge/Package%20version-0.1.1-orange.svg?style=flat-square)](commits/master) [![Last-changedate](https://img.shields.io/badge/last%20change-2023--01--11-yellowgreen.svg)](/commits/master)

GeoJSON is an open standard format that is handy for representing geographic data structures in text-based JSON files. The only function (ne2geojson) in this package streamlines the process of downloading a Natural Earth shapefile (<http://www.naturalearthdata.com>) and converting it to GeoJSON format.

## Installation

```r
# install.packages("devtools")
devtools::install_github("kamermanpr/ne2geojson")
```

## Examples

Create 110m scale country-level GeoJSON in current working directory:

```r
ne2geojson(input_filename = 'ne_110m_admin_0_countries')
```

Create 110m scale country-level GeoJSON in new directory:

```r
ne2geojson(input_filename = 'ne_110m_admin_0_countries',
           output_dir = './country')
```

Create simplified 110m scale country-level GeoJSON, retaining 50% of the data:

```r
ne2geojson(input_filename = 'ne_110m_admin_0_countries',
           simplify = TRUE,
           retain = 0.5)
```

## Documentation

Documentation can be found at: [kamermanpr.github.io/ne2geojson/](https://kamermanpr.github.io/ne2geojson/).
