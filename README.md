ne2geojson
================

[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip) [![Build Status](https://travis-ci.org/search/search.svg?branch=master)](https://travis-ci.org/search/search)

------------------------------------------------------------------------

[![minimal R version](https://img.shields.io/badge/R%3E%3D-3.3.0-6666ff.svg)](https://cran.r-project.org/) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/ne2geojson)](https://cran.r-project.org/package=ne2geojson) [![packageversion](https://img.shields.io/badge/Package%20version-0.1.0-orange.svg?style=flat-square)](commits/master)

------------------------------------------------------------------------

[![Last-changedate](https://img.shields.io/badge/last%20change-2017--06--17-yellowgreen.svg)](/commits/master)

Geojson is an open standard format that is handy for representing geographic data structures on the open web. Services such as GitHub, the Twitter API, and Leaflet have embraced the format. The main function (ne2geojson) in this package streamlines the process of downloading a Natural Earth shapefile (<http://www.naturalearthdata.com>) and converting it to geoJSON format. It also provides a helper function (usa\_compactor) to create a GeoJSON of the contiguous states of the USA with an inset of Alaska and/or Hawaii.
