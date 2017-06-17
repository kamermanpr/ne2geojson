#' Create compact view of the contiguous states of the USA with an inset of Alasaka and/or Hawaii.
#'
#' A convenience wrapper building on functions \code{\link[rgdal]{readOGR}},
#' \code{\link[sp]{spTransform}}, \code{\link[rgeos]{gSimplify}},
#' \code{\link[geojsonio]{geojson_json}}, and \code{\link[geojsonio]{geojson_write}} to convert a level 0 or 1 level administrative map shapefile from Natural Earth to a geoJSON files of the contiguous states of the USA with an inset of Alaska and/or Hawaii.
#'
#' @param admin_level Numeric value (\emph{0} or \emph{1}) to specify the map administrative level. Default value is \emph{1} (states). If you require a country-level map, change value to \emph{0}.
#'
#' @param scale Character string (\emph{1:50} or \emph{1:110}) to specify the scale level of the map. Default value is \emph{1:50}. If you require a higher scale, change value to \emph{1:110}.
#'
#' @param include_alaska Logical. Default value is \emph{TRUE}.
#'
#' @param include_hawaii Logical. Default value is \emph{TRUE}.
#'
#' @param save_output Logical. Should the object be saved to disk as a geojson? Default value is \emph{FALSE}. If set to \emph{TRUE}, and parameters \emph{'output_dir'} and \emph{'output_filename'} are not specified, the file will be saved in the current working directory under the file name: \emph{usa_compact.geojson}.
#'
#' @param output_dir Optional character string specifying the path to the directory the geojson should be saved if parameter \emph{'save_file'} is set as \emph{TRUE}. Default value is \emph{NULL}.
#'
#' @param output_filename Optional character string specifying the filename under which the geojson should be saved if parameter \emph{'save_file'} is set to \emph{TRUE}. Default value is \emph{NULL}.
#'
#' @export
usa_compactor <- function(admin_level = 1,
                        scale = '1:50',
                        include_alaska = TRUE,
                        include_hawaii = TRUE,
                        save_output = FALSE,
                        output_dir = NULL,
                        output_filename = NULL){

    # Import geojson -----------------------------------------------------
    # create 'dsn' for import
    if(admin_level == 0 & scale == '1:50'){
        map <- 'admin0_countries_50m.geojson'
    } else if(admin_level == 0 & scale == '1:110') {
        map <- 'admin0_countries_110m.geojson'
    } else if(admin_level == 1 & scale == '1:50') {
        map <- 'admin1_provinces_50m.geojson'
    } else {
        map <- 'admin1_provinces_110m.geojson'
    }
library(rgdal)
    # Access external package data
    map_file <- system.file("extdata",
                map,
                package = 'negeojson')
    # Read file
    world <- rgdal::readOGR(dsn = map_file)
    # Extract and clean US data (excluding Puerto Rico and Guam)
    us <- world[world$admin == 'United States of America', ]
    us$name <- as.character(factor(us$name))

    # Convert to Albers equal area----------------------------------------
    us_area <- sp::spTransform(us,
                           CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
    us_area@data$id <- rownames(us_area@data)

    # Extract and move Alaska and Hawaii----------------------------------
    if(include_alaska == TRUE) {
        # Alaska: extract, rotate, shrink, move, and reset projection
        # Use state IDs
        alaska <- us_area[us_area$name == "Alaska",]
        alaska <- maptools::elide(alaska,
                               rotate = -37)
        alaska <- maptools::elide(alaska,
                               scale = max(apply(bbox(alaska), 1, diff)) / 2.3)
        alaska <- maptools::elide(alaska,
                               shift = c(-2300000, -2500000))
        proj4string(alaska) <- proj4string(us_area)
    }

    if(include_hawaii == TRUE) {
        # Hawaii: extract, move, and reset projection
        # Use state IDs
        hawaii <- us_area[us_area$name == "Hawaii",]
        hawaii <- maptools::elide(hawaii,
                                  shift = c(5100000, -1400000))
        proj4string(hawaii) <- proj4string(us_area)
    }

    # Remove old states and put new ones back in--------------------------
    us_area <- us_area[!us_area$name %in% c("Alaska", "Hawaii"), ]
    if(include_alaska == TRUE & include_hawaii == TRUE) {
        usa_compact <- rbind(us_area, alaska, hawaii)
    } else if(include_alaska == TRUE & include_hawaii == FALSE) {
        usa_compact <- rbind(us_area, alaska)
    } else if(include_alaska == FALSE & include_hawaii == TRUE) {
        usa_compact <- rbind(us_area, hawaii)
    }

    # If save_output == TRUE----------------------------------------------
    if(save_output == TRUE) {
        # Set path if not included in function parameters
        if(is.null(output_dir)) {
            output_dir <- '.'
        }
        # Set file name is not included in function parameters
        if(is.null(output_filename)) {
            output_filename <- 'usa_compact.geojson'
        }
        path <- paste0(output_dir, '/', output_filename)
        geojsonio::geojson_write(input = usa_compact,
                                 geometry = 'polygon',
                                 file = path)
    } else {
        usa_compact
    }
}
