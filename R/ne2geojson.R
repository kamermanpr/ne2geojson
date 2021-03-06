#' Convert Natural Earth shapefiles to geoJSON.
#'
#' A convenience wrapper building on functions \code{\link[rgdal]{readOGR}}, \code{\link[sp]{spTransform}}, \code{\link[rgeos]{gSimplify}}, \code{\link[geojsonio]{geojson_json}}, \code{\link[geojsonio]{geojson_write}} to import and convert shapefiles from \href{http://www.naturalearthdata.com/downloads/}{Natural Earth} to geoJSON format.
#'
#' @param input_filename Character string specifying the map layer. Default value is \emph{'ne_50m_admin_0_countries'}. Do not add the file extension; import assumes that the input file is in '.zip' format. For a full list of available maps, please see \emph{Details}.
#'
#' @param local Logical specifying whether the input file is located locally or whether the file must be downloaded from Natural Earth. Default value is \emph{FALSE}. In the case of \emph{TRUE}, the path to the file must be specified under \emph{local_path}.
#'
#' @param local_path Character string specifying the path to the local file specified under \emph{local}. Default value is the current working directory.
#'
#' @param output_dir Character string specifying the path (no trailing '/') to the output directory where the geoJSON will be saved. Default value is the current working directory.
#'
#' @param output_filename Character string specifying the filename of the output file (no file extension is required). Default value is \emph{NULL}, and the file will be given the name of the input file.
#'
#' @param crs Character string specifying the coordinate reference system to use. The default value is \emph{'+init=epsg:4326 +proj=longlat +ellp=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'}.
#'
#' @param box_long Longitudinal scope of the bounding box. Default range is \emph{-180} to \emph{180} degrees.
#'
#' @param box_lat Latitudinal scope of the bounding box. Default range
#' is \emph{-90} to \emph{90} degrees.
#'
#' @param simplify Logical specifying whether to simplify the given geometry using the Douglas-Peuker algorithm. Default value is \emph{FALSE}.
#'
#' @param retain Numerical value between 0 and 1 giving the proportion of values to retain when simplifying. Simplification is done through the Douglas-Peuker algorithm, and smaller \emph{tolerance} values produce greater simplification. Default value is \emph{0.1}.
#'
#' @param topology Logical specifying whether the Douglas-Peuker algorithm
#' should attempt to preserve the topology of the original geometry. Default
#' value is \emph{TRUE}.
#'
#' @details A full list of available maps is provided below. \emph{XX} specifies the scale, and must be substituted with 10 (1:10m), 50 (1:50m) or 110 (1:110m). The values in square parentheses indicate the scale(s) a map is available in.
#'
#' \itemize{
#' \strong{\emph{CULTURAL MAPS}}
#'
#' \strong{Administrative level 0 (countries)}
#'   \item ne_XXm_admin_0_countries [10m, 50m, 110m]
#'   \item ne_XXm_admin_0_countries_lakes (without boundary lakes) [10m, 50m, 110m]
#'   \item ne_XXm_admin_0_sovereignty [10m, 50m, 110m]
#'   \item ne_XXm_admin_0_tiny_countries [50m, 110m]
#'   \item ne_XXm_admin_0_tiny_countries_scale_rank [50m]
#'   \item ne_XXm_admin_0_map_units [10m, 50m, 110m]
#'   \item ne_XXm_admin_0_map_subunits [10m, 50m]
#'   \item ne_XXm_admin_0_scale_rank [10m, 50m, 110m]
#'   \item ne_XXm_admin_0_scale_rank_minor_islands (with minor islands) [10m]
#'   \item ne_XXm_admin_0_boundary_lines_land (boundary lines) [10m, 50m, 110m]
#'   \item ne_XXm_admin_0_boundary_lines_map_units [10m]
#'   \item ne_XXm_admin_0_boundary_map_units [50m]
#'   \item ne_XXm_admin_0_boundary_lines_maritime_indicator [10m, 50m]
#'   \item ne_XXm_admin_0_pacific_groupings [10m, 50m, 110m]
#'   \item ne_XXm_admin_0_disputed_areas [10m]
#'   \item ne_XXm_admin_0_breakaway_disputed_areas [50m]
#'   \item ne_XXm_admin_0_disputed_areas_scales_rank_minor_islands [10m]
#'   \item ne_XXm_admin_0_boundary_lines_disputed_areas (boundary lines) [10m, 50m]
#'   \item ne_XXm_admin_0_antarctic_claims [10m]
#'   \item ne_XXm_admin_0_antarctic_claim_limit_lines (boundary lines) [10m]
#'
#'   \strong{Administrative level 1 (states/provinces)}
#'   \item ne_XXm_admin_1_states_provinces [10m, 110m]
#'   \item ne_XXm_admin_1_states_provinces_scale_rank [10m]
#'   \item ne_XXm_admin_1_states_provinces_shp_scale_rank [50m, 110m]
#'   \item ne_XXm_admin_1_states_provinces_lakes (without large lakes) [10m, 50m, 110m]
#'   \item ne_XXm_admin_1_states_provinces_lines (boundary lines) [10m, 50m, 110m]
#'
#'   \strong{Populated places}
#'   \item ne_XXm_populated_places [10m, 50m, 110m]
#'   \item ne_XXm_polulated_places_simple (reduced set) [10m, 50m, 110m]
#'
#'   \strong{Roads and railways}
#'   \item ne_XXm_roads [10m]
#'   \item ne_XXm_roads_north_america [10m]
#'   \item ne_XXm_railroads [10m]
#'   \item ne_XXm_railroads_north_america [10m]
#'
#'   \strong{Airports and ports}
#'   \item ne_XXm_airports [10m, 50m]
#'   \item ne_XXm_ports [10m, 50m]
#'
#'   \strong{Other}
#'   \item ne_XXm_urban_areas [10m, 50m]
#'   \item ne_XXm_parks_and_protected_lands (USA) [10m]
#'   \item ne_XXm_time_zones [10m]
#'
#'   \strong{Cultural building blocks}
#'   \item ne_XXm_admin_0_label_points [10m]
#'   \item ne_XXm_admin_0_seams [10m]
#'   \item ne_XXm_admin_1_label_points [10m]
#'   \item ne_XXm_admin_1_seams [10m]
#'   \item ne_XXm_cultural_building_blocks_all (labels and seams) [10m]
#'
#'   \strong{PHYSICAL MAPS}
#'
#'   \strong{Coastline}
#'   \item ne_XXm_coastline [10m, ]
#'
#'   \strong{Land}
#'   \item ne_XXm_land [10m, ]
#'   \item ne_XXm_land_scale_rank [10m, ]
#'
#'   \strong{Minor islands and reefs}
#'   \item ne_XXm_minor_islands [10m, ]
#'   \item ne_XXm_minor_islands_coastline [10m, ]
#'   \item nn_XXm_reefs [10m, ]
#'
#'   \strong{Oceans}
#'   \item ne_XXm_ocean [10m, ]
#'   \item ne_XXm_ocean_scale_rank [10m, ]
#'
#'   \strong{Rivers, lake centerlines, lakes, and resevoirs}
#'   \item ne_XXm_rivers_lake_centerlines [10m, ]
#'   \item ne_XXm_rivers_lake_centerline_scale_rank [10m, ]
#'   \item ne_XXm_rivers_north_america [10m, ]
#'   \item ne_XXm_rivers_europe [10m, ]
#'   \item ne_XXm_lakes [10m, ]
#'   \item ne_XXm_lakes_historic [10m, ]
#'   \item ne_XXm_lakes_pluvial [10m, ]
#'   \item ne_XXm_lakes_north_america [10m,]
#'   \item ne_XXm_lakes_europe [10m,]
#'
#' }
#'
#' @examples
#' \dontrun{
#' # Create 110m scale country-level geoJSON in current working directory
#' ne2geojson(input_filename = 'ne_110m_admin_0_countries')
#'
#' # Create 110m scale country-level geoJSON in new directory
#' ne2geojson(input_filename = 'ne_110m_admin_0_countries', output_dir = './country'
#'
#' # Create simplified 110m scale country-level geoJSON, retaining 50% of points
#' ne2geojson(input_filename = 'ne_110m_admin_0_countries', simplify = TRUE, retain = 0.5)
#' }
#'
#' @export
ne2geojson <- function(input_filename = 'ne_50m_admin_0_countries',
                       local = FALSE,
                       local_path = '.',
                       output_dir = '.',
                       output_filename = NULL,
                       crs = '+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0',
                       box_long = c(-180, 180),
                       box_lat = c(-90, 90),
                       simplify = FALSE,
                       retain = 0.1,
                       topology = TRUE) {

    ############################################################
    #                                                          #
    #                      Download file                       #
    #                                                          #
    ############################################################
    # Set download url---------------------------------------------------------
    url <- paste0('http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/',
                  ifelse(stringr::str_detect(input_filename, '110m'),
                         yes = '110m',
                         no = ifelse(stringr::str_detect(input_filename, '50m'),
                                     yes = '50m',
                                     no = '10m')),
                  '/',
                  ifelse(stringr::str_detect(input_filename, 'admin') |
                             stringr::str_detect(input_filename, 'airports') |
                             stringr::str_detect(input_filename, 'populated') |
                             stringr::str_detect(input_filename, 'ports') |
                             stringr::str_detect(input_filename, 'urban_areas'),
                         yes = 'cultural',
                         no = 'physical'),
                  '/',
                  input_filename,
                  '.zip')

    # Download and extract shapefile into temp output directory----------------
    ## Name of downloaded file
    file <- basename(url)
    ## Create temp directory
    temp_dir <- tempdir()
    ## Download into temp file
    download.file(url, file)
    ## Extract temp file into output directory
    unzip(file, exdir = temp_dir)

    ############################################################
    #                                                          #
    #                 Convert file to geojson                  #
    #                                                          #
    ############################################################
    # Default action: set file name--------------------------------------------
    if(is.null(output_filename)) {
        file_name <- input_filename
    } else {
        file_name <- output_filename
    }

    ## Check if output directory exists----------------------------------------
    if(!dir.exists(output_dir)) {
        dir.create(output_dir)
    }

    ## Add geoJSON extension to file_name--------------------------------------
    file_name_2 <- paste0(output_dir,
                          '/',
                          file_name,
                          '.geojson')

    # Read in the shape file---------------------------------------------------
    shape <- rgdal::readOGR(dsn = temp_dir,
                            layer = input_filename)

    # Set coordinate reference system-------------------------------------
    shape_1 <- sp::spTransform(x = shape,
                               CRSobj = sp::CRS(crs))

    # Set bounds----------------------------------------------------------
    box <- raster::extent(c(x = box_long,
                            y = box_lat))
    shape_1b <- raster::crop(shape_1,
                             box)

    # Simplify------------------------------------------------------------
    if(simplify == TRUE) {
        shape_2 <- rmapshaper::ms_simplify(input = shape_1b,
                                           keep = tolerance,
                                           method = 'dp')
    } else {
        shape_2 <- shape_1
    }

    # Transform to SpatialPolygonsDataFrame-------------------------------
    shape_3 <- geojsonio::geojson_json(shape_2)

    # Transform to geoJSON and write to file------------------------------
    geojsonio::geojson_write(input = shape_3,
                             file = file_name_2)
}
