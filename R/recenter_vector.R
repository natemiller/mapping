#' Convert center longitude to cut longitude for vector recentering
#'
#' @param center numeric, central longitude of map
#' @return longitude to cut vector
#' @export
#' @examples
#' shift_center_v(-80)
#' shift_center_v(160)
#'
# helper function to convert lon/lat to center
shift_center_v <- function(center) {
  ifelse(center >=0, (-180 - center) + 360, -180 - center)
}



#' Recenter a vector shapefile on new longitude in Equal Earth projection
#'
#' @param shp_file sf object to be recentered
#' @param center numeric, central longitude of map
#' @return recentered sf object
#'
#' @importFrom glue glue
#' @importFrom smoothr densify
#' @importFrom magrittr "%>%"
#' @importFrom sp Lines
#' @importFrom sp SpatialLines
#' @importFrom sp Line
#' @importFrom sp CRS
#' @import sf
#' @export
#' @examples
#' land_sf <- rnaturalearth::ne_countries(returnclass = 'sf')
#' recenter_sf(land_sf, -80)
#' recenter_sf(land_sf, 160)

# function to take global shape and recenter using equal earth projection
# adapted from https://gist.github.com/valentinitnelav/c7598fcfc8e53658f66feea9d3bafb40
recenter_sf <- function(shp_file, center) {
  sf::sf_use_s2(FALSE) # buffering in s2 is messy, so stick with GEOS here
  prj <- glue::glue("+proj=eqearth +lon_0={center} +wktext")
  
  shift <- 180 + shift_center_v(center = center)
  # create "split line" to split polygon/lines
  WGS84 <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  split_line <- sp::SpatialLines(list(sp::Lines(list(sp::Line(cbind(180 - shift, c(-90, 90)))),
                                                ID = "line")),
                                 proj4string = WGS84
  )
  split_line <- sf::st_as_sf(split_line)
  
  # does split intersect shape?
  line_int <- sf::st_intersection(split_line, shp_file)
  
  # if shape is a point or doesn't intersect the split, just reproject
  if (nrow(line_int) < 1 | grepl("POINT", sf::st_geometry_type(shp_file,
                                                               by_geometry = FALSE),
                                 fixed = TRUE)) {
    
    output_shape <- sf::st_transform(shp_file, crs = prj)
  } else {
    # create a very thin polygon (buffer) out of the intersecting "split line"
    line_buf <- sf::st_buffer(line_int, dist = 0.00001) %>%
      sf::st_union(.) %>%
      sf::st_cast(., "MULTIPOLYGON") %>%
      smoothr::densify(., n = 25)
    
    # split polygons/lines using intersecting thin polygon (buffer)
    output_shape <- sf::st_difference(shp_file, line_buf) %>%
      sf::st_transform(shape_split, crs = prj)
  }
  return(output_shape)
}