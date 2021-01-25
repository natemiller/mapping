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

#' Helper function modifying proj string longitude
#'
#' @param proj string, proj4 string
#' @return modified proj string
#' @export
#' @examples
#' proj1 <- '+proj=moll'
#' proj2 <- '+proj=eqearth +lon_0=0 +wktext'
#' proj3 <- '+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs'
#' mod_proj(proj1)
#' mod_proj(proj2)
#' mod_proj(proj3)

mod_proj <- function(proj) {
  if (grepl("lon_0=", proj)) {
    gsub(
      x = proj,
      pattern = "(?<=lon_0=)[0-9]{1,3}",
      replacement = "{center}",
      perl = TRUE
    )
  } else {
    paste0(proj, " +lon_0={center}")
  }
}


#' Recenter a vector shapefile on new longitude in Equal Earth projection
#'
#' @param shp_file sf object to be recentered
#' @param center numeric, central longitude of map
#' @param proj string, proj4 string
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
#' proj1 <- '+proj=moll'
#' proj2 <- '+proj=eqearth +lon_0=0 +wktext'
#' proj3 <- '+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs'
#' recenter_sf(land_sf, -80, proj = proj1)
#' recenter_sf(land_sf, 160, proj = proj2)
#' recenter_sf(land_sf, 110, proj = proj3)

# function to take global shape and recenter using equal earth projection
# adapted from https://gist.github.com/valentinitnelav/c7598fcfc8e53658f66feea9d3bafb40
recenter_sf <- function(shp_file, center = 0, proj = '+proj=eqearth +lon_0=0 +wktext') {
  sf::sf_use_s2(FALSE) # buffering in s2 is messy, so stick with GEOS here
  
  if (!is.character(proj)) {
    stop('proj must be a valid proj4 string')
  }
  proj <- mod_proj(proj)
  recentered_proj <- glue::glue(proj)
  
  shift <- 180 + shift_center_v(center = center)
  # create "split line" to split polygon/lines
  split_line <- sf::st_as_sf(
    sf::st_set_crs(
      sf::st_sfc(
        sf::st_linestring(
          cbind(180 - shift, c(-90, 90))
        )
      ),
      value = 4326
    )
  )
  
  # does split intersect shape?
  line_int <- sf::st_intersection(split_line, shp_file)
  
  # if shape is a point or doesn't intersect the split, just reproject
  if (nrow(line_int) < 1 | grepl("POINT", sf::st_geometry_type(shp_file,
                                                               by_geometry = FALSE),
                                 fixed = TRUE)) {
    
    output_shape <- sf::st_transform(shp_file, crs = recentered_proj)
  } else {
    # create a very thin polygon (buffer) out of the intersecting "split line"
    line_buf <- sf::st_buffer(line_int, dist = 0.00001) %>%
      sf::st_union(.) %>%
      sf::st_cast(., "MULTIPOLYGON") %>%
      smoothr::densify(., n = 25)
    
    # split polygons/lines using intersecting thin polygon (buffer)
    output_shape <- sf::st_difference(shp_file, line_buf) %>%
      sf::st_transform(shape_split, crs = recentered_proj)
  }
  return(output_shape)
}