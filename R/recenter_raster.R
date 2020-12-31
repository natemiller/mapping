#' Convert center longitude to cut longitude for raster recentering
#'
#' @param center numeric, central longitude of map
#' @return longitude to cut vector
#' @export
#' @examples
#' shift_center_v(-80)
#' shift_center_v(160)

# helper function for shifting raster center
shift_center_r <- function(center) {
  ifelse(center >=0, (360 - center), (-1 * center))
}


#' Recenter a raster dataframe on new longitude in Equal Earth projection
#'
#' @param raster_df dataframe of gridded data
#' @param res numeric, resolution of raster data
#' @param center numeric, central longitude of map
#' @return recentered raster dataframe
#'
#' @importFrom dplyr rename
#' @importFrom dplyr left_join
#' @importFrom magrittr "%>%"
#' @importFrom  raster rasterFromXYZ
#' @importFrom  raster projectRaster
#' @importFrom  raster as.data.frame
#'
#' @export
#' @examples
#' recenter_raster(raster_df, 1, -80)
#' recenter_raster(raster_df, 0.5, 160)
###
# function to recenter global raster in equal earth projection
###
recenter_raster <- function(raster_df, res, center) {
  shift <- shift_center_r(center = center)
  
  # shift the raster coordinates
  raster_mod <- raster_df %>%
    mutate(
      lon_bin = lon_bin + (res / 2),
      lat_bin = lat_bin + (res / 2)
    ) %>%
    mutate(
      lon_bin = lon_bin + shift,
      lon_bin = ifelse(lon_bin > 180, lon_bin - 360, lon_bin)
    )
  
  # create raster grid
  raster_grid <- expand.grid(
    lon = seq(
      min(raster_mod$lon_bin, na.rm = TRUE),
      max(raster_mod$lon_bin, na.rm = TRUE),
      res
    ),
    lat = seq(
      min(raster_mod$lat_bin, na.rm = TRUE),
      max(raster_mod$lat_bin, na.rm = TRUE), res
    )
  )
  
  # join full raster grid and project
  output_raster <- raster_grid %>%
    dplyr::left_join(raster_mod, by = c(
      "lon" = "lon_bin",
      "lat" = "lat_bin"
    )) %>%
    raster::rasterFromXYZ(.,
                          crs = "+proj=longlat +datum=WGS84 +no_defs"
    ) %>%
    raster::projectRaster(.,
                          over = TRUE,
                          crs = "+proj=eqearth +wktext"
    ) %>%
    raster::as.data.frame(.,
                          xy = TRUE
    ) %>%
    dplyr::rename(
      lon = x,
      lat = y
    )
  
  return(output_raster)
}

