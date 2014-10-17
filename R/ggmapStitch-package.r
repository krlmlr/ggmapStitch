#' Arbitrarily large maps with ggmap
#'
#' The ggmap package offers downloading map tiles from a variety of
#' providers, but (at least for Google Maps) the maps are restricted to a maximum
#' size.  This package removes this restriction by providing a function to
#' construct maps for given bounding box and zoom level.
#'
#' @name ggmapStitch
#' @docType package
#' @examples
#' require(ggmap)
#'
#' bb_ch <- data.frame(ll.lat = 45.68, ur.lat = 47.9,
#'                     ll.lon = 5.79, ur.lon = 10.7)
#'
#' # Zoom level 8 is a good choice, but default ggmap() will clip away Geneva
#' ggmap(stitch_map(bb = bb_ch, location = "Switzerland", zoom = 8))
#'
#' \dontrun{
#' # At zoom level 9, about 10 tiles will be downloaded
#' ggmap(stitch_map(bb = bb_ch, location = "Switzerland", zoom = 9))}
NULL
