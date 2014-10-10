#' Crops a map to a new bounding box
#'
#' This function reduces the dimensions of a map to fit a given bounding box
#' as closely as possible.  The bounding box is readjusted to fit pixel
#' boundaries.
#'
#' @param map An object of class \code{ggmap}
#' @param new_bb A data frame with one row and four columns \code{\{ll|ur\}.\{lat|lon\}}
#'
#' @export
crop_map <- function(map, new_bb) {
  stop("NYI")
  new_bb_pix <- ll2pix(map, new_bb)
  new_bb_pix_ll <- pix2ll(map, new_bb_pix)

  stopifnot(new_bb_pix_ll$ll.lat <= new_bb$ll.lat)
  stopifnot(new_bb_pix_ll$ur.lat >= new_bb$ur.lat)
  stopifnot(new_bb_pix_ll$ll.lon <= new_bb$ll.lon)
  stopifnot(new_bb_pix_ll$ur.lon >= new_bb$ur.lon)

  structure(
    map[seq(from = new_bb_pix$ur.y, to = new_bb_pix$ll.y, by = 1),
        seq(from = new_bb_pix$ll.x, to = new_bb_pix$ur.x, by = 1)],
    class = class(map),
    bb = new_bb_pix_ll
  )
}
