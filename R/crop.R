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
  aligned <- align_bb_to_pixels(map, new_bb)

  structure(
    map[seq(from = aligned$pixels$ur.y, to = aligned$pixels$ll.y, by = 1),
        seq(from = aligned$pixels$ll.x, to = aligned$pixels$ur.x, by = 1)],
    class = class(map),
    bb = aligned$corrected
  )
}
