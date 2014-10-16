#' Downloads a map with a specified bounding box, stitches tiles if necessary
#'
#' This creates a map that corresponds to a given bounding box by repeatedly
#' calling \code{\link[ggmap]{get_map}} and seamlessly stitching the resulting
#' tiles.
#'
#' @param bb A data frame with one row and four columns \code{\{ll|ur\}.\{lat|lon\}}
#' @param stepwidth The default will attempt to download tiles that perfectly align.
#'   This may fail due to rounding errors.  Use a value slightly less than 1 to allow
#'   for an overlap in the downloaded tiles.
#' @param ... Further arguments passed to \code{\link[ggmap]{get_map}}
#'
#' @export
stitch_map <- function(bb, stepwidth = 1, ...) {
  args <- list(...)
  nargs <- names(args)
  stopifnot(!is.null(nargs) && all(nargs != ""))

  if (stepwidth < 1)
    stop("Seamless stitching not yet supported")

  if ("location" %in% nargs) {
    location <- args[["location"]]
    nargs <- setdiff(nargs, "location")
    args <- args[nargs]
  } else {
    location <- c(lon = (bb$ll.lon + bb$ur.lon) / 2, lat = (bb$ll.lat + bb$ur.lat) / 2)
  }

  if ("filename" %in% nargs) {
    filename <- args[["filename"]]
    nargs <- setdiff(nargs, "filename")
    args <- args[nargs]
  } else {
    filename <- "ggmapTemp"
  }
  get_filename <- function(kx, ky) sprintf("%s_%s_%s", filename, kx, ky)

  rm(nargs)

  m <- do.call(ggmap::get_map, c(
    list(location = location,
         filename = get_filename(0, 0)),
    args))

  g <- expand_bb(bb = attr(m, "bb"), new_bb = bb, stepwidth = stepwidth)
  g$id <- seq_along(g$kx)

  l <- plyr::alply(g, 1,
    function(x) {
      do.call(ggmap::get_map, c(
        list(location = unlist(x[c("lon", "lat")]),
             filename = get_filename(x$kx, x$ky)),
        args))
    }
  )

  crop_map(paste_maps(l), bb)
}

paste_maps <- function(l) {
  g <- attr(l, "split_labels")
  d <- dim(l[[1L]])
  max_x <- max(g$x)
  max_y <- max(g$y)
  dm <- d * c(max_y, max_x)
  mm <- matrix(data = character(), nrow = dm[[1L]], ncol = dm[[2L]])
  bbs <- plyr::adply(g, 1, function(x) {
    m <- l[[x$id]]
    xi <- seq(to =              x$x  * d[[2L]], by = 1, length.out = d[[2L]])
    yi <- seq(to = (max_y + 1 - x$y) * d[[1L]], by = 1, length.out = d[[1L]])
    mm[yi, xi] <<- as.matrix(m)
    attr(m, "bb")
  })

  bb <- plyr::summarize(
    bbs,
    ll.lat = min(ll.lat), ll.lon = min(ll.lon),
    ur.lat = max(ur.lat), ur.lon = max(ur.lon)
  )

  structure(as.raster(mm), class = c("ggmap", "raster"), bb = bb)
}
