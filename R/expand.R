#' Compute a query set to obtain tiles that cover a given bounding box
#'
#' This function assumes that \code{bb} is the bounding box that results from
#' a certain call to \code{\link[ggmap]{get_map}}.  From this, this function
#' computes a set of queries (given as latitude/longitude pairs) that will
#' result in maps covering the target bounding box when passed to otherwise
#' identical \code{get_map} calls.
#'
#' @param bb,new_bb Data frames, each with one row and four columns
#'   \code{\{ll|ur\}.\{lat|lon\}},
#'   representing the base and the desired bounding boxes
#' @param stepwidth Relative width of each step, defaults to \code{0.9}.
expand_bb <- function(bb, new_bb, stepwidth = 0.9) {
  stopifnot(stepwidth > 0)

  bb <- normalize_bb(bb)
  new_bb <- normalize_bb(new_bb)

  bb_v <- bb_to_vector(bb)
  new_bb_v <- bb_to_vector(new_bb)
  proj_frame <- data.frame(
    x = c(bb_v[X], new_bb_v[X]),
    y = c(bb_v[Y], new_bb_v[Y])
  )

  proj_results <- mapproj::mapproject(proj_frame$x, proj_frame$y, "mercator")

  x <- proj_results$x
  y <- proj_results$y

  kxl <- ceiling((x[[1L]] - x[[3L]]) / stepwidth / (x[[2L]] - x[[1L]]))
  kxr <- ceiling((x[[4L]] - x[[2L]]) / stepwidth / (x[[2L]] - x[[1L]]))
  kyl <- ceiling((y[[1L]] - y[[3L]]) / stepwidth / (y[[2L]] - y[[1L]]))
  kyr <- ceiling((y[[4L]] - y[[2L]]) / stepwidth / (y[[2L]] - y[[1L]]))

  stopifnot(kxl > 0)
  stopifnot(kxr > 0)
  stopifnot(kyl > 0)
  stopifnot(kyr > 0)

  kx <- seq.int(-kxl, kxr, by = 1L)
  ky <- seq.int(-kyl, kyr, by = 1L)

  sx = x[[1L]] + (kx + 0.5) * stepwidth * (x[[2L]] - x[[1L]])
  sy = y[[1L]] + (ky + 0.5) * stepwidth * (y[[2L]] - y[[1L]])

  base_x <- c(new_bb_v[X], mean(bb_v[X]))
  base_y <- c(new_bb_v[Y], mean(bb_v[Y]))
  map_x <- function(xx)
    mapproj::mapproject(c(new_bb_v[X], xx), base_y, "mercator")$x[[3L]]
  map_y <- function(yy)
    mapproj::mapproject(base_x, c(new_bb_v[Y], yy), "mercator")$y[[3L]]

  revmap_x <- function(x) {
    force(x)
    r <- uniroot(function(xx) map_x(xx) - x, new_bb_v[X], tol = 1e-12, extendInt = "yes")
    stopifnot(all.equal(r$f.root, 0))
    r$root
  }
  revmap_y <- function(y) {
    force(y)
    r <- uniroot(function(yy) map_y(yy) - y, new_bb_v[Y], tol = 1e-12, extendInt = "yes")
    stopifnot(all.equal(r$f.root, 0))
    r$root
  }

  lons <- vapply(sx, revmap_x, numeric(1))
  lats <- vapply(sy, revmap_y, numeric(1))

  g <- expand.grid(x = seq_along(lons), y = seq_along(lats))
  g <- transform(g, kx = kx[x], ky = ky[y], lon = lons[x], lat = lats[y], x = NULL, y = NULL)
  g

#   with(proj_results, kxl >= (x[[1]] - x[[3]]) / stepwidth / (x[[2]] - x[[1]]))
#   with(proj_results, kxr >= (x[[4]] - x[[2]]) / stepwidth / (x[[2]] - x[[1]]))

#   with(proj_results, kxl >= (x[[3]] - x[[1]]) / stepwidth / (x[[1]] - x[[2]]))
#   with(proj_results, kxr >= (x[[4]] - x[[2]]) / stepwidth / (x[[2]] - x[[1]]))

#   with(proj_results, kxl * (x[[1]] - x[[2]]) <= (x[[3]] - x[[1]]) / stepwidth)
#   with(proj_results, kxr * (x[[2]] - x[[1]]) >= (x[[4]] - x[[2]]) / stepwidth)

#   with(proj_results, kxl * stepwidth * (x[[1]] - x[[2]]) <= x[[3]] - x[[1]])
#   with(proj_results, kxr * stepwidth * (x[[2]] - x[[1]]) >= x[[4]] - x[[2]])

#   with(proj_results, x[[1]] + kxl * stepwidth * (x[[1]] - x[[2]]) <= x[[3]])
#   with(proj_results, x[[2]] + kxr * stepwidth * (x[[2]] - x[[1]]) >= x[[4]])
#   with(proj_results, y[[1]] + kyl * stepwidth * (y[[1]] - y[[2]]) <= y[[3]])
#   with(proj_results, y[[2]] + kyr * stepwidth * (y[[2]] - y[[1]]) >= y[[4]])
}
