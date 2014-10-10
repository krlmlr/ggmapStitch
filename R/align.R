bb_names_in <- c("ll.lon", "ur.lon", "ll.lat", "ur.lat")
bb_names_out <- c("ll.lat", "ll.lon", "ur.lat", "ur.lon")
pix_names_in <- c("ll.x", "ur.x", "ll.y", "ur.y")
pix_names_out <- c("ll.x", "ll.y", "ur.x", "ur.y")

LLX <- 1L
URX <- 2L
X <- c(LLX, URX)
LLY <- 3L
URY <- 4L
Y <- c(LLY, URY)

bb_to_vector <- function(bb) {
  unlist(bb[bb_names_in])
}

vector_to_bb <- function(bb_vec) {
  as.data.frame(setNames(as.list(bb_vec), bb_names_in))[bb_names_out]
}

vector_to_pixels <- function(pix_vec) {
  as.data.frame(setNames(as.list(pix_vec), pix_names_in))[pix_names_out]
}

get_pixels <- function(map, new_bb) {
  d <- dim(map)
  bb <- bb_to_vector(attr(map, "bb"))

  proj_frame <- data.frame(
    x = c(bb[X], new_bb[X]),
    y = c(bb[Y], new_bb[Y])
  )

  proj_results <- mapproj::mapproject(proj_frame$x, proj_frame$y, "mercator")

  OLD <- c(1L, 2L)
  NEW <- c(3L, 4L)

  new_bb_pix_x <- scales::rescale(proj_results$x[NEW], c(0, d[[2L]]), proj_results$x[OLD])
  new_bb_pix_y <- scales::rescale(proj_results$y[NEW], c(d[[1L]], 0), proj_results$y[OLD])

  c(new_bb_pix_x, new_bb_pix_y)
}

align_bb_to_pixels <- function(map, new_bb) {
  d <- dim(map)

  if (new_bb$ll.lon > new_bb$ur.lon) {
    new_bb = transform(new_bb, ll.lon = ur.lon, ur.lon = ll.lon)
  }

  if (new_bb$ll.lat > new_bb$ur.lat) {
    new_bb = transform(new_bb, ll.lat = ur.lat, ur.lat = ll.lat)
  }

  new_bb_vec <- bb_to_vector(new_bb)

  new_bb_pix_v <- get_pixels(map, new_bb_vec)

  stopifnot(new_bb_pix_v[c(LLX, URY)] >= 0)
  stopifnot(new_bb_pix_v[c(LLY, URX)] <= d)

  new_bb_pix_v_int <- as.integer(c(
    floor(new_bb_pix_v[c(LLX,URY)]),
    ceiling(new_bb_pix_v[c(URX,LLY)]))[c(1L,3L,4L,2L)])

  TOL <- 1e-9
  CHECK_TOL <- TOL * 1000

  root_res <- rootSolve::multiroot(
    function(x) get_pixels(map, x) - new_bb_pix_v_int,
    new_bb_vec,
    rtol = TOL
  )

  new_bb_vec_int <- root_res$root
  if (root_res$estim.precis > CHECK_TOL) {
    warning("Could not compute bounding box that aligns at pixel boundaries. Map can appear slightly distorted.")
    new_bb_vec_int <- new_bb_vec
  }

  list(
    pixels = vector_to_pixels(new_bb_pix_v_int + c(1L, 0L, 0L, 1L)),
    corrected = vector_to_bb(new_bb_vec_int)
  )
}
