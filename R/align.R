align_bb_to_pixels <- function(map, new_bb) {
  d <- dim(map)
  bb <- attr(map, "bb")
  new_bb_pix <- data.frame(
    ll.x = 1 + floor((new_bb$ll.lon - bb$ll.lon) / (bb$ur.lon - bb$ll.lon) * d[[2L]]),
    ll.y = d[[1L]] - floor((new_bb$ll.lat - bb$ll.lat) / (bb$ur.lat - bb$ll.lat) * d[[1L]]),
    ur.x = ceiling((new_bb$ur.lon - bb$ll.lon) / (bb$ur.lon - bb$ll.lon) * d[[2L]]),
    ur.y = d[[1L]] + 1 - ceiling((new_bb$ur.lat - bb$ll.lat) / (bb$ur.lat - bb$ll.lat) * d[[1L]])
  )

  stopifnot(new_bb_pix$ll.x >= 1L)
  stopifnot(new_bb_pix$ll.y >= 1L)
  stopifnot(new_bb_pix$ur.x <= d[[2L]])
  stopifnot(new_bb_pix$ur.y <= d[[1L]])

  new_bb_pix_ll <- data.frame(
    ll.lat = (d[[1L]] - new_bb_pix$ll.y) / d[[1L]] * (bb$ur.lat - bb$ll.lat) + bb$ll.lat,
    ll.lon = (new_bb_pix$ll.x - 1) / d[[2L]] * (bb$ur.lon - bb$ll.lon) + bb$ll.lon,
    ur.lat = (d[[1L]] + 1 - new_bb_pix$ur.y) / d[[1L]] * (bb$ur.lat - bb$ll.lat) + bb$ll.lat,
    ur.lon = (new_bb_pix$ur.x) / d[[2L]] * (bb$ur.lon - bb$ll.lon) + bb$ll.lon
  )

  list(
    pixels = new_bb_pix,
    corrected = new_bb_pix_ll
  )
}
