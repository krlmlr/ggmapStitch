align_bb_to_pixels <- function(map, new_bb) {
  d <- dim(map)
  bb <- attr(map, "bb")

  if (new_bb$ll.lon > new_bb$ur.lon) {
    new_bb = transform(new_bb, ll.lon = ur.lon, ur.lon = ll.lon)
  }

  if (new_bb$ll.lat > new_bb$ur.lat) {
    new_bb = transform(new_bb, ll.lat = ur.lat, ur.lat = ll.lat)
  }

  new_bb_pix <- data.frame(
    ll.x = 1L + as.integer(floor((new_bb$ll.lon - bb$ll.lon) / (bb$ur.lon - bb$ll.lon) * d[[2L]])),
    ll.y = 1L + as.integer(floor((new_bb$ll.lat - bb$ll.lat) / (bb$ur.lat - bb$ll.lat) * d[[1L]])),
    ur.x = as.integer(ceiling((new_bb$ur.lon - bb$ll.lon) / (bb$ur.lon - bb$ll.lon) * d[[2L]])),
    ur.y = as.integer(ceiling((new_bb$ur.lat - bb$ll.lat) / (bb$ur.lat - bb$ll.lat) * d[[1L]]))
  )

  stopifnot(new_bb_pix$ll.x >= 1L)
  stopifnot(new_bb_pix$ll.y >= 1L)
  stopifnot(new_bb_pix$ur.x <= d[[2L]])
  stopifnot(new_bb_pix$ur.y <= d[[1L]])

  new_bb_pix_ll <- data.frame(
    ll.lat = (new_bb_pix$ll.y - 1) / d[[1L]] * (bb$ur.lat - bb$ll.lat) + bb$ll.lat,
    ll.lon = (new_bb_pix$ll.x - 1) / d[[2L]] * (bb$ur.lon - bb$ll.lon) + bb$ll.lon,
    ur.lat = (new_bb_pix$ur.y) / d[[1L]] * (bb$ur.lat - bb$ll.lat) + bb$ll.lat,
    ur.lon = (new_bb_pix$ur.x) / d[[2L]] * (bb$ur.lon - bb$ll.lon) + bb$ll.lon
  )

  list(
    pixels = new_bb_pix,
    corrected = new_bb_pix_ll
  )
}
