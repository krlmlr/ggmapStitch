context("Align")

test_that("Check sanity of transformations", {
  load("CH07.rda")

  d <- dim(CH07)
  bb <- attr(CH07, "bb")

  expect_equal(d %% 4, c(0, 0))

  bb_new <- transform(
    bb,
    ll.lat = ll.lat + 0.25 * (ur.lat - ll.lat),
    ur.lat = ll.lat + 0.75 * (ur.lat - ll.lat),
    ll.lon = ll.lon + 0.25 * (ur.lon - ll.lon),
    ur.lon = ll.lon + 0.75 * (ur.lon - ll.lon))

  bb_pix_new <- data.frame(
    ll.x = d[[2]] / 4 + 1,
    ll.y = d[[1]] * 3 / 4,
    ur.x = d[[2]] * 3 / 4,
    ur.y = d[[1]] / 4 + 1
  )

  aligned <- align_bb_to_pixels(CH07, bb_new)
  expect_equal(aligned$corrected, bb_new)
  expect_equal(aligned$pixels, bb_pix_new)
})


test_that("Moving border less than one pixel does not change bounding box", {
  load("CH07.rda")

  d <- dim(CH07)
  bb <- attr(CH07, "bb")

  # Establish comparison base
  bb_pix <- align_bb_to_pixels(CH07, bb)$pixels
  expect_equal(bb_pix, data.frame(ll.x = 1, ll.y = d[[1]], ur.x = d[[2]], ur.y = 1))

  check_unchanged <- function(bb_new) {
    aligned <- align_bb_to_pixels(CH07, bb_new)
    expect_equal(aligned$pixels, bb_pix)
    expect_equal(aligned$corrected, bb)
  }

  check_unchanged(bb)

  epsilon = 2 ** -16

  # Move left border almost one pixel to the right
  bb_new <- transform(bb, ll.lon = ll.lon + (1 - epsilon) / d[[2]] * (ur.lon - ll.lon))
  check_unchanged(bb_new)

  # Move right border almost one pixel to the left
  bb_new <- transform(bb, ur.lon = ur.lon - (1 - epsilon) / d[[2]] * (ur.lon - ll.lon))
  check_unchanged(bb_new)

  # Move top border almost one pixel to the bottom
  bb_new <- transform(bb, ur.lat = ur.lat - (1 - epsilon) / d[[1]] * (ur.lat - ll.lat))
  check_unchanged(bb_new)

  # Move bottom border almost one pixel to the top
  bb_new <- transform(bb, ll.lat = ll.lat + (1 - epsilon) / d[[1]] * (ur.lat - ll.lat))
  check_unchanged(bb_new)
})
