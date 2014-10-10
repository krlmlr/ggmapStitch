context("Align")


test_that("Alignment works correctly", {
  load("CH07.rda")

  d <- dim(CH07)
  bb <- attr(CH07, "bb")

  bb_pix <- align_bb_to_pixels(CH07, bb)$pixels
  expect_equal(bb_pix, data.frame(ll.x = 1, ll.y = d[[1]], ur.x = d[[2]], ur.y = 1))

  expect_equal(align_bb_to_pixels(CH07, bb)$corrected, bb)
})
