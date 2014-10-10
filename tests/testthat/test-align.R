context("Align")


test_that("Alignment works correctly", {
  load("CH07.rda")

  bb <- attr(CH07, "bb")

  bb_pix <- align_bb_to_pixels(CH07, bb)$pixels

  expect_equal(align_bb_to_pixels(CH07, bb)$corrected, bb)
})
