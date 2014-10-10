load("CH07.rda")

# Check on a square and a non-square map
map_list <- list(
  `square` = CH07,
  `non-square` = crop_map(CH07, data.frame(ll.lat = 45.68, ur.lat = 47.9, ll.lon = 5.79, ur.lon = 10.7)))

lapply(
  names(map_list),

  function(map_name) {
    context(sprintf("Alignment: %s", map_name))

    map <- map_list[[map_name]]

    test_that("Check squareness", {
      d <- dim(map)
      if (map_name == "square")
        expect_that(diff(d), equals(0))
      else
        expect_that(diff(d), not(equals(0)))
    })

    test_that("Check sanity of transformations", {
      d <- dim(map)
      bb <- attr(map, "bb")

      expect_equal(d[[1]] %% 4, 0)
      expect_equal(d[[2]] %% 4, 0)

      bb_new <- transform(
        bb,
        ll.lat = ll.lat + 0.25 * (ur.lat - ll.lat),
        ur.lat = ll.lat + 0.5  * (ur.lat - ll.lat),
        ll.lon = ll.lon + 0.25 * (ur.lon - ll.lon),
        ur.lon = ll.lon + 0.5  * (ur.lon - ll.lon))

      bb_pix_new <- data.frame(
        ll.x = d[[2]]     / 4 + 1,
        ll.y = d[[1]]     / 4 + 1,
        ur.x = d[[2]] * 2 / 4,
        ur.y = d[[1]] * 2 / 4
      )

      aligned <- align_bb_to_pixels(map, bb_new)
      expect_equal(aligned$corrected, bb_new)
      expect_equal(aligned$pixels, bb_pix_new)

      expect_equal(unique(unlist(lapply(aligned$pixels, class))), "integer")
    })

    test_that("Moving border less than one pixel does not change bounding box; can mirror bounding box", {
      d <- dim(map)
      bb <- attr(map, "bb")

      # Establish comparison base
      bb_pix <- align_bb_to_pixels(map, bb)$pixels
      expect_equal(bb_pix, data.frame(ll.x = 1, ll.y = 1, ur.x = d[[2]], ur.y = d[[1]]))

      check_unchanged <- function(bb_new) {
        aligned <- align_bb_to_pixels(map, bb_new)
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

      # Can mirror horizontally
      bb_new <- transform(bb, ll.lat = ur.lat, ur.lat = ll.lat)
      check_unchanged(bb_new)

      # Can mirror vertically
      bb_new <- transform(bb, ll.lon = ur.lon, ur.lon = ll.lon)
      check_unchanged(bb_new)
    })

    test_that("Can't enlarge bounding box", {
      d <- dim(map)
      bb <- attr(map, "bb")

      epsilon <- 2 ** -16

      bb_new <- transform(bb, ll.lat = ll.lat - epsilon * (ur.lat - ll.lat))
      expect_error(align_bb_to_pixels(map, bb_new))
      bb_new <- transform(bb, ur.lat = ur.lat + epsilon * (ur.lat - ll.lat))
      expect_error(align_bb_to_pixels(map, bb_new))
      bb_new <- transform(bb, ll.lon = ll.lon - epsilon * (ur.lon - ll.lon))
      expect_error(align_bb_to_pixels(map, bb_new))
      bb_new <- transform(bb, ur.lon = ur.lon + epsilon * (ur.lon - ll.lon))
      expect_error(align_bb_to_pixels(map, bb_new))
    })
  }
)
