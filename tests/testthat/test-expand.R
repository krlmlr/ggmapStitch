load("CH07.rda")

# Check on a square and a non-square map
map_list <- list(
  `square` = CH07,
  `non-square` = crop_map(CH07, data.frame(ll.lat = 45.68, ur.lat = 47.9, ll.lon = 5.79, ur.lon = 10.7))
)

target_bb <- data.frame(ll.lat = 30, ur.lat = 60, ll.lon = 2, ur.lon = 15)

lapply(
  names(map_list),

  function(map_name) {
    context(sprintf("Expansion: %s", map_name))

    map <- map_list[[map_name]]

    test_that("Check squareness", {
      d <- dim(map)
      if (map_name == "square")
        expect_that(diff(d), equals(0))
      else
        expect_that(diff(d), not(equals(0)))
    })

    test_that("Sensible return values", {
      bb <- attr(map, "bb")
      g <- expand_bb(bb, target_bb)

      ulon <- unique(g$lon)
      ulat <- unique(g$lat)

      # kx/ky and lon/lat correspond
      expect_equal(unique(g[c("kx", "lon")])$kx, unique(g$kx))
      expect_equal(unique(g[c("kx", "lon")])$lon, ulon)

      expect_equal(unique(g[c("ky", "lat")])$ky, unique(g$ky))
      expect_equal(unique(g[c("ky", "lat")])$lat, ulat)

      # lon/lat are originally sorted
      expect_equal(ulon, sort(ulon))
      expect_equal(ulat, sort(ulat))

      # intermediate points are inside target bounding box
      expect_equal(.bincode(ulon[-1] - diff(ulon) / 2, c(target_bb$ll.lon, target_bb$ur.lon), include.lowest = TRUE), rep(1, length(ulon) - 1))
      expect_equal(.bincode(ulat[-1] - diff(ulat) / 2, c(target_bb$ll.lat, target_bb$ur.lat), include.lowest = TRUE), rep(1, length(ulat) - 1))
    })
  }
)
