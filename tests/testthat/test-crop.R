load("CH07.rda")
set.seed(123)

# Check on a square and a non-square map
map_list <- list(
  `square` = CH07,
  `non-square` = crop_map(CH07, data.frame(ll.lat = 45.68, ur.lat = 47.9, ll.lon = 5.79, ur.lon = 10.7))
)

lapply(
  names(map_list),

  function(map_name) {
    context(sprintf("Cropping: %s", map_name))

    map <- map_list[[map_name]]

    test_that("Cropping is transitive", {
      bb <- attr(map, "bb")

      N <- 8
      bb.new <- with(bb, data.frame(ll.lat = ur.lat - (ur.lat - ll.lat) * cumprod(runif(N, 0.9, 0.96)),
                                    ll.lon = ur.lon - (ur.lon - ll.lon) * cumprod(runif(N, 0.9, 0.96)),
                                    ur.lat = ll.lat - (ll.lat - ur.lat) * cumprod(runif(N, 0.9, 0.96)),
                                    ur.lon = ll.lon - (ll.lon - ur.lon) * cumprod(runif(N, 0.9, 0.96)),
                                    seq = seq_len(N)))

      bb.new.m <- subset(merge(bb.new, bb.new, by = c(), suffixes = c(".int", "")), seq.int < seq)

      lapply(
        seq_len(nrow(bb.new.m)),
        function(x) {
          m1 <- crop_map(map, setNames(bb.new.m[x,1:4], bb_names_out))
          m2 <- crop_map(m1, bb.new.m[x,6:9])
          m <- crop_map(map, bb.new.m[x,6:9])
          expect_identical(as.matrix(m2), as.matrix(m))
        }
      )
    })
  }
)
