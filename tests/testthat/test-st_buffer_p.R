test_that("st_buffer_p works", {
  library(sf)
  library(furrr)
  l1 = st_as_sfc("LINESTRING(0 0,1 5,4 5,5 2,8 2,9 4,4 6.5)")
  buff <- st_buffer(l1, dist = 1)

  plan(cluster, workers = 2)
  buffp <- pst_buffer(l1, dist = 1)
  plan(sequential)
  expect_in("sfc", class(buffp))
})
