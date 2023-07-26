test_that("pst_buffer works", {
  library(sf)
  library(furrr)

  nc = st_read(system.file("shape/nc.shp", package="sf")) |>
    st_transform(3857)
  buff <- st_buffer(nc, dist = 1)

  plan(cluster, workers = 2)
  buffp <- pst_buffer(nc, dist = 1)
  plan(sequential)
  expect_in("sf", class(buffp))

  diff <- st_difference(st_union(buff), st_union(buffp)) |>
    suppressMessages()

  expect_equal(buff, buffp)
})
