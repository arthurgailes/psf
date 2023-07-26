#' Combine sf and furrr to parallelize sf functions
#'
#' @param x object of class sfg, sfc or sf
#' @param rows_per_chunk The data.frame will be split into chunks of this many
#' rows. Default is 1 (rowwise).
#' @inheritParams furrr::future_map
#'
#' @param ... arguments passed on to `sf::st_buffer`
#'
#' @export
pst_buffer <- function(
  x,
  rows_per_chunk = 1,
  .options = furrr::furrr_options(),  ...){

  # grab s2 usage from working environment
  use_s2 = sf::sf_use_s2()

  num_chunks <- ceiling(nrow(x) / rows_per_chunk)
  chunk_assn <- rep(1:num_chunks, each = rows_per_chunk, length.out = nrow(x))

  x <- split(x, f = chunk_assn)
  p <- progressr::progressor(along = x)

  x <- furrr::future_map_dfr(x, function(x){
    p()
    sf::sf_use_s2(use_s2 = use_s2) |> suppressMessages()
    return(sf::st_buffer(x, ...))
  }, .options = .options)

  return(x)
}
