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
  .options = furrr::furrr_options(seed = TRUE),  ...)
  UseMethod("pst_buffer")

#' @export
pst_buffer.sfc <- function(
    x,
  rows_per_chunk = 1,
  .options = furrr::furrr_options(seed = TRUE),  ...){

  if(length(x) == 1 | future::nbrOfWorkers() == 1){
    message("length or workers == 1, defaulting to st_buffer")
    return(sf::st_buffer(x, ...))
  }

  # grab s2 usage from working environment
  use_s2 = sf::sf_use_s2()

  num_chunks <- ceiling(length(x) / rows_per_chunk)
  chunk_assn <- rep(1:num_chunks, each = rows_per_chunk, length.out = length(x))

  x <- split(x, f = chunk_assn)
  p <- progressr::progressor(along = x)

  x <- suppressMessages(furrr::future_map(x, function(x){
    p()
    sf::sf_use_s2(use_s2 = use_s2)
    return(sf::st_buffer(x, ...))
  }, .options = .options))

  x <- do.call(c, x)

  return(x)
}

#' @export
pst_buffer.sf <- function(x, ...) st_set_geometry(x, pst_buffer(st_geometry(x), ...))

#' @export
pst_buffer.sfg <- function() stop("not yet implemented")
