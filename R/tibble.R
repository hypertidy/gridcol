#' @name gridcol
#' @export
gc_tibble <- function(x, ...) {
  UseMethod("gc_tibble")
}
#' @name gridcol
#' @export
gc_tibble.BasicRaster <- function(x, ...) {
 dplyr::mutate(tabularaster::as_tibble(x), cellindex = gridcol(cellindex, x))
}

