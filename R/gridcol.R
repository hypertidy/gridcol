#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' library(raster)
#' grid_spec(raster(volcano))
#' grid_spec(grid_spec(raster(volcano)))
#' grid_spec(grid_spec(volcano))
#' gridcol(1, raster(volcano))
grid_spec <- function(x, ...) {
  UseMethod("grid_spec")
}
#' @name grid_spec
#' @export
grid_spec.default <- function(x, ...) {
  ras <- raster::raster(x)
  if (identical(raster::extent(ras), raster::extent(raster::raster(matrix(1))))) {
    ras <- raster::setExtent(ras, raster::extent(0, ncol(ras), 0, nrow(ras)))
  }
  grid_spec(ras, ...)
}
#' @name grid_spec
#' @export
grid_spec.BasicRaster <- function(x, ...) {
  out <- tibble::tibble(xmin = raster::xmin(x), xmax  = raster::xmax(x),
                 ymin = raster::ymin(x), ymax = raster::ymax(x),
                 nrow = raster::nrow(x), ncol = raster::ncol(x),
                 crs = raster::projection(x))
  structure(out, class = c("gridspec", class(out)))
}
#' @name grid_spec
#' @export
grid_spec.gridcol <- function(x, ...) {
  attr(x, "grid_spec")
}
#' @name grid_spec
#' @export
grid_spec.gridspec <- function(x, ...) {
  x
}

#' Title
#'
#' @param x
#' @param grid
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' library(raster)
#' gridcol(1, raster(volcano))
#' gridcol(1, volcano)
#' gridcol(seq_len(prod(dim(volcano))), volcano)
gridcol <- function(x, grid,  ...) {
  UseMethod("gridcol")
}
#' @name gridcol
#' @export
gridcol.numeric <- function(x, grid, ...) {
  dif <- x - as.integer(x)
  if (any(abs(dif) > 0)) warning("input is floating point, truncated to integer")
  x <- as.integer(x)

  gridcol(x, grid, ...)
}
#' @name gridcol
#' @export
gridcol.integer <- function(x, grid, ...) {

  gs <- grid_spec(grid)
  if (any(x < 1)) warning("cell values less than 1")
  if (any(x > prod(gs$nrow * gs$ncol))) warning("cell value greater than length of grid")
  structure(x, class = "gridcol", grid_spec = gs)
}
#' @name gridcol
#' @export
gridcol.RasterLayer <- function(x, grid, ...) {
  if (!missing(grid)) warning("grid input (second argument) is ignored")
  gs <- grid_spec(x)
  gridcol(seq_len(ncell(x)), gs)
}


#' gridcol methods
#'
#' @param x
#' @param i
#'
#' @return
#' @export
#'
#' @examples
`[.gridcol` <- function(x, i)  {
  gs <- grid_spec(x)
  gridcol(unclass(x)[i], gs)
}
#' @name [.gridcol
#' @export
`[[.gridcol` <- function(x, i) {
  if (length(i) > 1) stop("i must be of length 1")
  x[i]
}

#' Title
#'
#' @param x
#' @param i
#' @param value
#'
#' @return
#' @export
#'
#' @examples
`[<-.gridcol` <- function(x, i, value) {
  out <- x
  out[i] <- value
  gridcol(out, grid_spec(x))
}
#' @name [.gridcol
#' @export
