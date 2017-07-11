#' raster methods
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
raster.gridspec <- function(x, ...) {
  do.call(raster, dplyr::rename(x, xmn = xmin, xmx = xmax, ymn = ymin, ymx = ymax, nrows = nrow, ncols = ncol))
}
setMethod("raster", "gridspec", raster.gridspec)

#' raster methods
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples

raster.gridcol <- function(x, ...) {
  raster(grid_spec(x))
}
setMethod("raster", "gridcol", raster.gridcol)
#' @name xyFromCell
#' @export
#' @examples
#' y <- gridcol(1, raster(volcano))
#' plot(raster(volcano))
#' x <- gridcol(1:12, setExtent(raster(matrix(1:12, 3)), extent(0.4, 0.7, 0.2, 0.6)))
grid_map <- function(x, y) {
 cellFromXY.gridcol(y, xyFromCell.gridcol(x))
}
cellFromXY.gridcol <- function(x, ...) {
  cellFromXY(raster(x), x)
}
#setMethod("cellFromXY", "gridcol", cellFromXY.gridcol)
#' @name xyFromCell
#' @export
xyFromCell.gridcol <- function(x, ...) {
  xyFromCell(raster(x), x)
}

#setMethod("xyFromCell", "gridcol", xyFromCell.gridcol)
