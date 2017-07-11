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
grid_map <- function(x, y) {
 cellFromXY.gridcol(raster(y), xyFromCell.gridcol(x))
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
