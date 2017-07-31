library(raadtools)

sst <- readsst(latest = TRUE, xylim = extent(0, 180, -90, 0))


## let's get tidy
library(tabularaster)
as_gridtabl <- function (x, dim = nlayers(x) > 1L, value = TRUE,
          split_date = FALSE, ...)
{
  dimindex <- raster::getZ(x)
  if (is.null(dimindex)) {
    dimindex <- seq(raster::nlayers(x))
    if (split_date) {
      e1 <- try(as.Date(dimindex), silent = TRUE)
      e2 <- try(as.POSIXct(dimindex, tz = "GMT"), silent = TRUE)
      if ((inherits(e1, "try-error") & inherits(e2, "try-error")) |
          any(is.na(range(e1)))) {
        warning("cannot 'split_date', convert 'getZ(x)' not convertible to a Date or POSIXct")
        split_date <- FALSE
      }
    }
  }
  cellvalue <- cellindex <- NULL
  if (value)
    cellvalue <- as.vector(values(x))

    cellindex <- gridcol::gridcol(x)
      ##rep(seq(raster::ncell(x)), raster::nlayers(x))
  d <- dplyr::bind_cols(cellvalue = cellvalue, cellindex = cellindex)
  if (dim) {
    dimindex <- rep(dimindex, each = ncell(x))
    if (split_date) {
      d <- dplyr::mutate(d, year = as.integer(format(dimindex,
                                                     "%Y")), month = as.integer(format(dimindex, "%m")),
                         day = as.integer(format(dimindex, "%d")))
    }
    else {
      d[["dimindex"]] <- dimindex
    }
  }
  d
}


find_gridcol <- function(x) {
  for (i in seq_along(x)) {
    if (inherits(x[[i]], "gridcol")) return(i)
  }
  warning("no gridcol found!")
  NULL

}
raster.tbl_df <- function(x, rcol = 1) {
  gci <- find_gridcol(x)
  gc <- x[[gci]]
  r <- setValues(raster(gc), NA_real_)
  x <- x[-gci]
  r[unclass(gc)] <- x[[rcol]]
  r
}

raster.tbl_df(tib)


poly_over <- function(.data, poly) {
  raster::extract(raster.tbl_df(.data))
}
