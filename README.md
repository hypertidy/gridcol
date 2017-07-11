
<!-- README.md is generated from README.Rmd. Please edit that file -->
gridcol
=======

The goal of gridcol is to store *the abstraction of* a raster directly with an atomic vector in R.

Whaat
=====

I'm constantly following this kind of pattern:

``` r
dummy <- setValues(raster(<something or other>), NA)

xydata <- <points-or-lines-or-polygons-or...>

cells <- tibble(cell = cellFromXY(dummy, xydata))

## now, scan the entire history of remote sensing data
extract(raster(file_i), cells$cell)
```

I literally do this every day. Today I dug up some oldish code that makes a huuge grid, then a local window out of that, and then a third raster that was to contain an aggregation of finer pixels within it.

``` r
huge_grid <- raster(extent(-180, 180, -90, 90), res = 0.00001)

local_grid <- crop(huge_grid, extent(147, 147.01, -42, -41.99))

coarse_grid <- raster(extent(huge_grid), res = 1)

## now, build a map between local and coarse
tib <- tibble(local_cell = cellFromXY(huge_grid, coordinates(local_grid)), 
              coarse_cell = cellFromXY(coarse_grid, coordinates(local_grid)))
              
```

This is great, I can perform extractions or bin intensity of local data at very high resolution on the local grid, without burning down the compute node, and then I can group by the coarse grid cells and sum up the lower level details, and then map those aggregates at global scale, in a consistent structure that I can use easily.

But, I have to have two rasters lying around, their cell numbers are inextricably tied to their extent, dimension and coordinate system, and even while local and huge are from the "same grid" they don't share cell numbers for the "same" cells.

Hence, vectors with embedded raster specifications. We'll see if it works! See a similarish attempt from a rectlinear axis context (NetCDF-style). <https://github.com/hypertidy/discrete>

Installation
------------

You can install gridcol from github with:

``` r
# install.packages("devtools")
devtools::install_github("hypertidy/gridcol")
```

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
library(gridcol)
library(raster)
#> Loading required package: sp
(g <- gridcol(1:12, raster(matrix(1:12, 3), crs = "+proj=sinu +ellps=sphere +a=1 +b=1")))
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12
#> attr(,"class")
#> [1] "gridcol"
#> attr(,"grid_spec")
#> # A tibble: 1 x 7
#>    xmin  xmax  ymin  ymax  nrow  ncol                                crs
#> * <dbl> <dbl> <dbl> <dbl> <int> <int>                              <chr>
#> 1     0     1     0     1     3     4 +proj=sinu +ellps=sphere +a=1 +b=1
raster(g)
#> class       : RasterLayer 
#> dimensions  : 3, 4, 12  (nrow, ncol, ncell)
#> resolution  : 0.25, 0.3333333  (x, y)
#> extent      : 0, 1, 0, 1  (xmin, xmax, ymin, ymax)
#> coord. ref. : +proj=sinu +ellps=sphere +a=1 +b=1

xyFromCell(raster(g), 4:5)
#>          x         y
#> [1,] 0.875 0.8333333
#> [2,] 0.125 0.5000000
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:raster':
#> 
#>     intersect, select, union
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(tibble)
(tib <- tibble(cell = 1:ncell(raster(g))) %>% inner_join(tibble(cell = g[6:9])))
#> Joining, by = "cell"
#> Warning: Column `cell` has different attributes on LHS and RHS of join
#> # A tibble: 4 x 1
#>    cell
#>   <int>
#> 1     6
#> 2     7
#> 3     8
#> 4     9


tibble(cell = g) %>% pull(cell) %>% raster()
#> class       : RasterLayer 
#> dimensions  : 3, 4, 12  (nrow, ncol, ncell)
#> resolution  : 0.25, 0.3333333  (x, y)
#> extent      : 0, 1, 0, 1  (xmin, xmax, ymin, ymax)
#> coord. ref. : +proj=sinu +ellps=sphere +a=1 +b=1
```

(And, well, why not recursive vectors too? Grids are so much more than the affine shadows we get told about in GIS, ragged arrays are also under utilized in R, the L3bin structures for NASA ocean colour an obvious example.)

Code
====

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
