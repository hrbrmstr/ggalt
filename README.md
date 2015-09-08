<!-- README.md is generated from README.Rmd. Please edit that file -->
`ggalt` : Extra Geoms, Stats and Coords for `ggplot2`

A package containing additional geoms, coords and stats for the revamped (late 2015) version of ggplot2.

The first two forays into this brave, new `ggplot2` world are *splines*! and being able to use the (much better) `KernSmooth::bkde` for density plots.

*NOTE*

Until the new `ggplot2` version is on CRAN, you'll need to install it from github (see below).

The following functions are implemented:

-   `geom_xspline` : Connect control points/observations with an X-spline
-   `stat_xspline` : Connect control points/observations with an X-spline
-   `geom_bkde` : Display a smooth density estimate (uses `KernSmooth::bkde`)
-   `stat_bkde` : Display a smooth density estimate (uses `KernSmooth::bkde`)

### News

-   Version 0.0.1.9000 released

### Installation

``` r
# you'll want to see the vignettes, trust me
devtools::install_github("hadley/ggplot2", build_vignettes=TRUE)
devtools::install_github("hrbrmstr/ggalt")
```

### Usage

``` r
library(ggalt)
#> Loading required package: ggplot2

# current verison
packageVersion("ggalt")
#> [1] '0.0.1.9001'

set.seed(1492)
dat <- data.frame(x=c(1:10, 1:10, 1:10),
                  y=c(sample(15:30, 10), 2*sample(15:30, 10), 3*sample(15:30, 10)),
                  group=factor(c(rep(1, 10), rep(2, 10), rep(3, 10)))
)

ggplot(dat, aes(x, y, group=group, color=group)) +
  geom_point() +
  geom_line()
```

<img src="README_figs/README-unnamed-chunk-4-1.png" title="" alt="" width="672" />

``` r

ggplot(dat, aes(x, y, group=group, color=factor(group))) +
  geom_point() +
  geom_line() +
  geom_smooth(se=FALSE, linetype="dashed", size=0.5)
```

<img src="README_figs/README-unnamed-chunk-4-2.png" title="" alt="" width="672" />

``` r

ggplot(dat, aes(x, y, group=group, color=factor(group))) +
  geom_point(color="black") +
  geom_smooth(se=FALSE, linetype="dashed", size=0.5) +
  geom_xspline(size=0.5)
```

<img src="README_figs/README-unnamed-chunk-4-3.png" title="" alt="" width="672" />

``` r

ggplot(dat, aes(x, y, group=group, color=factor(group))) +
  geom_point(color="black") +
  geom_smooth(se=FALSE, linetype="dashed", size=0.5) +
  geom_xspline(spline_shape=-0.4, size=0.5)
```

<img src="README_figs/README-unnamed-chunk-4-4.png" title="" alt="" width="672" />

``` r

ggplot(dat, aes(x, y, group=group, color=factor(group))) +
  geom_point(color="black") +
  geom_smooth(se=FALSE, linetype="dashed", size=0.5) +
  geom_xspline(spline_shape=0.4, size=0.5)
```

<img src="README_figs/README-unnamed-chunk-4-5.png" title="" alt="" width="672" />

``` r

ggplot(dat, aes(x, y, group=group, color=factor(group))) +
  geom_point(color="black") +
  geom_smooth(se=FALSE, linetype="dashed", size=0.5) +
  geom_xspline(spline_shape=1, size=0.5)
```

<img src="README_figs/README-unnamed-chunk-4-6.png" title="" alt="" width="672" />

``` r

ggplot(dat, aes(x, y, group=group, color=factor(group))) +
  geom_point(color="black") +
  geom_smooth(se=FALSE, linetype="dashed", size=0.5) +
  geom_xspline(spline_shape=0, size=0.5)
```

<img src="README_figs/README-unnamed-chunk-4-7.png" title="" alt="" width="672" />

``` r

ggplot(dat, aes(x, y, group=group, color=factor(group))) +
  geom_point(color="black") +
  geom_smooth(se=FALSE, linetype="dashed", size=0.5) +
  geom_xspline(spline_shape=-1, size=0.5)
```

<img src="README_figs/README-unnamed-chunk-4-8.png" title="" alt="" width="672" />

``` r

# Better density plots

data(geyser, package="MASS")

ggplot(geyser, aes(x=duration)) + 
  stat_bkde(bandwidth=0.25)
```

<img src="README_figs/README-unnamed-chunk-4-9.png" title="" alt="" width="672" />

``` r

ggplot(geyser, aes(x=duration)) +
  geom_bkde(bandwidth=0.25)
```

<img src="README_figs/README-unnamed-chunk-4-10.png" title="" alt="" width="672" />

### Test Results

``` r
library(ggalt)
library(testthat)

date()
#> [1] "Tue Sep  8 16:16:44 2015"

test_dir("tests/")
#> testthat results ========================================================================================================
#> OK: 0 SKIPPED: 0 FAILED: 0
#> 
#> DONE
```

### Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
