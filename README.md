<!-- README.md is generated from README.Rmd. Please edit that file -->
`ggalt` : Extra Geoms, Stats and Coords for `ggplot2`

A package containing additional geoms, coords and stats for the revamped (late 2015) version of ggplot2.

The first three forays into this brave, new `ggplot2` world are *splines*! and being able to use the (much better) `KernSmooth::bkde` and `KernSmooth::bkde2D` for density plots.

*NOTE*

Until the new `ggplot2` version is on CRAN, you'll need to install it from github (see below).

The following functions are implemented:

-   `geom_xspline` : Connect control points/observations with an X-spline
-   `stat_xspline` : Connect control points/observations with an X-spline
-   `geom_bkde` : Display a smooth density estimate (uses `KernSmooth::bkde`)
-   `stat_bkde` : Display a smooth density estimate (uses `KernSmooth::bkde`)
-   `geom_bkde2d` : Contours from a 2d density estimate. (uses `KernSmooth::bkde2D`)
-   `stat_bkde2d` : Contours from a 2d density estimate. (uses `KernSmooth::bkde2D`)

### News

-   Version 0.0.2.9002 released - working 2D density plots
-   Version 0.0.2.9000 released

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
#> [1] '0.0.2.9004'

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
  stat_bkde(alpha=1/2)
#> Bandwidth not specified. Using '0.14', via KernSmooth::dpik.
```

<img src="README_figs/README-unnamed-chunk-4-9.png" title="" alt="" width="672" />

``` r

ggplot(geyser, aes(x=duration)) +
  geom_bkde(alpha=1/2)
#> Bandwidth not specified. Using '0.14', via KernSmooth::dpik.
```

<img src="README_figs/README-unnamed-chunk-4-10.png" title="" alt="" width="672" />

``` r

ggplot(geyser, aes(x=duration)) + 
  stat_bkde(bandwidth=0.25)
```

<img src="README_figs/README-unnamed-chunk-4-11.png" title="" alt="" width="672" />

``` r

ggplot(geyser, aes(x=duration)) +
  geom_bkde(bandwidth=0.25)
```

<img src="README_figs/README-unnamed-chunk-4-12.png" title="" alt="" width="672" />

``` r

set.seed(1492)
dat <- data.frame(cond = factor(rep(c("A","B"), each=200)), 
                   rating = c(rnorm(200),rnorm(200, mean=.8)))

ggplot(dat, aes(x=rating, color=cond)) + geom_bkde(alpha=0)
#> Bandwidth not specified. Using '0.36', via KernSmooth::dpik.
#> Bandwidth not specified. Using '0.31', via KernSmooth::dpik.
```

<img src="README_figs/README-unnamed-chunk-4-13.png" title="" alt="" width="672" />

``` r

ggplot(dat, aes(x=rating, fill=cond)) + geom_bkde(alpha=0.3)
#> Bandwidth not specified. Using '0.36', via KernSmooth::dpik.
#> Bandwidth not specified. Using '0.31', via KernSmooth::dpik.
```

<img src="README_figs/README-unnamed-chunk-4-14.png" title="" alt="" width="672" />

``` r


# 2D KernSmooth::bkde2D plots are a WIP

geyser_dat <- data.frame(x=geyser$duration, y=geyser$waiting)

ggplot(geyser_dat, aes(x, y)) +
  geom_point() +
  geom_bkde2d(bandwidth=c(0.7, 7))
```

<img src="README_figs/README-unnamed-chunk-4-15.png" title="" alt="" width="672" />

``` r

ggplot(geyser_dat, aes(x, y)) +
  geom_point() +
  stat_bkde2d(bandwidth=c(0.7, 7))
```

<img src="README_figs/README-unnamed-chunk-4-16.png" title="" alt="" width="672" />

### Test Results

``` r
library(ggalt)
library(testthat)

date()
#> [1] "Fri Sep 11 16:19:52 2015"

test_dir("tests/")
#> testthat results ========================================================================================================
#> OK: 0 SKIPPED: 0 FAILED: 0
#> 
#> DONE
```

### Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
