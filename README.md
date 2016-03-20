
<!-- README.md is generated from README.Rmd. Please edit that file -->


[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/0.1.0/active.svg)](http://www.repostatus.org/#active) 
[![Travis-CI Build Status](https://travis-ci.org/hrbrmstr/ggalt.svg?branch=master)](https://travis-ci.org/hrbrmstr/ggalt) 
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/ggalt)](http://cran.r-project.org/web/packages/ggalt) 
![downloads](http://cranlogs.r-pkg.org/badges/grand-total/ggalt)

`ggalt` : Extra Coordinate Systems, Geoms and Statistical Transformations for 'ggplot2'

A compendium of 'geoms', 'coords' and 'stats' for 'ggplot2', including splines, 1d and 2d densities, univariate average shifted histograms and a new map coordinate system based on the 'PROJ.4'-library.

The first three forays into this brave, new `ggplot2` world are _splines_! and being able to use the (much better) `KernSmooth::bkde` and `KernSmooth::bkde2D` for density plots and an initial port of the (still needing work) `coord_proj`.

The following functions are implemented:

- `coord_proj` : Like `coord_map` only better 😜
- `geom_xspline` : Connect control points/observations with an X-spline
- `stat_xspline` : Connect control points/observations with an X-spline
- `geom_bkde` :	Display a smooth density estimate (uses `KernSmooth::bkde`)
- `stat_bkde` :	Display a smooth density estimate (uses `KernSmooth::bkde`)
- `geom_bkde2d` :	Contours from a 2d density estimate. (uses `KernSmooth::bkde2D`)
- `stat_bkde2d` :	Contours from a 2d density estimate. (uses `KernSmooth::bkde2D`)
- `stat_ash` : Compute and display a univariate averaged shifted histogram (polynomial kernel) (uses `ash::ash1`/`ash::bin1`)
- `scale_color_pokemon` :
- `scale_fill_pokemon` : discrete pokemon scales (data taken from the hard work by the <http://www.pokegraphs.com/>)
- `byte_format`: + helpers. e.g. turn `10000` into `10 Kb`


### News

- Version 0.1.5.9000 - Pokemon discrete color scales!
- Version 0.1.2.9000 - Fixed bug with limits not working in coord_proj thx to @mstrimas 
- Version 0.1.1 - CRAN!
- Version 0.1.0.9000 - Tweaks for ggplot2 2.0 release
- Version 0.0.4.9000 - `stat_ash`
- Version 0.0.3.9000 - `coord_proj`! (requires my github copy of ggplot2 for now)
- Version 0.0.2.9005 - cleanup before blog post
- Version 0.0.2.9002 - working 2D density plots
- Version 0.0.2.9000 initial release

### Installation


```r
# you'll want to see the vignettes, trust me
install.packages("ggplot2")
install.packages("ggalt")
# OR: devtools::install_github("hrbrmstr/ggalt")
```



### Usage


```r
library(ggplot2)
library(gridExtra)
library(ggalt)

# current verison
packageVersion("ggalt")
#> [1] '0.2.0.9000'

set.seed(1492)
dat <- data.frame(x=c(1:10, 1:10, 1:10),
                  y=c(sample(15:30, 10), 2*sample(15:30, 10), 3*sample(15:30, 10)),
                  group=factor(c(rep(1, 10), rep(2, 10), rep(3, 10)))
)
```

### Splines!


```r
ggplot(dat, aes(x, y, group=group, color=group)) +
  geom_point() +
  geom_line()
```

<img src="README_figs/README-splines-1.png" title="" alt="" width="672" />

```r

ggplot(dat, aes(x, y, group=group, color=factor(group))) +
  geom_point() +
  geom_line() +
  geom_smooth(se=FALSE, linetype="dashed", size=0.5)
```

<img src="README_figs/README-splines-2.png" title="" alt="" width="672" />

```r

ggplot(dat, aes(x, y, group=group, color=factor(group))) +
  geom_point(color="black") +
  geom_smooth(se=FALSE, linetype="dashed", size=0.5) +
  geom_xspline(size=0.5)
```

<img src="README_figs/README-splines-3.png" title="" alt="" width="672" />

```r

ggplot(dat, aes(x, y, group=group, color=factor(group))) +
  geom_point(color="black") +
  geom_smooth(se=FALSE, linetype="dashed", size=0.5) +
  geom_xspline(spline_shape=-0.4, size=0.5)
```

<img src="README_figs/README-splines-4.png" title="" alt="" width="672" />

```r

ggplot(dat, aes(x, y, group=group, color=factor(group))) +
  geom_point(color="black") +
  geom_smooth(se=FALSE, linetype="dashed", size=0.5) +
  geom_xspline(spline_shape=0.4, size=0.5)
```

<img src="README_figs/README-splines-5.png" title="" alt="" width="672" />

```r

ggplot(dat, aes(x, y, group=group, color=factor(group))) +
  geom_point(color="black") +
  geom_smooth(se=FALSE, linetype="dashed", size=0.5) +
  geom_xspline(spline_shape=1, size=0.5)
```

<img src="README_figs/README-splines-6.png" title="" alt="" width="672" />

```r

ggplot(dat, aes(x, y, group=group, color=factor(group))) +
  geom_point(color="black") +
  geom_smooth(se=FALSE, linetype="dashed", size=0.5) +
  geom_xspline(spline_shape=0, size=0.5)
```

<img src="README_figs/README-splines-7.png" title="" alt="" width="672" />

```r

ggplot(dat, aes(x, y, group=group, color=factor(group))) +
  geom_point(color="black") +
  geom_smooth(se=FALSE, linetype="dashed", size=0.5) +
  geom_xspline(spline_shape=-1, size=0.5)
```

<img src="README_figs/README-splines-8.png" title="" alt="" width="672" />

#### Alternate (better) density plots


```r
# bkde

data(geyser, package="MASS")

ggplot(geyser, aes(x=duration)) + 
  stat_bkde(alpha=1/2)
#> Bandwidth not specified. Using '0.14', via KernSmooth::dpik.
```

<img src="README_figs/README-bkde_ash-1.png" title="" alt="" width="672" />

```r

ggplot(geyser, aes(x=duration)) +
  geom_bkde(alpha=1/2)
#> Bandwidth not specified. Using '0.14', via KernSmooth::dpik.
```

<img src="README_figs/README-bkde_ash-2.png" title="" alt="" width="672" />

```r

ggplot(geyser, aes(x=duration)) + 
  stat_bkde(bandwidth=0.25)
```

<img src="README_figs/README-bkde_ash-3.png" title="" alt="" width="672" />

```r

ggplot(geyser, aes(x=duration)) +
  geom_bkde(bandwidth=0.25)
```

<img src="README_figs/README-bkde_ash-4.png" title="" alt="" width="672" />

```r

set.seed(1492)
dat <- data.frame(cond = factor(rep(c("A","B"), each=200)), 
                   rating = c(rnorm(200),rnorm(200, mean=.8)))

ggplot(dat, aes(x=rating, color=cond)) + geom_bkde(fill="#00000000")
#> Bandwidth not specified. Using '0.36', via KernSmooth::dpik.
#> Bandwidth not specified. Using '0.31', via KernSmooth::dpik.
```

<img src="README_figs/README-bkde_ash-5.png" title="" alt="" width="672" />

```r

ggplot(dat, aes(x=rating, fill=cond)) + geom_bkde(alpha=0.3)
#> Bandwidth not specified. Using '0.36', via KernSmooth::dpik.
#> Bandwidth not specified. Using '0.31', via KernSmooth::dpik.
```

<img src="README_figs/README-bkde_ash-6.png" title="" alt="" width="672" />

```r

# ash

set.seed(1492)
dat <- data.frame(x=rnorm(100))
grid.arrange(ggplot(dat, aes(x)) + stat_ash(),
             ggplot(dat, aes(x)) + stat_bkde(),
             ggplot(dat, aes(x)) + stat_density(),
             nrow=3)
#> Estimate nonzero outside interval ab.
#> Bandwidth not specified. Using '0.43', via KernSmooth::dpik.
```

<img src="README_figs/README-bkde_ash-7.png" title="" alt="" width="672" />

```r

cols <- RColorBrewer::brewer.pal(3, "Dark2")
ggplot(dat, aes(x)) + 
  stat_ash(alpha=1/3, fill=cols[3]) + 
  stat_bkde(alpha=1/3, fill=cols[2]) + 
  stat_density(alpha=1/3, fill=cols[1]) + 
  geom_rug() +
  labs(x=NULL, y="density/estimate") +
  scale_x_continuous(expand=c(0,0)) +
  theme_bw() +
  theme(panel.grid=element_blank()) +
  theme(panel.border=element_blank())
#> Estimate nonzero outside interval ab.
#> Bandwidth not specified. Using '0.43', via KernSmooth::dpik.
```

<img src="README_figs/README-bkde_ash-8.png" title="" alt="" width="672" />

### Alternate 2D density plots


```r
m <- ggplot(faithful, aes(x = eruptions, y = waiting)) +
       geom_point() +
       xlim(0.5, 6) +
       ylim(40, 110)

m + geom_bkde2d(bandwidth=c(0.5, 4))
```

<img src="README_figs/README-bkde2d-1.png" title="" alt="" width="672" />

```r

m + stat_bkde2d(bandwidth=c(0.5, 4), aes(fill = ..level..), geom = "polygon")
```

<img src="README_figs/README-bkde2d-2.png" title="" alt="" width="672" />

### `coord_proj` LIVES! (still needs a teensy bit of work)


```r
world <- map_data("world")
#> 
#>  # ATTENTION: maps v3.0 has an updated 'world' map.        #
#>  # Many country borders and names have changed since 1990. #
#>  # Type '?world' or 'news(package="maps")'. See README_v3. #
world <- world[world$region != "Antarctica",]

gg <- ggplot()
gg <- gg + geom_map(data=world, map=world,
                    aes(x=long, y=lat, map_id=region))
gg <- gg + coord_proj("+proj=wintri")
gg
```

<img src="README_figs/README-coord_proj-1.png" title="" alt="" width="672" />

### ProPublica StateFace


```r
# Run show_stateface() to see the location of the TTF StateFace font
# You need to install it for it to work

set.seed(1492)
dat <- data.frame(state=state.abb,
                  x=sample(100, 50),
                  y=sample(100, 50),
                  col=sample(c("#b2182b", "#2166ac"), 50, replace=TRUE),
                  sz=sample(6:15, 50, replace=TRUE),
                  stringsAsFactors=FALSE)
gg <- ggplot(dat, aes(x=x, y=y))
gg <- gg + geom_stateface(aes(label=state, color=col, size=sz))
gg <- gg + scale_color_identity()
gg <- gg + scale_size_identity()
gg
```

<img src="README_figs/README-stateface-1.png" title="" alt="" width="672" />

### Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). 
By participating in this project you agree to abide by its terms.
