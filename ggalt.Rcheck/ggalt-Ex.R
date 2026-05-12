pkgname <- "ggalt"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "ggalt-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('ggalt')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("annotate_textp")
### * annotate_textp

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: annotate_textp
### Title: Text annotations in plot coordinate system
### Aliases: annotate_textp

### ** Examples

p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
p <- p + geom_smooth(method = "lm", se = FALSE)
p + annotate_textp(x = 0.9, y = 0.35, label="A relative linear\nrelationship", hjust=1, color="red")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("annotate_textp", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("annotation_ticks")
### * annotation_ticks

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: annotation_ticks
### Title: Annotation: tick marks
### Aliases: annotation_ticks

### ** Examples


p <- ggplot(msleep, aes(bodywt, brainwt)) + geom_point()

# Default behavior

# add identity scale minor ticks on y axis
p + annotation_ticks(sides = 'l')

# add identity scale minor ticks on x,y axis
p + annotation_ticks(sides = 'lb')

# Control number of minor ticks of each side independently

# add identity scale minor ticks on x,y axis
p + annotation_ticks(sides = 'lb', ticks_per_base = c(10,5))

# log10 scale
p1 <- p + scale_x_log10()

# add minor ticks on log10 scale
p1 + annotation_ticks(sides = 'b', scale = 'log10')

# add minor ticks on both scales
p1 + annotation_ticks(sides = 'lb', scale = c('identity','log10'))

# add minor ticks on both scales, but force x axis to be identity
p1 + annotation_ticks(sides = 'lb', scale = 'identity')

# log scale
p2 <- p + scale_x_continuous(trans = 'log')

# add minor ticks on log scale
p2 + annotation_ticks(sides = 'b', scale = 'log')

# add minor ticks on both scales
p2 + annotation_ticks(sides = 'lb', scale = c('identity','log'))

# add minor ticks on both scales, but force x axis to be identity
p2 + annotation_ticks(sides = 'lb', scale = 'identity')




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("annotation_ticks", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("byte_format")
### * byte_format

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: byte_format
### Title: Bytes formatter: convert to byte measurement and display symbol.
### Aliases: byte_format Kb Mb Gb bytes

### ** Examples

byte_format()(sample(3000000000, 10))
bytes(sample(3000000000, 10))
Kb(sample(3000000000, 10))
Mb(sample(3000000000, 10))
Gb(sample(3000000000, 10))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("byte_format", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_bkde")
### * geom_bkde

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_bkde
### Title: Display a smooth density estimate.
### Aliases: geom_bkde stat_bkde

### ** Examples

data(geyser, package="MASS")

ggplot(geyser, aes(x=duration)) +
  stat_bkde(alpha=1/2)

ggplot(geyser, aes(x=duration)) +
  geom_bkde(alpha=1/2)

ggplot(geyser, aes(x=duration)) +
 stat_bkde(bandwidth=0.25)

ggplot(geyser, aes(x=duration)) +
  geom_bkde(bandwidth=0.25)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_bkde", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_bkde2d")
### * geom_bkde2d

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_bkde2d
### Title: Contours from a 2d density estimate.
### Aliases: geom_bkde2d stat_bkde2d

### ** Examples

m <- ggplot(faithful, aes(x = eruptions, y = waiting)) +
       geom_point() +
       xlim(0.5, 6) +
       ylim(40, 110)

m + geom_bkde2d(bandwidth=c(0.5, 4))

m + stat_bkde2d(bandwidth=c(0.5, 4), aes(fill = ..level..), geom = "polygon")

# If you map an aesthetic to a categorical variable, you will get a
# set of contours for each value of that variable
set.seed(4393)
dsmall <- diamonds[sample(nrow(diamonds), 1000), ]
d <- ggplot(dsmall, aes(x, y)) +
       geom_bkde2d(bandwidth=c(0.5, 0.5), aes(colour = cut))
d

# If we turn contouring off, we can use use geoms like tiles:
d + stat_bkde2d(bandwidth=c(0.5, 0.5), geom = "raster",
                aes(fill = ..density..), contour = FALSE)

# Or points:
d + stat_bkde2d(bandwidth=c(0.5, 0.5), geom = "point",
                aes(size = ..density..),  contour = FALSE)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_bkde2d", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_cartogram")
### * geom_cartogram

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_cartogram
### Title: Map polygons layer enabling the display of show statistical
###   information
### Aliases: geom_cartogram

### ** Examples

## Not run: 
##D # When using geom_polygon, you will typically need two data frames:
##D # one contains the coordinates of each polygon (positions),  and the
##D # other the values associated with each polygon (values).  An id
##D # variable links the two together
##D 
##D ids <- factor(c("1.1", "2.1", "1.2", "2.2", "1.3", "2.3"))
##D 
##D values <- data.frame(
##D   id = ids,
##D   value = c(3, 3.1, 3.1, 3.2, 3.15, 3.5)
##D )
##D 
##D positions <- data.frame(
##D   id = rep(ids, each = 4),
##D   x = c(2, 1, 1.1, 2.2, 1, 0, 0.3, 1.1, 2.2, 1.1, 1.2, 2.5, 1.1, 0.3,
##D   0.5, 1.2, 2.5, 1.2, 1.3, 2.7, 1.2, 0.5, 0.6, 1.3),
##D   y = c(-0.5, 0, 1, 0.5, 0, 0.5, 1.5, 1, 0.5, 1, 2.1, 1.7, 1, 1.5,
##D   2.2, 2.1, 1.7, 2.1, 3.2, 2.8, 2.1, 2.2, 3.3, 3.2)
##D )
##D 
##D ggplot() +
##D   geom_cartogram(aes(x, y, map_id = id), map = positions, data=positions)
##D 
##D ggplot() +
##D   geom_cartogram(aes(x, y, map_id = id), map = positions, data=positions) +
##D   geom_cartogram(data=values, map=positions, aes(fill = value, map_id=id))
##D 
##D ggplot() +
##D   geom_cartogram(aes(x, y, map_id = id), map = positions, data=positions) +
##D   geom_cartogram(data=values, map=positions, aes(fill = value, map_id=id)) +
##D   ylim(0, 3)
##D 
##D # Better example
##D crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
##D crimesm <- reshape2::melt(crimes, id = 1)
##D 
##D if (require(maps)) {
##D 
##D   states_map <- map_data("state")
##D 
##D   ggplot() +
##D     geom_cartogram(aes(long, lat, map_id = region), map = states_map, data=states_map) +
##D     geom_cartogram(aes(fill = Murder, map_id = state), map=states_map, data=crimes)
##D 
##D   last_plot() + coord_map("polyconic")
##D 
##D   ggplot() +
##D     geom_cartogram(aes(long, lat, map_id=region), map = states_map, data=states_map) +
##D     geom_cartogram(aes(fill = value, map_id=state), map = states_map, data=crimesm) +
##D     coord_map("polyconic") +
##D     facet_wrap( ~ variable)
##D }
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_cartogram", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_dumbbell")
### * geom_dumbbell

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_dumbbell
### Title: Dumbbell charts
### Aliases: geom_dumbbell

### ** Examples

library(ggplot2)

df <- data.frame(trt=LETTERS[1:5], l=c(20, 40, 10, 30, 50), r=c(70, 50, 30, 60, 80))

ggplot(df, aes(y=trt, x=l, xend=r)) +
  geom_dumbbell(size=3, color="#e3e2e1",
                colour_x = "#5b8124", colour_xend = "#bad744",
                dot_guide=TRUE, dot_guide_size=0.25) +
  labs(x=NULL, y=NULL, title="ggplot2 geom_dumbbell with dot guide") +
  theme_minimal() +
  theme(panel.grid.major.x=element_line(size=0.05))

## with vertical dodging
df2 <- data.frame(trt = c(LETTERS[1:5], "D"),
                 l = c(20, 40, 10, 30, 50, 40),
                 r = c(70, 50, 30, 60, 80, 70))

ggplot(df2, aes(y=trt, x=l, xend=r)) +
  geom_dumbbell(size=3, color="#e3e2e1",
                colour_x = "#5b8124", colour_xend = "#bad744",
                dot_guide=TRUE, dot_guide_size=0.25,
                position=position_dodgev(height=0.4)) +
  labs(x=NULL, y=NULL, title="ggplot2 geom_dumbbell with dot guide") +
  theme_minimal() +
  theme(panel.grid.major.x=element_line(size=0.05))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_dumbbell", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_encircle")
### * geom_encircle

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_encircle
### Title: Automatically enclose points in a polygon
### Aliases: geom_encircle

### ** Examples

d <- data.frame(x=c(1,1,2),y=c(1,2,2)*100)

gg <- ggplot(d,aes(x,y))
gg <- gg + scale_x_continuous(expand=c(0.5,1))
gg <- gg + scale_y_continuous(expand=c(0.5,1))

gg + geom_encircle(s_shape=1, expand=0) + geom_point()

gg + geom_encircle(s_shape=1, expand=0.1, colour="red") + geom_point()

gg + geom_encircle(s_shape=0.5, expand=0.1, colour="purple") + geom_point()

gg + geom_encircle(data=subset(d, x==1), colour="blue", spread=0.02) +
  geom_point()

gg +geom_encircle(data=subset(d, x==2), colour="cyan", spread=0.04) +
  geom_point()

gg <- ggplot(mpg, aes(displ, hwy))
gg + geom_encircle(data=subset(mpg, hwy>40)) + geom_point()
gg + geom_encircle(aes(group=manufacturer)) + geom_point()
gg + geom_encircle(aes(group=manufacturer,fill=manufacturer),alpha=0.4)+
       geom_point()
gg + geom_encircle(aes(group=manufacturer,colour=manufacturer))+
       geom_point()

ss <- subset(mpg,hwy>31 & displ<2)

gg + geom_encircle(data=ss, colour="blue", s_shape=0.9, expand=0.07) +
  geom_point() + geom_point(data=ss, colour="blue")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_encircle", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_lollipop")
### * geom_lollipop

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_lollipop
### Title: Lollipop charts
### Aliases: geom_lollipop

### ** Examples

df <- data.frame(trt=LETTERS[1:10],
                 value=seq(100, 10, by=-10))

ggplot(df, aes(trt, value)) + geom_lollipop()

ggplot(df, aes(value, trt)) + geom_lollipop(horizontal=TRUE)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_lollipop", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_spikelines")
### * geom_spikelines

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_spikelines
### Title: Draw spikelines on a plot
### Aliases: geom_spikelines

### ** Examples


mtcars$name <- rownames(mtcars)

p <- ggplot(data = mtcars, aes(x=mpg,y=disp)) + geom_point()

p + geom_spikelines(data = mtcars[mtcars$carb==4,],linetype = 2)

p + geom_spikelines(data = mtcars[mtcars$carb==4,],aes(colour = factor(gear)), linetype = 2)

## Not run: 
##D require(ggrepel)
##D p + geom_spikelines(data = mtcars[mtcars$carb==4,],aes(colour = factor(gear)), linetype = 2) +
##D ggrepel::geom_label_repel(data = mtcars[mtcars$carb==4,],aes(label = name))
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_spikelines", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_stateface")
### * geom_stateface

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_stateface
### Title: Use ProPublica's StateFace font in ggplot2 plots
### Aliases: geom_stateface

### ** Examples

## Not run: 
##D library(ggplot2)
##D library(ggalt)
##D 
##D # Run show_stateface() to see the location of the TTF StateFace font
##D # You need to install it for it to work
##D 
##D set.seed(1492)
##D dat <- data.frame(state=state.abb,
##D                   x=sample(100, 50),
##D                   y=sample(100, 50),
##D                   col=sample(c("#b2182b", "#2166ac"), 50, replace=TRUE),
##D                   sz=sample(6:15, 50, replace=TRUE),
##D                   stringsAsFactors=FALSE)
##D gg <- ggplot(dat, aes(x=x, y=y))
##D gg <- gg + geom_stateface(aes(label=state, color=col, size=sz))
##D gg <- gg + scale_color_identity()
##D gg <- gg + scale_size_identity()
##D gg
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_stateface", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_ubar")
### * geom_ubar

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_ubar
### Title: Uniform "bar" charts
### Aliases: geom_ubar

### ** Examples

library(ggplot2)

data(economics)
ggplot(economics, aes(date, uempmed)) +
  geom_ubar()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_ubar", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_xspline")
### * geom_xspline

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_xspline
### Title: Connect control points/observations with an X-spline
### Aliases: geom_xspline stat_xspline

### ** Examples

set.seed(1492)
dat <- data.frame(x=c(1:10, 1:10, 1:10),
                  y=c(sample(15:30, 10), 2*sample(15:30, 10),
                      3*sample(15:30, 10)),
                  group=factor(c(rep(1, 10), rep(2, 10), rep(3, 10)))
)

ggplot(dat, aes(x, y, group=group, color=group)) +
  geom_point() +
  geom_line()

ggplot(dat, aes(x, y, group=group, color=factor(group))) +
  geom_point() +
  geom_line() +
  geom_smooth(se=FALSE, linetype="dashed", size=0.5)

ggplot(dat, aes(x, y, group=group, color=factor(group))) +
  geom_point(color="black") +
  geom_smooth(se=FALSE, linetype="dashed", size=0.5) +
  geom_xspline(size=0.5)

ggplot(dat, aes(x, y, group=group, color=factor(group))) +
  geom_point(color="black") +
  geom_smooth(se=FALSE, linetype="dashed", size=0.5) +
  geom_xspline(spline_shape=-0.4, size=0.5)

ggplot(dat, aes(x, y, group=group, color=factor(group))) +
  geom_point(color="black") +
  geom_smooth(se=FALSE, linetype="dashed", size=0.5) +
  geom_xspline(spline_shape=0.4, size=0.5)

ggplot(dat, aes(x, y, group=group, color=factor(group))) +
  geom_point(color="black") +
  geom_smooth(se=FALSE, linetype="dashed", size=0.5) +
  geom_xspline(spline_shape=1, size=0.5)

ggplot(dat, aes(x, y, group=group, color=factor(group))) +
  geom_point(color="black") +
  geom_smooth(se=FALSE, linetype="dashed", size=0.5) +
  geom_xspline(spline_shape=0, size=0.5)

ggplot(dat, aes(x, y, group=group, color=factor(group))) +
  geom_point(color="black") +
  geom_smooth(se=FALSE, linetype="dashed", size=0.5) +
  geom_xspline(spline_shape=-1, size=0.5)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_xspline", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("position-dodgev")
### * position-dodgev

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: position_dodgev
### Title: Vertically dodge position
### Aliases: position_dodgev PositionDodgev
### Keywords: datasets

### ** Examples


if(interactive()){

dat <- data.frame(
  trt = c(LETTERS[1:5], "D"),
  l = c(20, 40, 10, 30, 50, 40),
  r = c(70, 50, 30, 60, 80, 70)
)

ggplot(dat, aes(y=trt, x=l, xend=r)) +
 geom_dumbbell(size=3, color="#e3e2e1",
               colour_x = "#5b8124", colour_xend = "#bad744",
               dot_guide=TRUE, dot_guide_size=0.25,
               position=position_dodgev(height=0.8)) +
 labs(x=NULL, y=NULL, title="ggplot2 geom_dumbbell with dot guide") +
 theme_minimal() +
 theme(panel.grid.major.x=element_line(size=0.05))

 }




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("position-dodgev", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("stat_ash")
### * stat_ash

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: stat_ash
### Title: Compute and display a univariate averaged shifted histogram
###   (polynomial kernel)
### Aliases: stat_ash

### ** Examples

# compare
library(gridExtra)
set.seed(1492)
dat <- data.frame(x=rnorm(100))
grid.arrange(ggplot(dat, aes(x)) + stat_ash(),
             ggplot(dat, aes(x)) + stat_bkde(),
             ggplot(dat, aes(x)) + stat_density(),
             nrow=3)

cols <- RColorBrewer::brewer.pal(3, "Dark2")
ggplot(dat, aes(x)) +
  stat_ash(alpha=1/2, fill=cols[3]) +
  stat_bkde(alpha=1/2, fill=cols[2]) +
  stat_density(alpha=1/2, fill=cols[1]) +
  geom_rug() +
  labs(x=NULL, y="density/estimate") +
  scale_x_continuous(expand=c(0,0)) +
  theme_bw() +
  theme(panel.grid=element_blank()) +
  theme(panel.border=element_blank())



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("stat_ash", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("stat_stepribbon")
### * stat_stepribbon

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: stat_stepribbon
### Title: Step ribbon statistic
### Aliases: stat_stepribbon

### ** Examples

x <- 1:10
df <- data.frame(x=x, y=x+10, ymin=x+7, ymax=x+12)

gg <- ggplot(df, aes(x, y))
gg <- gg + geom_ribbon(aes(ymin=ymin, ymax=ymax),
                       stat="stepribbon", fill="#b2b2b2")
gg <- gg + geom_step(color="#2b2b2b")
gg

gg <- ggplot(df, aes(x, y))
gg <- gg + geom_ribbon(aes(ymin=ymin, ymax=ymax),
                       stat="stepribbon", fill="#b2b2b2",
                       direction="hv")
gg <- gg + geom_step(color="#2b2b2b")
gg



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("stat_stepribbon", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
