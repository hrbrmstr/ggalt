## ----include=FALSE-------------------------------------------------------
knitr::opts_chunk$set(collapse=TRUE, comment="##", fig.retina=1, fig.height=6, fig.width=8,message=FALSE,warning = FALSE)

## ------------------------------------------------------------------------

library(ggplot2)
library(gridExtra)
library(ggalt)
library(scales)


## ----annoticks, message=FALSE, fig.width=7, fig.height=2.5---------------

p <- ggplot(msleep, aes(bodywt, brainwt)) + geom_point()

# add identity scale minor ticks on y axis
p + annotation_ticks(sides = 'l')

# add identity scale minor ticks on x,y axis
p + annotation_ticks(sides = 'lb')

# log10 scale
p1 <- p + scale_x_log10()

# add minor ticks on both scales
p1 + annotation_ticks(sides = 'lb', scale = c('identity','log10'))


