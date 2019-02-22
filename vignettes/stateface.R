## ----include=FALSE-------------------------------------------------------
knitr::opts_chunk$set(collapse=TRUE, comment="##", fig.retina=1, fig.height=6, fig.width=8,message=FALSE,warning = FALSE)

## ------------------------------------------------------------------------
library(ggplot2)
library(gridExtra)
library(ggalt)
library(scales)


## ----stateface-----------------------------------------------------------

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

