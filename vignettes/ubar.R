## ----include=FALSE-------------------------------------------------------
knitr::opts_chunk$set(collapse=TRUE, comment="##", fig.retina=1, fig.height=6, fig.width=8,message=FALSE,warning = FALSE)

## ------------------------------------------------------------------------
library(ggplot2)
library(gridExtra)
library(ggalt)
library(scales)


## ------------------------------------------------------------------------
ggplot(economics, aes(date, uempmed)) +
  geom_ubar()

