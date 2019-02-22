## ----include=FALSE-------------------------------------------------------
knitr::opts_chunk$set(collapse=TRUE, comment="##", fig.retina=1, fig.height=6, fig.width=8,message=FALSE,warning = FALSE)

## ------------------------------------------------------------------------
library(ggplot2)
library(gridExtra)
library(ggalt)
library(scales)


## ----stepribbon----------------------------------------------------------

x <- 1:10

df <- data.frame(x=x, y=x+10, ymin=x+7, ymax=x+12)

gg <- ggplot(df, aes(x, y))
gg <- gg + geom_ribbon(aes(ymin=ymin, ymax=ymax),
                       stat="stepribbon", fill="#b2b2b2")
gg <- gg + geom_step(color="#2b2b2b")
gg


## ------------------------------------------------------------------------
gg <- ggplot(df, aes(x, y))
gg <- gg + geom_ribbon(aes(ymin=ymin, ymax=ymax),
                       stat="stepribbon", fill="#b2b2b2",
                       direction="vh")
gg <- gg + geom_step(color="#2b2b2b")
gg

