## ----include=FALSE-------------------------------------------------------
knitr::opts_chunk$set(collapse=TRUE, comment="##", fig.retina=1, fig.height=6, fig.width=8,message=FALSE,warning = FALSE)

## ------------------------------------------------------------------------
library(ggplot2)
library(gridExtra)
library(ggalt)
library(scales)


## ----encircle------------------------------------------------------------

d <- data.frame(x=c(1,1,2),y=c(1,2,2)*100)

gg <- ggplot(d,aes(x,y))

gg <- gg + 
  scale_x_continuous(expand=c(0.5,1))

gg <- gg + 
  scale_y_continuous(expand=c(0.5,1))

gg + 
  geom_encircle(s_shape=1, expand=0) + 
  geom_point()

gg + 
  geom_encircle(s_shape=1, expand=0.1, colour="red") + 
  geom_point()

gg + 
  geom_encircle(s_shape=0.5, expand=0.1, colour="purple") + 
  geom_point()

gg + 
  geom_encircle(data=subset(d, x==1), colour="blue", spread=0.02) +
  geom_point()

gg +
  geom_encircle(data=subset(d, x==2), colour="cyan", spread=0.04) + 
  geom_point()


## ------------------------------------------------------------------------

gg <- ggplot(mpg, aes(displ, hwy))

gg + geom_encircle(data=subset(mpg, hwy>40)) + geom_point()

ss <- subset(mpg,hwy>31 & displ<2)

gg + 
  geom_encircle(data=ss, colour="blue", s_shape=0.9, expand=0.07) +
  geom_point() + 
  geom_point(data=ss, colour="blue")

