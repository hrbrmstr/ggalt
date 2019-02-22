## ----include=FALSE-------------------------------------------------------
knitr::opts_chunk$set(collapse=TRUE, comment="##", fig.retina=1, fig.height=6, fig.width=8,message=FALSE,warning = FALSE)

## ------------------------------------------------------------------------

library(ggplot2)
library(gridExtra)
library(ggalt)
library(scales)


## ----spikelines, message=FALSE, fig.width=7, fig.height=7----------------

mtcars$name <- rownames(mtcars)

p <- ggplot(data = mtcars, aes(x=mpg,y=disp)) + geom_point()

p + 
  geom_spikelines(data = mtcars[mtcars$carb==4,],aes(colour = factor(gear)), linetype = 2) + 
  ggrepel::geom_label_repel(data = mtcars[mtcars$carb==4,],aes(label = name))


