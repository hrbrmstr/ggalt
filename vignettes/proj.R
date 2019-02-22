## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(ggalt)
library(proj4)

## ----coord_proj----------------------------------------------------------

world <- ggplot2::map_data("world")

world <- world[world$region != "Antarctica",]

gg <- ggplot()

gg <- gg + geom_cartogram(data=world, map=world,
                    aes(x=long, y=lat, map_id=region))

gg <- gg + coord_proj("+proj=wintri")

gg

