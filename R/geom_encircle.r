draw_key_hack <- function(data, params, size) {
  data$fill <- alpha(data$fill, data$alpha)
  data$alpha <- 1

  grobTree(
    if (!is.na(data$fill)) grid::rectGrob(gp = gpar(col = NA, fill = data$fill)),
    draw_key_path(data, params)
  )
}

#' @rdname ggalt-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomEncircle <- ggproto("GeomEncircle", Geom,
  required_aes = c("x", "y"),
  default_aes = aes(colour = "black",
                    fill = NA, ## ???
                    alpha = 1,
                    linetype=1,
                    size=1,
                    s_shape=0.5,  ## corresponds to default shape in xspline of -0.5
                    s_open=FALSE,
                    expand=0.05,
                    spread=0.1),
  draw_key = draw_key_hack, ## ???

  draw_group = function(data, panel_scales, coord) {
    ## browser()
    coords <- coord$transform(data, panel_scales)
    first_row <- coords[1, , drop = FALSE]
    rownames(first_row) <- NULL ## prevent warning later

    m <- lapply(coords[,c("x","y")],mean,na.rm=TRUE)
    ch <- grDevices::chull(coords[c("x","y")])

    mkcoords <- function(x,y) {
        data.frame(x,y,first_row[!names(first_row) %in% c("x","y")])
    }

    coords <- coords[ch,]
    ## FIXME: using grid:: a lot. importFrom instead?

    ## convert from lengths to physical units, for computing *directions*
    cc <- function(x,dir="x")
        grid::convertUnit(grid::unit(x,"native"),"mm",typeFrom="dimension",
                          axisFrom=dir,valueOnly=TRUE)

    ## convert back to native (e.g. native + snpc offset)
    cc_inv <- function(x,dir="x")
        grid::convertUnit(x,"native",typeFrom="location",
                          axisFrom=dir,valueOnly=TRUE)

    cc_comb <- function(x1,x2,dir="x")
        cc_inv(unit(x1,"native")+unit(x2,"snpc"),dir=dir)

    ## find normalized vector: d1 and d2 have $x, $y elements
    normFun <- function(d1,d2) {
        dx <- cc(d1$x-d2$x)
        dy <- cc(d1$y-d2$y)
        r <- sqrt(dx*dx+dy*dy)
        list(x=dx/r,y=dy/r)
    }

    if (nrow(coords)==1) {
        ## only one point: make a diamond by spreading points vertically
        ## and horizontally
        coords <- with(coords,
                       mkcoords(
                           c(x,x+spread,x,x-spread),
                           c(y+spread,y,y-spread,y)))
    } else if (nrow(coords)==2) {
        ## only two points: make a diamond by spreading points perpendicularly
        rot <- matrix(c(0,1,-1,0),2)
        dd <- c(rot %*% unlist(normFun(coords[1,],coords[2,])))*
                     coords$spread
        coords <- with(coords, {
            ## figure out rotated values, then convert *back* to native units
            ## already in scaled units, so ignore?
            x <- c(x[1],
                   m$x+dd[1], ## cc_comb(m$x,dd[1]),
                   x[2],
                   m$x-dd[1]) ## cc_comb(m$x,-dd[1]))
            y <- c(y[1],
                   m$y+dd[2], ## cc_comb(m$y,dd[2],"y"),
                   y[2],
                   m$y-dd[2]) ## cc_comb(m$y,-dd[2],"y"))
            mkcoords(x,y)
        })
    }

    disp <- normFun(coords,m)

    ## browser()

    gp <- grid::get.gpar()
    pars1 <- c("colour","linetype","alpha","fill","size")
    pars2 <- c("col","lty","alpha","fill","lwd")
    gp[pars2] <- first_row[pars1]
    grid::xsplineGrob(
        with(coords,unit(x,"npc")+disp$x*unit(expand,"snpc")),
        with(coords,unit(y,"npc")+disp$y*unit(expand,"snpc")),
        ## coords$x,
        ## coords$y,
      shape = coords$s_shape-1,  ## kluge!
      open = first_row$s_open,
      gp = gp)
  }
)

if (FALSE) {
    library("grid")
    library("gridBase")
    coords <- data.frame(x=c(1,1),y=c(1,2)*100,spread=c(0.1,0.1))
    plot(y~x,data=d,xlim=c(0,3),ylim=c(0,300))
    vps <- baseViewports()
    pushViewport(vps$inner)
    pushViewport(vps$figure)
    pushViewport(vps$plot)
    ## check that we're in the right place
    m <- as.list(colMeans(coords))
    grid.points(m$x,m$y,gp=gpar(col="red"))
    cc <- function(x,dir="x")
        grid::convertUnit(grid::unit(x,"native"),"mm",typeFrom="dimension",
                          axisFrom=dir,valueOnly=TRUE)
    cc_inv <- function(x,dir="x")
        grid::convertUnit(x,"native",typeFrom="location",
                          axisFrom=dir,valueOnly=TRUE)

    cc_comb <- function(x1,x2,dir="x")
        cc_inv(unit(x1,"native")+unit(x2,"snpc"),dir=dir)

    ## find normalized vector: d1 and d2 have $x, $y elements
    normFun <- function(d1,d2) {
        dx <- cc(d1$x-d2$x)
        dy <- cc(d1$y-d2$y)
        r <- sqrt(dx*dx+dy*dy)
        list(x=dx/r,y=dy/r)
    }

    dd <- c(rot %*% unlist(normFun(coords[1,],coords[2,])))*
                     coords$spread
    z <- with(coords, {
            ## figure out rotated values, then convert *back* to native units
            x <- c(x[1],
                   cc_comb(m$x,dd[1]),
                   x[2],
                   cc_comb(m$x,-dd[1]))
            y <- c(y[1],
                   cc_comb(m$y,dd[2],"y"),
                   y[2],
                   cc_comb(m$y,-dd[2],"y"))
         list(x=x,y=y)
    })
    with(z,grid.points(x,y,gp=gpar(col="blue")))

    print(grid::convertWidth(unit(1,'npc'),'native'))
    print(grid::convertHeight(unit(1,'npc'),'native'))

}

#' Automatically enclose points in a polygon
#'
#' @details
#' \if{html}{
#' A sample of the output from \code{geom_encircle()}:
#'
#' \figure{geomencircle01.png}{options: width="100\%" alt="Figure: geomencircle01.png"}
#' }
#'
#' \if{latex}{
#' A sample of the output from \code{geom_encircle()}:
#'
#' \figure{geomencircle01.png}{options: width=10cm}
#' }
#'
#' @param mapping mapping
#' @param data  data
#' @param stat  stat
#' @param position position
#' @param na.rm na.rm
#' @param show.legend  show.legend
#' @param inherit.aes inherit.aes
#' @param ...  dots
#' @return adds a circle around the specified points
#' @author Ben Bolker
#' @export
#' @examples
#' d <- data.frame(x=c(1,1,2),y=c(1,2,2)*100)
#'
#' gg <- ggplot(d,aes(x,y))
#' gg <- gg + scale_x_continuous(expand=c(0.5,1))
#' gg <- gg + scale_y_continuous(expand=c(0.5,1))
#'
#' gg + geom_encircle(s_shape=1, expand=0) + geom_point()
#'
#' gg + geom_encircle(s_shape=1, expand=0.1, colour="red") + geom_point()
#'
#' gg + geom_encircle(s_shape=0.5, expand=0.1, colour="purple") + geom_point()
#'
#' gg + geom_encircle(data=subset(d, x==1), colour="blue", spread=0.02) +
#'   geom_point()
#'
#' gg +geom_encircle(data=subset(d, x==2), colour="cyan", spread=0.04) +
#'   geom_point()
#'
#' gg <- ggplot(mpg, aes(displ, hwy))
#' gg + geom_encircle(data=subset(mpg, hwy>40)) + geom_point()
#' gg + geom_encircle(aes(group=manufacturer)) + geom_point()
#' gg + geom_encircle(aes(group=manufacturer,fill=manufacturer),alpha=0.4)+
#'        geom_point()
#' gg + geom_encircle(aes(group=manufacturer,colour=manufacturer))+
#'        geom_point()
#'
#' ss <- subset(mpg,hwy>31 & displ<2)
#'
#' gg + geom_encircle(data=ss, colour="blue", s_shape=0.9, expand=0.07) +
#'   geom_point() + geom_point(data=ss, colour="blue")
geom_encircle <- function(mapping = NULL, data = NULL, stat = "identity",
                         position = "identity", na.rm = FALSE, show.legend = NA,
                         inherit.aes = TRUE, ...) {
  layer(
    geom = GeomEncircle, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

