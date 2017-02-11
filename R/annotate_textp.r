globalVariables(c("Stepribbon"))

#' Text annotations in plot coordinate system
#'
#' Annotates the plot with text. Compared to \code{annotate("text",...)}, the
#' placement of the annotations is specified in plot coordinates (from 0 to 1)
#' instead of data coordinates.
#'
#' @param label text annotation to be placed on the plot
#' @param x,y positions of the individual annotations, in plot coordinates
#'   (0..1) instead of data coordinates!
#' @param facets facet positions of the individual annotations
#' @param hjust,vjust horizontal and vertical justification of the text relative
#'   to the bounding box
#' @param color, alpha, family, size, fontface, lineheight font properties
#' @param box_just placement of the bounding box for the text relative to x,y
#'   coordinates. Per default, the box is placed to the center of the plot. Be
#'   aware that parts of the box which are outside of the visible region of the
#'   plot will not be shown.
#' @param margin margins of the bounding box
#' @param alpha,family,size,fontface,lineheight standard aesthetic customizations
#' @examples
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
#' p <- p + geom_smooth(method = "lm", se = FALSE)
#' p + annotate_textp(x = 0.9, y = 0.35, label="A relative linear\nrelationship", hjust=1, color="red")
#' @export
annotate_textp <- function(label, x, y, facets=NULL, hjust=0, vjust=0, color='black', alpha=NA,
                          family=theme_get()$text$family, size=theme_get()$text$size, fontface=1, lineheight=1.0,
                          box_just=ifelse(c(x,y)<0.5,0,1), margin=unit(size/2, 'pt')) {
  # based on https://stackoverflow.com/questions/22488563/ggplot2-annotate-layer-position-in-r/22500252#22500252
  if (x < 0 || x > 1){
    stop("x values must be in plot coordinates (between 0 and 1)")
  }
  if (y < 0 || y > 1){
    stop("y values must be in plot coordinates (between 0 and 1)")
  }
  x <- scales::squish_infinite(x)
  y <- scales::squish_infinite(y)
  data <- if (is.null(facets)) data.frame(x=NA) else data.frame(x=NA, facets)

  tg <- grid::textGrob(
    label, x=0, y=0, hjust=hjust, vjust=vjust,
    gp=grid::gpar(col=alpha(color, alpha), fontsize=size, fontfamily=family, fontface=fontface, lineheight=lineheight)
  )
  ts <- grid::unit.c(grid::grobWidth(tg), grid::grobHeight(tg))
  vp <- grid::viewport(x=x, y=y, width=ts[1], height=ts[2], just=box_just)
  tg <- grid::editGrob(tg, x=ts[1]*hjust, y=ts[2]*vjust, vp=vp)
  inner <- grid::grobTree(tg, vp=grid::viewport(width=unit(1, 'npc')-margin*2, height=unit(1, 'npc')-margin*2))

  layer(
    data = NULL,
    stat = StatIdentity,
    position = PositionIdentity,
    geom = GeomCustomAnn,
    inherit.aes = TRUE,
    params = list(
        grob=grid::grobTree(inner),
        xmin=-Inf,
        xmax=Inf,
        ymin=-Inf,
        ymax=Inf
    )
  )
}
