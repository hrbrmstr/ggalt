#' Contours from a 2d density estimate.
#'
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::geom_path
#' @export
#' @examples
#' m <- ggplot(faithful, aes(x = eruptions, y = waiting)) +
#'        geom_point() +
#'        xlim(0.5, 6) +
#'        ylim(40, 110)
#'
#' m + geom_bkde2d(bandwidth=c(0.5, 5))
#'
#' m + stat_bkde2d(bandwidth=c(0.5, 5), aes(fill = ..level..), geom = "polygon")
#'
#' # If you map an aesthetic to a categorical variable, you will get a
#' # set of contours for each value of that variable
#' set.seed(4393)
#' dsmall <- diamonds[sample(nrow(diamonds), 1000), ]
#' d <- ggplot(dsmall, aes(x, y)) +
#'        geom_bkde2d(bandwidth=c(0.5, 0.5), aes(colour = cut))
#' d
#'
#' # If we turn contouring off, we can use use geoms like tiles:
#' d + stat_bkde2d(bandwidth=c(0.5, 0.5), geom = "raster",
#'                 aes(fill = ..density..), contour = FALSE)
#'
#' # Or points:
#' d + stat_bkde2d(bandwidth=c(0.5, 0.5), geom = "point",
#'                 aes(size = ..density..),  contour = FALSE)
geom_bkde2d <- function(mapping = NULL, data = NULL, stat = "bkde2d",
                           position = "identity",  bandwidth, range.x=NULL,
                           lineend = "butt", contour=TRUE,
                           linejoin = "round", linemitre = 1,
                           show.legend = NA,
                           inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBkde2d,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      bandwidth = bandwidth,
      range.x = range.x,
      ...
    )
  )
}


#' @rdname ggalt-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomBkde2d <- ggproto("GeomBkde2d", GeomPath,
  default_aes = aes(colour = "#3366FF", size = 0.5, linetype = 1, alpha = NA)
)


#' 2D density
#'
#' @export
#' @param contour If \code{TRUE}, contour the results of the 2d density
#'   estimation
#' @section Computed variables:
#' Same as \code{\link{stat_contour}}
stat_bkde2d <- function(mapping = NULL, data = NULL, geom = "density2d",
                           position = "identity", contour = TRUE,
                           bandwidth, grid_size=c(51, 51), range.x=NULL,
                           truncate=TRUE, show.legend = NA,
                           inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatBkde2d,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      bandwidth = bandwidth,
      grid_size = grid_size,
      range.x = range.x,
      truncate = truncate,
      contour = contour,
      ...
    )
  )
}

#' @rdname ggalt-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatBkde2d <- ggproto("StatBkde2d", Stat,

  default_aes = aes(colour = "#3366FF", size = 0.5),

  required_aes = c("x", "y"),

  compute_group = function(data, scales, contour=TRUE, bandwidth,
                           grid_size=c(51, 51), range.x=NULL,
                           truncate=TRUE) {

    if (is.null(range.x)) {
      x_range <- range(data$x)
      y_range <- range(data$y)
      x_range[1] <- x_range[1] - 1.5 * bandwidth[1]
      x_range[2] <- x_range[2] + 1.5 * bandwidth[1]
      y_range[1] <- y_range[1] - 1.5 * bandwidth[2]
      y_range[2] <- y_range[2] + 1.5 * bandwidth[2]
      range.x <- list(x_range, y_range)
    }

    dens <- KernSmooth::bkde2D(
      as.matrix(data.frame(x=data$x, y=data$y)),
      bandwidth,
      grid_size,
      range.x,
      truncate
    )
    df <- data.frame(expand.grid(x = dens$x1, y = dens$x2), z = as.vector(dens$fhat))
    df$group <- data$group[1]

    if (contour) {
      StatContour$compute_panel(df, scales)
    } else {
      names(df) <- c("x", "y", "density", "group")
      df$level <- 1
      df$piece <- 1
      df
    }
  }
)
