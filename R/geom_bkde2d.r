#' Contours from a 2d density estimate.
#'
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::geom_path
#' @export
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

  compute_group = function(data, scales, contour=TRUE, bandwidth, grid_size=c(51, 51), range.x=NULL,
                           truncate=TRUE) {

    if (is.null(range.x)) range.x <- list(range(data$x), range(data$y))

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
