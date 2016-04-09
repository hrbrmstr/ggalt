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
#' m + geom_bkde2d(bandwidth=c(0.5, 4))
#'
#' m + stat_bkde2d(bandwidth=c(0.5, 4), aes(fill = ..level..), geom = "polygon")
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
                           position = "identity",  bandwidth=NULL, range.x=NULL,
                           lineend = "butt", contour=TRUE,
                           linejoin = "round", linemitre = 1,
                           na.rm = FALSE, show.legend = NA,
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
      na.rm = na.rm,
      ...
    )
  )
}


#' Geom Proto
#' @rdname ggalt-ggproto
#' @format NULL
#' @usage NULL
#' @keywords internal
#' @export
GeomBkde2d <- ggproto("GeomBkde2d", GeomPath,
  default_aes = aes(colour = "#3366FF", size = 0.5, linetype = 1, alpha = NA)
)


#' Contours from a 2d density estimate.
#'
#' Perform a 2D kernel density estimation using \code{bkde2D} and display the
#' results with contours. This can be useful for dealing with overplotting
#'
#' \if{html}{
#' A sample of the output from \code{geom_bkde2d()}:
#'
#' \figure{geombkde2d01.png}{options: width="100\%" alt="Figure: geombkde2d01.png"}
#' }
#'
#' \if{latex}{
#' A sample of the output from \code{geom_bkde2d()}:
#'
#' \figure{geombkde2d01.png}{options: width=10cm}
#' }
#'
#' @param bandwidth	the kernel bandwidth smoothing parameter. see
#'        \code{\link[KernSmooth]{bkde2D}} for details. If \code{NULL},
#'        it will be computed for you but will most likely not yield optimal
#'        results. see \code{\link[KernSmooth]{bkde2D}} for details
#' @param grid_size vector containing the number of equally spaced points in each
#'        direction over which the density is to be estimated. see
#'        \code{\link[KernSmooth]{bkde2D}} for details
#' @param geom default geom to use with this stat
#' @param range.x	 a list containing two vectors, where each vector contains the
#'        minimum and maximum values of x at which to compute the estimate for
#'        each direction. see \code{\link[KernSmooth]{bkde2D}} for details
#' @param truncate logical flag: if TRUE, data with x values outside the range
#'        specified by range.x are ignored. see \code{\link[KernSmooth]{bkde2D}}
#'        for details
#' @param contour If \code{TRUE}, contour the results of the 2d density
#'        estimation
#' @section Computed variables:
#' Same as \code{\link{stat_contour}}
#' @seealso \code{\link{geom_contour}} for contour drawing geom,
#'  \code{\link{stat_sum}} for another way of dealing with overplotting
#' @rdname geom_bkde2d
#' @export
stat_bkde2d <- function(mapping = NULL, data = NULL, geom = "density2d",
                           position = "identity", contour = TRUE,
                           bandwidth=NULL, grid_size=c(51, 51), range.x=NULL,
                           truncate=TRUE, na.rm = FALSE, show.legend = NA,
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
      na.rm = na.rm,
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

  compute_group = function(data, scales, contour=TRUE, bandwidth=NULL,
                           grid_size=c(51, 51), range.x=NULL,
                           truncate=TRUE) {

    # See geom_bkde/stat_bkde
    if (is.null(bandwidth)) {
      tmp <- tempfile()
      on.exit(unlink(tmp))
      save(".Random.seed", file=tmp)
      set.seed(1492)
      bandwidth <- c(KernSmooth::dpik(data$x),
                     KernSmooth::dpik(data$y))
      message(
        sprintf("Bandwidth not specified. Using ['%3.2f', '%3.2f'], via KernSmooth::dpik.",
                bandwidth[1], bandwidth[2]))
      load(tmp)
    }

    if (is.null(range.x)) {
      x_range <- range(data$x)
      y_range <- range(data$y)
      x_range[1] <- x_range[1] - 1.75 * bandwidth[1]
      x_range[2] <- x_range[2] + 1.75 * bandwidth[1]
      y_range[1] <- y_range[1] - 1.75 * bandwidth[2]
      y_range[2] <- y_range[2] + 1.75 * bandwidth[2]
      range.x <- list(x_range, y_range)
    }

    dens <- KernSmooth::bkde2D(
      as.matrix(data.frame(x=data$x, y=data$y)),
      bandwidth,
      grid_size,
      range.x,
      truncate
    )

    df <- data.frame(expand.grid(x=dens$x1,
                                 y=dens$x2),
                     z=as.vector(dens$fhat))
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
