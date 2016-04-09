#' Display a smooth density estimate.
#'
#' A kernel density estimate, useful for displaying the distribution of
#' variables with underlying smoothness.
#'
#' \if{html}{
#' A sample of the output from \code{geom_bkde()}:
#'
#' \figure{geombkde01.png}{options: width="100\%" alt="Figure: geombkde01.png"}
#' }
#'
#' \if{latex}{
#' A sample of the output from \code{geom_bkde()}:
#'
#' \figure{geombkde01.png}{options: width=10cm}
#' }
#'
#' @section Aesthetics:
#' \code{geom_bkde} understands the following aesthetics (required aesthetics
#' are in bold):
#' \itemize{
#'   \item \strong{\code{x}}
#'   \item \strong{\code{y}}
#'   \item \code{alpha}
#'   \item \code{color}
#'   \item \code{fill}
#'   \item \code{linetype}
#'   \item \code{size}
#' }
#' @inheritParams ggplot2::geom_point
#' @param geom,stat Use to override the default connection between
#'   \code{geom_bkde} and \code{stat_bkde}.
#' @seealso See \code{\link{geom_histogram}}, \code{\link{geom_freqpoly}} for
#'   other methods of displaying continuous distribution.
#'   See \code{\link{geom_violin}} for a compact density display.
#' @export
geom_bkde <- function(mapping = NULL, data = NULL, stat = "bkde",
                      position = "identity", bandwidth=NULL, range.x=NULL,
                      na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                      ...) {

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBkde,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(bandwidth=bandwidth,
                  range.x=range.x,
                  na.rm=na.rm,
                  ...)
  )
}

#' Geom Proto
#' @rdname ggalt-ggproto
#' @format NULL
#' @usage NULL
#' @keywords internal
#' @export
GeomBkde <- ggproto("GeomBkde", GeomArea,
  default_aes = aes(colour = NA, fill = "gray20", size = 0.5,
                    linetype = 1, alpha = NA)
)


#' @param bandwidth	the kernel bandwidth smoothing parameter. see
#'        \code{\link[KernSmooth]{bkde}} for details. If \code{NULL},
#'        it will be computed for you but will most likely not yield optimal
#'        results.
#' @param kernel character string which determines the smoothing kernel. see
#'        \code{\link[KernSmooth]{bkde}} for details
#' @param canonical	logical flag: if TRUE, canonically scaled kernels are used.
#'        see \code{\link[KernSmooth]{bkde}} for details
#' @param gridsize the number of equally spaced points at which to estimate the
#'        density. see \code{\link[KernSmooth]{bkde}} for details.
#' @param range.x	vector containing the minimum and maximum values of x at which
#'        to compute the estimate. see \code{\link[KernSmooth]{bkde}} for details
#' @param truncate logical flag: if TRUE, data with x values outside the range
#'        specified by range.x are ignored. see \code{\link[KernSmooth]{bkde}}
#'        for details
#' @section Computed variables:
#' \describe{
#'   \item{density}{density estimate}
#'   \item{count}{density * number of points - useful for stacked density
#'      plots}
#'   \item{scaled}{density estimate, scaled to maximum of 1}
#' }
#' @export
#' @rdname geom_bkde
#' @examples
#' data(geyser, package="MASS")
#'
#' ggplot(geyser, aes(x=duration)) +
#'   stat_bkde(alpha=1/2)
#'
#' ggplot(geyser, aes(x=duration)) +
#'   geom_bkde(alpha=1/2)
#'
#' ggplot(geyser, aes(x=duration)) +
#'  stat_bkde(bandwidth=0.25)
#'
#' ggplot(geyser, aes(x=duration)) +
#'   geom_bkde(bandwidth=0.25)
stat_bkde <- function(mapping = NULL, data = NULL, geom = "area",
                      position = "stack", kernel="normal", canonical=FALSE,
                      bandwidth = NULL, gridsize=410, range.x=NULL,
                      truncate=TRUE, na.rm=FALSE, show.legend = NA, inherit.aes = TRUE,
                      ...) {

  layer(
    data = data,
    mapping = mapping,
    stat = StatBkde,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      kernel=kernel,
      canonical=canonical,
      bandwidth=bandwidth,
      gridsize=gridsize,
      range.x=range.x,
      truncate=truncate,
      na.rm=na.rm,
      ...
    )
  )
}

#' @rdname ggalt-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatBkde <- ggproto("StatBkde", Stat,

  required_aes = "x",

  default_aes = aes(y = ..density.., colour = NA, fill = "gray20", size = 0.5,
                    linetype = 1, alpha = NA),

  compute_group = function(data, scales, kernel="normal", canonical=FALSE,
                           bandwidth=NULL, gridsize=410, range.x=NULL,
                           truncate=TRUE, na.rm = TRUE) {

    # KernSmooth::dpik uses a generated normal distribution as part of its
    # operation but doesn't do this seed save/create/restore. When bandwidth
    # is NULL the only way to produce consistency in calculated resuts is to use
    # a dedicated random seed. This might be a candidate for parameterization.

    if (is.null(bandwidth)) {
      tmp <- tempfile()
      on.exit(unlink(tmp))
      save(".Random.seed", file=tmp)
      set.seed(1492)
      bandwidth <- KernSmooth::dpik(data$x)
      message(
        sprintf("Bandwidth not specified. Using '%3.2f', via KernSmooth::dpik.",
                bandwidth))
      load(tmp)
    }

    if (is.null(range.x)) range.x <- range(data$x)

    compute_bkde(data$x, kernel=kernel, canonical=canonical,
                    bandwidth=bandwidth, gridsize=gridsize, range.x=range.x,
                    truncate=truncate, na.rm)

  }

)

compute_bkde <- function(x, kernel="normal", canonical=FALSE,
                        bandwidth, gridsize=410, range.x, truncate=TRUE, na.rm=TRUE) {

  n <- length(x)

  if (missing(range.x) | is.null(range.x)) range.x <- range(x)

  dens <- KernSmooth::bkde(x, kernel, canonical, bandwidth, gridsize, range.x, truncate)

  data.frame(
    x = dens$x,
    density = dens$y,
    scaled =  dens$y / max(dens$y, na.rm = na.rm),
    count =   dens$y * n,
    n = n
  )
}
