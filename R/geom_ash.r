#' Compute and display a univariate averaged shifted histogram (polynomial kernel)
#'
#' See \code{\link[ash]{bin1}} & \code{\link[ash]{ash1}} for more information.
#'
#' \if{html}{
#' A sample of the output from \code{stat_ash()}:
#'
#' \figure{statash01.png}{options: width="100\%" alt="Figure: statash01.png"}
#' }
#'
#' \if{latex}{
#' A sample of the output from \code{stat_ash()}:
#'
#' \figure{statash01.png}{options: width=10cm}
#' }
#'
#' @inheritParams ggplot2::geom_area
#' @param geom Use to override the default Geom
#' @param ab half-open interval for bins \emph{[a,b)}. If no value is specified,
#'        the range of x is stretched by \code{5\%} at each end and used the
#'        interval.
#' @param nbin number of bins desired. Default \code{50}.
#' @param m	integer smoothing parameter; Default \code{5}.
#' @param kopt vector of length 2 specifying the kernel, which is proportional
#'        to \emph{( 1 - abs(i/m)^kopt(1) )i^kopt(2)}; (2,2)=biweight (default);
#'         (0,0)=uniform; (1,0)=triangle; (2,1)=Epanechnikov; (2,3)=triweight.
#' @references David Scott (1992), \emph{"Multivariate Density Estimation,"}
#'             John Wiley, (chapter 5 in particular).\cr
#'             \cr
#'             B. W. Silverman (1986), \emph{"Density Estimation for Statistics
#'             and Data Analysis,"} Chapman & Hall.
#' @section Aesthetics:
#' \code{geom_ash} understands the following aesthetics (required aesthetics
#' are in bold):
#' \itemize{
#'   \item \strong{\code{x}}
#'   \item \code{alpha}
#'   \item \code{color}
#'   \item \code{fill}
#'   \item \code{linetype}
#'   \item \code{size}
#' }
#' @section Computed variables:
#' \describe{
#'   \item{\code{density}}{ash density estimate}
#' }
#' @export
#' @examples
#' # compare
#' library(gridExtra)
#' set.seed(1492)
#' dat <- data.frame(x=rnorm(100))
#' grid.arrange(ggplot(dat, aes(x)) + stat_ash(),
#'              ggplot(dat, aes(x)) + stat_bkde(),
#'              ggplot(dat, aes(x)) + stat_density(),
#'              nrow=3)
#'
#' cols <- RColorBrewer::brewer.pal(3, "Dark2")
#' ggplot(dat, aes(x)) +
#'   stat_ash(alpha=1/2, fill=cols[3]) +
#'   stat_bkde(alpha=1/2, fill=cols[2]) +
#'   stat_density(alpha=1/2, fill=cols[1]) +
#'   geom_rug() +
#'   labs(x=NULL, y="density/estimate") +
#'   scale_x_continuous(expand=c(0,0)) +
#'   theme_bw() +
#'   theme(panel.grid=element_blank()) +
#'   theme(panel.border=element_blank())
stat_ash <- function(mapping = NULL, data = NULL, geom = "area",
                     position = "stack",
                     ab = NULL, nbin = 50, m = 5, kopt = c(2, 2),
                     na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...) {

  layer(
    data = data,
    mapping = mapping,
    stat = StatAsh,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ab = ab,
      nbin = nbin,
      m = m,
      kopt = kopt,
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
StatAsh <- ggproto("StatAsh", Stat,

  required_aes = c("x"),

  default_aes = aes(y = ..density.., colour = NA, fill = "gray20", size = 0.5,
                    linetype = 1, alpha = NA),


  setup_params = function(data, params) {
    if (!is.null(data$y) || !is.null(params$y)) {
      stop("stat_ash() must not be used with a y aesthetic.", call. = FALSE)
    }
    params
  },

  compute_group = function(data, scales, ab = NULL,
                           nbin = 50, m = 5, kopt = c(2, 2)) {

    if (is.null(ab)) ab <- nicerange(data$x)

    bin_res <- ash::bin1(data$x, ab, nbin)
    ash_msg <- capture.output(ash_res <- ash1(bin_res))

    if (ash_res$ier == 1) message("Estimate nonzero outside interval ab.")

    data.frame(x=ash_res$x, density=ash_res$y)

  }

)

nicerange <- function (x, beta = 0.1) {
    ab <- range(x)
    del <- ((ab[2] - ab[1]) * beta)/2
    return(c(ab + c(-del, del)))
}
