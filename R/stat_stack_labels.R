#' Labels position for stack position.
#'
#' Computes position of labels when using \code{position = "stack"}
#'
#' @inheritParams ggplot2::stat_count
#' @param geom Use to override the default connection between
#'   \code{geom_text} and \code{stat_stack_labels}.
#' @section Computed variables:
#' \describe{
#'   \item{count}{the number of observations}
#'   \item{cumcount}{the cumulative number of observations}
#'   \item{ylabel}{the y position of labels, i.e. \eqn{cumcount - count / 2}}
#' }
#' @seealso \code{\link{stat_fill_labels}} for \code{position = "fill"}.
#' @examples
#' ggplot(as.data.frame(Titanic)) +
#'   aes(x = Class, fill = Survived, weight = Freq) +
#'   geom_bar() + stat_stack_labels()
#' ggplot(as.data.frame(Titanic)) +
#'   aes(x = Class, fill = Survived, weight = Freq) +
#'   geom_bar() + geom_label(stat = "stack_labels")
#' ggplot(as.data.frame(Titanic)) +
#'   aes(x = Class, fill = Survived, weight = Freq) +
#'   geom_bar() + stat_stack_labels() + facet_grid(~Sex)
#' ggplot(as.data.frame(Titanic)) +
#'   aes(x = as.integer(Class), fill = Survived, weight = Freq) +
#'   geom_area(stat = "count") + stat_stack_labels()
#' @export
stat_stack_labels <- function(mapping = NULL, data = NULL, geom = "text",
                              position = "identity", width = NULL, na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE, ...) {
  layer(
    stat = StatStackLabels, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' @export
StatStackLabels <- ggproto(
  "StatStackLabels",
  StatCount,
  compute_panel = function (self, data, scales, ...) {
    if (ggplot2:::empty(data))
      return(data.frame())
    groups <- split(data, data$group)
    stats <- lapply(groups, function(group) {
      self$compute_group(data = group, scales = scales, ...)
    })
    stats <- mapply(function(new, old) {
      if (ggplot2:::empty(new))
        return(data.frame())
      unique <- ggplot2:::uniquecols(old)
      missing <- !(names(unique) %in% names(new))
      cbind(new, unique[rep(1, nrow(new)), missing, drop = FALSE])
    }, stats, groups, SIMPLIFY = FALSE)
    data <- do.call(plyr::rbind.fill, stats)
    plyr::ddply(
      data, "x", plyr::mutate,
      cumcount = cumsum(count),
      ylabel = cumsum(count) - count / 2,
      na.rm = TRUE
    )
  },
  default_aes = aes(y = ..ylabel.., label = ..count..)
)
