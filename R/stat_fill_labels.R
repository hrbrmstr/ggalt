#' Labels position for fill position.
#'
#' Computes position of labels when using \code{position = "fill"}
#'
#' @inheritParams ggplot2::stat_count
#' @param geom Use to override the default connection between
#'   \code{geom_text} and \code{stat_stack_labels}.
#' @section Computed variables:
#' \describe{
#'   \item{count}{the number of observations}
#'   \item{prop}{proportion on \code{x}}
#'   \item{cumprop}{the cumulative proportion on \code{x}}
#'   \item{ylabel}{the y position of labels, i.e. \eqn{cumprop - prop / 2}}
#' }
#' @seealso \code{\link{stat_stack_labels}} for \code{position = "stack"}.
#' @examples
#' ggplot(as.data.frame(Titanic)) +
#'   aes(x = Class, fill = Survived, weight = Freq) +
#'   geom_bar(position = "fill") + stat_fill_labels()
#' ggplot(as.data.frame(Titanic)) +
#'   aes(x = Class, fill = Survived, weight = Freq) +
#'   geom_bar(position = "fill") + geom_label(stat = "fill_labels")
#' ggplot(as.data.frame(Titanic)) +
#'   aes(x = Class, fill = Survived, weight = Freq) +
#'   geom_bar(position = "fill") + geom_text(stat = "fill_labels") + facet_grid(~Sex)
#' ggplot(as.data.frame(Titanic)) +
#'   aes(x = as.integer(Class), fill = Survived, weight = Freq) +
#'   geom_area(position = "fill", stat = "count") + geom_text(stat = "fill_labels")
#'
#' # Cumulative percentages with dodge position
#' ggplot(as.data.frame(Titanic)) +
#'   aes(x = Class, fill = Survived, weight = Freq) +
#'   geom_bar(aes(y = ..prop..), stat="fill_labels", position="dodge", width = .8) +
#'   geom_text(
#'     aes(label = scales::percent(..prop..), y = ..prop../2),
#'     stat = "fill_labels", position = position_dodge(width = .8)
#'   )
#' @export
stat_fill_labels <- function(mapping = NULL, data = NULL, geom = "text",
                             position = "identity", width = NULL, na.rm = FALSE, show.legend = NA,
                             inherit.aes = TRUE, ...) {
  layer(
    stat = StatFillLabels, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#' @export
StatFillLabels <- ggproto(
  "StatFillLabels",
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
      prop = count/sum(count),
      cumprop = cumsum(count)/sum(count),
      ylabel = (cumsum(count) - count / 2)/sum(count),
      na.rm = TRUE
    )
  },
  default_aes = aes(y = ..ylabel.., label = paste0(round(100 * ..prop.., digits =1), "%"))
)
