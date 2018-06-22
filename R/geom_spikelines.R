#' @title Draw spikelines on a plot
#' @description Segment reference lines that originate at an point
#' @inheritParams ggplot2::geom_line
#' @examples
#'
#' mtcars$name <- rownames(mtcars)
#'
#' p <- ggplot(data = mtcars, aes(x=mpg,y=disp)) + geom_point()
#'
#' p + geom_spikelines(data = mtcars[mtcars$carb==4,],linetype = 2)
#'
#' p + geom_spikelines(data = mtcars[mtcars$carb==4,],aes(colour = factor(gear)), linetype = 2)
#'
#' \dontrun{
#' require(ggrepel)
#' p + geom_spikelines(data = mtcars[mtcars$carb==4,],aes(colour = factor(gear)), linetype = 2) +
#' ggrepel::geom_label_repel(data = mtcars[mtcars$carb==4,],aes(label = name))
#' }
#'
#' @rdname geom_spikelines
#' @author Jonathan Sidi
#' @export

geom_spikelines <- function(mapping = NULL, data = NULL,
                         stat = "identity", position = "identity",
                         ...,
                         arrow = NULL,
                         lineend = "butt",
                         linejoin = "round",
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSpikelines,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      arrow = arrow,
      lineend = lineend,
      linejoin = linejoin,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggalt-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomSpikelines <- ggproto("GeomSpikelines", Geom,
                       required_aes = c("x", "y"),
                       non_missing_aes = c("linetype", "size", "shape"),
                       default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA),

                       draw_panel = function(data, panel_params, coord, arrow = NULL,
                                             lineend = "butt", linejoin = "round", na.rm = FALSE) {

                         data1 <- data
                         data2 <- data

                         data1$xend <- data$x
                         data1$yend <- data$y
                         data1$y <- 0


                         data2$xend <- data$x
                         data2$yend <- data$y
                         data2$x <- 0

                         data <- rbind(data1,data2)

                         data <- remove_missing(data, na.rm = na.rm,
                                                c("x", "y", "linetype", "size", "shape"),
                                                name = "geom_spikelines")
                         if (empty(data)) return(zeroGrob())

                         if (coord$is_linear()) {
                           coord <- coord$transform(data, panel_params)

                           ret <- grid::segmentsGrob(coord$x, coord$y, coord$xend, coord$yend,
                                                     default.units = "native",
                                                     gp = grid::gpar(
                                                       col = alpha(coord$colour, coord$alpha),
                                                       fill = alpha(coord$colour, coord$alpha),
                                                       lwd = coord$size * .pt,
                                                       lty = coord$linetype,
                                                       lineend = lineend,
                                                       linejoin = linejoin
                                                     ),
                                                     arrow = arrow
                           )

                           return(ret)
                         }

                         data$group <- 1:nrow(data)
                         starts <- subset(data, select = c(-xend, -yend))
                         ends <- plyr::rename(subset(data, select = c(-x, -y)), c("xend" = "x", "yend" = "y"),
                                              warn_missing = FALSE)

                         pieces <- rbind(starts, ends)
                         pieces <- pieces[order(pieces$group),]

                         GeomPath$draw_panel(pieces, panel_params, coord, arrow = arrow,
                                             lineend = lineend)
                       },

                       draw_key = draw_key_path
)

empty <- function (df)
{
  is.null(df) || nrow(df) == 0 || ncol(df) == 0
}
