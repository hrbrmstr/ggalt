#' Plot a time series as a horizon plot
#'
#' A horizon plot breaks the Y dimension down using colours. This is useful
#' when visualising y values spanning a vast range and / or trying to highlight
#' outliers without losing context of the rest of the data.\cr \cr Horizon
#' plots are best viewed in an aspect ratio of very low vertical length.
#'
#' @md
#' @section Aesthetics: `x`, `y`, `fill`. `fill` defaults to `..band..` which is
#'     the band number the current data fill area belongs in.
#' @section Other parameters: `bandwidth`, to dictate the span of a band.
#' @export
geom_horizon <-  function(mapping = NULL, data = NULL, show.legend = TRUE,
                          inherit.aes = TRUE, na.rm = TRUE, bandwidth = NULL, ...) {

  list(
    layer(
      data = data,
      mapping = mapping,
      stat = "horizon",
      geom = GeomHorizon,
      position = 'identity',
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(bandwidth = bandwidth, na.rm = na.rm, ...)
    )
  )

}

#' @rdname geom_horizon
#' @keywords internal
#' @export
GeomHorizon <- ggproto("GeomHorizon", GeomArea,
  required_aes = c("x", "y"),
  default_aes = plyr::defaults(
    aes(fill=NA, size = 0.15, linetype = 1, alpha = NA, colour = "gray20"),
    ggplot2::GeomArea$default_aes
  ),
  draw_key = ggplot2::draw_key_rect
)


#' Transforms data for a horizon plot
#' @rdname geom_horizon
#' @export
stat_horizon <- function(mapping = NULL, data = NULL, geom = "horizon", show.legend = TRUE,
                         inherit.aes = TRUE, na.rm = TRUE, bandwidth = NULL, ...) {

  list(
    layer(
      stat = StatHorizon,
      data = data,
      mapping = mapping,
      geom = geom,
      position = 'identity',
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(bandwidth = bandwidth, na.rm = na.rm, ...)
    )
  )

}

#' @rdname geom_horizon
#' @keywords internal
#' @export
StatHorizon <- ggproto(
  "StatHorizon",
  Stat,
  required_aes = c("x", "y"),
  default_aes = aes(fill=..band..),
  setup_params = function(data, params) {

    # calculating a default bandwidth
    if (is.null(params$bandwidth)) {
      params$bandwidth <- diff(range(data$y)) / 4
      message(sprintf("bandwidth not specified. Using computed bandwidth %s",
                      params$bandwidth))
    }

    params$n_min_y <- min(data$y, na.rm = TRUE)

    params

  },

  compute_group = function(data, scales, bandwidth, n_min_y) {

    # calculating the band in which the values fall
    data$fillb <- ((data$y - n_min_y) %/% bandwidth) + 1

    # calculating the banded y value
    orig_y <- data$y
    orig_fill_b <- data$fillb

    data$y <- data$y - (bandwidth * (data$fillb - 1)) - n_min_y

    fill_bands <- sort(unique(data$fillb))

    # for each band, calculating value at a particular x
    banded_data <- lapply(

      fill_bands,

      function(i_fill_band) {

        df_banded_data <- data[data$fillb == i_fill_band,]

        df_banded_data_high <- data[data$fillb > i_fill_band,]

        if (nrow(df_banded_data_high) > 0) {
          df_banded_data_high$y <- bandwidth
          df_banded_data_high$fillb <- i_fill_band
        }

        df_banded_data_low <- data[data$fillb < i_fill_band,]

        if (nrow(df_banded_data_low) > 0) {
          df_banded_data_low$y <- 0
          df_banded_data_low$fillb <- i_fill_band
        }

        data <- rbind(
          rbind(df_banded_data, df_banded_data_low),
          df_banded_data_high
        )

        data$fillb <- data$fillb * bandwidth

        data$band <- i_fill_band
        data$group <- i_fill_band

        data

      }

    )

    data <- do.call(rbind, banded_data)

    data$band <- factor(data$band)

    data

  }

)
