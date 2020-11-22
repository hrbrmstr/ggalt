#' Jitter points to avoid overplotting
#'
#' Designed primarily for [geom_dumbbell()] to jitter the second point vertically
#' when both x values are equal.
#'
#' @family position adjustments
#' @param amount Amount of vertical jitter (0..1)
#' @export
position_jitter_on_equal <- function(amount = 0) {

  ggproto(
    NULL,
    PositionJitterOnEqual,
    amount = amount
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionJitterOnEqual <- ggproto(
  "PositionJitterOnEqual", Position,
  required_aes = c("x", "y"),

  setup_params = function(self, data) {
    list(
      amount = self$amount
    )
  },

  compute_layer = function(self, data, params, layout) {

    data[data$x == data$xend,]$point_y <-
      data[data$x == data$xend,]$point_y + (data[data$x == data$xend,]$point_y * params$amount)

    data

  }

)
