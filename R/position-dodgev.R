#' @title Vertically dodge position
#' @param height numeric, height of vertical dodge, Default: NULL
#' @examples
#'
#' if(interactive()){
#'
#' dat <- data.frame(
#'   trt = c(LETTERS[1:5], "D"),
#'   l = c(20, 40, 10, 30, 50, 40),
#'   r = c(70, 50, 30, 60, 80, 70)
#' )
#'
#' ggplot(dat, aes(y=trt, x=l, xend=r)) +
#'  geom_dumbbell(size=3, color="#e3e2e1",
#'                colour_x = "#5b8124", colour_xend = "#bad744",
#'                dot_guide=TRUE, dot_guide_size=0.25,
#'                position=position_dodgev(height=0.8)) +
#'  labs(x=NULL, y=NULL, title="ggplot2 geom_dumbbell with dot guide") +
#'  theme_minimal() +
#'  theme(panel.grid.major.x=element_line(size=0.05))
#'
#'  }
#'
#' @rdname position-dodgev
#' @author @@ggstance authors
#' @note position-dodgev(): unmodified from lionel-/ggstance/R/position-dodgev.R 73f521384ae8ea277db5f7d5a2854004aa18f947
#' @export
position_dodgev <- function(height = NULL) {
  ggplot2::ggproto(NULL, PositionDodgev, height = height)
}

#' @rdname position-dodgev
#' @format NULL
#' @usage NULL
#' @export
PositionDodgev <- ggplot2::ggproto("PositionDodgev", ggplot2::Position,
                                   required_aes = "y",
                                   height = NULL,
                                   setup_params = function(self, data) {
                                     if (is.null(data$ymin) && is.null(data$ymax) && is.null(self$height)) {
                                       warning("Height not defined. Set with `position_dodgev(height = ?)`",
                                               call. = FALSE)
                                     }
                                     list(height = self$height)
                                   },

                                   compute_panel = function(data, params, scales) {
                                     collidev(data, params$height, "position_dodgev", pos_dodgev, check.height = FALSE)
                                   }
)

pos_dodgev <- function(df, height) {
  n <- length(unique(df$group))
  if (n == 1) return(df)

  if (!all(c("ymin", "ymax") %in% names(df))) {
    df$ymin <- df$y
    df$ymax <- df$y
  }

  d_height <- max(df$ymax - df$ymin)

  # df <- data.frame(n = c(2:5, 10, 26), div = c(4, 3, 2.666666,  2.5, 2.2, 2.1))
  # ggplot(df, aes(n, div)) + geom_point()

  # Have a new group index from 1 to number of groups.
  # This might be needed if the group numbers in this set don't include all of 1:n
  groupidx <- match(df$group, sort(unique(df$group)))

  # Find the center for each group, then use that to calculate ymin and lmax
  df$y <- df$y + height * ((groupidx - 0.5) / n - .5)
  df$ymin <- df$y - d_height / n / 2
  df$ymax <- df$y + d_height / n / 2

  df
}

collidev <- function(data, height = NULL, name, strategy, ..., check.height = TRUE, reverse = FALSE) {
  # Determine height
  if (!is.null(height)) {
    # Width set manually
    if (!(all(c("ymin", "ymax") %in% names(data)))) {
      data$ymin <- data$y - height / 2
      data$ymax <- data$y + height / 2
    }
  } else {
    if (!(all(c("ymin", "ymax") %in% names(data)))) {
      data$ymin <- data$y
      data$ymax <- data$y
    }

    # Width determined from data, must be floating point constant
    heights <- unique(data$ymax - data$ymin)
    heights <- heights[!is.na(heights)]

    #   # Suppress warning message since it's not reliable
    #     if (!zero_range(range(heights))) {
    #       warning(name, " requires constant height: output may be incorrect",
    #         call. = FALSE)
    #     }
    height <- heights[1]
  }

  # Reorder by x position, then on group. The default stacking order reverses
  # the group in order to match the legend order.
  if (reverse) {
    data <- data[order(data$ymin, data$group), ]
  } else {
    data <- data[order(data$ymin, -data$group), ]
  }


  # Check for overlap
  intervals <- as.numeric(t(unique(data[c("ymin", "ymax")])))
  intervals <- intervals[!is.na(intervals)]

  if (length(unique(intervals)) > 1 & any(diff(scale(intervals)) < -1e-6)) {
    warning(name, " requires non-overlapping y intervals", call. = FALSE)
    # This is where the algorithm from [L. Wilkinson. Dot plots.
    # The American Statistician, 1999.] should be used
  }

  data$group <- seq_len(nrow(data)) ## reset grouping

  if (!is.null(data$xmax)) {
    plyr::ddply(data, "ymin", strategy, ..., height = height)
  } else if (!is.null(data$x)) {
    data$xmax <- data$x
    data <- plyr::ddply(data, "ymin", strategy, ..., height = height)
    data$x <- data$xmax
    data$yend <- data$y ## ALLOW FOR A YEND COLUMN
    data
  } else {
    stop("Neither x nor xmax defined")
  }
}
