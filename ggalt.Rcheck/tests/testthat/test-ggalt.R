context("geom_dumbbell functionality")
test_that("geom_dumbbell basic creation works", {
  df <- data.frame(trt = LETTERS[1:5], l = c(20, 40, 10, 30, 50), r = c(70, 50, 30, 60, 80))

  p <- ggplot(df, aes(y = trt, x = l, xend = r)) +
    geom_dumbbell(size = 3, color = "#e3e2e1",
                  colour_x = "#5b8124", colour_xend = "#bad744")

  expect_s3_class(p, "ggplot")
  expect_true(length(p$layers) == 1)
})

context("geom_lollipop functionality")
test_that("geom_lollipop basic creation works", {
  df <- data.frame(trt = LETTERS[1:10], value = seq(100, 10, by = -10))

  p <- ggplot(df, aes(trt, value)) + geom_lollipop()

  expect_s3_class(p, "ggplot")
  expect_true(length(p$layers) == 1)
})

context("geom_xspline functionality")
test_that("geom_xspline basic creation works", {
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_xspline()

  expect_s3_class(p, "ggplot")
  expect_true(length(p$layers) == 1)
})

context("geom_encircle functionality")
test_that("geom_encircle basic creation works", {
  d <- data.frame(x = c(1, 1, 2), y = c(1, 2, 2) * 100)

  p <- ggplot(d, aes(x, y)) +
    scale_x_continuous(expand = c(0.5, 1)) +
    scale_y_continuous(expand = c(0.5, 1)) +
    geom_encircle(s_shape = 1, expand = 0) + geom_point()

  expect_s3_class(p, "ggplot")
  expect_true(length(p$layers) == 2)
})

context("annotation_ticks functionality")
test_that("annotation_ticks basic creation works", {
  p <- ggplot(msleep, aes(bodywt, brainwt)) + geom_point() + annotation_ticks(sides = 'l')

  expect_s3_class(p, "ggplot")
  expect_true(length(p$layers) == 2) # point layer + annotation_ticks layer
})

context("position_dodgev functionality")
test_that("position_dodgev works with dumbbell", {
  df <- data.frame(
    trt = c(LETTERS[1:5], "D"),
    l = c(20, 40, 10, 30, 50, 40),
    r = c(70, 50, 30, 60, 80, 70)
  )

  p <- ggplot(df, aes(y = trt, x = l, xend = r)) +
    geom_dumbbell(size = 3, color = "#e3e2e1",
                  colour_x = "#5b8124", colour_xend = "#bad744",
                  position = position_dodgev(height = 0.4))

  expect_s3_class(p, "ggplot")
})

context("geom_ubar functionality")
test_that("geom_ubar basic creation works", {
  p <- ggplot(economics, aes(date, uempmed)) + geom_ubar()

  expect_s3_class(p, "ggplot")
  expect_true(length(p$layers) == 1)
})

context("stat_stepribbon functionality")
test_that("stat_stepribbon basic creation works", {
  df <- data.frame(
    x = 1:10,
    ymin = runif(10),
    ymax = runif(10) + 0.5
  )

  p <- ggplot(df, aes(x = x, ymin = ymin, ymax = ymax)) + stat_stepribbon()

  expect_s3_class(p, "ggplot")
  expect_true(length(p$layers) == 1)
})