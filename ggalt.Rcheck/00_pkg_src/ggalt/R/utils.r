"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

"%|W|%" <- function(a, b) {
  if (!is.waive(a)) a else b
}

is.waive <- function(x) inherits(x, "waiver")

# Compute central angle between two points.
# Multiple by radius of sphere to get great circle distance
# @arguments longitude
# @arguments latitude
dist_central_angle <- function(lon, lat) {
  # Convert to radians
  lat <- lat * pi / 180
  lon <- lon * pi / 180

  hav <- function(x) sin(x / 2) ^ 2
  ahav <- function(x) 2 * asin(x)

  n <- length(lat)
  ahav(sqrt(hav(diff(lat)) + cos(lat[-n]) * cos(lat[-1]) * hav(diff(lon))))
}


expand_default <- function(scale, discrete = c(0, 0.6), continuous = c(0.05, 0)) {
  scale$expand %|W|% if (scale$is_discrete()) discrete else continuous
}


# Col union
# Form the union of columns in a and b.  If there are columns of the same name in both a and b, take the column from a.
#
# @param data frame a
# @param data frame b
# @keyword internal
cunion <- function(a, b) {
  if (length(a) == 0) return(b)
  if (length(b) == 0) return(a)

  cbind(a, b[setdiff(names(b), names(a))])
}

# Given a theme object and element name, return a grob for the element
element_render <- function(theme, element, ..., name = NULL) {

  # Get the element from the theme, calculating inheritance
  el <- calc_element(element, theme)
  if (is.null(el)) {
    message("Theme element ", element, " missing")
    return(zeroGrob())
  }

  ggname(paste(element, name, sep = "."), element_grob(el, ...))
}


# Name ggplot grid object
# Convenience function to name grid objects
#
# @keyword internal
ggname <- function(prefix, grob) {
  grob$name <- grobName(grob, prefix)
  grob
}

# Convert a snake_case string to camelCase
camelize <- function(x, first = FALSE) {
  x <- gsub("_(.)", "\\U\\1", x, perl = TRUE)
  if (first) x <- firstUpper(x)
  x
}

snakeize <- function(x) {
  x <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", x)
  x <- gsub(".", "_", x, fixed = TRUE)
  x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
  tolower(x)
}

firstUpper <- function(s) {
  paste(toupper(substring(s, 1,1)), substring(s, 2), sep = "")
}

snake_class <- function(x) {
  snakeize(class(x)[1])
}


# Look for object first in parent environment and if not found, then in
# ggplot2 namespace environment.  This makes it possible to override default
# scales by setting them in the parent environment.
find_global <- function(name, env, mode = "any") {
  if (exists(name, envir = env, mode = mode)) {
    return(get(name, envir = env, mode = mode))
  }

  nsenv <- asNamespace("ggalt")
  if (exists(name, envir = nsenv, mode = mode)) {
    return(get(name, envir = nsenv, mode = mode))
  }

  NULL
}

find_subclass <- function (super, class, env) {
  name <- paste0(super, camelize(class, first = TRUE))
  obj <- find_global(name, env = env)
  if (is.null(name)) {
    stop("No ", tolower(super), " called ", name, ".", call. = FALSE)
  }
  else if (!inherits(obj, super)) {
    stop("Found object is not a ", tolower(super), ".", call. = FALSE)
  }
  obj
}

alt_aesthetics <- function(type, name) {
  obj <- switch(type,
                geom = find_subclass("Geom", name, globalenv()),
                stat = find_subclass("Stat", name, globalenv())
  )
  aes <- alt_aesthetics_item(obj)

  paste("\\code{", type, "_", name, "} ",
        "understands the following aesthetics (required aesthetics are in bold):\n\n",
        "\\itemize{\n",
        paste("  \\item \\code{", aes, "}", collapse = "\n", sep = ""),
        "\n}\n", sep = "")
}

alt_aesthetics_item <- function(x) {
  req <- x$required_aes
  all <- union(req, sort(x$aesthetics()))

  ifelse(all %in% req,
         paste0("\\strong{", all, "}"),
         all
  )
}
