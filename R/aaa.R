ggalt_defaults <- function (x, y) {
  c(x, y[setdiff(names(y), names(x))])
}

ggalt_mapvalues <- function (x, from, to, warn_missing = TRUE)  {
  if (length(from) != length(to)) {
    stop("`from` and `to` vectors are not the same length.")
  }
  if (!is.atomic(x) && !is.null(x)) {
    stop("`x` must be an atomic vector or NULL.")
  }
  if (is.factor(x)) {
    levels(x) <- ggalt_mapvalues(levels(x), from, to, warn_missing)
    return(x)
  }
  mapidx <- match(x, from)
  mapidxNA <- is.na(mapidx)
  from_found <- sort(unique(mapidx))
  if (warn_missing && length(from_found) != length(from)) {
    message("The following `from` values were not present in `x`: ",
            paste(from[!(1:length(from) %in% from_found)], collapse = ", "))
  }
  x[!mapidxNA] <- to[mapidx[!mapidxNA]]
  x
}

ggalt_revalue <- function (x, replace = NULL, warn_missing = TRUE) {
  if (!is.null(x) && !is.factor(x) && !is.character(x)) {
    stop("x is not a factor or a character vector.")
  }
  ggalt_mapvalues(x, from = names(replace), to = replace, warn_missing = warn_missing)
}

ggalt_rename <- function (x, replace, warn_missing = TRUE,
                          warn_duplicated = TRUE) {
  names(x) <- ggalt_revalue(names(x), replace, warn_missing = warn_missing)
  duplicated_names <- names(x)[duplicated(names(x))]
  if (warn_duplicated && (length(duplicated_names) > 0L)) {
    duplicated_names_message <- paste0("`", duplicated_names,
                                       "`", collapse = ", ")
    warning("The ggalt_rename operation has created duplicates for the ",
            "following name(s): (", duplicated_names_message,
            ")", call. = FALSE)
  }
  x
}
