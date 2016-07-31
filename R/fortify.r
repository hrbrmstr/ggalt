#' Fortify contingency tables
#'
#' @param model the contingency table
#' @param data data (unused)
#' @param ... (unused)
#' @export
fortify.table <- function(model, data, ...) {
  as_tibble(as.data.frame(model, stringsAsFactors=FALSE))
}
