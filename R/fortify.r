#' Fortify contingency tables
#'
#' @param model the contingency table
#' @param data data (unused)
#' @param ... (unused)
#' @export
fortify.table <- function(model, data, ...) {
  out <- as.data.frame(model, stringsAsFactors=FALSE)
  class(out) <- c("tbl_df", "tbl", "data.frame")
  out
}
