#' Bytes formatter: convert to byte measurement and display symbol.
#'
#' @return a function with three parameters, \code{x}, a numeric vector that
#'   returns a character vector, \code{symbol} the byte symbol (e.g. "\code{Kb}")
#'   desired and the measurement \code{units} (traditional \code{binary} or
#'   \code{si} for ISI metric units).
#' @param x a numeric vector to format
#' @param symbol byte symbol to use. If "\code{auto}" the symbol used will be
#'   determined by the maximum value of \code{x}. Valid symbols are
#'   "\code{b}", "\code{K}", "\code{Mb}", "\code{Gb}", "\code{Tb}", "\code{Pb}",
#'   "\code{Eb}", "\code{Zb}", and "\code{Yb}", along with their upper case
#'   equivalents and "\code{iB}" equivalents.
#' @param units which unit base to use, "\code{binary}" (1024 base) or
#'   "\code{si}" (1000 base) for ISI units.
#' @references Units of Information (Wikipedia) :
#'   \url{http://en.wikipedia.org/wiki/Units_of_information}
#' @export
#' @examples
#' byte_format()(sample(3000000000, 10))
#' bytes(sample(3000000000, 10))
#' Kb(sample(3000000000, 10))
#' Mb(sample(3000000000, 10))
#' Gb(sample(3000000000, 10))
byte_format <- function(symbol="auto", units="binary") {
  function(x) bytes(x, symbol, units)
}

#' @export
#' @rdname byte_format
Kb <- byte_format("Kb", "binary")

#' @export
#' @rdname byte_format
Mb <- byte_format("Mb", "binary")

#' @export
#' @rdname byte_format
Gb <- byte_format("Gb", "binary")

#' @export
#' @rdname byte_format
bytes <- function(x, symbol="auto", units=c("binary", "si")) {

  symbol <- match.arg(symbol, c("auto",
        "b",  "Kb",  "Mb",  "Gb",  "Tb",  "Pb",  "Eb",  "Zb",  "Yb",
				"B",  "KB",  "MB",  "GB",  "TB",  "PB",  "EB",  "ZB",  "YB",
				     "KiB", "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB", "YiB"))

  units <- match.arg(units, c("binary", "si"))

  base <- switch(units, `binary`=1024, `si`=1000)

  if (symbol == "auto") {
    symbol <-
      if      (max(x) >= (base^5)) { "Pb" }
      else if (max(x) >= (base^4)) { "Tb" }
      else if (max(x) >= (base^3)) { "Gb" }
      else if (max(x) >= (base^2)) { "Kb" }
      else if (max(x) >= (base^1)) { "Mb" }
      else                         {  "b" }
  }

  switch(symbol,
	   "b" =,  "B"  = paste(x,                                 "bytes"),

	   "Kb" =, "KB" = paste(scales::comma(round(x/(base^1), 1L)), "Kb"),
	   "Mb" =, "MB" = paste(scales::comma(round(x/(base^2), 1L)), "Mb"),
	   "Gb" =, "GB" = paste(scales::comma(round(x/(base^3), 1L)), "Gb"),
	   "Tb" =, "TB" = paste(scales::comma(round(x/(base^4), 1L)), "Tb"),
	   "Pb" =, "PB" = paste(scales::comma(round(x/(base^5), 1L)), "Pb"),
	   "Eb" =, "EB" = paste(scales::comma(round(x/(base^6), 1L)), "Eb"),
	   "Zb" =, "ZB" = paste(scales::comma(round(x/(base^7), 1L)), "Zb"),
	   "Yb" =, "YB" = paste(scales::comma(round(x/(base^8), 1L)), "Yb"),

	   "KiB"        = paste(scales::comma(round(x/(base^1), 1L)), "KiB"),
	   "MiB"        = paste(scales::comma(round(x/(base^2), 1L)), "MiB"),
	   "GiB"        = paste(scales::comma(round(x/(base^3), 1L)), "GiB"),
	   "TiB"        = paste(scales::comma(round(x/(base^4), 1L)), "TiB"),
	   "PiB"        = paste(scales::comma(round(x/(base^5), 1L)), "PiB"),
	   "EiB"        = paste(scales::comma(round(x/(base^6), 1L)), "EiB"),
	   "ZiB"        = paste(scales::comma(round(x/(base^7), 1L)), "ZiB"),
	   "YiB"        = paste(scales::comma(round(x/(base^8), 1L)), "YiB")
  )

}
