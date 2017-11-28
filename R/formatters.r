#' Bytes formatter: convert to byte measurement and display symbol.
#'
#' @return a function with three parameters, \code{x}, a numeric vector that
#'   returns a character vector, \code{symbol} a single or a vector of byte
#' symbol(s) (e.g. "\code{Kb}") desired and the measurement \code{units}
#' (traditional \code{binary} or \code{si} for ISI metric units).
#' @param x a numeric vector to format
#' @param symbol byte symbol to use. If "\code{auto}" the symbol used will be
#'   determined by the maximum value of \code{x}. Valid symbols are
#'   "\code{b}", "\code{K}", "\code{Mb}", "\code{Gb}", "\code{Tb}", "\code{Pb}",
#'   "\code{Eb}", "\code{Zb}", and "\code{Yb}", along with their upper case
#'   equivalents and "\code{iB}" equivalents.
#' @param units which unit base to use, "\code{binary}" (1024 base) or
#'   "\code{si}" (1000 base) for ISI units.
#' @param only_highest Whether to use the unit of the highest number or
#'   each number uses its own base.
#' @references Units of Information (Wikipedia) :
#'   \url{http://en.wikipedia.org/wiki/Units_of_information}
#' @export
#' @examples
#' byte_format()(sample(3000000000, 10))
#' bytes(sample(3000000000, 10))
#' Kb(sample(3000000000, 10))
#' Mb(sample(3000000000, 10))
#' Gb(sample(3000000000, 10))
byte_format <- function (symbol = "auto", units = "binary", only_highest = TRUE) {
  function(x) bytes(x, symbol, units, only_highest)
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
bytes <- function (x, symbol = 'auto', units = c('binary', 'si'),
                   only_highest = FALSE) {
  bin_names <- c("bytes", "Kb", "Mb", "Gb", "Tb", "Pb", "Eb", "Zb", "Yb")
  si_names  <- c("bytes", "KiB", "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB", "YiB")
  symbol    <- match.arg(symbol, unique(c("auto", bin_names, toupper(bin_names),
                                          si_names)), several.ok = TRUE)
  units     <- match.arg(units, c("binary", "si"))
  base      <- switch(units, binary = 1024,      si = 1000)
  out_names <- switch(units, binary = bin_names, si = si_names)
  sym_len   <- length(symbol)
  inp_len   <- length(x)
  if (sym_len == 1) {
    symbol  <- rep(symbol, inp_len)
    sym_len <- inp_len
  }
  if (sym_len != inp_len) {
    stop('Symbols argument must be either long 1 or of the same length as the input vector.')
  }
  symbol <- ifelse(symbol == "auto",
    pmax(floor(log(x, base)), 0),
    match(tolower(symbol), tolower(out_names)) - 1)
  if (only_highest) {
    symbol <- max(symbol, na.rm = TRUE)
  }
  res <- paste(scales::comma(round(x / base^symbol, 1L)),
               out_names[symbol + 1])
  ifelse(!is.na(x), res, x)
}

byte_format <- function (symbol = "auto", units = "binary", only_highest = TRUE) {
    function(x) bytes(x, symbol, units, only_highest)
}
