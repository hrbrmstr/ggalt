.onAttach <- function(...) {

  if (!interactive()) return()

  load_stateface()

  packageStartupMessage(paste0("ggalt is under *active* development. ",
                               "See https://github.com/hrbrmstr/ggalt for changes"))

}

