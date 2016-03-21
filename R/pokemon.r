#' Pokemon color scales
#'
#' Color scales using the colors in the Pokemon characters
#'
#' @references \href{http://www.pokegraphs.com/}{Original JSON color list}.
#' @note Pokémon & Pokémon character names are trademarks of Nintendo.
#' @inheritParams ggplot2::scale_colour_hue
#' @inheritParams pokemon_pal
#' @family pokemon aeshetics
#' @rdname scale_pokemon
#' @export
scale_colour_pokemon <- function(avatar="bulbasaur", ...) {
  discrete_scale("colour", "pokemon", pokemon_pal(avatar=avatar), ...)
}

#' @rdname scale_pokemon
#' @export
scale_color_pokemon <- scale_colour_pokemon

#' @rdname scale_pokemon
#' @export
scale_fill_pokemon <- function(avatar=FALSE, ...) {
  discrete_scale("fill", "pokemon", pokemon_pal(avatar=avatar), ...)
}

#' Pick a Pokemon palette
#'
#' @references \href{http://www.pokegraphs.com/}{Original JSON color list}.
#' @note Pokémon & Pokémon character names are trademarks of Nintendo.
#' @param avatar avatar name. Use \code{list_avatars()} to see them all!
#' @family pokemon aeshetics
#' @export
pokemon_pal <- function(avatar="bulbasaur") {

  avatar <- tolower(avatar)

  if (!(avatar %in% names(pokemon))) {
    message("avatar not found, using 'bulbasaur'")
    avatar <- "bulbasaur"
  }

  best_colors <- function(avatar, n=1) {
    unname(pokemon[[avatar]])
  }

  function(n) {
    best_colors(avatar, n)
  }

}

#' List Pokemon avatar names
#'
#' @references \href{http://www.pokegraphs.com/}{Original JSON color list}.
#' @note Warning: huge! list\cr
#'   \cr
#'   Pokémon & Pokémon character names are trademarks of Nintendo.
#' @family pokemon aeshetics
#' @export
list_avatars <- function() {
  sort(names(pokemon))
}
