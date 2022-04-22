#' A colour-blind friendly colour palette
#'
#' \code{cbf_palette} returns a colour friendly palette
#'
#' These colours come from https://jfly.uni-koeln.de/color
#' Accepts either a number, or a character/factor vector
#' If the supplied vale is a number, a vector of the specified length is returned.
#' If a character vector is supplied, a vector of the same length is returned.
#' If a factor vector is supplied, a vector of the same length as the number of factor levels is returned.
#'
#' If the length of the requested palette is greater than 8, a warning is generated.
#' If the length is 9 or 10, a palette containing greys is returned with a warning.
#' If the length greater than 10, a palette of length 10 containing greys is returned with a warning.
#'
#' If no value supplied the a palette of 8 colours is returned.
#' This palette is intended for categorical data.
#'
#' @param x         numeric/character/factor
#' @param named     logical    should the vector be named. default = FALSE
#'
#' @return named vector of colours
#'
#' @examples
#' cbf_palette()
#'
#' cbf_palette(4)
#'
#' @export
cbf_palette <- function(x = 8, named = FALSE) {
  colour_blind_palette <- c(
    'blue' = rgb(0,0.45,0.7),
    'vermillion' = rgb(0.8, 0.4, 0),
    'blue_green' = rgb(0, 0.6, 0.5),
    'yellow' = rgb(0.95, 0.9, 0.25),
    'sky_blue' = rgb(0.35, 0.7, 0.9),
    'purple' = rgb(0.8, 0.6, 0.7),
    'black' = rgb(0, 0, 0),
    'orange' = rgb(0.9, 0.6, 0),
    'grey60' = "#CCCCCC",
    'grey20' = "#666666"
  )
  if (class(x) == "character") {
    palette_size <- length(x)
  } else if (class(x) == "factor") {
    palette_size <- nlevels(x)
  } else {
    palette_size <- x[1]
  }

  if (palette_size > 8) {
    if (palette_size > 10) {
      warning("A palette longer than 10 was requested. 10 colours (including greys) have been returned")
      palette <- colour_blind_palette
    } else {
      warning("A palette longer than 8 was requested. A palette including greys has been returned")
      palette <- colour_blind_palette[seq_len(palette_size)]
    }
  } else if (palette_size < 1) {
    warning("A palette size of less than one was requested. 8 colours have been returned")
    palette <- colour_blind_palette
  } else {
    palette <- colour_blind_palette[seq_len(palette_size)]
  }

  if (!named) {
    palette <- unname(palette)
  } else {
    if (class(x) == "character") {
      names(palette) <- x
    } else if (class(x) == "factor") {
      names(palette) <- levels(x)
    }
  }

  return(palette)
}
