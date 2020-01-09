#' A colour-blind friendly colour palette
#'
#' \code{cbf_palette} returns a colour friendly palette
#'
#' Returns a vector of the specified length. If the supplied length is
#' greater than 8, a warning is generated and a vector of length 8 is returned.
#' If no length is supplied the whole palette of 8 colours is returned.
#' This palette is intended for categorical data.
#'
#' @param palette_size   numeric    number of colours required
#'
#' @return named vector of colours
#'
#' @examples
#' palette <- cbf_palette()
#'
#' palette <- cbf_palette(4)
#'
#' @export
cbf_palette <- function(palette_size = 8) {
  colour_blind_palette <- c(
    'blue' = rgb(0,0.45,0.7),
    'vermillion' = rgb(0.8, 0.4, 0),
    'blue_green' = rgb(0, 0.6, 0.5),
    'yellow' = rgb(0.95, 0.9, 0.25),
    'sky_blue' = rgb(0.35, 0.7, 0.9),
    'purple' = rgb(0.8, 0.6, 0.7),
    'black' = rgb(0, 0, 0),
    'orange' = rgb(0.9, 0.6, 0)
  )
  if (palette_size > 8) {
    warning("A palette longer than 8 was requested. 8 colours have been returned")
    return(colour_blind_palette)
  } else if (palette_size < 1) {
    warning("8 colours have been returned")
    return(colour_blind_palette)
  } else {
    return(colour_blind_palette[seq_len(palette_size)])
  }
}