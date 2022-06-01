#' A ggplot theme for bubble plots
#'
#' \code{theme_bubble} produces a ggplot bubble plot from a data.frame
#'
#' This is a ggplot theme for bubble plots.
#' For categorical axes, it is theme_void with x and y axis labels
#' It also has grey dotted grid lines and the legend is place at the top
#'
#' If both x and y variables are continuous, theme_minimal is returned
#'
#' @param base_size      numeric    base size of text in points (default: 11)
#' @param base_family    character  base font family (default: "")
#' @param categorical    logical    are either of the x and y variable categorical (default: TRUE)
#'
#' @return ggplot theme object
#'
#' @examples
#'
#' set.seed(20962)
#' test_data_cat <- data.frame(x = factor(rep(LETTERS[1:10], 10)),
#'   y = factor(rep(1:10, each = 10), levels = 10:1),
#'   size = sample(20:100, 100, replace = TRUE),
#'   fill = runif(100))
#'
#' cat_bubble_plot <- bubble_plot(test_data_cat)
#' cat_bubble_plot + theme_bubble() + theme(axis.text.x = element_text(angle = 0, face = 'italic'))
#'
#' @export
theme_bubble <- function(base_size = 11, base_family = "", categorical = TRUE, ...) {
  if (categorical) {
    ggplot2::theme_void(base_size = base_size, base_family = base_family, ...) +
      ggplot2::theme(
        axis.text =
          ggplot2::element_text(family = "", face = "plain", colour = 'black',
                                size = base_size, hjust = 0, vjust = 0, angle = 0,
                                lineheight = 0.9, debug = FALSE),
        axis.text.x = ggplot2::element_text(angle = 90, hjust = 0.5),
        axis.text.y = ggplot2::element_text(vjust = 0.5),
        panel.grid.major = ggplot2::element_line(colour = 'grey80', linetype = 'dotted'),
        legend.position = 'top', legend.box = "vertical", legend.box.just = "left",
        legend.title = ggplot2::element_text(size = base_size),
        legend.text = ggplot2::element_text(size = base_size - 2)
      )
  } else {
    ggplot2::theme_minimal()
  }
}
