#' A ggplot theme for heatmap plots
#'
#' \code{theme_heatmap} produces a ggplot heatmap plot from a data.frame
#'
#' This is a ggplot theme for heatmap plots.
#' For categorical axes, it is theme_void with x and y axis labels
#' It also has grey dotted grid lines and the legend is place at the top
#'
#' If both x and y variables are continuous, theme_minimal is returned
#'
#' @param xaxis_labels  logical   whether axis labels are printed, default=TRUE
#' @param yaxis_labels  logical   whether axis labels are printed, default=TRUE
#' @param ...           Other arguments passed on to \code{\link[ggplot2]{theme_void}}
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
#' cat_heatmap_plot <- heatmap_plot(test_data_cat)
#' cat_heatmap_plot + theme_heatmap() + theme(axis.text.x = element_text(angle = 0, face = 'italic'))
#'
#' @export
theme_heatmap <- function(xaxis_labels = TRUE, yaxis_labels = TRUE, ...) {
  args <- list(...)
  base_family <- ifelse(is.null(args$base_family), "", args$base_family)
  base_size <- ifelse(is.null(args$base_size), 11, args$base_size)
  heatmap_theme <- ggplot2::theme_void(...) +
    ggplot2::theme(
      axis.title =
        ggplot2::element_text(family = base_family, face = "plain", colour = 'black',
                              size = base_size, hjust = 0.5, vjust = 0.5, angle = 0,
                              lineheight = 0.9, debug = FALSE),
      axis.title.y =
        ggplot2::element_text(angle = 90),
      axis.text =
        ggplot2::element_text(family = base_family, face = "plain", colour = 'black',
                              size = base_size - 2, hjust = 1, vjust = 0.5, angle = 0,
                              lineheight = 0.9, debug = FALSE),
      axis.text.x = ggplot2::element_text(angle = 90),
      legend.position = 'right', legend.box = "vertical", legend.box.just = "left",
      legend.title = ggplot2::element_text(size = base_size),
      legend.text = ggplot2::element_text(size = base_size - 2)
    )
  if (!xaxis_labels){
    heatmap_theme <- heatmap_theme +
      ggplot2::theme(
        axis.text.x = ggplot2::element_blank()
      )
  }
  if (!yaxis_labels){
    heatmap_theme <- heatmap_theme +
      ggplot2::theme(
        axis.text.y = ggplot2::element_blank()
      )
  }
  return(heatmap_theme)
}
