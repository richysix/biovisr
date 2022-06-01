#' Create dendrogram from a clustering object
#'
#' \code{dendro_plot} produces a ggplot dendrogram from an hclust object
#'
#' This function takes an hclust object and uses ggdendro to produce
#' a ggplot dendrogram.
#'
#' @param hclust_obj  hclust  tree to plot
#' @param categorical_scale logical whether the x axis should be categorical or not
#' This affects the space around the axes and whether the tree plot will line up with
#' other plots with categorical axes
#'
#' @return plot - ggplot2 object
#'
#' @examples
#'
#'
#' @export
dendro_plot <- function(hclust_obj, categorical_scale = TRUE, ...) {
  tree_plot_data <- ggdendro::dendro_data(hclust_obj)

  if ( categorical_scale ) {
    label_data <- ggdendro::label(tree_plot_data) %>%
      dplyr::mutate(label = factor(label, levels = label))
    dendro_plot <- ggplot2::ggplot() +
      ggplot2::geom_text(data = label_data,
                ggplot2::aes(x = label, y = y, label = label),
                angle = 90, hjust = 1, colour = NA)
  } else {
    dendro_plot <- ggplot2::ggplot()
  }

  dendro_plot <- dendro_plot +
    ggplot2::geom_segment(data = ggdendro::segment(tree_plot_data),
                          size = 0.3, lineend = 'square',
                          ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
    ggplot2::theme_void()

  return(dendro_plot)
}
