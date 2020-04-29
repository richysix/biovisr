#' Create dendrogram from a clustering object
#'
#' \code{dendro_plot} produces a ggplot dendrogram from an hclust object
#'
#' This function takes an hclust object and use ggdendro to produce
#' a ggplot dendrogram.
#'
#' @param hclust_obj  hclust  tree to plot
#'
#' @return plot - ggplot2 object
#'
#' @examples
#'
#'
#' @export
dendro_plot <- function(hclust_obj, ...) {
  tree_plot_data <- ggdendro::dendro_data(hclust_obj)
  dendro_plot <- ggplot2::ggplot() +
    ggplot2::geom_segment(data = ggdendro::segment(tree_plot_data), size = 0.3, lineend = 'square',
                          ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
    ggplot2::theme_void()

  return(dendro_plot)
}
