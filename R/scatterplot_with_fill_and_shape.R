#' Plot 2 columns against each other with Fill and Shapes
#'
#' \code{scatterplot_with_fill_and_shape} Takes a data.frame and creates an x-y scatterplot
#'
#'        The plot data data.frame should contain the columns for the x, y and fill variables
#'        and optionally ones for the shape variable and text labels.
#'        The text label column should be named sample_name.
#'
#' @param plot_data data.frame - plot data
#' @param x_var character - name of the column to plot on the x axis
#' @param y_var character - name of the column to plot on the y axis
#' @param fill_var character - name of the column to use as the fill aesthetic
#' @param fill_palette character - a named character vectors of colours for the fill aesthetic
#' @param shape_var character - name of the column to use as the shape aesthetic
#' @param shape_palette character - a named character vectors of colours for the shape aesthetic
#' @param samples_names logical - whether text labels should be added to label the points
#'
#' @return ggplot2 object
#'
#' @examples
#' scatterplot_with_fill_and_shape( plot_data, 'PC1', 'PC2', 'Gene', fill_palette,
#'                              'Genotype', shape_palette, sample_names )
#'
#' @export
#'
scatterplot_with_fill_and_shape <- function(plot_data, x_var, y_var,
                                        fill_var, fill_palette,
                                        shape_var = 'None', shape_palette,
                                        sample_names = TRUE,
                                        point_size = 4, ...) {
  plot <- ggplot2::ggplot(data = plot_data,
                          ggplot2::aes_(x = as.name(x_var), y = as.name(y_var)))

  if (shape_var == 'None') {
    plot <- plot +
      ggplot2::geom_point(ggplot2::aes_(fill = as.name(fill_var)),
                 size = point_size, shape = 21,
                 colour = 'black')
  } else {
    plot <- plot +
      ggplot2::geom_point(ggplot2::aes_(fill = as.name(fill_var),
                                        shape = as.name(shape_var)),
                                        size = point_size,
                                        colour = 'black') +
      ggplot2::scale_shape_manual(values = shape_palette,
                                  guide = ggplot2::guide_legend(order = 2))
  }

  if (class(plot_data[[fill_var]]) == 'factor') {
    # add fill scale
    plot <- plot +
      ggplot2::scale_fill_manual(
        values = fill_palette,
        guide = ggplot2::guide_legend(override.aes = list(shape = 21),
                             order = 1)
      )
  } else {
    # fill_palette should be either viridis or diverging
    if(fill_palette == 'viridis'){
      plot <- plot + viridis::scale_fill_viridis(...)
    } else if (fill_palette == 'diverging') {
      plot <- plot +
        ggplot2::scale_fill_gradient2(low = '#2166ac', mid = 'white',
                                      high = '#b2182b', midpoint = 0)
    }
  }

  # add text labels
  if (sample_names) {
    plot <- plot +
      ggplot2::geom_text(
        ggplot2::aes_string(label = 'sample_name'),
        hjust = 0, vjust = 0,
        nudge_x = 0.5, nudge_y = 0.5,
        size=4, show.legend=FALSE)
  }
  # change theme
  plot <- plot + ggplot2::theme_minimal()

  return(plot)
}

