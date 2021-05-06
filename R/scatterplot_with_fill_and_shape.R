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
#' @param point_labels character - labels for the points. Either a column name
#' from the plot_data or a character vector. Default: NULL - no labels
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
                                        point_labels = NULL,
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
  if (!is.null(point_labels)) {
    # check whether sample_names is a column name in the data or a vector of labels
    if (length(point_labels) == 1) {
      if (point_labels %in% colnames(plot_data)) {
        plot <- plot +
          ggrepel::geom_text_repel(
            ggplot2::aes_(label = as.name(point_labels)),
#            hjust = 0, vjust = 0,
#            nudge_x = 0.5, nudge_y = 0.5,
            size=4, show.legend=FALSE)
      } else {
        stop(paste0('The supplied column for point_labels (', point_labels,
                   ') does not exist in the plot data.\n'))
      }
    } else {
      # check whether point_labels is a vector of the same length as plot data
      if (length(point_labels) == nrow(plot_data)) {
        plot_data <- cbind(plot_data,
                           'point_labels' = point_labels)
        plot <- plot +
          ggrepel::geom_text_repel(
            ggplot2::aes_string(label = 'point_labels'),
            # hjust = 0, vjust = 0,
            # nudge_x = 0.5, nudge_y = 0.5,
            size=4, show.legend=FALSE)
      } else {
        stop(paste('Length of point_labels does not match the data.\n',
             'Either specify a column name or a vector the same length as the plot data\n'))
      }
    }

  }
  # change theme
  plot <- plot + ggplot2::theme_minimal()

  return(plot)
}

