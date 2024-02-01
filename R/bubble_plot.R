#' Create 'bubble' plot
#'
#' \code{bubble_plot} produces a ggplot bubble plot from a data.frame
#'
#' This function takes a data.frame and creates a bubble plot.
#' An x-y scatterplot where an attribute is represented by the size of the
#' points and optionally an another attribute is mapped to the colour.
#'
#' @param plot_df   data.frame  data to plot
#' @param x         character   Name of the variable to plot on the x-axis
#' @param y         character   Name of the variable to plot on the y-axis
#' @param size      character   Name of the variable to use for the size of the
#'   points
#' @param fill      character   Name of the variable to use for the fill colour
#' @param x_labels  character   Optional labels to use instead of levels of
#'   x-axis variable
#' @param y_labels  character   Optional labels to use instead of levels of
#'   y-axis variable
#' @param ...           Other arguments passed on to \code{\link[ggplot2]{geom_point}}
#'
#' @return plot - ggplot2 object
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
#'
#' test_data_num <- test_data_cat
#' test_data_num$x <- sample(1:50, 20)
#' test_data_num$y <- sample(1:50, 20)
#'
#' num_bubble_plot <- bubble_plot(test_data_num)
#'
#' @export
bubble_plot <- function(plot_df, x = 'x', y = 'y', size = 'size',
                        fill = 'fill', x_labels = NULL,
                        y_labels = NULL, ... ){
  x_var <- rlang::sym(x)
  y_var <- rlang::sym(y)
  size_var <- rlang::sym(size)
  fill_var <- rlang::sym(fill)
  # check labels is the same length as the levels of the x/y column
  if (!is.null(x_labels)) {
    if (length(x_labels) != nlevels(plot_df[[x]])) {
      stop('Supplied labels vector for x axis is the wrong length')
    }
  }
  if (!is.null(y_labels)) {
    if (length(y_labels) != nlevels(plot_df[[y]])) {
      stop('Supplied labels vector for y axis is the wrong length')
    }
  }

  # check type of x and y data
  x_is_categorical <- ifelse( class(plot_df[[x]]) == 'character' |
                                class(plot_df[[x]]) == 'factor',
                                TRUE, FALSE)
  y_is_categorical <- ifelse( class(plot_df[[y]]) == 'character' |
                                class(plot_df[[y]]) == 'factor',
                              TRUE, FALSE)
  # create theme
  bubble_plot <- ggplot2::ggplot(data = plot_df) +
    ggplot2::geom_point(ggplot2::aes(x = !!x_var, y = !!y_var, size = !!size_var,
                    fill = !!fill_var), shape = 21, ... ) +
    viridis::scale_fill_viridis(direction = -1) +
    theme_bubble(base_size = 12, categorical = (x_is_categorical | y_is_categorical) )

  if (x_is_categorical) {
    if (!is.null(x_labels) ) {
      bubble_plot <- bubble_plot +
        ggplot2::scale_x_discrete(position = 'top', labels = x_labels)
    } else {
      bubble_plot <- bubble_plot +
        ggplot2::scale_x_discrete(position = 'top')
    }
  }

  if (y_is_categorical) {
    if( !is.null(y_labels) ) {
      bubble_plot <- bubble_plot +
        ggplot2::scale_y_discrete(labels = y_labels)
    }
  }

  return(bubble_plot)
}
