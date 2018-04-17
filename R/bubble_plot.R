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
    ggplot2::geom_point(ggplot2::aes_(x = as.name(x), y = as.name(y), size = as.name(size),
                    fill = as.name(fill)), shape = 21, ... ) +
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
#' \code{bubble_plot} uses this theme internally
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
theme_bubble <- function(base_size = 11, base_family = "", categorical = TRUE) {
  if (categorical) {
    ggplot2::theme_void(base_size = base_size, base_family = base_family) +
      ggplot2::theme(
        axis.text =
          ggplot2::element_text(family = "", face = "plain", colour = 'black',
                                size = base_size, hjust = 0, vjust = 0, angle = 0,
                                lineheight = 0.9, debug = FALSE),
        axis.text.x = ggplot2::element_text(angle = 90),
        panel.grid.major = ggplot2::element_line(colour = 'grey80', linetype = 'dotted'),
        legend.position = 'top',
        legend.title = ggplot2::element_text(size = base_size + 2),
        legend.text = ggplot2::element_text(size = base_size)
      )
  } else {
    ggplot2::theme_minimal()
  }
}
