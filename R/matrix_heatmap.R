#' Create heatmap plot for a data.frame
#'
#' \code{df_heatmap} produces a ggplot heatmap from a data.frame
#'
#' This function takes a data.frame and creates a heatmap plot.
#' The function assumes the data is in long form with a column for
#' the x variable, a column for the y variable and a column for the data values.
#' By default x and y axis labels are printed.
#'
#' @param plot_df       data.frame  data to plot
#' @param x             character   Name of the variable to plot on the x-axis
#' @param y             character   Name of the variable to plot on the y-axis
#' @param fill          character   Name of the variable to use for the fill colour
#' @param fill_palette  character   Name of the fill palette to use.
#' Must be one of 'viridis', 'plasma', 'magma', 'inferno', 'cividis'
#' @param colour        character  fixed value for the border colour of the heatmap tiles
#' @param size          integer  fixed value for the line width of the heatmap tiles border
#' @param xaxis_labels  logical/character   default=TRUE, FALSE means labels are not printed
#' if the value is a character vector of the same length as the levels of the x variable
#' these values are used instead
#' @param yaxis_labels  logical/character   default=TRUE, FALSE means labels are not printed
#' if the value is a character vector of the same length as the levels of the y variable
#' these values are used instead
#' @param na.translate  logical  whether to include NA values in the legend for categorical fill variables
#' @param ...           Other arguments passed on to \code{\link{theme_heatmap}}
#'
#' @return plot - ggplot2 object
#'
#' @examples
#'
#' set.seed(20962)
#' test_data_cat <- data.frame(cols = factor(rep(LETTERS[1:10], 10)),
#'   rows = factor(rep(1:10, each = 10), levels = 10:1),
#'   fill = runif(100))
#'
#' df_heatmap(test_data_cat, x = "cols", y = "rows", fill = "fill")
#'
#' df_heatmap(test_data_cat, x = "cols", y = "rows", fill = "fill",
#'     fill_palette = "inferno", xaxis_labels = FALSE, yaxis_labels = FALSE)
#'
#' df_heatmap(test_data_cat, x = "cols", y = "rows", fill = "fill",
#'     fill_palette = "inferno", xaxis_labels = letters[1:10], yaxis_labels = LETTERS[1:10])
#'
#' @export
df_heatmap <- function(plot_df, x, y, fill, fill_palette = "plasma",
                       colour = NULL, size = NULL,
                       xaxis_labels = TRUE, yaxis_labels = TRUE,
                       na.translate = TRUE, ...) {
  xvar <- rlang::sym(x)
  yvar <- rlang::sym(y)
  fillvar <- rlang::sym(fill)

  if (is.null(colour)) {
    if (is.null(size)) {
      heatmap_plot <- ggplot2::ggplot(data = plot_df) +
        ggplot2::geom_raster( ggplot2::aes(x = !!xvar, y = !!yvar, fill = !!fillvar ) )
    } else {
      heatmap_plot <- ggplot2::ggplot(data = plot_df) +
        ggplot2::geom_tile( ggplot2::aes(x = !!xvar, y = !!yvar, fill = !!fillvar ),
                            size = size )
    }
  } else {
    if (is.null(size)) {
      heatmap_plot <- ggplot2::ggplot(data = plot_df) +
        ggplot2::geom_tile( ggplot2::aes(x = !!xvar, y = !!yvar, fill = !!fillvar ),
                            colour = colour )
    } else {
      heatmap_plot <- ggplot2::ggplot(data = plot_df) +
        ggplot2::geom_tile( ggplot2::aes(x = !!xvar, y = !!yvar, fill = !!fillvar ),
                            colour = colour, linewidth = size )
    }
  }

  # sort out fill
  fill_is_categorical <- class(plot_df[[fill]]) %in% c('character', 'factor', 'logical')
  viridis_names <- c('magma', 'inferno', 'plasma', 'viridis', 'cividis',
                     'rocket', 'mako', 'turbo')
  brewer_qual <- c("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1",
                   "Set2", "Set3")
  brewer_quant <- c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "Blues",
                    "Greens", "Greys", "Oranges", "Purples", "Reds")
  illegal_name <- FALSE
  if (fill_is_categorical) {
    if (is.null(fill_palette)) {
      heatmap_plot <- heatmap_plot +
        ggplot2::scale_fill_manual(values = biovisr::cbf_palette(nlevels(plot_df[[fill]])),
                                   na.translate = na.translate,
                                   na.value = 'grey90')
    } else if (length(fill_palette) == 1) {
      if (fill_palette %in% viridis_names) {
        heatmap_plot <- heatmap_plot +
          ggplot2::scale_fill_viridis_d(option = fill_palette,
                                        na.translate = na.translate,
                                        na.value = 'grey90')
      } else if (fill_palette %in% brewer_qual) {
        heatmap_plot <- heatmap_plot +
          ggplot2::scale_fill_brewer(palette = fill_palette,
                                     na.translate = na.translate,
                                     na.value = 'grey90')
      } else {
        illegal_name <- TRUE
      }
    } else if (length(fill_palette) == nlevels(plot_df[[fill]])) {
      heatmap_plot <- heatmap_plot +
        ggplot2::scale_fill_manual(values = fill_palette,
                                   na.translate = na.translate,
                                   na.value = 'grey90')
    }
  } else {
    if (length(fill_palette) == 1) {
      if (fill_palette %in% viridis_names) {
        heatmap_plot <- heatmap_plot + ggplot2::scale_fill_viridis_c(option = fill_palette)
      } else if (fill_palette %in% brewer_quant) {
        heatmap_plot <- heatmap_plot + ggplot2::scale_fill_distiller(palette = fill_palette)
      } else {
        illegal_name <- TRUE
      }
    }
  }
  if (illegal_name) {
    stop("Could not match palette name. Must be one of ",
         paste0(c(viridis_names, brewer_qual, brewer_quant), collapse = ", "))
  }

  if (class(xaxis_labels) == "character") {
    if (length(xaxis_labels) == nlevels(plot_df[[x]])) {
      heatmap_plot <- heatmap_plot +
        ggplot2::scale_x_discrete(labels = xaxis_labels)
      xaxis_labels <- TRUE
    } else {
      var <- 'x'
      label_error_msg <- glue::glue(
        "Supplied {var}-axis labels are the wrong length.\n",
        "`{var}axis_labels` argument must be either TRUE or FALSE or ",
        "a character vector the same length as the number of unique values ",
        "of the {var} variable")
      rlang::abort(message = label_error_msg)
    }
  }
  if (class(yaxis_labels) == "character") {
    if (length(yaxis_labels) == nlevels(plot_df[[y]])) {
      heatmap_plot <- heatmap_plot +
        ggplot2::scale_y_discrete(labels = yaxis_labels)
      yaxis_labels <- TRUE
    } else {
      var <- 'y'
      label_error_msg <- glue::glue(
        "Supplied {var}-axis labels are the wrong length.\n",
        "`{var}axis_labels` argument must be either TRUE or FALSE or ",
        "a character vector the same length as the number of unique values ",
        "of the {var} variable")
      rlang::abort(message = label_error_msg)
    }
  }

  heatmap_plot <- heatmap_plot +
    theme_heatmap(xaxis_labels = xaxis_labels, yaxis_labels = yaxis_labels, ...)
  return(heatmap_plot)
}

#' Create heatmap plot for a matrix
#'
#' \code{matrix_heatmap} produces a ggplot heatmap from a matrix
#'
#' This function takes a matrix and creates a heatmap plot.
#' The default values for the x and y axis titles are Sample and Gene
#' respectively and the fill legend is labelled Value.
#' By default x and y axis labels are printed.
#'
#' @importFrom magrittr "%>%"
#'
#' @param plot_matrix   matrix      data to plot
#' @param x_title       character   Name for the x-axis
#' @param y_title       character   Name for the y-axis
#' @param fill_title    character   Name for the legend
#' @param fill_palette  character   Name of the fill palette to use.
#' Must be one of the viridis or color brewer palettes
#' @param xaxis_labels  logical/character   default=TRUE, FALSE means labels are not printed
#' if the value is a character vector of the same length as the levels of the x variable
#' these values are used instead
#' @param yaxis_labels  logical/character   default=TRUE, FALSE means labels are not printed
#' if the value is a character vector of the same length as the levels of the y variable
#' these values are used instead
#' @param colour        character  fixed value for the border colour of the heatmap tiles
#' @param size          integer  fixed value for the line width of the heatmap tiles border
#' @param na.translate  logical  whether to include NA values in the legend for categorical fill variables
#' @param ...           Other arguments passed on to \code{\link{theme_heatmap()}}
#'
#' @return plot - ggplot2 object
#'
#' @examples
#' set.seed(1638)
#' test_matrix <- matrix(
#'   sample(1:16),
#'   nrow = 4
#' )
#' matrix_heatmap(test_matrix)
#'
#' # specify x, y and fill titles and fill palette
#' matrix_heatmap(test_matrix, x_title = "Cols", y_title = "Rows",
#'                fill_title = "Log2FC", fill_palette = "inferno")
#'
#' # supply different labels for x and y axes
#' matrix_heatmap(test_matrix, x_title = "Sample", y = "Sample_2",
#'                fill_title = "Log2FC",
#'                fill_palette = "magma", xaxis_labels = letters[1:4],
#'                yaxis_labels = LETTERS[1:4])
#'
#' # remove x and y labels
#' matrix_heatmap(test_matrix, xaxis_labels = FALSE, yaxis_labels = FALSE)
#'
#' @export
matrix_heatmap <- function(data_matrix, x_title = "Sample", y_title = "Gene",
                           fill_title = "Value", fill_palette = "plasma",
                           xaxis_labels = TRUE, yaxis_labels = TRUE,
                           colour = NULL, size = NULL, na.translate = TRUE, ...) {
  # check dimnames of matrix
  if (is.null(dimnames(data_matrix))) {
    dimnames(data_matrix) <-
      list(paste('row', seq_len(nrow(data_matrix))),
           paste('col', seq_len(ncol(data_matrix))))
    names(dimnames(data_matrix)) <- c(y_title, x_title)
  } else if (!is.null(names(dimnames(data_matrix)))) {
    y_title <- names(dimnames(data_matrix))[1]
    x_title <- names(dimnames(data_matrix))[2]
  }
  # make sure x_title and y_title are not the same
  if (x_title == y_title) {
    xcol_name <- paste0(x_title, '2')
  } else {
    xcol_name <- x_title
  }

  # make matrix into long data frame and call df_heatmap
  matrix_df <- as.data.frame(data_matrix) %>%
    tibble::rownames_to_column(var = y_title) %>%
    tidyr::pivot_longer(., -!!y_title, names_to = xcol_name, values_to = fill_title)
  # make x and y columns factors
  matrix_df[[xcol_name]] <- factor(matrix_df[[xcol_name]],
                                 levels = colnames(data_matrix))
  # reverse levels of y col to make it look like the original matrix
  matrix_df[[y_title]] <- factor(matrix_df[[y_title]],
                                 levels = rev(rownames(data_matrix)))
  # also reverse order of y_labels if supplied
  if (class(yaxis_labels) == "character") {
    yaxis_labels <- rev(yaxis_labels)
  }

  # plot heatmap
  heatmap <- df_heatmap(matrix_df, x = xcol_name, y = y_title, fill = fill_title,
                        fill_palette = fill_palette, xaxis_labels = xaxis_labels,
                        yaxis_labels = yaxis_labels, colour = colour,
                        size = size, na.translate = na.translate, ...)
  heatmap <- heatmap + ggplot2::xlab(x_title)

  return(heatmap)
}
