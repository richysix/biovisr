library(biovisr)
library(vdiffr)

set.seed(20229)

test_data <- data.frame(
  sample_name = paste0('sample_', 1:20),
  PC1 = rnorm(20, mean = 20, sd = 10),
  PC2 = rnorm(20, sd = 10),
  condition = factor(c(rep('baseline', 12), rep('wt', 4), rep('hom', 4)),
                     levels = c('wt', 'hom', 'baseline') ),
  stage = factor(rep(paste0('somites', 1:4), 5))
)

test_PCA_plot <-
  scatterplot_with_fill_and_shape(test_data, x_var = 'PC1', y_var = 'PC2',
                                  fill_var = 'stage', fill_palette = c('orange', 'steelblue3', 'hotpink2', 'black'),
                                  shape_var = 'condition', shape_palette = c('baseline' = 21, 'wt' = 22, 'hom' = 23),
                                  point_labels = 'sample_name'
  )

test_PCA_plot_no_labels <-
  scatterplot_with_fill_and_shape(test_data, x_var = 'PC1', y_var = 'PC2',
                                  fill_var = 'stage', fill_palette = c('orange', 'steelblue3', 'hotpink2', 'black'),
                                  shape_var = 'condition', shape_palette = c('baseline' = 21, 'wt' = 22, 'hom' = 23)
  )

different_labels <- paste0('alt_name_', 1:20)
test_PCA_plot_diff_labels <-
  scatterplot_with_fill_and_shape(test_data, x_var = 'PC1', y_var = 'PC2',
                                fill_var = 'stage', fill_palette = c('orange', 'steelblue3', 'hotpink2', 'black'),
                                shape_var = 'condition', shape_palette = c('baseline' = 21, 'wt' = 22, 'hom' = 23),
                                point_labels = different_labels)

test_that('PCA plot', {
  expect_doppelganger("test PCA plot", test_PCA_plot)
  expect_doppelganger("test PCA plot no labels", test_PCA_plot_no_labels)
  expect_doppelganger("test PCA plot different labels", test_PCA_plot_diff_labels)

  expect_error(  scatterplot_with_fill_and_shape(test_data, x_var = 'PC1', y_var = 'PC2',
                                                 fill_var = 'stage', fill_palette = c('orange', 'steelblue3', 'hotpink2', 'black'),
                                                 shape_var = 'condition', shape_palette = c('baseline' = 21, 'wt' = 22, 'hom' = 23),
                                                 point_labels = 'non_existant_col_name'
                  ),
                 'The supplied column for point_labels (.*) does not exist in the plot data'
  )
  different_labels <- paste0('alt_name_', 1:19)
  expect_error(  scatterplot_with_fill_and_shape(test_data, x_var = 'PC1', y_var = 'PC2',
                                                 fill_var = 'stage', fill_palette = c('orange', 'steelblue3', 'hotpink2', 'black'),
                                                 shape_var = 'condition', shape_palette = c('baseline' = 21, 'wt' = 22, 'hom' = 23),
                                                 point_labels = different_labels
  ),
  'Length of point_labels does not match the data'
  )
})
