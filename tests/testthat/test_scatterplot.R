context('Scatterplot')
library(biovisr)

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
                                  shape_var = 'condition', shape_palette = c('baseline' = 21, 'wt' = 22, 'hom' = 23)
  )

test_that('PCA plot', {
  expect_equal_to_reference(test_PCA_plot, 'test_pca_plot.rda')
})
