library(biovisr)
library(miscr)
library(vdiffr)

set.seed(456)
test_matrix <-
  matrix(
    c( c(sample(40:60, 10, replace = TRUE), sample(1:20, 10, replace = TRUE)),
       c(sample(1:20, 10, replace = TRUE), sample(40:60, 10, replace = TRUE)),
       c(sample(5:25, 10, replace = TRUE), sample(45:65, 10, replace = TRUE)),
       c(sample(40:50, 10, replace = TRUE), sample(10:30, 10, replace = TRUE)) ),
    ncol = 4,
    dimnames = list("genes" = paste0('gene_', 1:20),
                    "samples" = paste0('sample_', 1:4))
  )

cluster_res <- cluster(test_matrix, clustering = TRUE)

tree_plot_cat <- dendro_plot(cluster_res$clustering)
tree_plot <- dendro_plot(cluster_res$clustering,
                         categorical_scale = FALSE)

test_that('dendrogram', {
  expect_doppelganger("test default tree plot", tree_plot_cat)
  expect_doppelganger("test non categorical tree plot", tree_plot)
})
