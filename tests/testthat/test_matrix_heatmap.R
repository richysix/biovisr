context('Heatmap')
library(biovisr)

set.seed(20962)
test_data_cat <- data.frame(
  cols = factor(rep(LETTERS[1:10], 10)),
  rows = factor(rep(1:10, each = 10), levels = 10:1),
  fill = runif(100))

df_heatmap_plot <- df_heatmap(test_data_cat, x = "cols", y = "rows", fill = "fill")

df_heatmap_plot_no_labels <- df_heatmap(test_data_cat, x = "cols", y = "rows", fill = "fill",
    fill_palette = "inferno", xaxis_labels = FALSE, yaxis_labels = FALSE)

df_heatmap_plot_diff_labels <- df_heatmap(test_data_cat, x = "cols", y = "rows", fill = "fill",
    fill_palette = "inferno", xaxis_labels = letters[1:10], yaxis_labels = LETTERS[1:10])

test_that('data.frame heatmap', {
  expect_known_value(df_heatmap_plot, 'test_df_heatmap_plot.rda')
  expect_known_value(df_heatmap_plot_no_labels, 'test_df_heatmap_plot_no_labels.rda')
  expect_known_value(df_heatmap_plot_diff_labels, 'test_df_heatmap_plot_diff_labels.rda')
})

set.seed(1638)
test_matrix <- matrix(
  sample(1:16),
  nrow = 4
)
default_matrix_heatmap <- matrix_heatmap(test_matrix)

# specify x, y and fill titles and fill palette
matrix_heatmap_titles <- matrix_heatmap(test_matrix, x_title = "Cols", y_title = "Rows",
               fill_title = "Log2FC", fill_palette = "inferno")

# supply different labels for x and y axes
matrix_heatmap_labels <- matrix_heatmap(test_matrix, x_title = "Sample", y = "Sample_2",
               fill_title = "Log2FC",
               fill_palette = "magma", xaxis_labels = letters[1:4],
               yaxis_labels = LETTERS[1:4])
# remove x and y labels
matrix_heatmap_no_labels <- matrix_heatmap(test_matrix, xaxis_labels = FALSE, yaxis_labels = FALSE)

test_that('matrix heatmap', {
  expect_known_value(default_matrix_heatmap, 'test_matrix_heatmap_plot.rda')
  expect_known_value(matrix_heatmap_titles, 'test_matrix_heatmap_titles.rda')
  expect_known_value(matrix_heatmap_labels, 'test_matrix_heatmap_labels.rda')
  expect_known_value(matrix_heatmap_no_labels, 'test_matrix_heatmap_no_labels.rda')
})
