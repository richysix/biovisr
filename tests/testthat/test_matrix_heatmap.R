context('Heatmap')
library(biovisr)

set.seed(20962)
test_data <- data.frame(
  cols = factor(rep(LETTERS[1:10], 10)),
  rows = factor(rep(1:10, each = 10), levels = 10:1),
  fill = runif(100),
  cat = factor(sample(c('otter', 'seal', 'orca'), 100, replace = TRUE))
)

# defaults
df_heatmap_plot <- df_heatmap(test_data, x = "cols", y = "rows", fill = "fill")
# no axis labels
df_heatmap_plot_no_labels <- df_heatmap(test_data, x = "cols", y = "rows", fill = "fill",
    fill_palette = "inferno", xaxis_labels = FALSE, yaxis_labels = FALSE)
# changing labels
df_heatmap_plot_diff_labels <- df_heatmap(test_data, x = "cols", y = "rows", fill = "fill",
    fill_palette = "inferno", xaxis_labels = letters[1:10], yaxis_labels = LETTERS[1:10])
# cat data NULL fill_palette
df_heatmap_plot_palette_null <- df_heatmap(test_data, x = "cols", y = "rows", fill = "cat",
                                           fill_palette = NULL)
# cat data viridis fill palette
df_heatmap_plot_palette_viridis <- df_heatmap(test_data, x = "cols", y = "rows", fill = "cat",
                              fill_palette = "magma")
# cat data brewer fill palette
df_heatmap_plot_palette_brewer <- df_heatmap(test_data, x = "cols", y = "rows", fill = "cat",
                              fill_palette = "Set2")
# cat data manual fill palette
df_heatmap_plot_palette_manual <-
  df_heatmap(test_data, x = "cols", y = "rows", fill = "cat",
             fill_palette = c('otter' = 'firebrick3', 'seal' = 'cornflowerblue', 'orca' = 'goldenrod') )

# cat data box colour and size
df_heatmap_plot_colour_size <- df_heatmap(test_data, x = "cols", y = "rows", fill = "cat",
                                           colour = "black", size = 0.8)
# continuous data viridis fill palette
df_heatmap_plot_palette_viridis_c <-
  df_heatmap(test_data, x = "cols", y = "rows", fill = "fill",
             fill_palette = 'cividis' )
# continuous data brewer fill palette
df_heatmap_plot_palette_distiller <-
  df_heatmap(test_data, x = "cols", y = "rows", fill = "fill",
             fill_palette = 'OrRd' )

test_that('data.frame heatmap', {
  expect_known_value(df_heatmap_plot, 'test_df_heatmap_plot.rds')
  expect_known_value(df_heatmap_plot_no_labels, 'test_df_heatmap_plot_no_labels.rds')
  expect_known_value(df_heatmap_plot_diff_labels, 'test_df_heatmap_plot_diff_labels.rds')
})

test_that('data.frame heatmap palettes', {
  expect_known_value(df_heatmap_plot_palette_null, 'test_df_heatmap_plot_palette_null.rds')
  expect_known_value(df_heatmap_plot_palette_viridis, 'test_df_heatmap_plot_palette_viridis.rds')
  expect_known_value(df_heatmap_plot_palette_brewer, 'test_df_heatmap_plot_palette_brewer.rds')
  # cat data non brewer, non viridis fill palette
  expect_warning(df_heatmap(test_data, x = "cols", y = "rows", fill = "cat", fill_palette = "cheese"),
                 regexp = "Unknown palette")
  expect_known_value(df_heatmap_plot_palette_manual, 'test_df_heatmap_plot_palette_manual.rds')
  expect_known_value(df_heatmap_plot_palette_viridis_c, 'test_df_heatmap_plot_palette_viridis_c.rds')
  expect_known_value(df_heatmap_plot_palette_distiller, 'test_df_heatmap_plot_palette_distiller.rds')
})

test_that('data.frame heatmap palettes', {
  expect_known_value(df_heatmap_plot_colour_size, 'test_df_heatmap_plot_colour_size.rds')
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
  expect_known_value(default_matrix_heatmap, 'test_matrix_heatmap_plot.rds')
  expect_known_value(matrix_heatmap_titles, 'test_matrix_heatmap_titles.rds')
  expect_known_value(matrix_heatmap_labels, 'test_matrix_heatmap_labels.rds')
  expect_known_value(matrix_heatmap_no_labels, 'test_matrix_heatmap_no_labels.rds')
})
