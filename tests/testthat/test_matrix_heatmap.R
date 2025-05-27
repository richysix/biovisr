library(biovisr)
library(vdiffr)

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
             fill_palette = 'Greens' )

test_that('df_heatmap: axis labels wrong length errors', {
  expect_error(df_heatmap(test_data, x = "cols", y = "rows", fill = "fill", xaxis_labels = letters[1:11]),
               regexp = "x-axis labels are the wrong length")
  expect_error(df_heatmap(test_data, x = "cols", y = "rows", fill = "fill", yaxis_labels = letters[1:11]),
               regexp = "y-axis labels are the wrong length")
})

test_that('data.frame heatmap', {
  expect_doppelganger("test df heatmap plot", df_heatmap_plot)
  expect_doppelganger("test df heatmap plot no labels", df_heatmap_plot_no_labels)
  expect_doppelganger("test df heatmap plot change labels", df_heatmap_plot_diff_labels)
})

test_that('data.frame heatmap palettes', {
  expect_doppelganger("test heatmap NULL fill pal", df_heatmap_plot_palette_null)
  expect_doppelganger("test heatmap viridis fill pal", df_heatmap_plot_palette_viridis)
  expect_doppelganger("test heatmap brewer fill pal", df_heatmap_plot_palette_brewer)
  # cat data non brewer, non viridis fill palette
  expect_error(df_heatmap(test_data, x = "cols", y = "rows", fill = "cat", fill_palette = "cheese"),
                 regexp = "Could not match palette name")
  expect_doppelganger("test heatmap manual fill pal", df_heatmap_plot_palette_manual)
  expect_doppelganger("test heatmap viridis cont fill pal", df_heatmap_plot_palette_viridis_c)
  expect_doppelganger("test heatmap brewer dist fill pal", df_heatmap_plot_palette_distiller)
})

test_that('data.frame heatmap palettes', {
  expect_doppelganger("test heatmap box colour size", df_heatmap_plot_colour_size)
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
  expect_doppelganger("test matrix heatmap default", default_matrix_heatmap)
  expect_doppelganger("test matrix heatmap titles", matrix_heatmap_titles)
  expect_doppelganger("test matrix heatmap labels", matrix_heatmap_labels)
  expect_doppelganger("test matrix heatmap no labels", matrix_heatmap_no_labels)
})

test_that('matrix_heatmap: axis labels wrong length errors', {
  expect_error(matrix_heatmap(test_matrix, xaxis_labels = LETTERS[1:5]),
               regexp = "x-axis labels are the wrong length")
  expect_error(matrix_heatmap(test_matrix, yaxis_labels = LETTERS[1:5]),
               regexp = "y-axis labels are the wrong length")
})
