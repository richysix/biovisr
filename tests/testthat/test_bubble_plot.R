context('bubble plot')
library(biovisr)

set.seed(20229)

test_data_cat <- data.frame(x = factor(rep(LETTERS[1:10], 10)),
                            y = factor(rep(1:10, each = 10), levels = 10:1),
                            size = sample(20:100, 100, replace = TRUE),
                            fill = runif(100))

test_data_num <- data.frame(x = runif(100, min = 1, max = 50),
                            y = runif(100, min = 1, max = 50),
                            size = sample(20:100, 100, replace = TRUE),
                            fill = runif(100))

cat_bubble_plot <- bubble_plot(test_data_cat)
num_bubble_plot <- bubble_plot(test_data_num)

# reverse y factor levels
test_data_cat$y <- factor(test_data_cat$y, levels = 10:1)
cat_bubble_plot_rev <- bubble_plot(test_data_cat)

test_that('categorical plot', {
  expect_equal_to_reference(cat_bubble_plot, 'cat_bubble_plot.rda')
})

test_that('continuous plot', {
  expect_equal_to_reference(num_bubble_plot, 'num_bubble_plot.rda')
})

test_that('categorical plot, y reversed', {
  expect_equal_to_reference(cat_bubble_plot_rev, 'cat_bubble_plot_rev.rda')
})

