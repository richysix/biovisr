context('cbf_palette')

library(biovisr)

colour_blind_palette <- c(
  'blue' = rgb(0,0.45,0.7),
  'vermillion' = rgb(0.8, 0.4, 0),
  'blue_green' = rgb(0, 0.6, 0.5),
  'yellow' = rgb(0.95, 0.9, 0.25),
  'sky_blue' = rgb(0.35, 0.7, 0.9),
  'purple' = rgb(0.8, 0.6, 0.7),
  'black' = rgb(0, 0, 0),
  'orange' = rgb(0.9, 0.6, 0)
)

test_that('palette size greater than 8', {
  expect_warning(cbf_palette(12),
                 regexp = "A palette longer than 10 was requested",
                 label = 'palette size too big warning - 12')
  expect_warning(cbf_palette(10),
                 regexp = "A palette longer than 8 was requested. A palette including greys has been returned",
                 label = 'palette size too big warning - 10')
  expect_warning(cbf_palette(-2),
                 regexp = "8 colours have been returned",
                 label = 'palette size too small warning')
})

test_that('named or unnamed', {
  expect_identical(cbf_palette(4, named = TRUE), colour_blind_palette[1:4])
  expect_equal(cbf_palette(4, named = TRUE), colour_blind_palette[1:4])
})
