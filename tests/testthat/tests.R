library(testthat)
expect_that(eq_clean_data(), throws_error())
expect_that(eq_loc_helper(), throws_error())
expect_that(eq_location_clean(), throws_error())
expect_that(geom_timeline(55), throws_error())
expect_that(eq_map(55), throws_error())

