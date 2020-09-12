library(stringr)

df <- Capstone::noaa.data %>% eq_clean_data()

test_that("eq_clean_data creates DATE column", {
  expect_true("date" %in% colnames(df))
})


test_that("eq_clean_data convert long/lat columns", {
  expect_true(is.numeric(df$LATITUDE))
  expect_true(is.numeric(df$LONGITUDE))
})

test_that("eq_clean_data cleans LOCATION_NAME column", {
  expect_true(length(which(df$LOCATION_NAME %in% df$COUNTRY)) == 0)
  expect_true(length(which(df$LOCATION_NAME != stringr::str_to_title(df$LOCATION_NAME))) == 0)
})
