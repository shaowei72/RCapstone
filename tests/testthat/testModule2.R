

df <- noaa.data %>% eq_clean_data()

test_that("geom_timeline returns ggplot object", {
  g <- df %>%
    dplyr::filter(COUNTRY %in% c("TURKEY"), YEAR > 2005) %>%
    ggplot2::ggplot(ggplot2::aes(x = DATE, y = COUNTRY)) +
    geom_timeline()
  expect_is(g, "ggplot")
})


test_that("geom_timeline_label returns ggplot object", {
  g <- df %>%
    dplyr::filter(COUNTRY %in% c("TURKEY"), YEAR > 2005) %>%
    ggplot2::ggplot(ggplot2::aes(x = DATE, y = COUNTRY)) +
    geom_timeline_label(ggplot2::aes(label = LOCATION_NAME))
  expect_is(g, "ggplot")
})
