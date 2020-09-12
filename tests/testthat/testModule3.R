library(leaflet)
library(ggplot2)

df <- noaa.data %>% eq_clean_data()

test_that("eq_map returns leaflet object", {
  map <- df %>%
    dplyr::filter(COUNTRY == "TURKEY", YEAR > 2005) %>%
    eq_map(annot_col = "COUNTRY")
  expect_is(map, "leaflet")
})

test_that("eq_create_label returns a proper html label", {
  data <- data.frame(LOCATION_NAME = c("A", "A", NA, "A"),
                     EQ_PRIMARY = c(5, NA, 4, 3),
                     TOTAL_DEATHS = c(NA, 10, 20, 30))
  expected <- c("<h3> Location: A<br>Magnitude: 5<br>Total deaths: NA</h3>",
                "<h3> Location: A<br>Magnitude: NA<br>Total deaths: 10</h3>",
                "<h3> Location: NA<br>Magnitude: 4<br>Total deaths: 20</h3>",
                "<h3> Location: A<br>Magnitude: 3<br>Total deaths: 30</h3>")
  expect_equal(eq_create_label(data), expected)
})
