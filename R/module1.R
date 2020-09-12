
#' Creates a Date variable from year, month, day where year could be positive (AD) or negative (BC)
#'
#' @param input_year An integer (positive or negative) indicating the year. A negative integer represents BC
#' @param input_month An integer indicating the month. If NA, it defaults to 1
#' @param input_day An integer indicating the day of the month. If NA, it defaults to 1
#'
#' @return This function returns a Date variable
#' @import dplyr
#' @export
#'
#' @examples
#' eq_date_clean(2015, 3, 30)
#' eq_date_clean(-2249, 1, 31)
#'
eq_date_clean <- function(input_year, input_month, input_day) {
  if (is.na(input_year)) return(NA)

  format_year <- stringr::str_pad(abs(input_year), 4, pad = "0", side = "left")
  format_month <- dplyr::if_else(is.na(input_month), "01", stringr::str_pad(input_month, 2, pad = "0", side = "left"))
  format_day <- dplyr::if_else(is.na(input_day), "01", stringr::str_pad(input_day, 2, pad = "0", side = "left"))

  if (input_year <0) {
     return (lubridate::ymd(paste("0000", format_month, format_day, sep = "-")) - lubridate::years(format_year))
  } else return (lubridate::ymd(paste(format_year, format_month, format_day, sep = "-")))
}


#' Strips out country name (including colon) and concerts names to title case
#'
#' @param input_location String denoting the raw location field in the NOAA dataset
#'
#' @return String with the country name stripped out and in title case
#' @import dplyr
#' @export
#'
#' @examples
#' eq_location_clean("JORDAN:  BAB-A-DARAA,AL-KARAK")
#'
eq_location_clean <- function(input_location){
  stop <- stringr::str_locate(input_location, ":")

  if(is.na(stop[1])) return ("")

  substr(input_location, start = stop[1]+1, stop = nchar(input_location)) %>%
    stringr::str_trim() %>%
    stringr::str_to_title()

}

#' Wrapper function to clean the NOAA data, specifically uniting the YEAR, MONTH and DAY columns, converting the LATITUDE and LONGITUDE columns from character to double and stripping out the country name from the LOCATION_NAME column
#'
#' @param raw_noaa_df Dataframe representing the raw NOAA dataset
#'
#' @return This function returns a dataframe with an additional column called "date" that combines YEAR, MONTH and DAY columns
#' @export
#' @import dplyr
#'
#' @examples
#' eq_clean_data(noaa.data)
#'
eq_clean_data <- function(raw_noaa_df) {

  df <- raw_noaa_df %>%
    dplyr::mutate(date = lubridate::ymd(paste(YEAR, MONTH, DAY, sep = "-")),
                  LATITUDE = as.double(LATITUDE),
                  LONGITUDE = as.double(LONGITUDE)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(date = eq_date_clean(YEAR, MONTH, DAY),
                  LOCATION_NAME = eq_location_clean(LOCATION_NAME)) %>%
    dplyr::ungroup()
  return(df)

}

# eq_clean_data(noaa.data) %>% View()
