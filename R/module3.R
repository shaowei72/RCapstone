


#' Creates a geographical map using OpenStreetMap leaflet tiles to display earthquake locations based on cleaned NOAA dataset
#'
#' @param data A preproccessed tibble (data.frame) of NOAA earthquakes dataset
#' @param annot_col A character. The name of the column in the data that should be used as descriptor.
#'
#' @return A leaflet map with earthquakes and annotations.
#' @export
#' @importFrom dplyr filter select mutate
#' @importFrom leaflet colorFactor addTiles addCircleMarkers
#'
#' @examples
#' \dontrun {
#' eq_map(data, annot_col = "LOCATION_NAME")
#' }
eq_map <- function(data, annot_col = NULL) {

  if (!is.null(annot_col)) { #check annot_col parameter is valid
    checkout <- lapply(annot_col, FUN = function(x) {
      x %in% colnames(data)
    })
    annot_col <- annot_col[unlist(checkout)]
  }

  if (is.null(annot_col)) return(NULL)

  data["my_popup"] = with(data, get(annot_col))

  # for (i in 1:length(annot_col)) {
  #   data$my_popup <- with(data, paste0(data$my_popup, annot_col[i], ": ", get(annot_col[i]), "<br>"))
  # }
  # data$my_popup <- with(data, paste0(data$my_popup, "</h3>"))

  m1 <- data %>%
    leaflet() %>% leaflet::addTiles() %>%
    leaflet::addCircleMarkers(lat = ~LATITUDE,
                              lng = ~LONGITUDE,
                              color = "blue",
                              stroke = FALSE,
                              fillOpacity = 0.5,
                              radius = ~EQ_PRIMARY,
                              popup = ~my_popup)

  return(m1)

}

#' Creates a html label for leaflet map based on location name, magnitude and casualties from NOAA earthquake data
#'
#' @param data A data frame containing cleaned NOAA earthquake data
#'
#' @return A character vector with labels
#' @details The input \code{data.frame} needs to include columns LOCATION_NAME, EQ_PRIMARY and TOTAL_DEATHS with the earthquake location, magintude and total casualties respectively.
#'
#' @export
#' @examples
#' #' \dontrun{
#' eq_create_label(data)
#' }
eq_create_label <- function(data) {
  txt_label <- paste0("<h3> Location: ", data$LOCATION_NAME, "<br>")
  txt_label <- paste0(txt_label, "Magnitude: ", data$EQ_PRIMARY, "<br>")
  txt_label <- paste0(txt_label, "Total deaths: ", data$TOTAL_DEATHS, "</h3>")
  return(txt_label)

}
