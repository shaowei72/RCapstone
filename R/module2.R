
#' @importFrom ggplot2 aes draw_key_point
#' @importFrom grid pointsGrob linesGrob polylineGrob gList gpar
#' @importFrom scales alpha
GeomTimeline <- ggproto("GeomTimeline", Geom,
                       required_aes = c("x"),
                       default_aes = ggplot2::aes(y = 0, size = 3, colour = "grey", fill = "grey",
                                                  alpha = 0.6, shape = 21, stroke = 0.5, yscale = 1.0),
                       extra_params = c("na.rm", "xmin", "xmax"),
                       setup_data = function(data, params){
                         # str(params)
                         # str(data)
                         if (!is.null(params$xmin[1])) data <- data %>% dplyr::filter(as.Date(x, origin = "1970-01-01") >=
                                                                                        as.Date(params$xmin[1]))
                         if (!is.null(params$xmax[1])) data <- data %>% dplyr::filter(as.Date(x, origin = "1970-01-01") <=
                                                                                        as.Date(params$xmax[1]))


                         return(data)
                       },
                       # setup_panel_params = function(scale_x, scale_y, params) {
                       #   scale_y = scale_y * params$yscale
                       # },
                       draw_key = ggplot2::draw_key_point,
                       draw_panel = function(data, panel_scales, coord) {
                         # str(data)

                         # if no points in data, return nullGrob
                         if (nrow(data) == 0) return(grid::nullGrob())

                         # data <- data %>% dplyr::filter(data$x >= xmin & data$x <= xmax)

                         ## Transform the data first
                         coords <- coord$transform(data, panel_scales)
                         # coords$y = 0.8 * coords$y

                         ## Construct a grid grob
                         points <- grid::pointsGrob(
                           x = coords$x,
                           y = coords$y,
                           pch = coords$shape,
                           size = grid::unit(coords$size *0.25, "char"),
                           gp = grid::gpar(
                             fill = coords$fill,
                             colour = coords$fill,
                             alpha = coords$alpha
                           )
                         )

                         y_lines <- unique(coords$y)

                         lines <- grid::polylineGrob(
                           x = unit(rep(c(0,1), each= length(y_lines)), "npc"),
                           y = unit(c(y_lines, y_lines), "npc"),
                           id = rep(seq_along(y_lines), 2),
                           gp = grid::gpar(col = "grey", lwd = 1.5)
                         )

                         grid::gList(lines, points)

                       })

#' Creates a new geom to graphically depict of timelines of earthquake events
#'
#' @param data A preproccessed tibble (data.frame) of NOAA earthquakes
#' @param xmin  a character vector indicating the mininum date specified for earthquakes time line plotting. Should follow the format of YYYY-MM-DD.
#' @param xmax  a character vector indicating the maximum date specified for earthquakes time line plotting. Should follow the format of YYYY-MM-DD.
#'
#' @import ggplot2
#'
#' @examples
#' Capstone::noaa.data %>%
#'    Capstone::eq_clean_data() %>%
#'    dplyr::filter(COUNTRY %in% c("MEXICO", "INDONESIA", "CHINA")) %>%
#'    ggplot(aes(x = date, y = COUNTRY, label = LOCATION_NAME,group = COUNTRY, size = EQ_MAG_MW, fill = DEATHS/1000)) +
#'    geom_timeline(xmin = xmindate) +
#'    geom_timeline_label(n_max = 10, xmin = xmindate) +
#'    theme_classic()+ labs(y = "") +
#'    theme(legend.position = "bottom", axis.line.y = element_blank(), axis.ticks.y= element_blank())
#'

geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                         position = "identity", na.rm = FALSE, xmin = NULL, xmax = NULL,
                         show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(xmin = xmin, xmax = xmax, na.rm = na.rm, ...)
  )
}


GeomTimeline_Label <- ggproto("GeomTimeline_Label", Geom,
                        required_aes = c("x", "label"),
                        default_aes = ggplot2::aes(y = 0, size = 3, colour = "grey", fill = "grey",
                                                   alpha = 0.6, shape = 21, stroke = 0.5,
                                                   n_max = 5),
                        extra_params = c("na.rm", "n_max", "xmax", "xmin"),
                        setup_data = function(data, params) {
                          if (!is.null(params$xmin[1])) data <- data %>% dplyr::filter(as.Date(x, origin = "1970-01-01") >=
                                                                                         as.Date(params$xmin[1]))
                          if (!is.null(params$xmax[1])) data <- data %>% dplyr::filter(as.Date(x, origin = "1970-01-01") <=
                                                                                         as.Date(params$xmax[1]))

                          if (!is.null(params$n_max))
                            data <- data %>%
                              dplyr::group_by(group) %>%
                              dplyr::top_n(n = params$n_max, wt = size) %>%
                              dplyr::slice(1:params$n_max) %>%
                              dplyr::ungroup()

                        },
                        draw_key = ggplot2::draw_key_point,
                        draw_panel = function(data, panel_scales, coord) {
                          # if no points in data, return nullGrob
                          if (nrow(data) == 0) return(grid::nullGrob())

                          ## Transform the data first
                          coords <- coord$transform(data, panel_scales)

                          # coords$y = 0.8 * coords$y

                          ## Construct a grid grob
                          callout_line <- grid::segmentsGrob(
                            x0 = coords$x, x1 = coords$x,
                            y0 = coords$y, y1 = coords$y + 0.15,
                            gp = grid::gpar(
                              fill = coords$fill,
                              colour = coords$fill,
                              alpha = coords$alpha,
                              lwd = .5
                            )
                          )

                          callout_labels <- grid::textGrob(
                            label = coords$label,
                            x = coords$x, y = coords$y + 0.15, just = "left",
                            rot = 35,
                            gp = grid::gpar(fontsize = 6))

                          grid::gTree(children = grid::gList(callout_line, callout_labels))

                        })


#' Creates a new geom to label or annotate a timeline of earthquakes
#'
#' @param data A preproccessed tibble (data.frame) of NOAA earthquakes
#' @param xmin  a character vector indicating the mininum date specified for earthquakes time line plotting. Should follow the format of YYYY-MM-DD.
#' @param xmax  a character vector indicating the maximum date specified for earthquakes time line plotting. Should follow the format of YYYY-MM-DD.
#' #' @param n_max a numeric value for maximum number of earthquakes to be considered for annotations. The largest n_max earthquakes defined by the size aesthetic would be labelled
#'
#'
#' @examples
#' #' Capstone::noaa.data %>%
#'    Capstone::eq_clean_data() %>%
#'    dplyr::filter(COUNTRY %in% c("MEXICO", "INDONESIA", "CHINA")) %>%
#'    ggplot(aes(x = date, y = COUNTRY, label = LOCATION_NAME,group = COUNTRY, size = EQ_MAG_MW, fill = DEATHS/1000)) +
#'    geom_timeline(xmin = xmindate) +
#'    geom_timeline_label(n_max = 10, xmin = xmindate) +
#'    theme_classic()+ labs(y = "") +
#'    theme(legend.position = "bottom", axis.line.y = element_blank(), axis.ticks.y= element_blank())
#'
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE, xmin = NULL, xmax = NULL, n_max = NULL,
                          show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeline_Label, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n_max = n_max, xmin = xmin, xmax = xmax, ...)
  )
}



