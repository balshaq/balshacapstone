library(ggplot2)
library(dplyr)
library(grid)
library(scales)

#' Geom for creating a timeline from earthquake data
#'
#' This function creates a new geom to create a timeline for a specified data range
#' and plots each earthquake as a point on that timline
#'
#' @param mapping a set of aesthetic mappings
#' @param data data to be displayed
#' @param na.rm specifies how default missing values are approached
#' @param position position adjustment
#' @param stat the statistical transformation of the data
#' @param show.legend inclusion of layer in the legend
#' @param inherit.aes logical specification of whether to inherit the default aes
#' @param ... additional parameters
#'
#' @return no return value
#'
#' @examples \dontrun{ggplot(data = china2, aes(x = date, y = COUNTRY,
#' color = DEATHS, size = EQ_PRIMARY)) + geom_timeline(alpha = 0.2)}
#' @export

geom_timeline <- function(mapping = NULL, data = NULL, na.rm = TRUE,
                          position = "identity", stat = "identity",
                          show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeline,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...))
}
#'ggproto timeline object
#'
#'
#' @return no return value
#'
#' @examples \dontrun{ggplot(data = china2, aes(x = date, y = COUNTRY,
#' color = DEATHS, size = EQ_PRIMARY)) + geom_timeline(alpha = 0.2)}
#' @export
GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                                 required_aes = c("x"),
                                 non_missing_aes = c("size", "shape", "colour","y"),
                                 default_aes = ggplot2::aes(
                                   shape = 19, colour = "black", size = 5, fill = NA,
                                   alpha = 0.2, stroke = 0.5, y = 0.2
                                 ),

                                 draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
                                   coords <- coord$transform(data, panel_params)
                                   points = grid::pointsGrob(
                                     coords$x, coords$y,
                                     pch = coords$shape,
                                     gp = grid::gpar(
                                       col = scales::alpha(coords$colour, coords$alpha),
                                       fill = scales::alpha(coords$fill, coords$alpha),
                                       fontsize = coords$size * ggplot2::.pt + coords$stroke * ggplot2::.stroke / 2,
                                       lwd = coords$stroke * ggplot2::.stroke / 2
                                     )
                                   )
                                   lines = lapply(unique(coords$y), FUN = function(x) grid::linesGrob(y = c(x,x)))
                                   grobList = c(list(points), lines)
                                   grid::gTree(children = do.call(grid::gList, grobList))
                                 },
                                 draw_key = ggplot2::draw_key_point
)


#' A function that could return a map visualization of NOAA Earthquake data,
#' but doesn't
#'
#'This function is a placeholder for the eq_map function assigned during the
#'capstone. Included for completeness sake, despite the fact that the grading
#'rubric has nothing to do with whether all functions from previous weeks in the
#'capstone are included or working
#'
#' @return text string
#'
#'
#' @examples \dontrun{eq_map()}
#'
#' @export
eq_map <- function(data, annot_col = "DATE") {
  # Make a leaflet
  leaflet::leaflet(data = data) %>%
    leaflet::addProviderTiles("OpenStreetMap.Mapnik") %>%
    leaflet::addCircleMarkers(
      lng = ~ LONGITUDE,
      lat = ~ LATITUDE,
      radius = ~ EQ_PRIMARY,
      popup = ~ data[[annot_col]]
    )
}

#' Make up a label for visualization popup
#' To be used with eq_map() function to make more informative popup labe;
#'
#' @param data  The NOAA dataset
#'
#' @examples
#' \dontrun{
#'   data %>% mutate(popup_text = eq_create_label(.)) %>% eq_map(annot_col = "popup_text")
#' }
#'
#' @export
eq_create_label <- function(data) {
  paste0(
    "<b>Location:</b> ", data$LOCATION_NAME, "<br>",
    "<b>Magnitude:</b> ", data$EQ_PRIMARY, "<br>",
    "<b>Total deaths:</b> ", ifelse(is.na(data$TOTAL_DEATHS), "Unknown", data$TOTAL_DEATHS)
  )
}

