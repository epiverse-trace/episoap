#' @importFrom ggplot2 %+replace% theme theme_grey element_rect element_text element_line
theme_custom <- function(...) {
  # https://ggplot2.tidyverse.org/articles/ggplot2-in-packages.html#creating-a-new-theme

  custom_grey <- "#505B5B"
  green_grey <- "#5E7E80"

  theme_custom <- theme_grey(...) %+replace%
    theme(
      plot.background = element_rect(fill = "white"),
      panel.grid.major = element_line(colour = "#96a3a3"),
      panel.grid.minor = element_line(colour = "#9DC1C2", linetype = "dotted"),
      panel.background = element_rect(colour = custom_grey, fill = "white"),
      strip.background = element_rect(fill = green_grey, color = "white", size = 1),
      strip.text = element_text(colour = "white"),
      text = element_text(size = 16, colour = custom_grey),
      axis.ticks = element_line(colour = custom_grey)
    )

}

#' A custom \pkg{ggplot2} theme for \pkg{soap} reports
#'
#' @returns
#' A \pkg{ggplot2} [theme] object
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   labs(title = "Fuel economy declines as weight increases") +
#'   theme_soap()
#'
#' @export
theme_soap <- function() {

  theme_custom()

}
