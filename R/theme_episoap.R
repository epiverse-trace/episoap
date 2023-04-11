#' @importFrom ggplot2 %+replace%
#' @importFrom ggplot2 theme theme_grey element_rect element_text element_line
theme_custom <- function(...) {

  # https://ggplot2.tidyverse.org/articles/ggplot2-in-packages.html

  custom_grey <- "#505B5B"
  green_grey <- "#5E7E80"

  theme_grey(...) %+replace%
    theme(
      plot.background = element_rect(fill = "white"),
      panel.grid.major = element_line(colour = "#96a3a3"),
      panel.grid.minor = element_line(colour = "#9DC1C2", linetype = "dotted"),
      panel.background = element_rect(colour = custom_grey, fill = "white"),
      strip.background = element_rect(
        fill = green_grey,
        color = "white",
        linewidth = 1
      ),
      strip.text = element_text(colour = "white"),
      text = element_text(size = 16, colour = custom_grey),
      axis.ticks = element_line(colour = custom_grey)
    )

}

#' A custom \pkg{ggplot2} theme for \pkg{episoap} reports
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
#'   theme_episoap()
#'
#' @export
theme_episoap <- function() {

  theme_custom()

}
