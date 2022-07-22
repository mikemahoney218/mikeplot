#' Opinionated ggplot2 themes
#'
#' @param base_fill The color to use for both plot and panel background.
#' @param base_size The base text size, used for axis labels. All other elements
#' are sized in proportion to this value.
#' @param base_family The font family to use.
#'
#' @return An object of classes `theme` and `gg`.
#'
#' @examples
#' ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
#'   geom_point() +
#'   labs(
#'     title = "Fuel economy declines as weight increases",
#'     subtitle = "Larger engines are generally heavier"
#'    ) +
#'   theme_pub()
#'
#' @rdname themes
#' @export
theme_pub <- function(base_fill = "white",
                      base_size = 11,
                      base_family = "") {
  ggplot2::theme_minimal() %+replace%
    ggplot2::theme(axis.line = element_line(),
          panel.grid = element_blank(),
          axis.text = element_text(size = base_size * 0.9),
          axis.title = element_text(size = base_size * 1.15),
          text = element_text(family = base_family),
          strip.background = element_blank(),
          strip.text = element_text(size = base_size),
          legend.text = element_text(size = base_size * 0.85),
          legend.title = element_text(hjust = 0, size = base_size * 1.1),
          legend.background = element_blank(),
          legend.spacing = unit(base_size, "pt"),
          legend.margin = margin(0, 0, 0, 0),
          legend.key = element_blank(),
          legend.key.size = unit(1.2 * base_size, "pt"),
          legend.box.margin = margin(0, 0, 0, 0),
          legend.box.background = element_blank(),
          legend.box.spacing = unit(base_size, "pt"),
          panel.background = element_rect(fill = base_fill, color = NA),
          plot.background = element_rect(fill = base_fill, color = NA),
          title = element_text(size = base_size * 1.3, family = base_family),
          complete = TRUE
    )
}

#' @rdname themes
#' @export
theme_map <- function(base_line_color = "grey80",
                      base_fill = "white",
                      base_size = 11,
                      base_family = "") {
  theme_pub(
    base_fill = base_fill,
    base_size = base_size,
    base_family = base_family
  ) %+replace%
    theme(legend.position = "bottom",
          panel.background = element_rect(fill = base_fill, color = base_fill),
          plot.background = element_rect(fill = base_fill, color = base_fill),
          panel.grid.major = element_line(size = 0.25, color = base_line_color),
          axis.line = element_blank(),
          legend.justification = c("center"),
          legend.key.width = unit(1.75 * base_size, "pt"))
}
