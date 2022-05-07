# define colors
n_gray   <- rgb(88, 88, 90, maxColorValue = 255)
n_blue   <- rgb(0, 17, 88, maxColorValue = 255)
n_lblue  <- rgb(92,177,235, maxColorValue = 255)
n_green  <- rgb(0,136,58, maxColorValue = 255)
n_orange <- rgb(241, 135, 0, maxColorValue = 255)
n_purple <- rgb(176, 32, 121, maxColorValue = 255)
n_turquoise <- rgb(52, 163, 169, maxColorValue = 255)
n_red    <- rgb(190,25,8, maxColorValue = 255)
n_brown  <- rgb(139, 69, 19, maxColorValue = 255)

naglr_palette <- c(
    n_gray,
    n_green,
    n_blue,
    n_orange,
    n_purple,
    n_turquoise,
    n_red,
    n_lblue,
    n_brown
)

#' A qualitative color palette
#'
#'
#' @examples
#' library(scales)
#' scales::show_col(naglr_pal()(8))
#' @export
naglr_pal <- function() {
    scales::manual_pal(naglr_palette)
}

#' Discrete color & fill scales
#'
#' @rdname naglr_pal
#'
#' @inheritParams ggplot2::scale_colour_hue
#' @export
scale_color_naglr <- function(...) {
    ggplot2::discrete_scale("colour", "naglr", naglr_pal(), ...)
}


#' Make colors brighter or darker
#' @param x a color
#' @param fac the factor (between 0 and 1).
#' @export
tint <- function(x, fac) {
    x <- c(grDevices::col2rgb(x))
    x <- (x + (255 - x) * fac) / 255
    grDevices::rgb(x[1], x[2], x[3])
}

#' @rdname tint
#' @export
shade <- function(x, fac) {
    x <- c(grDevices::col2rgb(x))
    x <- x * (1 - fac) / 255
    grDevices::rgb(x[1], x[2], x[3])
}
