# define colors
n_gray   <- rgb(88, 88, 90, maxColorValue = 255)
n_blue   <- rgb(0, 100, 198, maxColorValue = 255)
n_lblue  <- rgb(130, 180, 220, maxColorValue = 255)
n_green  <- rgb(162, 173, 0, maxColorValue = 255)
n_orange <- rgb(227, 114, 37, maxColorValue = 255)
n_purple <- rgb(158, 86, 235, maxColorValue = 255)
n_yellow <- rgb(255, 205, 0, maxColorValue = 255)
n_red    <- rgb(206, 15, 105, maxColorValue = 255)
n_brown  <- rgb(139, 69, 19, maxColorValue = 255)

naglr_palette <- c(
    n_lblue,
    n_green,
    n_orange,
    n_purple,
    n_yellow,
    n_red,
    n_brown,
    n_gray
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
