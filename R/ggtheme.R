#' A [ggplot2] theme based on hrbrthemes
#'
#' Most of this theme and documentation is a simple copy & paste from
#' \url{https://github.com/hrbrmstr/hrbrthemes} with only few modifications. The
#' panel grid has other defaults (thicker, but greyer) so that plots look still
#' nice when using the [Cairo] device.
#'
#' You should [import_roboto_condensed]() first and also install the fonts on
#' your system before trying to use this theme.
#'
#' @param base_family,base_size base font family and size
#' @param plot_title_family,plot_title_face,plot_title_size,plot_title_margin
#'   plot tilte family, face, size and margi
#' @param subtitle_family,subtitle_face,subtitle_size plot subtitle family, face
#'   and size
#' @param subtitle_margin plot subtitle margin bottom (single numeric value)
#' @param strip_text_family,strip_text_face,strip_text_size facet label font
#'   family, face and size
#' @param caption_family,caption_face,caption_size,caption_margin plot caption
#'   family, face, size and margin
#' @param axis_title_family,axis_title_face,axis_title_size axis title font
#'   family, face and size
#' @param axis_title_just axis title font justificationk one of `[blmcrt]`
#' @param plot_margin plot margin (specify with [ggplot2::margin])
#' @param grid panel grid (`TRUE`, `FALSE`, or a combination of `X`, `x`, `Y`,
#'   `y`)
#' @param axis add x or y axes? `TRUE`, `FALSE`, "`xy`"
#' @param ticks ticks if `TRUE` add ticks
#'
#'
#' @examples \dontrun{
#' library(ggplot2)
#' library(dplyr)
#'
#' # seminal scatterplot
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point() +
#'   labs(x="Fuel effiiency (mpg)", y="Weight (tons)",
#'        title="Seminal ggplot2 scatterplot example",
#'        subtitle="A plot that is only useful for demonstration purposes",
#'        caption="Brought to you by the letter 'g'") +
#'   theme_naglr()
#'
#' # seminal bar chart
#'
#' update_geom_font_defaults(family=font_rc_light)
#'
#' count(mpg, class) %>%
#'   ggplot(aes(class, n)) +
#'   geom_col() +
#'   geom_text(aes(label=n), nudge_y=3) +
#'   labs(x="Fuel effiiency (mpg)", y="Weight (tons)",
#'        title="Seminal ggplot2 bar chart example",
#'        subtitle="A plot that is only useful for demonstration purposes",
#'        caption="Brought to you by the letter 'g'") +
#'   theme_naglr(grid="Y") +
#'   theme(axis.text.y=element_blank())
#' }
#'
#' @export
#' @import ggplot2
theme_naglr <- function(base_family="sans", base_size = 11,
                        plot_title_family=base_family, plot_title_size = 16,
                        plot_title_face="plain", plot_title_margin = 10,
                        subtitle_family="sans", subtitle_size = 12,
                        subtitle_face = "plain", subtitle_margin = 10,
                        strip_text_family = base_family, strip_text_size = 12,
                        strip_text_face = "plain",
                        caption_family = "sans", caption_size = 9,
                        caption_face = "plain", caption_margin = 10,
                        axis_title_family = base_family, axis_title_size = 9,
                        axis_title_face = "plain", axis_title_just = "rt",
                        legend_position = "bottom",
                        plot_margin = margin(1, 1, 1, 1),
                        grid = TRUE, axis = FALSE, ticks = FALSE) {

    ret <- ggplot2::theme_minimal(base_family=base_family, base_size=base_size)

    ret <- ret + theme(legend.background=element_blank())
    ret <- ret + theme(legend.key=element_blank())

    if (inherits(grid, "character") | grid == TRUE) {

        ret <- ret + theme(panel.grid = element_line(color = "#2b2b2bdd", size = 0.1))
        ret <- ret + theme(panel.grid.major = element_line(colour = "grey90", size = 0.3))
        ret <- ret + theme(panel.grid.minor = element_line(colour = "grey97", size = 0.2))

        if (inherits(grid, "character")) {
            if (regexpr("X", grid)[1] < 0) ret <- ret + theme(panel.grid.major.x=element_blank())
            if (regexpr("Y", grid)[1] < 0) ret <- ret + theme(panel.grid.major.y=element_blank())
            if (regexpr("x", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.x=element_blank())
            if (regexpr("y", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.y=element_blank())
        }

    } else {
        ret <- ret + theme(panel.grid=element_blank())
    }

    if (inherits(axis, "character") | axis == TRUE) {
        ret <- ret + theme(axis.line=element_line(color="#2b2b2b", size=0.15))
        if (inherits(axis, "character")) {
            axis <- tolower(axis)
            if (regexpr("x", axis)[1] < 0) {
                ret <- ret + theme(axis.line.x=element_blank())
            } else {
                ret <- ret + theme(axis.line.x=element_line(color="#2b2b2b", size=0.15))
            }
            if (regexpr("y", axis)[1] < 0) {
                ret <- ret + theme(axis.line.y=element_blank())
            } else {
                ret <- ret + theme(axis.line.y=element_line(color="#2b2b2b", size=0.15))
            }
        } else {
            ret <- ret + theme(axis.line.x=element_line(color="#2b2b2b", size=0.15))
            ret <- ret + theme(axis.line.y=element_line(color="#2b2b2b", size=0.15))
        }
    } else {
        ret <- ret + theme(axis.line=element_blank())
    }

    if (!ticks) {
        ret <- ret + theme(axis.ticks = element_blank())
        ret <- ret + theme(axis.ticks.x = element_blank())
        ret <- ret + theme(axis.ticks.y = element_blank())
    } else {
        ret <- ret + theme(axis.ticks = element_line(size=0.15))
        ret <- ret + theme(axis.ticks.x = element_line(size=0.15))
        ret <- ret + theme(axis.ticks.y = element_line(size=0.15))
        ret <- ret + theme(axis.ticks.length = grid::unit(5, "pt"))
    }

    # xj <- switch(tolower(substr(axis_title_just, 1, 1)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)
    # yj <- switch(tolower(substr(axis_title_just, 2, 2)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)

    ret <- ret + theme(axis.text.x=element_text(margin=margin(t=0)))
    ret <- ret + theme(axis.text.y=element_text(margin=margin(r=0)))
    ret <- ret + theme(axis.title=element_text(size=axis_title_size, family=axis_title_family))
    ret <- ret + theme(axis.title.x=element_text(size=axis_title_size,
                                                 family=axis_title_family, face=axis_title_face))
    ret <- ret + theme(axis.title.y=element_text(size=axis_title_size,
                                                 family=axis_title_family, face=axis_title_face))
    ret <- ret + theme(strip.text=element_text(hjust=0, size=strip_text_size,
                                               face=strip_text_face, family=strip_text_family))
    ret <- ret + theme(panel.spacing.x=grid::unit(2, "lines"))
    ret <- ret + theme(panel.spacing.y=grid::unit(2, "lines"))
    ret <- ret + theme(plot.title=element_text(hjust=0, size=plot_title_size,
                                               margin=margin(b=plot_title_margin),
                                               family=plot_title_family, face=plot_title_face))
    ret <- ret + theme(plot.subtitle=element_text(hjust=0, size=subtitle_size,
                                                  margin=margin(b=subtitle_margin),
                                                  family=subtitle_family, face=subtitle_face))
    ret <- ret + theme(plot.caption=element_text(hjust=1, size=caption_size,
                                                 margin=margin(t=caption_margin),
                                                 family=caption_family, face=caption_face))
    ret <- ret + theme(plot.margin=plot_margin)
    ret <- ret + theme(legend.position=legend_position)
    ret <- ret + theme(legend.margin=margin(1, 1, 1, 1))

    ret
}
