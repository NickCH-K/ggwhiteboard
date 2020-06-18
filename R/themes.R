#' Default whiteboard theme
#'
#' This is a ggplot2 theme designed for use with ggwhiteboard. This is a complete theme that controls all non-data display. Use \code{theme()} to modify afterwards.
#'
#' @param base_size Base font size
#' @param base_family Base font family
#' @param base_line_size Base size for line elements
#' @param Base size for rect elements
#'
#' @export

theme_board <- function(base_size = 18, base_family = "", base_line_size = base_size/10, base_rect_size = base_size/10) {

  theme_minimal(base_size = base_size, base_family = base_family,
                base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    theme(axis.text = element_blank(),
          axis.line = element_line(),
          axis.title.x = element_text(hjust = 1),
          axis.title.y = element_text(vjust = 1),
          panel.grid = element_blank())

}

#' Whiteboard theme for fixed-axis graphs
#'
#' This is a ggplot2 theme designed for graphs in which axes are fixed at (0,0) or some other numbers. This is technically not a theme as it includes geometries as well. Use \code{theme()} to modify afterwards.
#'
#' @param x_axis The value of y at which the x-axis will be drawn.
#' @param y_axis The value of x at which the y-axis will be drawn.
#' @param x_min,x_max,y_min,y_max The edges of the graph on the x and y axes, for the purposes of figuring out how far to draw the axes and place the axis titles.
#' @param x_title_loc,y_title_loc The locations of the axis titles, in terms of actual x and y values.
#' @param title_opts,title_opts_x,title_opts_y A list of options to be passed to \code{ggplot2::annotate} for the axis titles, for both axes or only x or y specifically.
#' @param line_opts,line_opts_x,line_opts_y A list of options to be passed to \code{ggplot2::geom_segment} for the axis lines, for both axes or only x or y specifically.
#' @param base_theme The **ggplot2** theme on which to build the axis-zero graph. Note that if this is not \code{ggplot2::theme_void}, you will likely have to do a lot of adjustments in \code{ggplot2::theme()} afterwards to make it look good.
#' @param base_size Base font size
#' @param base_family Base font family
#' @param base_line_size Base size for line elements
#' @param base_rect_size size for rect elements
#'
#' @export

axis_zero <- function(x_axis = 0, y_axis = 0,
                      quadrants = 1,
                      x_min = -100, y_min = -100,
                      x_max = 100, y_max = 100,
                      x_title = 'X', y_title = 'Y',
                      x_title_loc = c(x_max, -4 - 2*(any(1:2 %in% quadrants) &
                                                       any(3:4 %in% quadrants))),
                      y_title_loc = c(-2-2*(any(c(1,4) %in% quadrants) &
                                              any(c(2,3) %in% quadrants)),y_max),
                      title_opts = list(),
                      title_opts_x = NULL,
                      title_opts_y = NULL,
                      line_opts = list(),
                      line_opts_x = NULL,
                      line_opts_y = NULL,
                      base_theme = ggplot2::theme_void,
                      base_size = 18, base_family = "", base_line_size = base_size/10, base_rect_size = base_size/10) {

  # If specific x and y options weren't filled in, use the overall
  if (is.null(title_opts_x)) {
    title_opts_x <- title_opts
  }
  if (is.null(title_opts_y)) {
    title_opts_y <- title_opts
  }
  if (is.null(line_opts_x)) {
    line_opts_x <- line_opts
  }
  if (is.null(line_opts_y)) {
    line_opts_y <- line_opts
  }

  # Start with a void and the axis titles
  thm <- list(theme_void(base_size = base_size, base_family = base_family,
             base_line_size = base_line_size, base_rect_size = base_rect_size),
             do.call(ggplot2::annotate,
                     append(list(geom='text',size=base_size/ggplot2::.pt,
                                 x=x_title_loc[1],y=x_title_loc[2],
                                 label=x_title),
                            title_opts_x)),
             do.call(ggplot2::annotate,
                     append(list(geom='text',size=base_size/ggplot2::.pt,
                                 x=y_title_loc[1],y=y_title_loc[2],
                                 label=y_title),
                            title_opts_y)))

  # Draw the y-axis.
  # If there's only a top half or a bottom half, draw to 0. Otherwise draw top to bottom
  if (!(any(c(3,4) %in% quadrants))) {
    y_bottom <- 0
    y_top <- y_max
  } else if (!(any(c(1,2) %in% quadrants))) {
    y_bottom <- y_min
    y_top <- 0
  } else {
    y_bottom <- y_min
    y_top <- y_max
  }
  # Same with the x-axis
  if (!(any(c(2,3) %in% quadrants))) {
    x_bottom <- 0
    x_top <- x_max
  } else if (!(any(c(1,4) %in% quadrants))) {
    x_bottom <- x_min
    x_top <- 0
  } else {
    x_bottom <- x_min
    x_top <- x_max
  }

  # Now add the axes
  thm <- append(thm,
                list(
                  do.call(ggplot2::geom_segment,
                          append(list(
                            mapping=ggplot2::aes(x=x_bottom,xend=x_top,y=y_axis,yend=y_axis),
                            size = 3*base_line_size/ggplot2::.stroke
                          ), line_opts_x)),
                  do.call(ggplot2::geom_segment,
                          append(list(
                            mapping=ggplot2::aes(x=x_axis,xend=x_axis,y=y_bottom,yend=y_top),
                            size = 3*base_line_size/ggplot2::.stroke
                          ), line_opts_y))
                ))

  return(thm)
}
