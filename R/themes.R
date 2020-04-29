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
#' @param base_size Base font size
#' @param base_family Base font family
#' @param base_line_size Base size for line elements
#' @param Base size for rect elements
#'
#' @export

theme_fixed <- function(x_axis = 0, y_axis = 0, base_size = 18, base_family = "", base_line_size = base_size/5, base_rect_size = base_size/5) {

  list(theme_void(base_size = base_size, base_family = base_family,
             base_line_size = base_line_size, base_rect_size = base_rect_size),
    geom_vline(aes(xintercept = y_axis),size=base_line_size/.stroke),
    geom_hline(aes(yintercept = x_axis),size=base_line_size/.stroke))

}
