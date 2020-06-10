#' Start a blank whiteboard
#'
#' This function initializes a blank whiteboard object, which can then be added to.
#'
#' @param domain A numeric vector giving the full set of points on the x-axis to be evaluated in formulas.
#' @param range A two-element numeric vector giving the limits of the whiteboard's y-axis.
#' @param axis_zero Applies the **ggwhiteboard** pseudo-theme \code{axis_zero}, which forces the x and y axes to center at (0, 0), but also takes some thematic control from the user.
#' @param x_title Axis title for the x-axis.
#' @param y_title Axis title for the y-axis.
#' @param title Graph title.
#' @export

blank_board <- function(domain = 0:100,
                        range = NULL,
                        axis_zero = TRUE,
                        x_title = 'X',
                        y_title = 'Y',
                        title = NULL) {
  if (!is.numeric(domain) | !is.vector(domain)) {
    stop('domain must be a numeric vector')
  }
  if (!is.null(range) & !(is.vector(range) & is.numeric(range) & length(range) == 2)) {
    stop('range must be a two-element vector consisting of the bottom and top limits of the y-axis.')
  }

  if (!(is.character(x_title) | is.null(x_title)) |
      !(is.character(y_title) | is.null(y_title)) |
      !(is.character(title) | is.null(title))) {
    stop('x_title, y_title, and title must all be character variables or NULL.')
  }

  board <- list(list(board = list(domain = domain,
                               range = range,
                             axis_zero = axis_zero,
                             x_title = x_title,
                             y_title = y_title,
                             title = NULL),
                elements=list()))

  class(board) <- c('whiteboard','list')

  return(board)
}
