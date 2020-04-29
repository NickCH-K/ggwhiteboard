#' Start a blank whiteboard
#'
#' This function initializes a blank whiteboard object, which can then be added to.
#'
#' @param domain A numeric vector giving the full set of points on the x-axis to be evaluated in formulas.
#' @param range A two-element numeric vector giving the limits of the whiteboard's y-axis.
#' @param theme The **ggplot2** theme to be used. Set to \code{NULL} to use the default **ggplot2** theme. Other theme modifications can be added after the whiteboard is turned into a \code{ggplot} object with \code{print()} or \code{animate_whiteboard()}.
#' @param x_title Axis title for the x-axis.
#' @param y_title Axis title for the y-axis.
#' @param title Graph title.
#' @export

blank_board <- function(domain = 0:100,
                        range = NULL,
                        theme = theme_board(),
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
                             theme = theme,
                             x_title = x_title,
                             y_title = y_title,
                             title = NULL),
                elements=list()))

  class(board) <- c('whiteboard','list')

  return(board)
}
