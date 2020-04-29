#' Intersection Between Two Curves
#'
#' This function finds a point that represents the intersection between two curves.
#'
#' This code is lightly modified from the function written by Andrew Heiss [here](https://github.com/andrewheiss/supply-demand-ggplot).
#'
#' @param curve1,curve2 These are each either \code{data.frame}s where the first column contains the \code{x}-axis values of each line, and the second column contains the \code{y}-axis values, or functions with a single input and a single numerical output. If you mix a function with a data frame, the function will be evaluated at all of the \code{x}-axis points included in the \code{data.frame}.
#' @param domain If both \code{curve1} and \code{curve2} are functions, they will be evaluated over the domain of \code{domain}.
#' @export

curve_intersect <- function(curve1, curve2, domain=NULL) {
  if(!(is.function(curve1) | is.data.frame(curve1)) &
              (is.function(curve2) | is.data.frame(curve2))) {
    stop('curve1 and curve2 must each be data.frames or functions.')
  }

  empirical <- is.data.frame(curve1) | is.data.frame(curve2)

  if (!empirical & missing(domain)) {
    stop("'domain' must be provided with non-empirical curves")
  }

  if (!empirical & (length(domain) != 2 | !is.numeric(domain))) {
    stop("'domain' must be a two-value numeric vector, like c(0, 10)")
  }

  # If only one is a data frame, convert the other to it
  if (is.data.frame(curve1) & is.function(curve2)) {
    curve2 <- data.frame(x = curve1[[1]],
                         y = curve2(curve1[[1]]))
  }
  if (is.data.frame(curve2) & is.function(curve1)) {
    curve1 <- data.frame(x = curve2[[1]],
                         y = curve1(curve2[[1]]))
  }

  if (empirical) {
    # Approximate the functional form of both curves
    curve1_f <- approxfun(curve1$x, curve1$y, rule = 2)
    curve2_f <- approxfun(curve2$x, curve2$y, rule = 2)

    # Calculate the intersection of curve 1 and curve 2 along the x-axis
    point_x <- uniroot(function(x) curve1_f(x) - curve2_f(x),
                       c(min(curve1$x), max(curve1$x)))$root

    # Find where point_x is in curve 2
    point_y <- curve2_f(point_x)
  } else {
    # Calculate the intersection of curve 1 and curve 2 along the x-axis
    # within the given domain
    point_x <- uniroot(function(x) curve1(x) - curve2(x), domain)$root

    # Find where point_x is in curve 2
    point_y <- curve2(point_x)
  }

  return(list(x = point_x, y = point_y))
}
