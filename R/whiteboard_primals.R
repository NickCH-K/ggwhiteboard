#' Adds a line to a whiteboard object
#'
#' This function adds a line (not necessarily straight) to a whiteboard object.
#'
#' @param wb A whiteboard object.
#' @param name The name of the line to be used in labeling it on the whiteboard and referring to it when adding new elements.
#' @param function A function that takes a single input and produces a single output, describing the line to be drawn.
#' @param nstage For animated whiteboards, which stage should be added to?
#' @param domain A numeric vector giving the full set of points on the x-axis to be evaluated in formulas. Defaults to the domain given for the existing board.
#' @param df If preferred, instead of \code{name, function, domain, range} you can provide a two-column \code{data.frame}, where the first column \code{x} gives the x-axis values, and the second column, given the name of the line, contains the y-axis values.
#' @param label Set to \code{''} to draw only the line by itself. Set to \code{'left'}, \code{'right'}, or \code{'both'} to put the name of the line near the first, last, or both points of the line.
#' @param labelopts A list of aesthetic values to be passed to \code{ggplot2::annotate} when adding the label. Note you can include a \code{label} option in here and use a different label than \code{name} on the line. Set font size by setting the \code{size} argument, dividing your desired font size by \code{ggplot2::.pt}.
#' @param ggopts List of aesthetic values to be passed on to \code{ggplot2::geom_line}, such as \code{color}, \code{size}, or \code{linetype}.
#' @export

add_line <- function(wb,name=NULL,fun=function(x) x, nstage = 1, domain = NULL, df = NULL, label = 'right', labelopts = list(size=18/ggplot2::.pt), ggopts = list(color='#0066cc',size=2)) {
  if (is.null(df) & is.null(name)) {
    stop('Either df or name must be specified.')
  }

  # In case of NULLs, fill in
  if (is.null(domain)) {
    domain <- wb[[nstage]]$board$domain
  }

  if (is.null(df)) {
    df <- tibble::tibble(x = domain, y = fun(domain))
    names(df)[2] <- name
  } else {
    # If df is specified, don't have a function
    fun <- NULL
  }

  if (is.null(name)) {
    name <- names(df)[2]
  }

  wb[[nstage]]$elements[[length(wb[[nstage]]$elements)+1]] <- list(name=name,type='line',df=df,fun=fun,
                                                                   domain=domain,label=label,
                                                                   labelopts=labelopts,ggopts=ggopts)

  return(wb)
}

