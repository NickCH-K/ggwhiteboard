#' Adds a point to a whiteboard object
#'
#' This function adds a point to a whiteboard object, usually at the intersection of two lines
#'
#' @param wb A whiteboard object.
#' @param name The name of the point to be used in labeling it on the whiteboard and referring to it when adding new elements.
#' @param at A two-element list containing the names of two objects on the board, where the point should be placed at their (leftmost) intersection. Alternately, replace the first element with a number to get the intersection between the second element and that x-axis value, or replace the second element with a number to get the intersection between the first element and that y-axis value. Or replace both with numbers to just draw a point at that \code{list(x,y)} coordinate.
#' @param vline Add a vertical line from the point to the x-axis.
#' @param hline Add a vertical line from the point to the y-axis.
#' @param nstage For animated whiteboards, which stage should be added to?
#' @param label Set to \code{''} to draw only the point by itself. Set to \code{'left'}, \code{'right'}, or \code{'both'} to put the name of the point to the left, right, or both sides of the point. Note you can also move the label around by setting \code{hjust} and \code{vjust} in \code{labelopts}.
#' @param lineopts A list of aesthetic values to be passed to \code{ggplot2::geom_segment} when adding the \code{vline} or \code{hline} options.
#' @param labelopts A list of aesthetic values to be passed to \code{ggplot2::annotate} when adding the label. Note you can include a \code{label} option in here and use a different label than \code{name} on the line. Set font size by setting the \code{size} argument, dividing your desired font size by \code{ggplot2::.pt}.
#' @param ggopts List of aesthetic values to be passed on to \code{ggplot2::geom_point}, such as \code{color}, \code{size}, or \code{linetype}.
#' @export

add_point <- function(wb,name,at,vline=FALSE,hline=FALSE, nstage = 1, label = 'right',
                      labelopts = list(size=18/ggplot2::.pt), lineopts = list(size = 1.5, linetype = 'dashed'), ggopts = list(size=6)) {

  wb[[nstage]]$elements[[length(wb[[nstage]]$elements)+1]] <- list(name=name,type='point',
                                                                   at=at,vline=vline,hline=hline,
                                                                   label=label,lineopts=lineopts,
                                                                   labelopts=labelopts,ggopts=ggopts)

  return(wb)
}

