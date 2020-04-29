#' Print a static whiteboard
#'
#' Takes a whiteboard object and produces an image out of it. If you want a ggplot object that can be further customized, instead use \code{ggboard()}.
#'
#' @param wb A whiteboard object.
#' @param nstage For animated whiteboards, which stage should be printed? To animate all stages, use \code{board_animate()}.
#' @export

print.whiteboard <- function(wb,nstage=1) {
  print(ggboard(wb,nstage=1))
}


#' Produce a static ggplot object from a whiteboard
#'
#' Takes a whiteboard object and produces a \code{ggplot} object that can be further customized.
#'
#' @param wb A whiteboard object.
#' @param nstage For animated whiteboards, which stage should be used? To animate all stages, use \code{board_animate()}.
#' @export

ggboard <- function(wb,nstage=1) {
  frame <- wb[[nstage]]

  df <- construct_board_data(frame)

  # Basic construction of ggplot and add labels
  p <- ggplot2::ggplot(df) +
    ggplot2::labs(x=frame$board$x_title,y=frame$board$y_title,
         title=frame$board$title)

  # If range is specified
  if (!is.null(frame$board$range)) {
    p <- p + ggplot2::ylim(frame$board$range)
  }

  # If there's a theme
  if (!is.null(frame$board$theme)) {
    p <- p + frame$board$theme
  }

  # Now add the geometries for each element
  index <- 1
  for (e in frame$elements) {
    if (e$type == 'line') {
      p <- p +
        do.call(ggplot2::geom_line,
                append(list(mapping=ggplot2::aes_string(x=paste0('x.',index),
                                                 y=paste0('y.',index)),na.rm = TRUE),
                       e$ggopts))


      # Add the line label
      # find our first point and our last point
      nonmiss <- which(!is.na(df[[paste0('x.',index)]]) &
                         !is.na(df[[paste0('y.',index)]]))
      first_index <- nonmiss[1]
      first_x <- df[[paste0('x.',index)]][first_index]
      first_y <- df[[paste0('y.',index)]][first_index]
      last_index <- rev(nonmiss)[1]
      last_x <- df[[paste0('x.',index)]][last_index]
      last_y <- df[[paste0('y.',index)]][last_index]
      p <- p + ggboard_annotate(first_x,first_y,last_x,last_y,e$name,e$label,e$labelopts)
    }
    if (e$type == 'point') {
      if (e$vline) {
        p <- p +
          do.call(ggplot2::geom_segment,
                  append(list(mapping=ggplot2::aes_string(x=paste0('x.',index),
                                                          xend=paste0('x.',index),
                                                          y=0,
                                                          yend=paste0('y.',index)),
                              na.rm = TRUE),
                         e$lineopts))
      }
      if (e$hline) {
        p <- p +
          do.call(ggplot2::geom_segment,
                  append(list(mapping=ggplot2::aes_string(x=0,
                                                          xend=paste0('x.',index),
                                                          y=paste0('y.',index),
                                                          yend=paste0('y.',index)),
                              na.rm = TRUE),
                         e$lineopts))
      }

      p <- p +
        do.call(ggplot2::geom_point,
                append(list(mapping=ggplot2::aes_string(x=paste0('x.',index),
                                                        y=paste0('y.',index)),
                            na.rm = TRUE),
                       e$ggopts))


      # Add the label

      p <- p + ggboard_annotate(df[[paste0('x.',index)]][1],df[[paste0('y.',index)]][1],
                                df[[paste0('x.',index)]][1],df[[paste0('y.',index)]][1],
                                e$name,e$label,e$labelopts)
    }
    index <- index + 1
  }

  return(p)
}

# Add the annotation element-label
ggboard_annotate <- function(x1,y1,x2,y2,name,label,opts) {
  if (label %in% c('left','both')) {
    annotate_opts <- list(geom='text',
                          x=x1,y=y1,
                          hjust=1.5,label=name)
    # Any overwritten, go for it, and add others too
    for (ao in names(opts)) {
      annotate_opts[[ao]] <- opts[[ao]]
    }

    return(do.call(ggplot2::annotate,
              annotate_opts))
  }
  if (label %in% c('right','both')) {
    annotate_opts <- list(geom='text',
                          x=x2,y=y2,
                          hjust=-.5,label=name)
    # Any overwritten, go for it, and add others too
    for (ao in names(opts)) {
      annotate_opts[[ao]] <- opts[[ao]]
    }

    return(do.call(ggplot2::annotate,
              annotate_opts))
  }

  return(NULL)
}

# Internal function to construct the data set that is then used as a basis for the ggplot object
construct_board_data <- function(frame) {
  # Create lookup table of the element names
  namelist <- sapply(frame$elements,function(x) x[['name']])

  # Construct data for elements that don't rely on anything else
  # For now just lines
  # Separate this out because this behavior will be special
  # for animation
  df <- board_data_primals(frame)

  # Then loop through everything dependent on those primals
  index <- 1
  for (e in frame$elements) {
    if (e$type == 'point') {
      if (is.numeric(e$at[[1]]) & is.numeric(e$at[[2]])) {
        # If it's just a numeric data point, draw that
        point_x <- e$at[[1]]
        point_y <- e$at[[2]]
      } else if ((is.numeric(e$at[[1]]) & !is.numeric(e$at[[2]]))) {
        # if the x value is set and we are looking for a line intersection
        # Get the index of the line
        line_2_name <- which(namelist == e$at[[2]])

        # Our x-axis point
        point_x <- e$at[[1]]

        # Get our intersection point
        linefun <- approxfun(df[[paste0('x.',line_2_name)]], df[[paste0('y.',line_2_name)]], rule = 2)
        point_y <- linefun(point_x)
      } else if (!is.numeric(e$at[[1]]) & is.numeric(e$at[[2]])) {
        # if the y value is set and we are looking for a line intersection
        # Get the index of the line
        line_1_name <- which(namelist == e$at[[1]])

        # Our y-axis point
        point_y <- e$at[[2]]
        point_x <- curve_intersect(
          data.frame(x=df[[paste0('x.',line_1_name)]],y=df[[paste0('y.',line_1_name)]]),
          function(x) point_y
        )[['x']]
      } else {
        # If they're both lines!
        # Get the indices of the lines
        line_1_name <- which(namelist == e$at[[1]])
        line_2_name <- which(namelist == e$at[[2]])

        # Get both our points
        intersection <- curve_intersect(
          data.frame(x=df[[paste0('x.',line_1_name)]],y=df[[paste0('y.',line_1_name)]]),
          data.frame(x=df[[paste0('x.',line_2_name)]],y=df[[paste0('y.',line_2_name)]])
        )
        point_x <- intersection[['x']]
        point_y <- intersection[['y']]
      }

      edf <- data.frame(x = point_x,y = point_y)
      names(edf) <- paste0(c('x.','y.'),index)
      df <- cbind_unequal(list(df,edf))
    }
    index <- index + 1
  }

  return(df)
}

# Internal function to construct the data set for the ggplot object
# But just for primals (unreliant on other objects)
board_data_primals <- function(frame) {
  df <- list()

  index <- 1
  for (e in frame$elements) {
    if (e$type == 'line') {
      eldf <- e$df
      names(eldf) <- paste0(c('x.','y.'),index)
      df[[index]] <- eldf
    }
    index <- index + 1
  }

  # Bind it all together into a tibble
  # Keeping in mind column lengths might not be equal
  return(cbind_unequal(
    append(
      list(tibble::tibble(x=frame$board$domain)),
      df)))

}

# Internal function to column-bind a list of data frames
# of unequal length, for use in constructing all the ggplot elements into one data set
cbind_unequal <- function(dfs) {
  # Find longest data set in the list
  longestN <- max(sapply(dfs,nrow))

  for (i in 1:length(dfs)) {
    if (nrow(dfs[[i]]) < longestN) {
      dfs[[i]][(nrow(dfs[[i]])+1):longestN,] <- NA
    }
  }

  return(dplyr::bind_cols(dfs))
}
