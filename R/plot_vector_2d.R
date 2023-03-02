#' Function plots 2d arrowed vectors
#'
#' Function plots one or more arrowed vectors across a 2d plotting space. Options are provided to add an x-y axis
#' and a grid. Either the \code{from_to} or \code{to} arguments may be specified to populate the plot.
#'
#' @param from_to A n x 4 numeric matrix that defines n vectors where the first two columns
#'  define the x/y "from" point and the last two columns define the x/y "to" point of each vector.
#' @param to An n x 2 numeric matrix that defines n vectors where the x/y "from" point is assumed to be
#'  at the origin \code{(0,0)} and the two columns of \code{to} define the x/y "to" point for each vector.
#' @param vector_labels A vector of strings with the same length as \code{from_to} or \code{to} that defines a label for each vector.
#' @param vector_colors A vector of strings with the same length as \code{to} or \code{from_to} that defines a color for each vector.
#' @param vector_widths A numeric vector with the same length as \code{to} or \code{from_to} that defines a width for each vector.
#' @param vector_linetypes A vector of strings with the same length as \code{to} or \code{from_to} that defines a line type for each vector.
#'  Acceptable values are "blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash".
#' @param draw_axis A logical which if TRUE, draws the numeric x/y axis'.
#' @param axis_color A string that defines the axis color.
#' @param grid_color A string that defines the grid color.
#' @param lines_00_color A string that defines the color for vertical/horizontal lines from (0,0).
#' @param x_breaks A numeric three element vector of upper, lower, and interval for exact x axis tic locations.
#' @param y_breaks A numeric three element vector of upper, lower, and interval for exact y axis tic locations.
#' @param x_title A string that sets the x axis title.
#' @param y_title A string that sets the y axis title.
#' @param char_mag A numeric that defines the magnification of the vector's label size.
#'
#' @importFrom graphics text
#' @importFrom graphics axis
#' @importFrom graphics arrows
#' @importFrom graphics abline
#'
#' @export
plot_vector_2d <- function(
  from_to = NULL,
  to = NULL,
  vector_labels = NULL,
  vector_colors = NULL,
  vector_widths = NULL,
  vector_linetypes = NULL,
  draw_axis = TRUE,
  axis_color = "black",
  grid_color = "lightgreen",
  lines_00_color = "darkgreen",
  x_breaks = c(-5,5,1),
  y_breaks = c(-5,5,1),
  x_title = "X",
  y_title = "Y",
  char_mag = 1.5){
    # check the number of columns for "from_to" and "to"
    if(!is.null(to)){
      if(ncol(to) != 2)
        stop("The 'to' matrix argument requires 2 fixed numeric columns defining the x/y end point of a vector(s)")
      if(!is.null(vector_labels)){
        if(length(vector_labels) != nrow(to)){
          stop(paste0("The 'vector_labels' argument requires ", nrow(to), " labels"))
        }
      }
      if(!is.null(vector_colors)){
        if(length(vector_colors) != nrow(to)){
          stop(paste0("The 'vector_colors' argument requires ", nrow(to), " colors"))
        }
      }
      if(!is.null(vector_widths)){
        if(length(vector_widths) != nrow(to)){
          stop(paste0("The 'vector_widths' argument requires ", nrow(to), " widths"))
        }
      }
      if(!is.null(vector_linetypes)){
        if(length(vector_linetypes) != nrow(to)){
          stop(paste0("The 'vector_linetypes' argument requires ", nrow(to), " linetypes"))
        }
      }
    }


    if(!is.null(from_to)){
      if(ncol(from_to) != 4)
        stop("The 'from_to' matrix argument requires 4 fixed numeric columns defining the x/y start and end points of a vector(s)")
      if(!is.null(vector_labels)){
        if(length(vector_labels) != nrow(from_to)){
          stop(paste0("The 'vector_labels' argument requires ", nrow(from_to), " labels"))
        }
      }
      if(!is.null(vector_colors)){
        if(length(vector_colors) != nrow(from_to)){
          stop(paste0("The 'vector_colors' argument requires ", nrow(from_to), " colors"))
        }
      }
      if(!is.null(vector_widths)){
        if(length(vector_widths) != nrow(from_to)){
          stop(paste0("The 'vector_widths' argument requires ", nrow(from_to), " widths"))
        }
      }
      if(!is.null(vector_linetypes)){
        if(length(vector_linetypes) != nrow(from_to)){
          stop(paste0("The 'vector_linetypes' argument requires ", nrow(from_to), " linetypess"))
        }
      }
    }

    if(is.null(vector_colors)) {
      if(!is.null(from_to)){
        vector_colors <- rep("black", nrow(from_to))
      }else if(!is.null(to)){
        vector_colors <- rep("black", nrow(to))
      }
    }

    if(is.null(vector_widths)) {
      if(!is.null(from_to)){
        vector_widths <- rep(1, nrow(from_to))
      }else if(!is.null(to)){
        vector_widths <- rep(1, nrow(to))
      }
    }

    if(is.null(vector_linetypes)) {
      if(!is.null(from_to)){
        vector_linetypes <- rep("solid", nrow(from_to))
      }else if(!is.null(to)){
        vector_linetypes <- rep("solid", nrow(to))
      }
    }

    if(draw_axis) {
      plot(
        x = NULL,
        y = NULL,
        xaxt = "none",
        yaxt = "none",
        xlab = x_title,
        ylab = y_title,
        xlim = c(x_breaks[1],x_breaks[2]),
        ylim = c(y_breaks[1],y_breaks[2])
      )

      # define axis
      x_tics <- seq.int(from = x_breaks[1], to = x_breaks[2], by = x_breaks[3])
      y_tics <- seq.int(from = y_breaks[1], to = y_breaks[2], by = y_breaks[3])
      graphics::axis(side = 1, x_tics, col = axis_color)
      graphics::axis(side = 2, y_tics, las = 1, col = axis_color)

      # draw 0 lines
      graphics::arrows(
        x_breaks[1],
        0,
        x_breaks[2],
        0,
        col = lines_00_color,
        lwd = 2,
        length = 0.15,
        angle = 13
      )
      graphics::arrows(
        0,
        y_breaks[1],
        0,
        y_breaks[2],
        col = lines_00_color,
        lwd = 2,
        length = 0.15,
        angle = 13
      )

      #draw grids
      graphics::abline(
        h = y_breaks[1]:y_breaks[2],
        v = x_breaks[1]:x_breaks[2],
        col = grid_color, lty=3
      )
    }

    #draw vectors
    if(!is.null(to)){
      for(i in 1:nrow(to)){
        graphics::arrows(
          0,
          0,
          to[i,1],
          to[i,2],
          length = 0.35,
          angle = 15,
          col = vector_colors[[i]],
          lwd = vector_widths[[i]],
          lty = vector_linetypes[[i]]
        )
      }
    }

    if(!is.null(vector_labels) & !is.null(to)){
      for(i in 1:nrow(to)){
        graphics::text(
           to[i,1],
           to[i,2],
           vector_labels[[i]],
           cex = char_mag,
           pos = 4,
           col = vector_colors[[i]]
        )
      }
    }

    if(!is.null(from_to)){
      for(i in 1:nrow(from_to)){
        graphics::arrows(
          from_to[i,1],
          from_to[i,2],
          from_to[i,3],
          from_to[i,4],
          length = 0.35,
          angle = 15,
          col = vector_colors[[i]],
          lwd = vector_widths[[i]],
          lty = vector_linetypes[[i]]
        )
      }
    }

    if(!is.null(vector_labels) & !is.null(from_to)){
      for(i in 1:nrow(from_to)){
        x_loc <- from_to[i,3]
        y_loc <- from_to[i,4]
        graphics::text(
          x_loc,
          y_loc,
          vector_labels[[i]],
          cex = char_mag,
          pos = 4,
          col = vector_colors[[i]])
      }
    }
}
