#' Function plot points in a 3d device window
#'
#' Function plots the 3d points in the current device window.  A data frame provides the x, y, and z
#' location of the points.  Options for point color and size along with labeling and axis options are provided.
#'
#' @param df A data frame  with columns for the x, y, and z locations and optionally a column for grouping the points.
#' @param x_col,y_col,z_col Strings that set the columns names from \code{df} for the x, y, z numeric coordinates.
#' @param group_col An optional string that sets the column name from \code{df} for grouping the points.  The
#'  column must be a factor with discrete levels.
#' @param win_width,win_height An integer defining the dimensions of the plot window.
#' @param pt_colors Can be single string that sets the color for all the points. The default is "black". If \code{group_col}
#'  has been assigned then this parameter is a vector that sets the colors for each of factor column's levels.
#' @param pt_size A numeric defining the size of all the points in pixels. The default is 6.0.
#' @param pt_alpha A numeric between 0.0 and 1.0 that sets the alpha value for all the points. The default is 1.0.
#' @param bkg_color A character string defining the window background color. The default is "white".
#' @param draw_axis A logical which if TRUE draws the axes.
#' @param main_title A character string defining the plot's main title.
#' @param x_lab,y_lab,z_lab A character string defining the axis labels.
#' @param axis_color A character string defining the axis color. The default is "black".
#' @param xlim,ylim,zlim An integer vector defining the min/max of an axis.
#' @param x_ticks,y_ticks,z_ticks An integer defining the number of ticks for an axis.
#' @param html_file A character string that defines the file path for creating
#'  a WebGL version of the plot. File names should have an html extension.
#'
#' @import data.table
#' @importFrom rgl title3d
#' @importFrom rgl points3d
#' @importFrom rgl segments3d
#' @importFrom rgl axis3d
#' @importFrom rgl writeWebGL
#'
#' @return  The device window id (an integer).
#'
#' @export
plot_points_3d <- function(
  df,
  x_col, y_col, z_col,
  group_col = NULL,
  win_width = 1000,
  win_height = 800,
  pt_colors = "black",
  pt_size = 6.0,
  pt_alpha = 1.0,
  bkg_color = "white",
  draw_axis = TRUE,
  main_title = NULL, x_lab = "X", y_lab = "Y", z_lab = "Z",
  axis_color="black",
  xlim = c(0,10),
  ylim = c(0,10),
  zlim = c(0,10),
  x_ticks = 11, y_ticks = 11, z_ticks = 11,
  html_file = NULL){

  dt <- data.table::setDT(df)

  # check "group_col"
  if(!is.null(group_col)){
    if(!dt[, is.factor(get(group_col))]){
      stop("The group column must be of type 'factor' with discrete values.")
    }else{
      group_levels <- dt[, levels(get(group_col))]
      if(length(group_levels) != length(pt_colors)){
        stop("The argument 'pt_colors' must have the length as the factor levels in the group column.")
      }
    }
  }

  # set up window
  device_id <- RmatrixPkg::device_new(bgrd_color = bkg_color, width=win_width, height=win_height)

  # set up axis'
  if(draw_axis){
    # add axes
    rgl::segments3d(xlim, c(0,0), c(0,0), color=axis_color)
    rgl::segments3d(c(0,0), ylim, c(0,0), color=axis_color)
    rgl::segments3d(c(0,0), c(0,0), zlim, color=axis_color)

    # add axis ticks
    rgl::axis3d("x--", pos = c(NA,0,0), nticks = x_ticks, col = axis_color)
    rgl::axis3d("y--", pos = c(0,NA,0), nticks = y_ticks, col = axis_color)
    rgl::axis3d("z--", pos = c(0,0,NA), nticks = z_ticks, col = axis_color)

    rgl::title3d(main = main_title, xlab = x_lab, ylab = z_lab, zlab = y_lab)

    # rgl::grid3d(c("x", "y+", "z"), col = axis_color, n = x_ticks)
  }

  # draw points
  if(is.null(group_col)){
    select_cols <- c(x_col,y_col,z_col)
    dt <- dt[, ..select_cols]
    rgl::points3d(x = dt, color=pt_colors, size = pt_size, alpha = pt_alpha)
  }else{
    select_cols <- c(x_col,y_col,z_col,group_col)
    dt <- dt[, ..select_cols]
    group_levels <- dt[,levels(get(group_col))]
    dt[, {
      group_val <- .SD[1L, get(group_col)]
      index <- which(group_levels == group_val)
      rgl::points3d(x = .SD, color=pt_colors[index], size = pt_size, alpha = pt_alpha)
    }, by = get(group_col)]
    # dt[, plot_group(.SD,group_col,group_levels,pt_colors,pt_size,pt_alpha), by = get(group_col)]
  }


  if(!is.null(html_file)) {
    rgl::rglwidget(filename = html_file, width = win_width, height = win_height)
  }

  RmatrixPkg::device_current()
}

# plot_group <- function(x,group_col,group_levels,pt_colors,pt_size,pt_alpha){
#   group_val <- as.character(x[,..group_col][[1]][1])
#   index <- which(group_levels == group_val)
#   rgl::points3d(x = x, color=pt_colors[index], size = pt_size, alpha = pt_alpha)
# }
