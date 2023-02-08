#' Function plots line segments in a 3d device window
#'
#' Function plots a collection of line segments where \code{segments} is a numeric matrix of six
#'  columns of start and end points in 3d space. Options are provided for sizing the device window,
#' width/color of the segments, and defining/labeling axis,
#'
#' @param segments A numeric matrix where each row is a line segment defined by a pair of points in 3d space.
#'  The first three columns defines the x/y/z coordinates for the first point; the next three columns define
#'  the second point of the segment.
#' @param win_width,win_height An integer defining the dimensions of the plot window.
#' @param segment_colors A vector of strings defining each segment's color. The default is "black".
#'  The length of \code{segment_colors} should be the same as \code{segments}.
#' @param segment_widths A numeric vector defining each segment's width. The default is 1.0.
#'  The length of \code{segment_widths} should be the same as \code{segments}.
#' @param bkg_color A character string defining the window background color.
#' @param draw_axis A logical which if TRUE draws the axes.
#' @param main_title A character string defining the plot's main title.
#' @param x_lab,y_lab,z_lab A character string defining the axis labels.
#' @param axis_color A character string defining the axis color.
#' @param xlim,ylim,zlim An integer vector defining the min/max of an axis.
#' @param x_ticks,y_ticks,z_ticks An integer defining the number of ticks for an axis.
#' @param html_file A character string that defines the file path for creating
#'  a WebGL version of the plot. File names should have an html extension.
#'
#' @importFrom rgl title3d
#' @importFrom rgl segments3d
#' @importFrom rgl axis3d
#' @importFrom rgl segments3d
#' @importFrom rgl writeWebGL
#'
#' @return  The device window id (an integer).
#'
#' @export
plot_lines_3d <- function(
  segments,
  win_width = 1000,
  win_height = 800,
  segment_colors = NULL,
  segment_widths = NULL,
  bkg_color = "white",
  draw_axis = TRUE,
  main_title = NULL, x_lab = "X", y_lab = "Y", z_lab = "Z",
  axis_color="black",
  xlim = c(0,10),
  ylim = c(0,10),
  zlim = c(0,10),
  x_ticks = 11, y_ticks = 11, z_ticks = 11,
  html_file = NULL){

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

    rgl::title3d(main = main_title, xlab = x_lab, ylab = y_lab, zlab = z_lab)
  }

  # draw segments
  if(is.null(segment_colors)){
    segment_colors <- rep("black",nrow(segments))
  }else{
    if(length(segment_colors) != nrow(segments)){
      stop(paste0("The 'segment_colors' argument requires ", nrow(segemnts), " colors."))
    }
  }
  if(is.null(segment_widths)){
    segment_widths <- rep(1.0,nrow(segments))
  }else{
    if(length(segment_widths) != nrow(segments)){
      stop(paste0("The 'segment_widths' argument requires ", nrow(segemnts), " widths."))
    }
  }

  for(i in 1:nrow(segments)){
    seg <- matrix(segments[i,], ncol = 3, byrow = T)
    rgl::segments3d(seg, col=segment_colors[[i]], lwd = segment_widths[[i]])
  }

  if(!is.null(html_file)) {
    rgl::writeWebGL(filename = html_file, width = win_width, height = win_height)
  }

  RmatrixPkg::device_current()
}

