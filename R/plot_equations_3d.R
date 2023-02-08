#' Function plots linear equations in 3d space.
#'
#' Function plots one or more system of linear equations in the form \eqn{A x = b} with three unknowns,
#' x1, x2, and x3, by plotting a plane for each equation.
#'
#' @param win_width,win_height An integer defining the dimensions of the plot window.
#' @param A Either the matrix of coefficients of a system of linear equations, or a data frame with both \code{A}
#'  and \code{b} as combined columns.
#' @param b If supplied, the vector of constants on the right hand side of the equations, of length matching
#'        the number of rows of \code{A}.
#' @param equation_color A scalar or vector that defines each equation's plane fill color.
#' @param alpha A numeric that defines the plane color's alpha component.
#' @param lit logical, specifying if lighting calculation should take place on geometry; see \code{rgl::rgl.material}
#' @param bkg_color A character string defining the window background color.
#' @param draw_axis A logical which if TRUE draws the axes.
#' @param main_lab A character string defining the plot's main title.
#' @param x_lab,y_lab,z_lab A character string defining the axis labels.
#' @param axis_color A character string defining the axis color.
#' @param xlim,ylim,zlim An integer vector defining the min/max of an axis.
#' @param html_file A character string that defines the file path to creating an html of the plot.
#'
#' @author Rick Dean
#'
#' @importFrom rgl plot3d
#' @importFrom rgl planes3d
#'
#' @return  The device window id (an integer).
#'
#' @export
plot_equations_3d <- function(
  win_width = 1000,
  win_height = 800,
  A,
  b,
  equation_color="red",
  alpha = .5,
  lit = TRUE,
  bkg_color = "white",
  draw_axis = TRUE,
  main_lab = NULL, x_lab = "X", y_lab = "Y", z_lab = "Z",
  axis_color="black",
  xlim = c(0,10),
  ylim = c(0,10),
  zlim = c(0,10),
  html_file = NULL){

  if(missing(b)) {
    b <- A[,ncol(A)] # assume last column of Ab
    A <- A[,-ncol(A)] # remove b from Ab
  }

  # set up window
  device_id <- RmatrixPkg::device_new(bgrd_color = bkg_color, width=win_width, height=win_height)

  if(draw_axis){
    # Initialize the scene, no data plotted
    # Create some dummy data
    dat <- replicate(2, 1:3)
    rgl::plot3d(dat, type="n",xlim = xlim, ylim = ylim, zlim = zlim, xlab = x_lab, ylab = y_lab, zlab = z_lab)
  }
  av <-  A[,1]
  bv <- A[,2]
  cv <- A[,3]
  bv <- b
 # rgl::planes3d(av, bv, cv, -bv, col="red", alpha=0.5)
  rgl::planes3d(a=1,b=1,c=2,d=0,col="red",alpha=0.6) # debug

  if(!is.null(html_file)) {
    rgl::rglwidget(filename = html_file, width = win_width, height = win_height)
  }

  RmatrixPkg::device_current()
}
