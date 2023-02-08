library(rgl)
library(data.table)
library(RmatrixPkg)
library(here)

current_dir <- here()

# plot iris data without grouping
RmatrixPkg::device_close_all()
dev_id <- RmatrixPkg::plot_points_3d(
  df = iris,
  x_col = "Sepal.Length",
  y_col = "Petal.Length",
  z_col = "Sepal.Width",
  x_lab = "Sepal.Length",
  y_lab = "Petal.Length",
  z_lab = "Sepal.Width",
  pt_colors = "blue",
  pt_size = 8.0,
  pt_alpha = 0.3,
  bkg_color = "black",
  axis_color = "white",
  main_title = "IRIS Plot"
)

RmatrixPkg::device_close_all()
html_file <- file.path(current_dir,"demos/plot_points_3d/plot_points.html")
#plot iris data with grouping by "Species"
dev_id <- RmatrixPkg::plot_points_3d(
  df = iris,
  x_col = "Sepal.Length",
  y_col = "Petal.Length",
  z_col = "Sepal.Width",
  group_col = "Species",
  x_lab = "Sepal.Length",
  y_lab = "Petal.Length",
  z_lab = "Sepal.Width",
  pt_colors = c("red", "blue", "yellow"),
  pt_size = 8.0,
  pt_alpha = 0.3,
  bkg_color = "black",
  axis_color = "white",
  main_title = "IRIS Plot",
  html_file = html_file
)



