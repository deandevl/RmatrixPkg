library(rgl)
library(RmatrixPkg)

# default look - white background; black lines, text
RmatrixPkg::device_close_all()
seg_1 <- matrix(c(1,2,0,1,2,5), ncol = 6, byrow = T)
device_id <- RmatrixPkg::plot_lines_3d(
  segments = seg_1,
  bkg_color = "black",
  segment_colors = "white",
  axis_color = "white"
)


RmatrixPkg::device_close_all()
seg_1 <- c(0,0,0,5,0,0)
seg_2 <- c(0,0,0,0,5,0)
seg_3 <- c(0,0,0,0,0,5)
segments <- matrix(c(seg_1, seg_2, seg_3), ncol = 6, byrow = T)

device_id <- RmatrixPkg::plot_lines_3d(
  segments = segments,
  segment_colors = c("red","green","blue"),
  segment_widths = c(2.5, 3.0, 4.0),
  bkg_color = "black",
  axis_color = "white"
)

