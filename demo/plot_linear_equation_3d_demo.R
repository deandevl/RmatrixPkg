library(rgl)
library(RmatrixPkg)

# ways to submit A and b for linear equation x + y + 2z = 0

# separate A and b
RmatrixPkg::device_close_all()
A <- matrix(c(1, 1, 2), ncol = 3)
b = 0
dev_id <- RmatrixPkg::plot_equations_3d(
  A = A,
  b = b,
  draw_axis = TRUE,
  xlim = c(-4,4),
  ylim = c(-4,4),
  zlim = c(-4,4))

RmatrixPkg::device_close_all()
# as one matrix or data.frame
A_df <- data.frame(
  x = 1,
  y = 1,
  z = 2,
  b = 0
)
dev_id <- RmatrixPkg::plot_equations_3d(
  A = A_df,
  draw_axis = TRUE,
  xlim = c(-4,4),
  ylim = c(-4,4),
  zlim = c(-4,4))

RmatrixPkg::device_close_all()
A_2_df <- data.frame(
  x = c(1,2),
  y = c(1,1),
  z = c(2,3),
  b = c(0,2)
)
dev_id <- RmatrixPkg::plot_equations_3d(
  A = A_2_df,
  draw_axis = TRUE,
  xlim = c(-4,4),
  ylim = c(-4,4),
  zlim = c(-4,4)
)

