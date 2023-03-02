library(RmatrixPkg)

to_mat <- rbind(c(2, 5),
                c(5, -2))

vector_labels <- c("(2,5)", "(5,-2)")

RmatrixPkg::plot_vector_2d(
  to = to_mat,
  vector_labels = vector_labels,
  width_height = c(8,8),
  char_mag = 0.7,
  x_breaks = c(-6, 6, 1),
  y_breaks = c(-6, 6, 1)
)

