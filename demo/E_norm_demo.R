library(RmatrixPkg)

w <- c(1,2,3)
x <- c(4,5,6)

w_norm <- RmatrixPkg::E_norm(w) # should be sqrt(14) = 3.74165738
x_norm <- RmatrixPkg::E_norm(x) # should be sqrt(77) = 8.77496438

W_mat <- cbind(c(1,2,3), c(4,5,6))

W_norm <- RmatrixPkg::E_norm(W_mat)
