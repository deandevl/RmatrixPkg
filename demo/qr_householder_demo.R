library(RmatrixPkg)

# ------------QR decomposition for square matrix----------
X <- cbind(c(1,1,1), c(1,1,0), c(1,0,0))
X_qr <- RmatrixPkg::qr_householder(X)

# t(Q)*Q = I ?
test_id_1 <- t(X_qr$Q) %*% X_qr$Q

# Q * t(Q) = I ?
test_id_2 <- X_qr$Q %*% t(X_qr$Q)

# R is upper triangular ?
test_upper_tri <- upper.tri(X_qr$R, diag = T)

# X = QR ?
test_QR <- X_qr$Q %*% X_qr$R

# -----------QR decomposition for rectangular matrix----
X <- cbind(c(1,0,1), c(2,1,0))
X_qr <- RmatrixPkg::qr_householder(X)

# t(Q)*Q = I ?
test_id_1 <- t(X_qr$Q) %*% X_qr$Q

# R is upper triangular ?
test_upper_tri <- upper.tri(X_qr$R, diag = T)

# X = QR ?
test_QR <- X_qr$Q %*% X_qr$R
