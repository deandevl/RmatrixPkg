#' Function performs a QR decomposition of numeric matrix by converting it
#'  to a Householder matrix or reflector matrix.
#'
#' For a numeric matrix X of n x m  (m <= n) then the Householder
#'  process rotates/reflects into an orthogonal matrix Q. The function also
#'  returns an upper triangular matrix \code{R} where \code{X = QR},
#'  known as QR factorization.
#'
#' @param x A numeric matrix or data frame
#' @param complete A logical which it TRUE returns a "thin" orthogonal matrix Q
#'  and triangular matrix R.
#'
#' @return A named list with the orthogonal matrix Q and triangular matrix R.
#'
#' @author Rick Dean
#'
#' @export
qr_householder <- function(x, complete = FALSE){
  R <- as.matrix(x)
  n <- nrow(R)
  p <- ncol(R)
  Q <- diag(n)

  for(k in 1:p){
    # extract the kth column of the matrix
    col <- as.matrix(R[k:n, k])
    # calculation of the norm of the column in order to create the vector r
    norm1 <- sqrt(drop(crossprod(col)))
    # calculate the reflection vector a_r
    a_r <- col
    a_r[1] <- a_r[1] + sign(a_r[1]) * norm1
    # beta = 2 / ||a_r||^2
    beta <- 2 / drop(crossprod(a_r))
    # update matrix Q (trailing matrix only) by Householder reflection

    #val <-  tcrossprod(Q[,k:n] %*% a_r, beta * a_r)
    val_1 <- Q[,k:n] %*% a_r

    val_2 <-  tcrossprod(val_1, beta * a_r)

    Q[,k:n] <- Q[,k:n] - val_2

    # update matrix R (trailing matrix only) by householder reflection
    R[k:n, k:p] <- R[k:n, k:p] - tcrossprod(beta * a_r, crossprod(R[k:n,k:p], a_r))
  }

  if(complete){
    R[lower.tri(R)] <- 0
    return(list(Q = Q, R = R))
  }else {
    R <- R[1:p, ]
    R[lower.tri(R)] <- 0
    return(list(Q = Q[,1:p], R = R))
  }
}
