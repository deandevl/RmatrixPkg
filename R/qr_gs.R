#' Function produces an orthogonal basis \code{Q} for a vector space represented
#'  by a numeric matrix \code{X} via Gram-Schmidt
#'
#' Function uses the Gram-Schmidt process to convert the
#'  columns of a basis matrix \code{X} into an orthogonal basis \code{Q}. The
#'  function also returns an upper triangular matrix \code{R} where \code{X = QR},
#'  known as QR factorization.
#'
#' @param x A numeric matrix or data frame
#'
#' @return A numeric matrix of orthogonal basis column vectors
#'
#' @author  Rick Dean
#'
#' @export
qr_gs <- function(x){
  X <- as.matrix(x)
  n <- ncol(X)
  m <- nrow(X)

  Q <- matrix(0, m, n)
  R <- matrix(0, m, n)

  for(j in 1:n){
    q <- X[,j]
    # skip the first column
    if(j > 1){
      for(i in 1:(j-1)){
        R[i,j] <- as.numeric(t(Q[,i]) %*% X[,j])
        # subtract the projection from q which causes q to become perpendicular to
        #   all columns of Q
        q <- q - R[i,j] * Q[,i]
      }
    }
    # calculate the norm of the jth diagonal of R
    R[j,j] <- sqrt(sum(q^2))
    # normalize the basis vector and store in Q
    Q[,j] <- q/R[j,j]
  }
  return(list(Q = Q, R = R))
}

