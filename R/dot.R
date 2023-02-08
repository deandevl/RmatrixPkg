#' Function computes the dot product (inner product) of two 3x1 matrices
#'
#' @description Function accepts two numeric vectors or two 3x1 matrices
#'
#' @param v1 A numeric vector or 3x1 numeric matrix
#' @param v2 A numeric vector or 3x1 numeric matrix
#'
#' @return A numeric value
#'
#' @author Rick Dean
#'
#' @export
dot <- function(v1, v2){
  x1 <- as.matrix(v1)
  x2 <- as.matrix(v2)

  return(drop(t(x1) %*% x2))
}
