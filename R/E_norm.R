#
#' Function returns the length of a vector or column lengths from a matrix
#'
#' @description Function calculates the Euclidean norm of a vector
#'  or norm for each column if a matrix is submitted.
#'
#' @param X a numeric vector or matrix
#'
#' @return a scalar norm (if \code{X} is a vector) or a vector (if \code{X} is a matrix)
#'  of norms for each column.
#'
#' @author Rick Dean
#'
#' @export
E_norm <- function(X){
  if(!is.numeric(X)) stop("X must be numeric")
  if(is.vector(X)) X <- as.matrix(X)

  return(sqrt(colSums(X^2)))
}
