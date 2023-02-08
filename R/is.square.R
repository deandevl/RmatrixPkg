#' Function returns TRUE if matrix is square
#'
#' @param x A matrix or data frame that can be converted to a matrix.
#'
#' @return Returns TRUE if argument \code{x} is square
#'
#' @export
is.square <- function(x){
  X <- as.matrix(x)
  return(nrow(X) == ncol(X))
}
