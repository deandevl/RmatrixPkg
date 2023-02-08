#' Function returns the trace of a square numerical matrix
#'
#' @param x A square numeric matrix or data frame that can be
#'  converted to a square numeric matrix.
#'
#' @return A scalar that represents the trace of a matrix
#'
#' @export
tr <- function(x)  {
  X <- as.matrix(x)
  if(!is.numeric(X) | !RmatrixPkg::is.square(X)){
    stop("Matrix argument is not numeric or square.")
  }
  return(sum(diag(X)))
}
