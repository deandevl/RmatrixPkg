#' Function computes the projection of a vector onto another vector
#'
#' Function returns the vector coordinates of a vector projected
#'  onto another vector.
#'
#' @param x A numeric vector that is to be projected onto \code{v}
#' @param v A numeric vector
#'
#' @return The coordinates of a projected vector onto another vector..
#'
#' @author Rick Dean
#'
#' @export
proj_vector <- function(x, v){

  v_norm <- RmatrixPkg::E_norm(v)

  scalar_proj <- as.numeric((x %*% v)/v_norm)

  vector_proj <- scalar_proj * v/v_norm

  return(vector_proj)
}
