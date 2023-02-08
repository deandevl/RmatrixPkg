#' Function returns the rank of a linear system Ax = b and determines if
#'  the system has a unique solution, an infinite number of solutions, or
#'  no solution.
#'
#' @param A A numeric matrix or data frame that represents a system of equations.
#' @param b A numeric vector or matrix that represents the constant values \code{b}
#'  in \code{Ax = b}.
#'
#' @return Function returns a named list with the ranks of both \code{A} and the
#'  augmented matrix \code{A|b}, and whether the solution is "unique", "infinite", or
#'  "none".
#'
#' @export
ls_rank <- function(A, b){
  AA <- as.matrix(A)
  bb <- as.matrix(b)

  Ab <- cbind(AA,bb)

  AA_rref <- RmatrixPkg::rref(AA)
  Ab_rref <- RmatrixPkg::rref(Ab)

  rank_AA <- 0
  n_equations <- nrow(AA_rref)
  n_AA_cols <- ncol(AA_rref)
  equation_zeros <- rbind(rep(0,n_AA_cols))
  for(i in 1:n_equations){
    equation_zero_sum <- sum(AA_rref[i,] == equation_zeros)
    if(equation_zero_sum != n_AA_cols){
      rank_AA <- rank_AA + 1
    }
  }

  rank_Ab <- 0
  n_Ab_cols <- ncol(Ab_rref)
  equation_zeros <- rbind(rep(0,n_Ab_cols))
  for(i in 1:n_equations){
    equation_zero_sum <- sum(Ab_rref[i,] == equation_zeros)
    if(equation_zero_sum != n_Ab_cols){
      rank_Ab <- rank_Ab + 1
    }
  }

  if(rank_AA == rank_Ab & (rank_AA == n_AA_cols)){
    solution <- "unique"
  }else if(rank_AA == rank_Ab & (rank_AA < n_AA_cols)){
    solution <- "infinite"
  }else if(rank_AA != rank_Ab){
    solution <- "none"
  }

  return(list(
    rank_A = rank_AA,
    rank_Ab = rank_Ab,
    solution = solution
  ))
}
