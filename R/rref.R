#' Function implements Gauss-Jordan elimination algorithm in converting a numeric matrix to
#'  reduced row-echelon form (rref).
#'
#' @param x a numeric matrix or data frame.
#'
#' @return A matrix in reduced reduced row echelon form.
#'
#' @author Rick Dean
#'
#' @export
rref <- function(x){
  X <- as.matrix(x)

  m <- nrow(X)
  n <- ncol(X)
  currCol <- 1
  nonZeroRowCount <- 0

  while((currCol < n+1) & (nonZeroRowCount+1 <= m)){
    if(sum(X[(nonZeroRowCount+1):m, currCol]) == 0){
      currCol <- currCol + 1
    }else{
      rowIndex <- 0
      for(i in (nonZeroRowCount+1):m){
        if(X[i,currCol] != 0){
          rowIndex <- i
          break
        }
      }

      nonZeroRowCount <- nonZeroRowCount + 1

      #switch rows
      row1 <- X[rowIndex,]
      row2 <- X[nonZeroRowCount,]
      X[rowIndex,] <- row2
      X[nonZeroRowCount,] <- row1

      # Convert the entry in row "nonZeroRowCount" and "currCol" to a 1
      X[nonZeroRowCount,] <- (1/X[nonZeroRowCount, currCol]) * X[nonZeroRowCount,]

      # Convert every other row entry of "currCol" to zero
      for(k in 1:m){
        if((X[k,currCol] != 0) & (k != nonZeroRowCount)){
          # make X[k,currCol] = 0
          scalar <- X[k,currCol]/X[nonZeroRowCount,currCol]
          X[k,] <- -1 * scalar * X[nonZeroRowCount,] + X[k,]
        }
      }
      # increment and repeat
      currCol <- currCol + 1
    }
  }
  return(X)
}
