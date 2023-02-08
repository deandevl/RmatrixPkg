#' Function converts numeric matrix to Latex format
#'
#' @description Function returns a Latex formatted matrix for Rmarkdown documents.
#'
#'  Create the Latex string by calling the function:
#'  \code{A_matrix_str = matrix_to_latex(A)}. The matrix string could then
#'  be used in an in-line Rmarkdown statement:
#'  \code{` paste0("$$", "\\textbf{A} =", A_matrix_str, "$$"`}.
#'
#' @param x A numeric vector, matrix, data frame.
#' @param bracket_type A string that set the bracket type. Acceptable values are
#'  "none", "bracket", "paren", "curly_bracket", "verts",
#'  "double_verts".
#' @param small_inline A logical which if \code{TRUE} creates an inline small matrix.
#' @param fractions A logical which if \code{TRUE} will express non-integers
#'  as rational numbers.
#' @param matrix_name A string that sets the name of the matrix.
#' @param centered A logical which if \code{TRUE} creates Rmarkdown that centers the matrix.
#'
#' @return A string of inline Rmarkdown
#'
#' @author Rick Dean
#'
#' @export
matrix_to_latex <- function(
  x = NULL,
  bracket_type = "bracket",
  small_inline = FALSE,
  fractions = FALSE,
  matrix_name = NULL,
  centered = FALSE
){
  bracket_str <- switch(
    bracket_type,
    "none" = "matrix",
    "bracket" = "bmatrix",
    "paren" = "pmatrix",
    "curly_bracket" = "Bmatrix",
    "verts" = "vmatrix",
    "double_verts" = "Vmatrix"
  )

  begin_str <- paste0("\\begin{", bracket_str,"}")
  end_str <- paste0("\\end{", bracket_str,"}")

  if(small_inline){
    begin_str <- paste0("\\begin{smallmatrix}", begin_str)
    end_str <- paste0(end_str, "\\end{smallmatrix}")
  }

  a_matrix <- as.matrix(x)
  if(!fractions){
    rows <- apply(a_matrix, MARGIN = 1, paste, collapse = " & ")
    matrix_str <- paste(rows, collapse = " \\\\ ")
    latex_str <- paste(begin_str, matrix_str, end_str)
  }else{
    rows <- apply(a_matrix, MARGIN = 1, make_row)
    matrix_str <- paste(rows, collapse = " \\\\ ")
    latex_str <- paste(begin_str, matrix_str, end_str)
  }

  if(is.null(matrix_name)){
    name_str <- ""
  }else {
    name_str <- paste0("\\textbf{", matrix_name, "} = ")
  }

  if(!small_inline){
    if(centered){
      return(paste0("$$", name_str,  latex_str, "$$"))
    }else{
      return(paste0("$", name_str,  latex_str, "$"))
    }
  }else{
    return(paste0("$", latex_str, "$"))
  }
}

make_row <- function(a_row){
  row_str <- ""
  for(i in 1:(length(a_row)-1)){
    val_str <- ratio(a_row[[i]])
    row_str <- paste0(row_str, val_str, " & ")
  }
  val_str <- ratio(a_row[[i+1]])
  row_str <- paste0(row_str, val_str)
  return(row_str)
}

ratio <- function(x){
  denom <- 1
  while(nchar(x*denom) != nchar(round(x*denom))){
    denom <- denom + 1
  }
  if(denom == 1 | denom %% 10 == 0){
    return(x)
  }else {
    return(paste0(x * denom, "/", denom))
  }
}
