library(RmatrixPkg)

# Example from "Linear Algebra Step by Step" by Kuldeep Singh
# (Section 3.6.3, Example 3.39, page 266)

# Determine whether the following systems have infinite, unique, or no
# solutions.

# A system with no solution:
A <- rbind(c( 1, -1, -2, -3),
           c(-4,  4,  8, 12))
b <- cbind(c(5, 2))

Ab_rank_none <- RmatrixPkg::ls_rank(A = A, b = b)
Ab_rank_none

# A system with a unique solution
A <- rbind(c(1, 2, 3),
           c(4, 5, 6),
           c(7, 8, 8))
b <- cbind(c(1, 2 ,3))

Ab_rank_unique <- RmatrixPkg::ls_rank(A = A, b = b)
Ab_rank_unique
