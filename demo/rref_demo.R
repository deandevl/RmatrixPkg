library(RmatrixPkg)

# Example from "Linear Algebra Step by Step" by Kuldeep Singh
# (Section 1.2.4, Example 1.9, page 24)

# Create an augmented matrix that represents a non-homogeneous
# linear system of equations in the form Ax = b where:

A <-rbind(c(1,   5,  -3),
          c(0, -13,   5),
          c(0,   0,   5))

b <- cbind(c(-9, 37, -15))

Ab <- cbind(A,b)

# calculate the rref of "Ab":
Ab_rref <- RmatrixPkg::rref(x = Ab)

# The fourth column of "Ab_rref" shows the specific
# solution "x" in Ax = b: (2, -4, -3)'
Ab_rref
