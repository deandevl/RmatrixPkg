library(RmatrixPkg)

# Demo of RmatrixPkg::dot(v1, v2)

# Define two 3x1 matrices and compute their dot product.
# Example source from "Linear Algebra Step by Step" by Kuldeep Singh
#  Section 1.3.6 page 36
m_1 <- cbind(c(-3, 1, 7))
m_2 <- cbind(c(9, 2, -4))

m_1_dot_m_2 <- RmatrixPkg::dot(v1 = m_1, v2 = m_2)

# Define two vectors and compute their dot product.
v_1 <- c(-3, 1, 7)
v_2 <- c(9, 2, -4)

v_1_dot_v_2 <- RmatrixPkg::dot(v1 = v_1, v2 = v_2)
