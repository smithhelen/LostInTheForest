## Helper functions for Ranger methods

# This is equivalent of scale(t(scale(t(x), scale=FALSE)),scale=FALSE)
# This is used in the cmdscale function
# To calculate Gower's B matrix
dbl_center <- function(x)
{
  x <- as.matrix(x)
  center1 <- colMeans(t(x), na.rm=TRUE)
  x <- sweep(t(x), 2L, center1, FUN = "-", check.margin=FALSE)
  center2 <- colMeans(t(x), na.rm=TRUE)
  x <- sweep(t(x), 2L, center2, FUN = "-", check.margin=FALSE)
  x
}


# A version of eigen() that maintains rownames on the eigenvectors
eigen_decomp <- function(X, symmetric) {
  E <- eigen(X, symmetric)
  rownames(E$vectors) <- rownames(X)
  colnames(E$vectors) <- paste0("V", 1:nrow(E$vectors))
  E
}


