## Helper functions for LostInTheForest methods

epsilon <- sqrt(.Machine$double.eps)

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


# Filter out eigenvalues based on some criteria
filter_eigenvalues <- function(ev, m = NULL, mp = 100) {
  if(!length(m)){ # to catch NULL and ingeter(0) i.e. empty vector
    if(!length(mp)){
      mp <- 100
      message(paste("m and mp are NULL, mp defaults to 100"))
    }
    VarExp <- cumsum(ev/sum(ev)*100)
    m <- min(which(VarExp >= mp)) 
  }
  m <- min(m, length(ev))
  ev[seq_len(m)]
}
