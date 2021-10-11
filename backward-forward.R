sfunc <- function (n, i, x) {
  s <- diag (n)
  s [i, i] <- x
  return (s)
}

tfunc <- function (n, i, j, x) {
  t <- diag (n)
  t [i, j] <- x
  return (t)    
}
set.seed (12345)
print (a <- matrix (sample (16), 4, 4))
#==================FORWARD================================
gaussMatrixForward <- function (a, verbose = TRUE) {
  n <- nrow (a)
  for (i in 1 : n) {
    a <- sfunc (n, i, 1 / a[i, i]) %*% a
    if (verbose) {
      print (noquote (formatC (a, digits = 4, width = 7, format = "f")))
    }
    if (i == n) {
      break ()
    }
    for (j in (i + 1) : n) {
      a <- tfunc (n, j, i, - a[j, i]) %*% a
      if (verbose) {
        print (noquote (formatC (a, digits = 4, width = 7, format = "f")))
      }
    }
  }
  return (a)
}
fa <- gaussMatrixForward (cbind(a,diag(4)))

#==================BACKWARD====================================================
gaussMatrixBackward <- function (a, verbose = TRUE) {
  n <- nrow (a)
  for (i in n : 2) {
    for (j in (i - 1) : 1) {
      a <- tfunc (n, j, i, - a[j, i]) %*% a
      if (verbose) {
        print (noquote (formatC (a, digits = 4, width = 7, format = "f")))
      }
    }
  }
  return (a)
}
ga <- gaussMatrixBackward (fa)

#============================================================================
print (fx <- gaussMatrixBackward (gaussMatrixForward (cbind (a, 1:4), verbose = FALSE), verbose = FALSE))

aa <- matrix(c(2,3,1,1,2,-5,-1,-2,4),nrow=3)
bb <- c(1,1,3)
solve(aa,bb)

am <- matrix(c(2,3,1,1,2,-5,-1,-2,4,1,1,3),nrow=3)
print (fx <- gaussMatrixBackward (gaussMatrixForward (am, verbose = FALSE), verbose = FALSE))
print (fx <- gaussMatrixBackward (gaussMatrixForward (cbind(aa,bb), verbose = FALSE), verbose = FALSE))

3x + 2y ??? 1z = ???3
???3x ??? 3y ??? 3z = 9
1y ??? 1z = ???1

