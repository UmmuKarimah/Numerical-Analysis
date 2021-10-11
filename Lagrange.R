Lagrange<-function (x,y,xx)
{
  n <- length(x)
  if (length(y) != n) 
    {warning("x and y must be same length")}
  s <- 0
  for (i in 1:n)
  {
    product = y[i]
      for (j in 1:n)
      {
        if (i!= j)
        {product = product*((xx - x[j])/(x[i] -x[j]))}
      }
    s = s + product
    
  }
  yint = s
  print (yint)
}

x<-c(0,5,10,15,20,25,30,35,40)
y<-c(-0.33,0.06,0.06,0.05,0.04,0.03,0.03,0.03,0.02)
Lagrange(x,y,12)
x <- c(151,174,138,186,128,136,179,163,152,131)
y <- c(63,81,56,91,47,57,76,72,62,48)
Lagrange(x,y,170)
a<-c(0,0.05,1,0.15,0.2,0.25,0.3,0.35)
b<-c(0,0.04843,0.09389,0.13660,0.17675,0.21453,0.25011,0.28365)
Lagrange(a,b,0.03)