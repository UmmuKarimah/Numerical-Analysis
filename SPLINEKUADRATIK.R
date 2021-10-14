splinekuadratik<-function(x,y,xx)
{
  n <- length(x)
  if (xx < x[1] | xx>x[n])
  {
    error ('out of x range!')
  }
  for (i in 1:n)
  {
      if (xx > x[i] & xx<x[i+1])
          {
            print(i)
            s=y[i]+b[i]*(xx-x[i])+c[i]*(xx-x[i])^2
          }
  }
  print (s)
}

x<-c(3,4.5,7,9)
y<-c(2.5,1,2.5,0.5)

A <- matrix(c(1.5, 0, 0, 0, 0,
              0, 2.5, 0, 6.25, 0,
              0, 0, 2, 0, 4,
              1, -1, 0, 0, 0,
              0, 1, -1, 5, 0), ncol = 5, nrow = 5, byrow = T)
b <- c(-1.5, 1.5, -2,0,0)

gauss<-solve(A)%*%b
b<-gauss[1:3,]
c<-c(0,gauss[4:5])

splinekuadratik(x,y,5)

spline(x,y)