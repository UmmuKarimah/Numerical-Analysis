
NewtonUmum<-function (x,y,xx)
{
  n = length(x)
  if (length(y) != n) 
  {warning("x and y must be same length")}
  b = matrix(0,n,n)
  b[,1] = y
  for (j in 2:n)
    {
      for (i in 1:(n-j+1))
        {
         b[i,j] = (b[i+1,j-1]-b[i,j-1])/(x[i+j-1]-x[i])
        }
    }
  xt <- 1
  yint = b[1,1]
    for (j in 1:(n-1))
      {
        xt = xt*(xx-x[j])
        yint = yint+b[1,j+1]*xt
      }
  print(b)
  print(yint)
}

x<-c(1,1.3,1.6,1.9,2.2,2.5,2.8)
y<-c(1.449,2.060,2645,3.216,3.779,4.338,4.898)
NewtonUmum(x,y,1.69)
