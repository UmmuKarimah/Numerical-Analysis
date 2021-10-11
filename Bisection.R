root_bisection <- function( f, a, b, tol=1e-7, N=100)
{

  iter <- 0
  fa <- f(a)
  fb <- f(b)
  while(abs(b-a)>tol)
    {
    iter <- iter+1
      if(iter>N)
        {
          warning("iterations maximum exceeded")
          break
        }
    x <- (a+b)/2
    fx <- f(x)
    print(fx)
      if(fa*fx>0)
        {
          a <- x
          fa <- fx
        } 
      else
        {
          b <- x
          fb <- fx
        }
    }
# iterasi nilai x sebagai return value
root <- (a+b)/2
return(list(`function`=f, root=root, iter=iter))
}
root_bisection(  f<-function(x)
{f=x*exp(-x)+1},a=0,b=-1,tol=1e-7, N=100)


