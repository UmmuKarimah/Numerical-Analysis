f.lr.p <- function(X, beta)
{
  X <- as.matrix(X)
  beta <- as.vector(beta)
  p <- exp(X %*% beta) / (1 + exp(X %*% beta))
  return(p)
}

f.lr.l <- function(y, m, p)
{ # binomial log likelihood function
  # input:   vectors: y = counts; m = sample sizes; p = probabilities
  # output: log-likelihood l, a scalar
  l <- t(y) %*% log(p) + t(m - y) %*% log(1 - p)
  return(l)
}

beet <- read.table("http://statacumen.com/teach/SC1/SC1_11_beetles.dat", header = TRUE)
beet.rep   <- factor(beet$rep)
n <- nrow(beet)
m <- beet$n
y <- beet$y
X.temp <- beet$conc
X <- matrix(c(rep(1,n), X.temp, X.temp^2), nrow = n)
colnames(X) <- c("satu", "conc", "conc2")
r <- ncol(X) - 1
beta.1 <- c(log(sum(y) / sum(m - y)), rep(0, r))

f.lr.FS <- function(X, y, m, beta.1, eps1 = 1e-6, eps2 = 1e-7, maxit=50)
{
  beta.2 <- rep(-Inf, length(beta.1))
  diff.beta <- sqrt(sum((beta.1 - beta.2)^2))
  llike.1 <- f.lr.l(y, m, f.lr.p(X, beta.1))
  llike.2 <- f.lr.l(y, m, f.lr.p(X, beta.2))
  diff.like <- abs(llike.1 - llike.2)
    if (is.nan(diff.like))
      {diff.like <- 1e9}
    i <- 1  
    alpha.step <- seq(-1, 2, by = 0.1)[-11]
    NR.hist <- data.frame(i, diff.beta, diff.like, llike.1, step.size = 1) # iteration history
    beta.hist <- matrix(beta.1, nrow = 1)
        while ((i <= maxit) & (diff.beta > eps1) & (diff.like > eps2))
          { i <- i + 1 
            beta.2 <- beta.1 
            mu.2 <- m * f.lr.p(X, beta.2)
            v.2  <- diag(as.vector(m * f.lr.p(X, beta.2) * (1 - f.lr.p(X, beta.2))))
            score.2 <- t(X) %*% (y - mu.2)
            increm <- solve(t(X) %*% v.2 %*% X) %*% score.2
            #increm <- solve(t(X) %*% v.2 %*% X, score.2)
            llike.alpha.step <- rep(NA, length(alpha.step)) # init llike for line search
              for (i.alpha.step in 1:length(alpha.step))
                {llike.alpha.step[i.alpha.step] <- f.lr.l(y, m, f.lr.p(X, beta.2 + alpha.step[i.alpha.step] * increm))}
                
            ind.max.alpha.step <- which(llike.alpha.step == max(llike.alpha.step))[1]
            beta.1 <- beta.2 + alpha.step[ind.max.alpha.step] * increm
            diff.beta <- sqrt(sum((beta.1 - beta.2)^2))
            llike.2 <- llike.1 
            llike.1 <- f.lr.l(y, m, f.lr.p(X, beta.1))
            diff.like <- abs(llike.1 - llike.2)
                
                # iteration history
                NR.hist   <- rbind(NR.hist, c(i, diff.beta, diff.like, llike.1, alpha.step[ind.max.alpha.step]))
                beta.hist <- rbind(beta.hist, matrix(beta.1, nrow = 1))
        } 
                # prepare output
                out <- list()
                out$beta.MLE <- beta.1
                out$iter<- i - 1
                out$NR.hist <- NR.hist
                out$beta.hist <- beta.hist
                v.1  <- diag(as.vector(m * f.lr.p(X, beta.1) * (1 - f.lr.p(X, beta.1))))
                Iinv.1 <- solve(t(X) %*% v.1 %*% X)  # Inverse information matrix
                out$beta.cov <- Iinv.1
                if (!(diff.beta > eps1) & !(diff.like > eps2))
                  {out$note <- paste("Absolute convergence of", eps1, "for betas and", eps2, "for log-likelihood satisfied")}
                if (i > maxit)
                  {out$note <- paste("Exceeded max iterations of ", maxit)}
                return(out)
            
}

out <- f.lr.FS(X, y, m, beta.1)
out
