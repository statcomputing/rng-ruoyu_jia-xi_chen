#3(a)----------------------------------------------------------
f.sample <- function(theta,b, n=10000) {
  sample = rep(0, n)
  lambda = 0.6
  for (i in 1:n) {
    while(1){
      if (runif(1)>lambda){
        x = runif(1)^(1/theta)
      } else {
        x = 1-runif(1)^(1/b)
      }
      if (runif(1)*sqrt(3)/(lambda*b)*((1-lambda)*theta*x^(theta-1           )+lambda*b*(1-x)^(b-1))<x^(theta-1)/(1+x^2)+sqrt(2+x^2)*           (1-x)^(b-1)){
        sample[i] = x
        break
      }
    }
  }
  
  #plot
  intvl=c(0.05, 0.95)
  n.p=501
  df.obj = density(sample, from=intvl[1], to=intvl[2], n=n.p)
  x = df.obj$x
  df = df.obj$y
  f.val = x^(theta-1)/(1+x^2)+sqrt(2+x^2)*(1-x)^(b-1)
  f <- function(x) {x^(theta-1)/(1+x^2)+sqrt(2+x^2)*(1-x)^(b-1)}
  c <- integrate(f, lower = 0, upper = 1)$value
  f.val = f.val*mean(df)/c
  title1 <- paste("theta=",theta)
  title2 <- paste("beta=",b)
  plot(ts(cbind(df, f.val), start=intvl[1],                              deltat=diff(intvl)/(n.p-1)), main = c(title1, title2),
       plot.type="single", col=c("blue", "red"),
       ylab="density", xlab="x")
  legend("topright",legend=c('sample','f'),col=c('blue','red'),
         lty=1)
  
  return(sample)
}

sample1 = f.sample(5,5)
sample2 = f.sample(5,10)

#3(b)-----------------------------------------------------------
f.separate <- function(theta,b, n=10000) {
  sample = rep(0, n)
  for (i in 1:n) {
    a = sample(1:2,1,prob = c(beta(theta,1),sqrt(3)*beta(1,b)))
    if (a==1){
      while (1) {
        x = rbeta(1,theta,1)
        if ((1+x^2)*runif(1)<1) {
          sample[i] = x
          break
        } 
      }
    } else {
      while (1) {
        x = rbeta(1,1,b)
        if (sqrt(3)*runif(1)<sqrt(2+x^2)) {
          sample[i] = x
          break
        } 
      }
    }
  }
  
  #plot
  intvl=c(0.05, 0.95)
  n.p=10001
  df.obj = density(sample, from=intvl[1], to=intvl[2], n=n.p)
  x = df.obj$x
  df = df.obj$y
  f.val = x^(theta-1)/(1+x^2)+sqrt(2+x^2)*(1-x)^(b-1)
  f <- function(x) {x^(theta-1)/(1+x^2)+sqrt(2+x^2)*(1-x)^(b-1)}
  c <- integrate(f, lower = 0, upper = 1)$value
  f.val = f.val/c
  title1 <- paste("theta=",theta)
  title2 <- paste("beta=",b)
  plot(ts(cbind(df, f.val), start=intvl[1],                              deltat=diff(intvl)/(n.p-1)), main = c(title1, title2),
       plot.type="single", col=c("blue", "red"),
       ylab="density", xlab="x")
  legend("topright",legend=c('sample','f'),col=c('blue','red'),
         lty=1)
  
  return(sample)
}

sample3 = f.separate(5,5)
sample4 = f.separate(5,10)