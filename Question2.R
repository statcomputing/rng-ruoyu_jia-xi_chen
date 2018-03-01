#2(b)---------------------------------------------------------
g.sample <- function(theta,n){
  sample = rep(0, n)
  for (i in 1:n){
    a = sample(1:2,1,prob = c(2*gamma(theta),gamma(theta+1/2)))
    if (a==1){
      x = rgamma(1,theta)
      sample[i] = x
    } else {
      x = rgamma(1,theta+1/2)
      sample[i] = x
    }
  }
  return(sample)
}

sample = g.sample(5,10000)
theta = 5 

#plot
intvl=c(.1, 15)
n.p=10001
dg.obj = density(sample, from=intvl[1], to=intvl[2], n=n.p)
x = dg.obj$x
dg = dg.obj$y
g.val = 2*x^(theta-1)*exp(-x)+x^(theta-1/2)*exp(-x)
g <- function(x) {2*x^(theta-1)*exp(-x)+x^(theta-1/2)*exp(-x)}
c <- integrate(g, lower = 0, upper = Inf)$value
g.val = g.val/c
title1 <- paste("theta=",theta)
plot(ts(cbind(dg,g.val),start=intvl[1],
        deltat=diff(intvl)/(n.p-1)), main = c(title1),
     plot.type="single", col=c("blue", "red"),
     ylab="density", xlab="x")
legend("topright",legend=c('sample','g'),col=c('blue','red'),
       lty=1)

#2(c)------------------------------------------------------------
f.sample <- function(theta, n=10000) {
  sample = rep(0, n)
  for (i in 1:n) {
    while (1) {
      x = g.sample(theta,1)
      if ((sqrt(2)/2)*(2+sqrt(x))*runif(1) < sqrt(4+x)) {
        sample[i] = x
        break
      }
    }
  }
  
  #plot
  intvl=c(.1, 15)
  n.p=10001
  df.obj = density(sample, from=intvl[1], to=intvl[2], n=n.p)
  x = df.obj$x
  df = df.obj$y
  f.val = sqrt(4+x)*x^(theta-1)*exp(-1*x)
  f <- function(x) {sqrt(4+x)*x^(theta-1)*exp(-1*x)}
  c <- integrate(f, lower = 0, upper = Inf)$value
  f.val = f.val/c
  title1 <- paste("theta=",theta)
  plot(ts(cbind(df,f.val),start=intvl[1],
          deltat=diff(intvl)/(n.p-1)),main = c(title1),
       plot.type="single", col=c("blue", "red"),
       ylab="density", xlab="x")
  legend("topright",legend=c('sample','f'),col=c('blue','red'),
         lty=1)
  
  return(sample)
}

sample = f.sample(5)
