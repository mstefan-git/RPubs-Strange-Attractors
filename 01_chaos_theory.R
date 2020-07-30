# Replication file for: "Using R to Find Strange Attractors"
# RPubs-link: https://rpubs.com/mstefan-rpubs/chaos
# (c) Martin Stefan, July 2020

# clear workspace
rm(list = ls())

# set parameters
n = 10
r <- 2
par(mfrow = c(1,2))

# exponentional growth
x1 <- c(.1,rep(NA,n-1))
x2 <- c(.2,rep(NA,n-1))
x3 <- c(.3,rep(NA,n-1))
for(i in 2:n){
  x1[i] <- r*x1[i-1]
  x2[i] <- r*x2[i-1]
  x3[i] <- r*x3[i-1]
} 
plot(x1, type="l",
     xlab="", ylab="", 
     main="Exponentional growth",
     ylim=c(0,150))
lines(x2, col="red")
lines(x3, col="blue")

# logistic equation
x1 <- c(.1,rep(NA,n-1))
x2 <- c(.2,rep(NA,n-1))
x3 <- c(.3,rep(NA,n-1))
for(i in 2:n){
  x1[i] <- r*x1[i-1]*(1-x1[i-1])
  x2[i] <- r*x2[i-1]*(1-x2[i-1])
  x3[i] <- r*x3[i-1]*(1-x3[i-1])
} 
plot(x1, type="l", xlab="", ylab="",
     main="Fixed point solution")
lines(x2, col="red")
lines(x3, col="blue")

# function to run logistic equation
logEq <- function(x0=.1, r=2, n=10, plot=T){
  
  # x0: starting value
  # r: growth rate
  # n: number of periods
  
  # simulation
  x <- rep(NA,n)
  x[1] <- x0
  for(i in 2:n) x[i] <- r*x[i-1]*(1-x[i-1])
  
  # plot
  if(plot == T){
    plot(x, type="l", xlab="", ylab="",
         main = paste("x0=",x0,", r=", r, sep=""))    
  }
  
  return(x)
}

# different starting values
par(mfrow = c(2,3))
for(x0 in c(.1,.3,.5,0,-.1,1.1)) logEq(x0=x0, n=6)

# different growth rates
par(mfrow = c(3,3))
for(r in c(.5,.1,1.5,2,2.8,3.2,3.5,3.8,4.1)) logEq(r=r, n=50)


# bifurcation diagram
r <- seq(2,4,.001)
x <- sapply(r,logEq,x0=.1,n=500,plot=F)[450:500,]
par(mfrow=c(1,1))
plot(as.numeric(x)~rep(r,each=nrow(x)), 
     pch=".", xlab="r", ylab="x")



# function to compute the lyapunov exponent
lyapunov <- function(x,r) sum(log2(abs(r*(1-2*x))))/length(x)

# plot lyapunov exponent over different growth rates
r <- seq(2,4,.001)
x <- sapply(r,logEq,x0=.1,n=500,plot=F)
l <- rep(NA,length(r))
for(i in 1:length(l)) l[i] <- lyapunov(x=x[,i],r=r[i])
plot(l ~ r, type="l", ylim=c(-1,1), ylab="Lyapunov exponent")
abline(h = 0, col="red")


