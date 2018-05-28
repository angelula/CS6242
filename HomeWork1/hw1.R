install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2) 
library(dplyr)

setwd("~/CS6242/HomeWork1/")

options(expressions=500000)

log_gamma_loop <- function(n) {
	if(n<=1)
		return(0)
	sum<-0
	for(i in seq(1,n,1)) {
		sum<-sum+log(i)	
	}	
	return (sum)
}

log_gamma_recursive <- function(n) {
	if(n<=1)
		return(0)
	return (log(n) + log_gamma_recursive(n-1))
}

sum_log_gamma_loop <- function(n) {
  sum<-0
  for(i in seq(1,n,1)) {
    sum<-sum+log_gamma_loop(i)
  }
  return(sum)
}

sum_log_gamma_recursive <- function(n) {
  sum<-0
  for(i in seq(1,n,1)) {
    sum<-sum+log_gamma_recursive(i)
  }
  return(sum)
}

sum_lgamma <- function(n) {
  sum<-0
  for(i in seq(1,n,1)) {
    sum<-sum+lgamma(i)
  }
  return(sum)
}

n = seq(1, 1000, by=1)

ssum_log_gamma_loop = sapply(n, function(v) {
  start = Sys.time()
  sum_log_gamma_loop(v)
  end = Sys.time()
  return (end-start)
})

ssum_log_gamma_recursive = sapply(n, function(v) {
  start = Sys.time()
  sum_log_gamma_recursive(v)
  end = Sys.time()
  return (end-start)
})

ssum_lgamma = sapply(n,function(v) {
  start = Sys.time()
  sum_lgamma(v)
  end = Sys.time()
  return (end-start)
})

png ("Log Gamma.png",1024,800,"px",12)
plot(n,ssum_log_gamma_loop,main="Gamma Loop", ylab="Run Time", xlab="N",type="l",col="red")
lines(ssum_log_gamma_recursive, col="blue")
lines(ssum_lgamma, col="orange")
legend(0,0.25,legend=c("Loop", "Recurssive", "Lgamma"),col=c("red", "blue","orange"),lty=1:2,cex=0.8)

dev.off()