install.packages("ggplot2")
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

n = seq(1, 100, by=1)

ssum_log_gamma_loop = sapply(n, function(v) {
  start = Sys.time()
  sum_log_gamma_loop(v)
  end = Sys.time()
  return (end-start)
})

png ("sum_log_gamma_loop.png",480,480,"px",12)
plot(n,ssum_log_gamma_loop,main="Gamma Loop", ylab="Run Time", xlab="N")
dev.off()