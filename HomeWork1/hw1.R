setwd("~/CS6242/HomeWork1/")

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

