setwd("~/CS6242/Activity1/")

options(expressions=500000)

logTimeVector = []
sumLogTimeVector = []
fibTimeVector = []

n = seq(1, 40, by=1)

log_factorial <- function (n) {
  # Return the log of factorial(n) for any integer n > 0
  if (n <= 1)
    return (0)
  return (log(n) + log_factorial(n - 1))
}

sum_log_factorial <- function (n) {
  # Return the sum of log_factorial(i) for i in 1..n
  sum <- 0
  for(i in seq(1, n, 1)) {
    sum <- sum + log_factorial(i)
  }
  return (sum)
}

fibonacci <- function(n) {
  # Return nth Fibonacci number
  if (n <= 1)
    return (n)
  return (fibonacci(n - 1) + fibonacci(n - 2))
}


fibTimeVector = sapply(n, function(v) {
  
  start = Sys.time()
  fibonacci(v)
  end = Sys.time()
  return (start-end)
})

png("fibonacci.png",480,480,"px",12)
plot(n, fibTimeVector, main="Fibonacci", ylab="Run Time", xlab="N")
dev.off()

logTimeVector = sapply(n, function(v) {
  
  start = Sys.time()
  log_factorial(v)
  end = Sys.time()
  return (start-end)
})

png("LogFactorial.png",480,480,"px",12)
plot(n, logTimeVector, main="", ylab="Run Time", xlab="N")
dev.off()

sumLogTimeVector = sapply(n, function(v) {
  
  start = Sys.time()
  sum_log_factorial(v)
  end = Sys.time()
  return (start-end)
})

png("SumLogFactorial.png",480,480,"px",12)
plot(n, sumLogTimeVector, main="Sum Log Factorial", ylab="Run Time", xlab="N")
dev.off()
