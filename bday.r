#!/usr/bin/env Rscript

# Taken from: https://stats.stackexchange.com/a/335132
#
# See: https://en.wikipedia.org/wiki/Birthday_problem
#
# Usage:
# % ./bday.r <days per year> <num people required with same bday> <num people>
#
# Example: Demonstrating that with 23 people, we have over 50% probability of any two of them sharing a birthday
# % ./bday.r 365 2 23
# 1,0
# 2,0.002739726
# ...
# 23,0.5072972
#
# Dependencies: r
# % brew install r

# Compute the chance that in `n` independent rolls of a `d`-sided die,
# no side appears more than `m` times.
#
tmultinom <- function(n, m, d, count=FALSE) tmultinom.full(n, m, d, count)[n+1]
#
# Compute the chances that in 0, 1, 2, ..., `n` independent rolls of a
# `d`-sided die, no side appears more than `m` times.
#
tmultinom.full <- function(n, m, d, count=FALSE) {
  if (n < 0) return(numeric(0))
  one <- rep(1.0, n+1); names(one) <- 0:n
  if (d <= 0 || m >= n) return(one)

  if(count) log.p <- 0 else log.p <- -log(d)
  f <- function(n, m, d) {                   # The recursive solution
    if (d==1) return(one)                    # Base case
    r <- floor(d/2)
    x <- double(f(n, m, r), m)               # Combine two equal values
    if (2*r < d) x <- combine(x, one, m)     # Treat odd `d`
    return(x)
  }
  one <- c(log.p*(0:m), rep(-Inf, n-m))      # Reduction modulo x^(m+1)
  double <- function(x, m) combine(x, x, m)
  combine <- function(x, y, m) {             # The Binomial Theorem
    z <- sapply(1:length(x), function(n) {   # Need all powers 0..n
      z <- x[1:n] + lchoose(n-1, 1:n-1) + y[n:1]
      z.max <- max(z)
      log(sum(exp(z - z.max), na.rm=TRUE)) + z.max
    })
    return(z)
  }
  x <- exp(f(n, m, d)); names(x) <- 0:n
  return(x)
}

args<-commandArgs(TRUE)
S = strtoi(args[1])
k = strtoi(args[2]) - 1
n = strtoi(args[3])

for (x in 1:n) {
  p_no_shard_is_hit_more_than_k_times <- as.numeric(tmultinom(x,k,S))
  p_any_shard_is_hit_at_least_k_times = 1 - p_no_shard_is_hit_more_than_k_times
  cat(x)
  cat(',')
  cat(p_any_shard_is_hit_at_least_k_times)
  cat('\n')
}
