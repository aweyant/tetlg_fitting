library(twosamples)

exponential_dts <- function(v) {
  dts_test(v, rexp(n = length(v), rate = 1/mean(v)))[2]
}
