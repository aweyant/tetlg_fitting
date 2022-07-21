###########################################
## Estimators #############################
###########################################
q_est <- function(len) {
  n_1 <- sum(len == 1)
  n_2 <- sum(len != 1)
  q <- n_1/(n_1 + n_2)
  return(q)
}

p_est <- function(len) {
  j <- length(len[len != 1])
  p <- j/(sum(len) - length(len))
  return(p)
}

b_est <- function(len, tot) {
  num <- mean(len)
  den <- mean(tot)
  b <- num/den
  return(b)
}

###########################################
### Random Sample Functions ###############
###########################################
rhgeom <- function(n, q, p) {
  return((rbinom(n,1,1-q) * (rgeom(n,p)+1)) + 1)
}

rgtetlg <- function(n,q,p,b,rounding = FALSE) {
  event_length = rhgeom(n,q,p)
  increment_totals = matrix(nrow = n, ncol = max(event_length))
  for(i in 1:n) {
    increment_totals[i,1:event_length[i]] = rexp(event_length[i], b)
  }
  event_magnitude = apply(increment_totals, MARGIN = 1, FUN = sum, na.rm = TRUE)
  event_max = apply(increment_totals, MARGIN = 1, FUN = max, na.rm = TRUE)
  if(rounding == TRUE) {
    return(data.frame(event_length, event_magnitude = ceiling(event_magnitude), event_max = ceiling(event_max)))
  }
  else{
    return(data.frame(event_length, event_magnitude, event_max))
  }
}

###########################################
### CDFS ##################################
###########################################
#pgenexp <- function(quantile,
#                    p,q,b,
#                    lower.tail = TRUE) {
#  if(lower.tail == TRUE) {
#    (1 - (1-q)/(1-p)) * (1 - exp(-b*quantile)) + ((1-q)/(1-p))*(1 - exp(-p*b*quantile))
#  }
#  else {
#    1 - ((1 - (1-q)/(1-p)) * (1 - exp(-b*quantile)) + ((1-q)/(1-p))*(1 - exp(-p*b*quantile)))
#  }
#}

#qgenexp <- function(p,
#                    p_geom,q,b) {
#  ifelse(p == 0) {
#    return(0)
#  }
#  
#  B = (1-q)/(1-p_geom)
#  A = 1 - B
#  
#  ((b*(p_geom-1))^(-1)) * log(-(B/A)*(p - A - B))
#} %>% Vectorize()


