source("DISTRIBUTIONS.R")
library(plotly)

# hist(rgamma(10000, shape = 1, scale = 1), xlim = c(0,6), breaks = 100)
# hist(rgamma(10000, shape = 7.5, scale = 2/15), xlim = c(0,6), breaks = 100)
# 
# hist(rgamma2(10000, 1, 1, 7.5, 2/15, 0.7), xlim = c(0,6), breaks = 500, probability = TRUE)
# hist(rnbinom2(10000, 1, 1, 7.5, 2/15, 0.7), breaks = 12, probability = TRUE, xlim = c(0,8))
# -------------Gibbs sampling -----------------------------
# shape1 = 1; scale1 = 1; shape2 = 7.5; scale2 = 2/15; p = 0.7
Gibbs = function(shape1 = 1, scale1 = 1, shape2 = 7.5, scale2 = 2/15, p = 0.7)
{ print("start Gibbs")
  # set parameters
  N = 20000
  # values holder
  lambda1 = lambda2 = lambda3 = lambda4 = rep(NA, N)
  x1 = x2 = x3 = x4 = rep(NA, N)
  R.x = R.lambda = rep(NA, N)
  convn = convn.x = convn.lambda = NA
  
  # simulation
  lambda1[1] = 1; x1[1] = 1 # initial values
  lambda2[1] = 10; x2[1] = 1 # initial values
  lambda3[1] = 1; x3[1] = 10 # initial values
  lambda4[1] = 10; x4[1] = 10 # initial values
  
  
  for (i in 2:N){
    # sample x1
    x1[i] = rpois(1, lambda1[i-1])
    # posterior parameters
    shape1_p = shape1 + x1[i]; scale1_p = scale1/(scale1 + 1)
    shape2_p = shape2 + x1[i]; scale2_p = scale2/(scale2 + 1)
    # sample lambda1
    lambda1[i] = rgamma2(1, shape1_p, scale1_p, shape2_p, scale2_p, p)
    
    # sample x2
    x2[i] = rpois(1, lambda2[i-1])
    # posterior parameters
    shape1_p = shape1 + x2[i]; scale1_p = scale1/(scale1 + 1)
    shape2_p = shape2 + x2[i]; scale2_p = scale2/(scale2 + 1)
    # sample lambda1
    lambda2[i] = rgamma2(1, shape1_p, scale1_p, shape2_p, scale2_p, p)
    
    # sample x3
    x3[i] = rpois(1, lambda3[i-1])
    # posterior parameters
    shape1_p = shape1 + x3[i]; scale1_p = scale1/(scale1 + 1)
    shape2_p = shape2 + x3[i]; scale2_p = scale2/(scale2 + 1)
    # sample lambda1
    lambda3[i] = rgamma2(1, shape1_p, scale1_p, shape2_p, scale2_p, p)
    
    # sample x4
    x4[i] = rpois(1, lambda4[i-1])
    # posterior parameters
    shape1_p = shape1 + x4[i]; scale1_p = scale1/(scale1 + 1)
    shape2_p = shape2 + x4[i]; scale2_p = scale2/(scale2 + 1)
    # sample lambda1
    lambda4[i] = rgamma2(1, shape1_p, scale1_p, shape2_p, scale2_p, p)
    
    # calculate R
    R.x[i] = R(x1[1:i],x2[1:i],x3[1:i],x4[1:i])
    R.lambda[i] = R(lambda1[1:i], lambda2[1:i], lambda3[1:i], lambda4[1:i])
      if (i >= 4){
      if (is.na(convn.x) & R.x[i] < 1.05)
        convn.x = i
      if (is.na(convn.lambda) & R.lambda[i] < 1.05)
        convn.lambda = i
    }
  }
  print("done Gibbs")
  return(list(x1 = x1, lambda1 = lambda1,
              x2 = x2, lambda2 = lambda2,
              x3 = x3, lambda3 = lambda3,
              x4 = x4, lambda4 = lambda4,
              R.x = R.x, R.lambda = R.lambda, conv.n = max(convn.x, convn.lambda)))
}

# Metropolis ----------------------------------
MH = function(shape1 = 1, scale1 = 1, shape2 = 7.5, scale2 = 2/15, p = 0.7)
{ print("start MH")
  # set parameters
  N = 20000
  # values holder
  lambda1 = lambda2 = lambda3 = lambda4 = rep(NA, N)
  x1 = x2 = x3 = x4 = rep(NA, N)
  R.x = R.lambda = rep(NA, N)
  convn = convn.x = convn.lambda = NA
  
  # simulation
  lambda1[1] = 1; x1[1] = 1 # initial values
  lambda2[1] = 10; x2[1] = 1 # initial values
  lambda3[1] = 1; x3[1] = 10 # initial values
  lambda4[1] = 10; x4[1] = 10 # initial values
  for (t in 2:N){
    # sequence 1
      # proposal 
      propose = NA
      propose[1] = round(runif(1, 0, 30))
      propose[2] = runif(1, 0, 30)
      # calculate ratio
      alpha = jointf(propose[1], propose[2], shape1, scale1, shape2, scale2, p)/
              jointf(x1[t-1], lambda1[t-1], shape1, scale1, shape2, scale2, p)
      # accept/reject
      u = runif(1)
      if (u <= alpha){
        x1[t] = propose[1]; lambda1[t] = propose[2]
      } else {
        x1[t] = x1[t-1]; lambda1[t] = lambda1[t-1]
      }
      
    # sequence 2
      # proposal 
      propose = NA
      propose[1] = round(runif(1, 0, 30))
      propose[2] = runif(1, 0, 30)
      # calculate ratio
      alpha = jointf(propose[1], propose[2], shape1, scale1, shape2, scale2, p)/
        jointf(x2[t-1], lambda2[t-1], shape1, scale1, shape2, scale2, p)
      # accept/reject
      u = runif(1)
      if (u <= alpha){
        x2[t] = propose[1]; lambda2[t] = propose[2]
      } else {
        x2[t] = x2[t-1]; lambda2[t] = lambda2[t-1]
      }
      
     # sequence 3
      # proposal 
      propose = NA
      propose[1] = round(runif(1, 0, 30))
      propose[2] = runif(1, 0, 30)
      # calculate ratio
      alpha = jointf(propose[1], propose[2], shape1, scale1, shape2, scale2, p)/
        jointf(x3[t-1], lambda3[t-1], shape1, scale1, shape2, scale2, p)
      # accept/reject
      u = runif(1)
      if (u <= alpha){
        x3[t] = propose[1]; lambda3[t] = propose[2]
      } else {
        x3[t] = x3[t-1]; lambda3[t] = lambda3[t-1]
      }
      
    # sequence 4
      # proposal 
      propose = NA
      propose[1] = round(runif(1, 0, 30))
      propose[2] = runif(1, 0, 30)
      # calculate ratio
      alpha = jointf(propose[1], propose[2], shape1, scale1, shape2, scale2, p)/
        jointf(x4[t-1], lambda4[t-1], shape1, scale1, shape2, scale2, p)
      # accept/reject
      u = runif(1)
      if (u <= alpha){
        x4[t] = propose[1]; lambda4[t] = propose[2]
      } else {
        x4[t] = x4[t-1]; lambda4[t] = lambda4[t-1]
      }
      
      # calculate R
      R.x[t] = R(x1[1:t],x2[1:t],x3[1:t],x4[1:t])
      R.lambda[t] = R(lambda1[1:t], lambda2[1:t], lambda3[1:t], lambda4[1:t])
      if (t >= 4){
        if (is.na(convn.x) & R.x[t] < 1.05)
          convn.x = t
        if (is.na(convn.lambda) & R.lambda[t] < 1.05)
          convn.lambda = t
      }
  }
  print("done MH")
  return(list(x1 = x1, lambda1 = lambda1,
              x2 = x2, lambda2 = lambda2,
              x3 = x3, lambda3 = lambda3,
              x4 = x4, lambda4 = lambda4,
              R.x = R.x, R.lambda = R.lambda, conv.n = max(convn.x, convn.lambda)))
  
}
# Metropolis - Hasting -------------------------
# shape1 = 1; scale1 = 1; shape2 = 7.5; scale2 = 2/15; p = 0.7
# N = 5000
# # values holder
# lambda = rep(NA, N)
# x = rep(NA, N)
# alpha_ = decision = rep(NA, N)
# 
# # simulation
# lambda[1] = 10; x[1] = 10 # initial values
# for (t in 2:N){
#   # proposal 
#   propose = NA
#   propose[1] = rpois(1, x[t-1])
#   propose[2] = rgamma(1, lambda[t-1]/100, scale = 100)
#   while (propose[2] < 10^-4) {
#     propose[2] = rgamma(1, lambda[t-1]/100, scale = 100)
#   }
#   
#   # calculate ratio
#   alpha = (jointf(propose[1], propose[2], shape1, scale1, shape2, scale2, p)
#            * dLamGivenLam.(lambda[t-1], propose[2], shape1, scale1, shape2, scale2, p)
#           ) / 
#           (jointf(x[t-1], lambda[t-1], shape1, scale1, shape2, scale2, p)
#            * dXGivenX.(x[t-1], propose[1], shape1, scale1, shape2, scale2, p))
#     
#   alpha_[t] = alpha
#   # accept/reject
#   u = runif(1)
#   if (u <= alpha){
#     x[t] = propose[1]; lambda[t] = propose[2]; decision[t] = 1
#   } else {
#     x[t] = x[t-1]; lambda[t] = lambda[t-1]; decision[t] = 0
#   }
# }
# table(lambda)
# hist(lambda, probability = TRUE)
# hist(x, breaks = 12, xlim = c(0,8), probability = TRUE)
# 
# # true distributions
# # lambda
# library(plotly)
# xseq = seq(0,12,0.1)
# cdf = pgamma2(xseq, shape1, scale1, shape2, scale2, p)
# plot_ly(x = xseq, y = cdf, type = "scatter", mode = "lines", name = "True") -> plot1
# ECDF = ecdf(lambda)
# add_lines(plot1, xseq, ECDF(xseq), line = list(color = 2), name = "Simulated") 
# 
# # x
# xseq = seq(0,12,0.1)
# cdf = pnbinom2(xseq, shape1, scale1, shape2, scale2, p)
# plot_ly(x = xseq, y = cdf, type = "scatter", mode = "lines", name = "True") -> plot1
# ECDF = ecdf(x)
# add_lines(plot1, xseq, ECDF(xseq), lines = list(color = 2), name = "Simulated")
# 
# pmf = dnbinom2(0:12, shape1, scale1, shape2, scale2, p); names(pmf) = 0:12
# plot_ly(x = 0:12, y = pmf, type = "bar", ylim = c(0, 0.5), name = "True") -> plot1
# h = table(x)/sum(table(x))
# add_lines(plot1, 0:12, h, name = "Simulated")
