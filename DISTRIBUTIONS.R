# poisson

# gamma mixture --------------------
dgamma2 = function(x, shape1, scale1, shape2, scale2, p){
  return(
    p * dgamma(x, shape1, scale = scale1) + 
    (1 - p) * dgamma(x, shape2, scale = scale2)
  )
}

pgamma2 = function(x, shape1, scale1, shape2, scale2, p){
  return(
    p * pgamma(x, shape1, scale = scale1) + 
    (1 - p) * pgamma(x, shape2, scale = scale2)
  )
}

rgamma2 = function(n, shape1, scale1, shape2, scale2, p){
  pr = runif(n)

  xout = rep(NA, n)
  for (i in 1:n){
    equation = function(x) return(pgamma2(x, shape1, scale1, shape2, scale2, p) - pr[i])
    xout[i] = uniroot(equation, c(0, 10^3))$root
  }
  return(xout)
}

# nbinom mixture ----------------------------------
dnbinom2 = function(x, shape1, scale1, shape2, scale2, p){
  return(
    p * dnbinom(x, shape1, 1/(1 + scale1)) + 
    (1 - p) * dnbinom(x, shape2, 1/(1 + scale2))
  )
}

pnbinom2 = function(x, shape1, scale1, shape2, scale2, p){
  return(
    p * pnbinom(x, shape1, 1/(1 + scale1)) + 
    (1 - p) * pnbinom(x, shape2, 1/(1 + scale2))
  )
}


rnbinom2 = function(n, shape1, scale1, shape2, scale2, p){
  pr = runif(n)
  xout = rep(NA, n)
  epsilon = 10^-6
  x = 0; cp = dnbinom2(0, shape1, scale1, shape2, scale2, p); i = 1
  while(1 - cp[length(cp)] > epsilon){
    i = i + 1
    x[i] = x[i - 1] + 1
    cp[i] = cp[i - 1] + dnbinom2(x[i], shape1, scale1, shape2, scale2, p)
  }
  for (i in 1:n){
    for (j in 1:length(x)){
      if (pr[i] <= cp[j]){
        xout[i] = x[j]; break
      }
    }
  }

  return(xout)
}


# -- joint distribution of x and lambda ------------
jointf = function(x, lambda, shape1, scale1, shape2, scale2, p){
  return(
    dpois(x, lambda) * 
    dgamma2(lambda, shape1, scale1, shape2, scale2, p)
  )
}

# --- conditional distr for M-H --------------------
dXGivenX. = function(x, x., shape1, scale1, shape2, scale2, p){
  likelh = function(X)
    dpois(x., X) * dnbinom2(X, shape1, scale1, shape2, scale2, p)
  
  return(
    likelh(x) / sum(likelh(0:1000))
  )
}
  
dLamGivenLam. = function(lambda, lambda., shape1, scale1, shape2, scale2, p){
  likelh = function(Lambda)
    dgamma(lambda., Lambda/100, scale = 100) * dgamma2(Lambda, shape1, scale1, shape2, scale2, p)
  return(
    likelh(lambda) / integrate(likelh, 0, Inf)$value
  )
}

# ----- Potential Scale Reduction --------------
R = function(x1, x2, x3, x4){
  n = length(x1); m = 8
  y1 = x1[1:(length(x1)/2)]; y2 = x1[-(1:(length(x1)/2))]
  y3 = x2[1:(length(x2)/2)]; y4 = x2[-(1:(length(x2)/2))]
  y5 = x3[1:(length(x3)/2)]; y6 = x3[-(1:(length(x3)/2))]
  y7 = x4[1:(length(x4)/2)]; y8 = x4[-(1:(length(x4)/2))]
  B = n * var(c(mean(y1), mean(y2), mean(y3), mean(y4), mean(y5), mean(y6), mean(y7), mean(y8)))
  W = mean(c(var(y1), var(y2), var(y3), var(y4), var(y5), var(y6), var(y7), var(y8)))
  return(sqrt(((n - 1)/n * W + 1/n * B)/W))
}
