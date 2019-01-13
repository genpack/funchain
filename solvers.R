# one dimensional equation solver using simple newtown algorithm:
eqSolver1d = function(fun, grad, x0, maxit = 10, epsilon = 0.0001){
  x    = x0
  i    = 2
  fval = fun(x)
  fpmx = grad(x)
  if (is.na(fval) | is.na(fpmx)){stop('Out of domain!')}
  while((i < maxit) & (abs(fval) > epsilon) & (abs(fpmx) > epsilon)){
    x = x - fval/fpmx
    fval = fun(x)
    fpmx = grad(x)
    if (is.na(fval) | is.na(fpmx)){stop('Out of domain!')}
  }
  return(x)
}


# finds k to minimize ||m*x^k + h - y|| given x,y as vectors and m, h as scalars
findk = function(x, y, m, h, k0 = 1){
  logx  = log(x)
  keep  = !is.na(logx)
  x     = x[keep]
  y     = y[keep]
  logx  = logx[keep]
  log2x = logx^2
  f = function(k) {
    xk = x^k
    sum(logx*xk*(m*xk + h - y))
  }
  g = function(k) {
    xk  = x^k
    x2k = xk^2
    sum(2*m*log2x*x2k + (h - y)*log2x*xk)    
  }
  eqSolver1d(f, g, k0, maxit = 20)
}


findk.2 = function(x, y, m, h){
  logx  = log(x)
  logy  = log((y - h)/m)
  keep  = !is.na(logx) & !is.na(logy)
  x     = x[keep]
  y     = y[keep]
  logx  = logx[keep]
  logy  = logy[keep]
  
  model = lm(logy ~ logx)
  model$coefficients['logx']
}

