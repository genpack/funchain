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


funsolve.single = function(f, parameter, epochs = 10, epsilon = 0.0001){
  i    = 2
  fval = f$get.output.agg()
  fpmx = f$get.gradients.agg(wrt = parameter) %>% unlist
  if (is.na(fval) | is.na(fpmx)){stop('Out of domain!')}
  while((i < epochs) & (abs(fval) > epsilon) & (abs(fpmx) > epsilon)){
    x = f$get.param(parameter) %>% unlist
    x = x - fval/fpmx
    f$set.param(x)
    fval = f$get.output.agg()
    fpmx = f$get.gradients.agg(wrt = parameter) %>% unlist
    if (is.na(fval) | is.na(fpmx)){stop('Out of domain!')}
  }
  return(list(parameter = f$get.param(parameter), fval = f$get.output.agg(), gradient = f$get.gradients.agg(wrt = parameter) %>% unlist))
}


funsolve = function(f, parameters, epochs = 10, epsilon = 0.0001){
  i    = 2
  fval = f$get.output.agg()
  fpmx = f$get.gradients.agg(wrt = parameters) %>% unlist
  if (is.na(fval) | is.na(fpmx)){stop('Out of domain!')}
  while((i < epochs) & (abs(fval) > epsilon) & (abs(fpmx) > epsilon)){
    x = f$get.param(parameters) %>% unlist
    x = x - fval*fpmx/sum(fpmx^2)
    f$set.param(x)
    fval = f$get.output.agg()
    fpmx = f$get.gradients.agg(wrt = parameter) %>% unlist
    if (is.na(fval) | is.na(fpmx)){stop('Out of domain!')}
  }
  return(list(parameters = f$get.param(parameters), fval = f$get.output.agg(), gradients = f$get.gradients.agg(wrt = parameters) %>% unlist))
}

minimize = function(f, parameters = f$list.parameter()){
  step = 0.1
  f$get.gradients.agg(wrt = parameters) %>% unlist
  f$set.param(f.p1 = 0)
  f$get.param('f.p1')
  funsolve(f, 'f.p1')
}



