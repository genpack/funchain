
# test eqSolver1d
f = function(x) x^3 + x^2*log(x) - 165
g = function(x) 3*x^2 + 2*x*log(x) + x
f(5)

eqSolver1d(f,g, 125)


# test findk:
x = rnorm(1000, mean = 2, sd = 2)
y = x^5 + 12 + rnorm(1000, mean = 0, sd = 0.1)

plot(x, y)

model = lm(y ~ x)
k = findk(x, y, model$coefficients[2], model$coefficients[1], k0 = 1)


imp     = Inf
R2.prev = Inf
i       = 1
while((imp > 0.0001) & (i < 1000)){
  model = lm(Y ~ X, data = data.frame(X = x^k, Y = y))
  k     = findk(x, y, model$coefficients[2], model$coefficients[1], k0 = k)
  R2    = sqrt(mean(model$residuals^2))
  cat('i = ', i, ', k = ', k, ', R2' = R2, '\n')
  imp = R2.prev - R2
  R2.prev = R2
  i   = i + 1
}



findk.2(x, y, m = model$coefficients[2], h = model$coefficients[1])
