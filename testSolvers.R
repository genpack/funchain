
# test eqSolver1d
f = function(x) x^3 + x^2*log(x) - 165
g = function(x) 3*x^2 + 2*x*log(x) + x
f(4.9975)

eqSolver1d(f,g, 1)


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

## Feature Builder concept:

# 1- Regular regression
# 2- Decision trees
# 3- Segmented regression for each categorical/binned numeric
# 4- geometric ridge regression: a0 + M*[(x1+a1)^b1]*[(x2+a2)]^b2]*... + e = y
# 5- Regression on non-linear mappings of output (Exponential): |b0 + b1*X1 + b2*x2 + ...| + e = exp(y) (works best if all x and y are positive!)
# 6- Regression on non-linear mappings of output (power): b0 + b1*x1 + b2*x2 + ... + e = y^p (works best if all x and y are positive!)
# 7- Absolute Regression (regression on absolute values): b0 + b1*|x1| + b2*|x2| + ... + e = y
# 8- Exponential Regression (regression on exponents of features): b0 + b1*exp(x1) + b2*exp(x2) + ... + e = y

# For example if:
x = rnorm(1000, mean = 10, sd = 2)
y = log(x) + rnorm(1000, mean = 0, sd = 0.1)
# Null model (without feature):
abs(y - mean(y)) %>% mean
# simple regression:
lm(y ~ x)$residuals %>% abs %>% mean

# Regression on non-linear mappings of output (Exponential):
Y = exp(y)
model = lm(Y ~ x)
model$residuals %>% abs %>% mean
(log(model$fitted.values) - y) %>% abs %>% mean

