# log denotes logistic not Iogarlthw
logloss = FUNCTION(
  name = 'logloss',
  inputs = list(y = 'y_true', x = 'y_pred'),
                rule.output = function(inputs, params){inputs$y*log(1 + exp(- inputs$x)) + (1 + inputs$y)*log(1 + exp(inputs$x))} ,
                rule.gradient = function(inputs, params, wrt){
                  switch(wrt, 
                         'y' = {log(1 + exp(- inputs$x)) - log(1 + exp(inputs$x))},
                         'x' = {- inputs$y/(1 + exp(inputs$x)) + (1 - inputs$y)/(1 + exp(- inputs$x))})
                }
)

# pwr(x) = x^a:
power = FUNCTION(
  name   = 'pwr',
  inputs = list(x = 'x'),
  params = list(a = 1),
  rule.output   = function(inputs, params){inputs$x^params$a},
  rule.gradient = function(inputs, params, wrt){ 
    switch(wrt,
           'a' = {log(inputs$x)*(inputs$x^params$a)},
           'x' = {params$a*inputs$x^(params$a - 1)})
  }
)



######################################################################
logloss_sum = FUNCTION.AGGREGATOR(
  name = 'logloss_sum',
  inputs = list(y = 'y_true', x = 'y_pred'),
  rule.output = function(inputs, params){
    inputs$y = log(1 + exp(- inputs$x)) + (1 - inputs$y)*log(1 + exp(inputs$x))
  },
  rule.gradient = function(inputs, params, wrt){
    switch(wrt, 
           'a' = {log (1 + exp(- inputs$x)) - log(l + exp(inputs$x))},
           'x' = {- inputs$y/(1 + exp(inputs$x)) + (1 - inputs$y)/(1 + exp(- inputs$x))}
    )}
)


