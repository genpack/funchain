library(magrittr)
library(gener)

source('funclass.R')


# Linear combination:
# L1: Linear function 1
L1 = FUNCTION(name = 'L1',
             inputs = list(x1 = c(4, 6, 12), x2 = c(8, -1, 3), x3 = c(2,1,0)), params = list(a0 = 0.01, a1 = -0.01, a2 = -0.02, a3 = 0.02), 
             rule.output   = function(inputs, params){params$a0 + params$a1*inputs$x1 + params$a2*inputs$x2 + params$a3*inputs$x3},
             rule.gradient = function(inputs, params, wrt){
               switch(wrt,
                      'x1'  = {params$a1},
                      'x2'  = {params$a2},
                      'x3'  = {params$a3},
                      'a0'  = {1},
                      'a1'  = {inputs$x1},
                      'a2'  = {inputs$x2},
                      'a3'  = {inputs$x3})
             })

Logit1 = FUNCTION(name = 'Logit1',
              inputs = list(x = L1), params = list(b0 = 1, b1 = 1), 
              rule.output    = function(inputs, params){params$b0/(params$b1 + exp(- inputs$x))},
              rule.gradient = function(inputs, params, wrt){
                switch(wrt,
                       'x'   = {params$b0*exp(-inputs$x)/((params$b1 + exp(- inputs$x))^2)},
                       'b0'  = {1/(params$b1 + exp(- inputs$x))},
                       'b1'  = {- params$b0/((params$b1 + exp(- inputs$x))^2)}
                ) 
              })

Logit1$get.output()
L1$get.output()

Logit1$get.dependencies('L1.x1')

Logit1$reset.var('L1.a1')
Logit1$values
L1$values


