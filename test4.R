# Build a simple neural net with polynomials:
library(magrittr)
library(gener)

sigmoid = function(x) 1.0/(1.0 + exp(-x))
sigradi = function(x) exp(-x)/((1.0 + exp(-x))^2)

x = 0.1*(-10:10)

# Build First Layer:
L1_1 = FUNCTION(name = 'L1_1',
              inputs = list(x1 = x, x2 = sin(x), x3 = log(x + 20)), params = list(a0 = 1, a1 = - 1, a2 = -2, a3 = 2), 
              rule.output   = function(inputs, params){sigmoid(params$a0 + params$a1*inputs$x1 + params$a2*inputs$x1^2 + params$a3*inputs$x1^3)},
              rule.gradient = function(inputs, params, wrt){
                u = sigradi(params$a0 + params$a1*inputs$x1 + params$a2*inputs$x2 + params$a3*inputs$x3)
                switch(wrt,
                       'x1'  = {params$a1*u},
                       'x2'  = {params$a2*u},
                       'x3'  = {params$a3*u},
                       'a0'  = {u},
                       'a1'  = {inputs$x1*u},
                       'a2'  = {inputs$x2*u},
                       'a3'  = {inputs$x3*u})
              })

L1_2      = L1_1$copy()
L1_2$name = 'L1_2'

L1_3      = L1_1$copy()
L1_3$name = 'L1_3'

L1_4      = L1_1$copy()
L1_4$name = 'L1_4'


L2_1        = L1_1$copy()
L2_1$name   = 'L2_1'
L2_1$inputs = list(x1 = L1_1, x2 = L1_2, x3 = L1_3)

L2_2        = L2_1$copy()
L2_2$name   = 'L2_2'

L2_3        = L2_1$copy()
L2_3$name   = 'L2_3'

L3        = L1_1$copy()
L3$name   = 'L3'
L3$inputs = list(x1 = L2_1, x2 = L2_2, x3 = L2_3)


#L3$get.output()
