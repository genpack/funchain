# Build a simple neural net:


sigmoid = function(x) 1.0/(1.0 + exp(-x))
sigradi = function(x) exp(-x)/((1.0 + exp(-x))^2)

x = 0.01*(-1000:1000)
plot(x, sigradi(x))

# Build First Layer:
L1.1 = FUNCTION(name = 'L1.1',
              inputs = list(x1 = c(4, 6, 12), x2 = c(8, -1, 3), x3 = c(2,1,0)), params = list(a0 = 0.01, a1 = -0.01, a2 = -0.02, a3 = 0.02), 
              rule.output   = function(inputs, params){sigmoid(logitparams$a0 + params$a1*inputs$x1 + params$a2*inputs$x2 + params$a3*inputs$x3)},
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

L1.2      = L1.1
L1.2$name = 'L1.2'

L1.3      = L1.1
L1.3$name = 'L1.3'

L1.4      = L1.1
L1.4$name = 'L1.4'


L2.1        = L1.1
L2.1$name   = 'L2.1'
L2.1$inputs = list(x1 = L1.1, x2 = L1.2, x3 = L1.3)

L2.2        = L2.1
L2.2$name   = 'L2.2'

L2.3        = L2.1
L2.3$name   = 'L2.3'

L3        = L1.1
L3$name   = 'L3'
L3$inputs = list(x1 = L2.1, x2 = L2.2, x3 = L2.3)
