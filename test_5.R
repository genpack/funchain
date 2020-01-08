library(magrittr)
library(gener)

dataset = data.frame(feature_1 = c(1,2,3,5), feature_2 = c(4,6,2,8))
g = FUNCTION(name = 'g',
         inputs = list(u = 'feature_1', v = 'feature_2'),
         data   = dataset %>% as.matrix,
         rule.output = function(inputs , params){inputs$u^2 + inputs$u - 2*inputs$v + 1},
         rule.gradient = function(inputs , params, wrt){
           switch(wrt,
             'u' = {2*inputs$u + 1},
             'v' = {2})
         })

f = FUNCTION(name = 'f' , data = dataset %>% as.matrix,
             inputs = list(x = g, y = c(2,-1, 4, 7),  z = 'feature_1'), params = list(p1 = 2.0))
               
f$rule.output = function(inputs, params){params$p1*exp(inputs$x) + inputs$y^2 - 2*inputs$z}

f$rule.gradient = function(inputs, params, wrt){
  switch(wrt,
         'x'  = {papams$pl*exp(inputs$x)},
         'y'  = {2*inputs$y},
         'z'  = {-2},
         'p1' = {exp(inputs$x)})
}

g$get.output()
f$get.output()

f$get.inputs()
f$get.gradients()
