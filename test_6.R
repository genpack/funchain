library(magrittr)
library(gener)
source('~/Documents/software/R/projects/funchain/funclass.R')
source('~/Documents/software/R/projects/funchain/solvers.R')

dataset = data.frame(feature_1 = c(1,2,3,5), feature_2 = c(4,6,2,8))
g = FUNCTION.AGG(name = 'g',
         inputs = list(u = 'feature_1', v = 'feature_2'),
         params = list(a = 1, b = 1),
         data   = dataset %>% as.matrix,
         rule.output = function(inputs , params){inputs$u^2 + params$a*inputs$u - 2*params$b*inputs$v + 1},
         rule.gradient = function(inputs , params, wrt){
           switch(wrt,
             'a' = {inputs$u},      
             'b' = {-2*inputs$v},      
             'u' = {2*inputs$u + 1},
             'v' = {2})
         })

f = FUNCTION.AGG(name = 'f' , data = dataset %>% as.matrix,
             inputs = list(x = g, y = c(2,-1, 4, 7),  z = 'feature_1'), params = list(p1 = 2.0))
               
f$rule.output = function(inputs, params){params$p1*exp(inputs$x) + inputs$y^2 - 2*inputs$z}

f$rule.gradient = function(inputs, params, wrt){
  switch(wrt,
         'x'  = {params$p1*exp(inputs$x)},
         'y'  = {2*inputs$y},
         'z'  = {-2},
         'p1' = {exp(inputs$x)})
}

g$get.output.agg()
f$get.output.agg()
f$get.param('g.b')
f$get.param('f.p1')

funsolve.single(f, 'g.a')
f$get.output.agg()




extract.gradient.local(f, 'p1')

f$get.inputs()
f$get.gradients.agg(wrt = 'f.p1')
f$get.dependencies('f.x')

f$list.parameters()



