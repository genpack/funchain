library(magrittr)
library(gener)

source('funclass.R')

g = FUNCTION(name = 'g',
             inputs = list(u = 4, v = 7), 
             rule.output   = function(inputs, params){inputs$u^2 + inputs$u - 2*inputs$v + 1},
             rule.gradient = function(inputs, params, wrt){
               switch(wrt,
                      'u'  = {2*inputs$u + 1},
                      'v'  = {2})
             })


f = FUNCTION(name = 'f',
             inputs = list(x = g, y = 2), params = list(p1 = 2.0))

f$rule.output   = function(inputs, params){params$p1*exp(inputs$x) + inputs$y^2}
f$rule.gradient = function(inputs, params, wrt){
  switch(wrt,
         'x'  = {params$p1*exp(inputs$x)},
         'y'  = {2*inputs$y},
         'p1' = {exp(inputs$x)})
  
  # wrt %>% lapply(function(w){switch(w,
  #                                   'x'  = {params$p1*exp(inputs$x)},
  #                                   'y'  = {2*inputs$y},
  #                                   'p1' = {exp(inputs$x)})})
  
}


g$get.output()
f$get.output()

f$get.inputs()
f$get.gradients('f.x')
f$get.gradients('x')[['f.x']]*g$get.gradients('u')[['g.u']]
# inputs(f) <- list(x = 12, y = g)
# f['x'] <- 12
# f['y'] <- g

f$reset()


f$get.dependencies()
f$get.dependencies('f.p1')



f$get.output()
f$reset.var('f.p1')
f$values$output
g$values$output

f$reset.var('g.u')
f$values$output

f$get.output()
f$reset.var('f.y')
f$values$output
g$values$output