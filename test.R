library(magrittr)
library(gener)

source('funclass.R')

g = FUNCTION(name = 'g',
             input = list(u = 4, v = 8), 
             rule.value    = function(input, param){input$u^2 + input$u - 2*input$v + 1},
             rule.gradient = function(input, param, wrt){
               switch(wrt,
                      'u'  = {2*input$u + 1},
                      'v'  = {2})
             })


f = FUNCTION(name = 'f',
             input = list(x = g, y = 2), param = list(p1 = 2.0))

f$rule.value = function(input, param){param$p1*exp(input$x) + input$y^2}
f$rule.gradient = function(input, param, wrt){
  switch(wrt,
         'x'  = {param$p1*exp(input$x)},
         'y'  = {2*input$y},
         'p1' = {exp(input$x)})
  
  # wrt %>% lapply(function(w){switch(w,
  #                                   'x'  = {param$p1*exp(input$x)},
  #                                   'y'  = {2*input$y},
  #                                   'p1' = {exp(input$x)})})
  
}


g$get.value()
f$get.value()

f$get.inputs()
f$get.gradient('f.x')
f$get.gradient('x')[['f.x']]*g$get.gradient('u')[['g.u']]
# inputs(f) <- list(x = 12, y = g)
# f['x'] <- 12
# f['y'] <- g

f$reset()
