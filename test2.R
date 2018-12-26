library(magrittr)
library(gener)

source('funclass.R')


# Linear combination:
# L1: Linear function 1
L1 = FUNCTION(name = 'L1',
             input = list(x1 = c(4, 6, 12), x2 = c(8, -1, 3), x3 = c(2,1,0)), param = list(a0 = 1, a1 = -1, a2 = -2, a3 = 2), 
             rule.value    = function(input, param){param$a0 + param$a1*input$x1 + param$a2*input$x2 + param$a3*input$x3},
             rule.gradient = function(input, param, wrt){
               switch(wrt,
                      'x1'  = {param$a1},
                      'x2'  = {param$a2},
                      'x3'  = {param$a3},
                      'a0'  = {1},
                      'a1'  = {input$x1},
                      'a2'  = {input$x2},
                      'a3'  = {input$x3})
             })

Logit1 = FUNCTION(name = 'Logit1',
              input = list(x = L1), param = list(b0 = 1, b1 = 1), 
              rule.value    = function(input, param){param$b0/(param$b1 + exp(- input$x))},
              rule.gradient = function(input, param, wrt){
                switch(wrt,
                       'x'   = {param$b0*exp(-input$x)/((param$b1 + exp(- input$x))^2)},
                       'b0'  = {1/(param$b1 + exp(- input$x))},
                       'b1'  = {- param$b0/((param$b1 + exp(- input$x))^2)}
                ) 
              })

for (i in 1:1000){
  L1$param$a0 = i
  Logit1$reset()
  Logit1$get.value()
  Logit1$get.gradient(wrt = 'L1.a2')[[1]] %>% sum %>% cat('\n')
}



