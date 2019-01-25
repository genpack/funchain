FUNCTION = setRefClass('FUNCTION',
  fields = list(
    name            = "character",
    objects         = "list",
    values          = "list",
    inputs          = "list",
    params          = "list", 
    dependencies    = "list",
    gradients       = "list",
    rule.output     = "function",
    rule.gradient   = "function"
  ),
  
  methods = list(
    # argument 'data' is used only if inputs are of character type
    get.inputs = function(labels = names(inputs), data = NULL){
      labels %<>% intersect(names(inputs))
      new_labels = labels %>% setdiff(names(values))
      for (inp in new_labels){
        if(inherits(inputs[[inp]], 'numeric')){
          values[[inp]] <<- inputs[[inp]]
        } else if (inherits(inputs[[inp]], 'FUNCTION')){
          values[[inp]] <<- inputs[[inp]]$get.output(data = data)
        } else if (inherits(inputs[[inp]], 'character')){
            values[[inp]] <<- data[, inputs[[inp]]]
        } else {
          stop("inputs[[" %++% inp %++% "]] must be either a numeric or an object of class FUNCTION!")
        }
      }
      
      values %>% list.extract(labels)
    },
    
    get.output = function(data = NULL){
      if(is.null(values$output)){
        # find values of all inputs
        # calculate output value of the function
        values$output <<- rule.output(inputs = get.inputs(data = data), params = params)
      }
      return(values$output)
    },
    
    get.dependencies = function(wrt = 'output'){
      pass = F
      if (wrt!= 'output'){
        if(grep(pattern = '.', x = wrt, fixed = T) %>% is.empty) {wrt <- name %>% paste(wrt, sep = '.')}
        terms = wrt %>% strsplit(".", fixed = T) %>% unlist
        pass  = terms[[1]] != name
      }
 
      if (is.null(dependencies[[wrt]])){
        if(pass){
          for(y in names(inputs)){
            dep = get.dependencies(y)
            if(wrt %in% dep){
              if(inherits(inputs[[y]], 'FUNCTION')){dependencies[[wrt]] <<- c(dependencies[[wrt]], inputs[[y]]$get.dependencies())}
            }
          }
        } else {
            i0 = list(); p0 = list()
            for (y in names(inputs)) i0[[y]] = runif(1)
            for (y in names(params)) p0[[y]] = runif(1)
            if(wrt == 'output'){
              v0 = rule.output(inputs = i0, params = p0)
            } else {
              v0 = rule.gradient(inputs = i0, params = p0, wrt = terms[2])
              if(is.empty(v0)){v0 = 0}
            }
            i1 = i0; p1 = p0
            for (y in names(inputs)){
              test = runif(5)
              tval = numeric(5)
              for (i in 1:5){
                i1[[y]] = test[i]
                if (wrt == 'output'){
                  tval[i] = rule.output(inputs = i1, params = p0)
                } else {
                  tval[i] = rule.gradient(inputs = i1, params = p0, wrt = terms[2])
                }
              }
              if(sum(tval != v0) > 0) {
                dependencies[[wrt]] <<- c(dependencies[[wrt]], name %>% paste(y, sep = '.'))
                if(inherits(inputs[[y]], 'FUNCTION')){dependencies[[wrt]] <<- c(dependencies[[wrt]], inputs[[y]]$get.dependencies())}
              }  
              i1[[y]] = i0[[y]]
            }
            
            
            for (y in names(params)){
              test = runif(5)
              tval = numeric(5)
              for (i in 1:5){
                p1[[y]] = test[i]
                if (wrt == 'output'){
                  tval[i] = rule.output(inputs = i0, params = p1)
                } else {
                  tval[i] = rule.gradient(inputs = i0, params = p1, wrt = terms[2])
                }
              }
              if(sum(tval != v0) > 0) {
                dependencies[[wrt]] <<- c(dependencies[[wrt]], name %>% paste(y, sep = '.'))
              }  
              p1[[y]] = p0[[y]]
            }
          }
      }
      return(dependencies[[wrt]])
    },
    
    # Computes gradient of the function with respect to a single input or paramter
    # get.gradient = function(wrt){
    #   terms = wrt %>% strsplit(".", fixed = T)
    #   
    #   vars = terms %>% lapply(function(x){if(length(x) > 1){x[[2]]} else {x[[1]]}}) %>% unlist
    #   funs = terms %>% lapply(function(x){if(length(x) > 1){x[[1]]} else {'@'}}) %>% unlist
    #   funs[vars %in% c(names(inputs), names(params)) & (funs == '@')] = name
    #   
    #   vars[funs == name & (vars %in% c(names(inputs), names(params)))] -> locals
    #   
    #   terms = terms[[1]]
    #   if((length(terms) == 1) & (wrt %in% c(names(inputs), names(params)))){
    #     terms = c(name, terms)
    #     wrt   = terms %>% collapse(sep = ".")
    #   }
    #   if(is.null(gradients[[wrt]])){
    #     if(terms[1] == name & length(terms) > 1){
    #       assert(terms[2] %in% c(names(inputs), names(params)), "Function " %++% terms[1] %++% " has no input or parameter named as " %++% terms[2] )
    #       gradients[[wrt]] <<- rule.gradient(inputs = get.inputs(), params = params, wrt = terms[2])}
    #     else {
    #       get.gradients(names(inputs))
    #       gradients[[wrt]] <<- gradients[[inp]]*inputs[[inp]]$get.gradients(wrt)
    #     }
    #   }
    # },
    # 
    
    is.param = function(var){
      var %in% names(params)
    },
    is.input = function(var){
      var %in% names(inputs)
    },
    
    varsplit = function(var){
      #N  = sequence(length(var))
      #ss = N %in% grep(pattern = '.', x = parameters, fixed = T)
      tbc = sequence(length(var)) %>% setdiff(grep(pattern = '.', x = var, fixed = T))
      var[tbc] <- 'UNK' %>% paste(var[tbc], sep = '.')
      
      terms = var %>% strsplit(".", fixed = T)
      
      vars = terms %>% lapply(function(x){x[[2]]}) %>% unlist
      funs = terms %>% lapply(function(x){x[[1]]}) %>% unlist
      
      # 
      local = vars %in% c(names(inputs), names(params))
      match = funs == name
      spcfy = funs != 'UNK'
      
      error = spcfy & !local & match
      if(sum(error) > 0){
        stop(vars[error] %>% paste('is not a parameter or input of', name, '!') %>% paste(collapse = '\n'))
      }
      
      return(list(locals = vars[locals & (!spcfy | match)], nonlocals = var[(!spcfy & !local) | (spcfy & !match)]))
    },
      
    get.gradients = function(wrt, data = NULL){
      tbc = sequence(length(wrt)) %>% setdiff(grep(pattern = '.', x = wrt, fixed = T))
      wrt[tbc] <- name %>% paste(wrt[tbc], sep = '.')
      newwrt = wrt %>% setdiff(names(gradients))
      
      terms = newwrt %>% strsplit(".", fixed = T)
      
      vars = terms %>% lapply(function(x){x[[2]]}) %>% unlist
      funs = terms %>% lapply(function(x){x[[1]]}) %>% unlist

      
      locals = vars[funs == name]
      valids = locals %^% (names(inputs) %U% names(params))
      locals = name %>% paste(locals, sep = '.')
      
      for(i in valids){
        gradients[[name %>% paste(i, sep = '.')]] <<- rule.gradient(inputs = get.inputs(data = data), params = params, wrt = i)
      }
      
      nonlocals <- newwrt %>% setdiff(locals)
      
      if(length(nonlocals) > 0){
        for(i in nonlocals){gradients[[i]] <<- 0}
        Gri = get.gradients(names(inputs))
        for (j in names(inputs)){
          if(inherits(inputs[[j]], 'FUNCTION')){
            Grj = inputs[[j]]$get.gradients(nonlocals)
            for (i in nonlocals){
              gradients[[i]] <<- gradients[[i]] + Gri[[name %>% paste(j, sep = '.')]]*Grj[[i]]
            }
          }
        }
      }
      gradients %>% list.extract(wrt)
    },
    
    reset = function(){
      values     <<- list()
      gradients  <<- list()

      for(inp in names(inputs)){
        if(inherits(inputs[[inp]], 'FUNCTION')){
          inputs[[inp]]$reset()
        }
      }
    },

    reset.var = function(var){
      tbc = sequence(length(var)) %>% setdiff(grep(pattern = '.', x = var, fixed = T))
      var[tbc] <- name %>% paste(var[tbc], sep = '.')
      deps = get.dependencies()
      if(sum(var %in% deps) > 0){values[['output']] <<- NULL}
      for(inp in names(inputs)){
        inpcmp = name %>% paste(inp, sep = '.')
        if((inpcmp) %in% deps){
          if(inpcmp %in% var){
            values[[inp]]      <<- NULL
            values[['output']] <<- NULL
          }
          
          if(inherits(inputs[[inp]], 'FUNCTION')){
            if(sum(var %in% inputs[[inp]]$get.dependencies()) > 0){
              values[[inp]]      <<- NULL
              values[['output']] <<- NULL
              inputs[[inp]]$reset.var(var)
            }
          }
        }
      }
      
      for(wrt in names(gradients)){
        if(sum(var %in% get.dependencies(wrt)) > 0){
          gradients[[wrt]] <<- NULL
        }
      }
    },
    
    set.param = function(...){
      vals = list(...)
      if(length(vals) == 1 & inherits(vals[[1]], 'list')){vals = vals[[1]]}
      if(length(vals) == 1 & inherits(vals[[1]], 'numeric')){vals = vals[[1]] %>% as.list}
      parameters = names(vals)
      reset.var(parameters)
      split  = varsplit(parameters)
      locpam = split$locals %^% names(params)
      for(pm in locpam){params[pm] <<- vals[[pm]]}
      for(inp in names(inputs)){
        if(inherits(inputs[[inp]], 'FUNCTION')){
          inputs[[inp]]$set.param(split$nonlocals)
        }
      }
    }
  )
)


