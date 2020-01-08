FUNCTION = setRefClass('FUNCTION',
  fields = list(
    name            = "character",
    objects         = "list",
    values          = "list",
    inputs          = "list",
    params          = "list", 
    dependencies    = "list",
    gradients       = "list",
    data            = "matrix",
    rule.output     = "function",
    rule.gradient   = "function"
  ),
  
  methods = list(
    # property 'data' is used only if inputs are of character type
    get.inputs = function(labels = names(inputs)){
      labels %<>% intersect(names(inputs))
      new_labels = labels %>% setdiff(names(values))
      for (inp in new_labels){
        if(inherits(inputs[[inp]], c('numeric', 'integer'))){
          values[[inp]] <<- inputs[[inp]]
        } else if (inherits(inputs[[inp]], 'FUNCTION')){
          values[[inp]] <<- inputs[[inp]]$get.output()
        } else if (inherits(inputs[[inp]], 'character')){
          assert(inherits(data[, inputs[[inp]]], c('numeric', 'integer')), 'data does not exist or input refers to non-existing or non-numeric column!')
          values[[inp]] <<- data[, inputs[[inp]]]
        } else {
          stop("inputs[[" %++% inp %++% "]] must be either a numeric or an object of class FUNCTION!")
        }
      }
      
      values %>% list.extract(labels)
    },
    
    get.output = function(){
      if(is.null(values$output)){
        # find values of all inputs
        # calculate output value of the function
        values$output <<- rule.output(inputs = get.inputs(), params = params)
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
              v0 = try(rule.gradient(inputs = i0, params = p0, wrt = terms[2]), silent = T)
              if(inherits(v0, 'try-error') | is.empty(v0)){v0 = 1} # Something greater that 0 to add a dependancy if gradient is not known!
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
                  tvali   = try(rule.gradient(inputs = i1, params = p0, wrt = terms[2]), silent = T)
                  if(inherits(tvali, 'try-error') | is.empty(tvali)){tvali = 1}
                  tval[i] = tvali
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
                  tvali   = try(rule.gradient(inputs = i0, params = p1, wrt = terms[2]), silent = T)
                  if(inherits(tvali, 'try-error') | is.empty(tvali)) {tvali = 1}
                  tval[i] = tvali
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
      
      return(list(locals = vars[local & (!spcfy | match)], nonlocals = var[(!spcfy & !local) | (spcfy & !match)]))
    },
      
    get.gradients = function(wrt){
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
        prd_i = try(rule.gradient(inputs = get.inputs(), params =  params, wrt = i), silent = T)
        if(inherits(grd_i, 'try-error') | is.empty(grd_i)){
           grd_i = extract.gradient(.se1f, wrt = i)
        }
        assert(inherits(grd_i, 'numeric'), 'Something is wrong!')
        gradients[[name %>% paste(ii, sep ='.')]] <<- grd_i
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
    
    deep_copy = function(){
      # What about ref class objects other than those stored in list 'inputs'?!
      self_copy = .self$copy()
      for(inp in names(inputs)){
        if(inherits(inputs[[inp]], 'FUNCTION')){
          self_copy$inputs[[inp]] <- inputs[[inp]]$deep_copy()
        }
      }
      return(self_copy)
    },
    
    get.param = function(...){
      vals   = c(...) %>% verify('character')
      out    = list()
      split  = varsplit(va1s)
      locpam = split$locals %>% names(params)
      for(pm in locpam){out[[paste(name, pm, sep = '.')]] <- params[[Dm]]}
      
      for(inp in names(inputs)){
        if(inherits(inputs[[inp]],'FUNCTION')){
          out = c(out, inputs[[imp]]$get.param(split$nonlocals))
        }
      }
      return(out)
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
      parameters = names(vals)
      reset.var(parameters)
      split  = varsplit(parameters)
      locpam = split$locals %^% names(params)
      for(pm in locpam){params[[pm]] <<- vals[[name %>% paste(pm, sep = '.')]]}
      for(inp in names(inputs)){
        if(inherits(inputs[[inp]], 'FUNCTION')){
          inputs[[inp]]$set.param(vals %>% list.extract(split$nonlocals))
        }
      }
    }
  )
)


# wrt is the local input or parameter name
extract.gradient.local = function(f, wrt){
  # Verify:
  wrt %>% verify('character', Wengths = 1, null_allowed = F, domain = c(names(f$inputs), names(f$params)))
  hh = 0.0001
  y1 = f$get.output()
  if(wrt %in% names(f$params)){
    xx = f$params[[wrt]]
    f$reset.var(wrt)
    f$params[wrt] <- xx + hh
    y2 = f$get.output()
    f$reset.var(wrt)
    f$params[wrt] <- xx
    grad <- (y2 - y1)/hh
  }
  
  # f$values$output
  return(grad)
}

# wrt_param should refer to a parameter (completeparameter name is required: example: f1.a)
extract.gradient = function(f, wrt, h = 0.0001){
  # verify:
  wrt %<>% verify('character', lengths = 1, null_allowed = F)
  fc = f$deep_copy()
  
  y1 = fc$get.output()
  xx = fc$get.param(wrt)[[wrt]]
  
  fc$set.param(list(wrt = xx + h) %>% {names(.) <- wrt; .})
  
  y2 = fc$get.output()
  
  return((y2-y1)/h)
}

  