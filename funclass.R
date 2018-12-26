FUNCTION = setRefClass('FUNCTION',
  fields = list(
    name           =  "character",
    input.values    = "list",
    input           = "list",
    param           = "list", 
    value           = "numeric",
    gradient        = "list",
    rule.value      = "function",
    rule.gradient   = "function"
  ),
  
  methods = list(
    get.inputs = function(input_labels = names(input)){
      input_labels %<>% intersect(names(input))
      new_labels = input_labels %-% names(input.values)
      for (inp in new_labels){
        if(inherits(input[[inp]], 'numeric')){
          input.values[[inp]] <<- input[[inp]]
        } else if (inherits(input[[inp]], 'FUNCTION')){
          input.values[[inp]] <<- input[[inp]]$get.value()
        } else {
          stop("input[[" %++% inp %++% "]] must be either a numeric or an object of class FUNCTION!")
        }
      }
      
      input.values %>% list.extract(input_labels)
    },
    
    get.value = function(){
      if(is.empty(value)){
        # find values of all inputs
        # calculate output value of the function
        value <<- rule.value(input = get.inputs(), param = param)
      }
      return(value)
    },
    
    # Computes gradient of the function with respect to a single input or paramter
    get.gradient.single = function(wrt){
      terms = wrt %>% strsplit(".", fixed = T)
      
      vars = terms %>% lapply(function(x){if(length(x) > 1){x[[2]]} else {x[[1]]}}) %>% unlist
      funs = terms %>% lapply(function(x){if(length(x) > 1){x[[1]]} else {'@'}}) %>% unlist
      funs[vars %in% c(names(input), names(param)) & (funs == '@')] = name
      
      vars[funs == name & (vars %in% c(names(input), names(param)))] -> locals
      
      terms = terms[[1]]
      if((length(terms) == 1) & (wrt %in% c(names(input), names(param)))){
        terms = c(name, terms)
        wrt   = terms %>% collapse(sep = ".")
      }
      if(is.null(gradient[[wrt]])){
        if(terms[1] == name & length(terms) > 1){
          assert(terms[2] %in% c(names(input), names(param)), "Function " %++% terms[1] %++% " has no input or parameter named as " %++% terms[2] )
          gradient[[wrt]] <<- rule.gradient(input = get.input(), param = param, wrt = terms[2])}
        else {
          get.gradient(names(input))
          gradient[[wrt]] <<- gradient[[inp]]*input[[inp]]$get.gradient(wrt)
        }
      }
    },
    
    get.gradient = function(wrt){
      tbc = sequence(length(wrt)) %-% grep(pattern = '.', x = wrt, fixed = T)
      wrt[tbc] <- name %>% paste(wrt[tbc], sep = '.')
      newwrt = wrt %-% names(gradient)
      
      terms = newwrt %>% strsplit(".", fixed = T)
      
      vars = terms %>% lapply(function(x){x[[2]]}) %>% unlist
      funs = terms %>% lapply(function(x){x[[1]]}) %>% unlist

      
      locals = vars[funs == name]
      valids = locals %^% (names(input) %U% names(param))
      locals = name %>% paste(locals, sep = '.')
      
      for(i in valids){
        gradient[[name %>% paste(i, sep = '.')]] <<- rule.gradient(input = get.inputs(), param = param, wrt = i)
      }
      
      nonlocals <- newwrt %-% locals
      
      if(length(nonlocals) > 0){
        for(i in nonlocals){gradient[[i]] <<- 0}
        Gri = get.gradient(names(input))
        for (j in names(input)){
          if(inherits(input[[j]], 'FUNCTION')){
            Grj = input[[j]]$get.gradient(nonlocals)
            for (i in nonlocals){
              gradient[[i]] <<- gradient[[i]] + Gri[[name %>% paste(j, sep = '.')]]*Grj[[i]]
            }
          }
        }
      }
      gradient %>% list.extract(wrt)
    },
    
    reset = function(){
      input.values <<- list()
      gradient     <<- list()
      value        <<- numeric()
      
      for(inp in names(input)){
        if(inherits(input[[inp]], 'FUNCTION')){
          input[[inp]]$reset()
        }
      }
    }
  )
)


