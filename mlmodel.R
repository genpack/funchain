MODEL = setRefClass('MODEL', 
   fields = list(name = "character", type = "character", settings = "list", objects = "list"), 
   methods = list(
     fit                 = function(X, y){objects$model <<- mean(y, na.rm = T)}, 
     reset               = function(){objects <<- list()},
     get.features.name   = function(){character()},
     get.features.weight = function(){},
     predict             = function(){},
     get.performance.fit = function(){},
     get.performance.cv  = function(){},
     get.parameters      = function(){},
     get.predictor       = function(){function(inputs, params){}},
     get.predictor.gradient = function(){function(inputs, params, wrt){}}
   ))

STATS.LM = setRefClass('STATS.LM', contains = "MODEL",
    methods = list(
      fit = function(X, y){
        forml = as.formula('y ~ ' %>% paste(paste(names(X), collapse = ' + ')))
        objects$model <<- lm(forml, data = cbind(X, y))
        singulars = is.na(objects$model$coefficients) %>% which %>% names
        if(length(singulars) > 0){
          X = X[, colnames(X) %>% setdiff(singulars)]
          fit(X, y)
        }
      },

      get.features.name = function(){
        names(objects$model$coefficients[-1])
      },
      
      get.features.weight = function(){
        if(is.null(objects$model.summary)){objects$model.summary <<- summary(objects$model)}
        pv   = objects$model.summary$coefficients[-1, 'Pr(>|t|)']
        keep = (pv < 0.1)
        weights = pv
        weights[!keep] <- 0
        weights[keep]  <- 1.0 - weights[keep]/0.1
        return(weights/sum(weights))
      },
        
      predict = function(X){
        objects$model %>% stats::predict(X)
      },
      
      get.performance.fit = function(X, y){
        if(is.null(objects$model)){fit(X, y)}
        if(is.null(objects$model.summary)){objects$model.summary <<- summary(objects$model)}
        return(objects$model.summary$adj.r.squared)
      },
      
      # todo: add k-fold, chronological shuffle, chronological split
      get.performance.cv = function(X, y, ntest = 20, split = 0.3, method = 'shuffle'){
        keep   = objects$model
        method = match.arg(method)
        N       = X %>% nrow
        acc     = c()
        for(i in sequence(ntest)){
          test    = N %>% sequence %>% sample(split*N, replace = F)
          X_train = X[- test, ]
          X_test  = X[  test, ]
          y_train = y[- test]
          y_test  = y[  test]
          fit(X_train, y_train)
          yht = predict(X_test)
          err = (yht - y_test)^2 %>% sum
          # den = (y_test - mean(y_test))^2 %>% sum
          den = y_test^2 %>% sum
          acc = c(acc, 1.0 - min(err/den, 1.0))
        }
        objects$model <<- keep
        return(acc)
      },
      
      # lazy
      get.parameters = function(){return(list(model = objects$model))},
      
      # lazy
      get.predictor = function(){
        function(inputs, params){
          params$model$predict(inputs %>% as.data.frame)
        }
      },
      
      get.predictor.gradient = function(){
        function(inputs, params, wrt){
          params$coefficient[wrt]
        }
      }
    )
  )

    
