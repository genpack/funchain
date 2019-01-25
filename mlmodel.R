



    

# This model, finds all numeric features in X and uses smbinning R package to optimally bin them to categorical columns and returns the table with additional features
# original features are NOT maintained in the output table
# This model is using stupid package smbinning. It only divides to 5 buckets, so it's not really optimal binning but better than nothing!
# Also, make sure there is no '.' character in column labels because the package author has problem with dots!!!!!! 
SMBINNING = setRefClass('SMBINNING', contains = "MODEL",
   methods = list(
     initialize = function(settings = list(), ...){
       callSuper(...)
       # refer to help page for package smbinning (?smbinning::smbinning)
       settings$percentageOfRecords %<>% verify('numeric', domain = c(0, 0.5), default = '0.05')
       settings$suffix              %<>% verify('character', default = 'BIN')
       settings <<- settings
       type     <<- 'Optimal Binner' 
     },
     
     fit = function(X, y){
       # This model currently only works for categorical y
       if(inherits(y, 'character')){y %<>% as.factor %>% as.integer}
       if(inherits(y, c('logical', 'numeric'))){y %<>% as.integer}

       columns = numerics(X)
       if(length(columns) > 0){
         ds = cbind(X[, columns], Y = y)
         for(col in columns){
           res = smbinning::smbinning(ds, y = 'Y', x = col)
           if(!inherits(res, 'character')){
             ns = names(objects$binners)
             res$col_id = length(objects$binners) + 1
             objects$binners <<- c(objects$binners, list(res))
             names(objects$binners) <<- c(ns, col)
             cat('Column ', col, ': Successfully binned to 5 buckets!', '\n')
           } else {
             cat('Column ', col, ': ', res, '\n')
           }
         }
       }
     },
     
     get.features.name = function(){
       names(objects$binners)
     },
     
     get.features.weight = function(){
     },
     
     # predict acts like transformer
     predict = function(X){
       ns = names(objects$binners)
       ds = X[, ns]
       for(ft in ns){
         ds  %<>% smbinning::smbinning.gen(objects$binners[[ft]], chrname = ft %>% paste(settings$suffix, sep = '.'))
       }
       columns = colnames(ds) %>% setdiff(colnames(X))
       return(ds[, columns])
     }
     
   )
)



# A simple logistic regression classifier from scikit python package: 
# It extracts only numeric features, does no dummification for categorical columns. 
SCIKIT.LR = setRefClass('SCIKIT.LR', contains = "MODEL",
     methods = list(
       initialize = function(settings = list()){
         settings$sig_level <<- settings$sig_level %>% verify('numeric', domain = c(0,1), default = '0.1')
         type               <<- 'Logistic Regression' 
         if(!require(reticulate)) stop("Package 'reticulate' is not installed!") 
         if(!is.null(settings$python_address)){
           use_python(settings$python_address)
         }
         module_lm = reticulate::import('sklearn.linear_model')
         objects$model <<- module_lm$LogisticRegression(penalty = 'l1',solver = 'liblinear')
         # pandas = reticulate::import('pandas')
       },
       
       transform = function(X){
         return(X[, numerics(X)])
       },
       
       fit = function(X, y){
         # pandas = reticulate::import('pandas')
         objects$model$fit(transform(X), y)
       },
       
       get.features.name = function(){
         #names(objects$model$coefficients[-1])
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
         objects$model$predict(transform(X))
       },
       
       get.performance.fit = function(){
         if(is.null(objects$model.summary)){objects$model.summary <<- summary(objects$model)}
         return(objects$model.summary$adj.r.squared)
       },
       
       # todo: add k-fold, chronological shuffle, chronological split
       get.performance.cv = function(X, y, ntest = 20, split = 0.3, method = 'shuffle'){
         method = match.arg(method)
         keep   = objects$model
         
         X  = transform(X)
         N  = nrow(X)
         scores = c()

         for (i in sequence(ntest)){
             trindex = N %>% sequence %>% sample(size = floor((1 - split)*N))
             
             X_train = X[trindex, ]
             y_train = y[trindex]
             X_test  = X[- trindex,]
             y_test  = y[- trindex]
             
             objects$model$fit(X_train, y_train)
             scores = c(scores, objects$model$score(X_test, y_test))
         } 
         objects$model <<- keep
         return(scores)
       },
       
       # lazy
       get.parameters = function(){
         list(model = objects$model)
       },
       
       # lazy
       get.predictor = function(){
         function(inputs, params){
           params$model$predict(inputs %>% as.data.frame)
         }
       },
       
       get.predictor.gradient = function(){
         function(inputs, params, wrt){
           params$coeff_[wrt]
         }
       },
       
       get.expert.predictor = function(X, y){
       }
     )
  )


