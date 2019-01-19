
## ESE: Evolving Society of Experts:

# What is an expert:
# Expert could be eiter an feature of the original dataset (original expert)
# or a predictive model, or even simply a function mapping a subset of other experts' predictions into a new vector of values
# in more general terms, an expert is a function mapping a subset of inputs into one or more outputs. inputs could themselves be outputs of other experts

# prediction of an original expert are the actual feature values

# - Expert Generator generates an expert from a subset of experts in a growing list (society)
# - Each expert can be either an "original expert (feature)" or a "model"
# - An original expert (feature) should be known at the time of predictoins and cannot be computed 
# - Predictions of an expert can be pre-computed and added as a new column to original dataset, this does not make the expert an original expert.
# - Each expert builder(experimenter) takes a random subset of experts and generates a new expert by 
#   either training a ml model on a subset of rows or by just picking a function to map the inputs into an output. 
#   The new born expert is given a score for its performance in CV. 
#   A new born expert, survives only if it's performance is significantly higher than all its parents (inputs). 
#   This score is distributed among all its, weighted by their importance(contribution).
#   inputs and each input in turn(if it is an expert itself), distributes its score among its inputs recursively so that
#   all dependencies receive their share of scores

# society is a growing list of experts
# parents: A character vector of expert names from which the new expert is born
# data: Dataset by which the expert is trained after being born
# school: Type of model used for training the expert
# label: label variable on which the expert is trained
build_expert = function(society, name, parents, school, label, n_sample = NULL){
  # select random subset from the big dataset
  expert = new('FUNCTION', name = name, inputs = society %>% list.extract(parents))
  # congrats: baby is now born! Now its time for the kid to be trained in a school:

  # prepare training material:   
  expert$reset()
  if(!is.null(n_sample)){
    expert$rows = dataset %>% nrow %>% sequence %>% sample(n_sample)
  } else {
    expert$rows = dataset %>% nrow %>% sequence
  }
  X = expert$get.inputs() %>% as.data.frame
  y = dataset[expert$rows, label]
  
  # train the expert and find input weights and performances: 
  school$fit(X, y)
  expert$params       = school$get.parameters() 
  expert$data$weights = school$get.features.weight()    
  expert$data$parents = school$get.features.name()    
  expert$data$performance = school$get.performance.cv(X, y)
  expert$data$goodfit = school$get.performance.fit(X, y)
  
  expert$rule.output = school$get.predictor()

  # if expert's performance is better than all its parents:
  nms = names(society)
  society = c(society, expert)
  names(society) <- c(nms, expert$name)
  return(society)
}

