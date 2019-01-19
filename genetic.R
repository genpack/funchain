library(magrittr)
library(gener)

dataset = read.csv('~/Documents/data/forex/rba/dataset.csv')
columns = names(dataset) %>% setdiff(c('Date', 'Y', 'Y2'))


flist   = data.frame(name = columns, father = NA, mother = NA, correlation = cor(dataset[, columns], dataset[,'Y']) %>% as.numeric %>% abs, safety = 0) %>% column2Rownames('name')

# nf features are born by random parents:
create_features = function(flist, nf, prefix = 'Feat'){
  features = rownames(flist)
  flist %>% rbind(
    data.frame(
      name = prefix %>% paste(nrow(flist) + sequence(nf)),
      father = features %>% sample(nf, replace = T),
      mother = features %>% sample(nf, replace = T),
      correlation = NA,
      safety = 0, stringsAsFactors = F) %>% column2Rownames('name'))
}

# immunes a subset of features to the highest safety level
immune = function(flist, features, level, columns){
  flist[features, 'safety'] = level
  have_parents = which(!(features %in% columns))
  if(length(have_parents) > 0){
    flist %<>% immune(flist[features[have_parents], 'father'], level, columns)
    flist %<>% immune(flist[features[have_parents], 'mother'], level, columns)
  }
  return(flist)
}

get_feature_value = function(flist, name, dataset){
  if(name %in% rownames(flist)){
    if(flist[name, 'father'] %in% names(dataset)) {father = dataset[, flist[name, 'father']]} else {father = get_feature_value(flist, flist[name, 'father'], dataset)}
    if(flist[name, 'mother'] %in% names(dataset)) {mother = dataset[, flist[name, 'mother']]} else {mother = get_feature_value(flist, flist[name, 'mother'], dataset)}
    return(father*mother)
  } else {stop('Feature name is not in the list!')}
}

evaluate = function(flist, dataset, top = 400){
  ns   = rownames(flist)
  keep = is.na(flist$correlation) & (flist$father %in% columns) & (flist$mother %in% columns)
  if(sum(keep) > 0){
    flist$correlation[keep] <- cor(dataset[, flist$father[keep]]*dataset[, flist$mother[keep]], dataset[, 'Y']) %>% as.numeric %>% abs
  }
  keep = is.na(flist$correlation) %>% which

  for(i in keep){
    flist$correlation[i] <- cor(get_feature_value(flist, ns[i], dataset), dataset[, 'Y'])
  }  
  
  high_level = max(flist$safety) + 1
  ord = flist$correlation %>% order(decreasing = T)
  flist %<>% immune(ns[ord[sequence(top)]], level = high_level, columns = colnames(dataset)) 
  
  keep = which(flist$safety == high_level)
  return(flist[keep, ])
}

i = 0
while(max(flist$correlation) < 0.2){
  i = i + 1
  flist = create_features(flist, 10000)
  flist %<>% evaluate(dataset)
  
  cat('Iteration: ', i, ': Best Correlation = ', 100*max(flist$correlation), ' nrow(flist) = ', nrow(flist), '\n')
}

