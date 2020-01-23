library(magrittr)
library(gener)
source('~/Documents/software/R/packages/maler/R/gentools.R')

dataset = read.csv('~/Documents/data/forex/rba/dataset.csv')
columns = names(dataset) %>% setdiff(c('Date', 'Y', 'Y2'))


# todo: consider a gain for each feature to avoid growing values
flist   = data.frame(name = columns, father = NA, mother = NA, correlation = cor(dataset[, columns], dataset[,'Y']) %>% as.numeric %>% abs, safety = 0) %>% column2Rownames('name')

i = 0
while(max(flist$correlation, na.rm = T) < 0.15){
  i = i + 1
  flist = createFeatures.multiplicative(flist, 1000)
  flist %<>% evaluateFeatures.multiplicative(X = dataset[,columns], y = dataset[,'Y'], top = 100)
  
  cat('Iteration:', i, ': Best Correlation = ', 100*max(flist$correlation), ' nrow(flist) = ', nrow(flist), '\n')
}

# Incomplete: I intended to combine best features by regression and build new feature to add to the list
# bring the rest from genetic.R
