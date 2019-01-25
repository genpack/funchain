library(magrittr)
library(gener)

dataset = read.csv('~/Documents/data/forex/rba/dataset.csv')
columns = names(dataset) %>% setdiff(c('Date', 'Y', 'Y2'))

# todo: consider a gain for each feature to avoid growing values
flist   = data.frame(name = columns, father = NA, mother = NA, correlation = cor(dataset[, columns], dataset[,'Y']) %>% as.numeric %>% abs, safety = 0) %>% column2Rownames('name')

i = 0
while(max(flist$correlation) < 0.1){
  i = i + 1
  flist = createFeatures.multiplicative(flist, 1000)
  flist %<>% evaluateFeatures.multiplicative(X = dataset[,columns], y = dataset[,'Y'])
  
  cat('Iteration: ', i, ': Best Correlation = ', 100*max(flist$correlation), ' nrow(flist) = ', nrow(flist), '\n')
}



#### make a training set from top features:
source('~/Documents/software/R/packages/maler/R/abstract.R')
source('~/Documents/software/R/packages/maler/R/regressors.R')


ord = order(flist$correlation, decreasing = T)[1:100]
flist$correlation[ord]
train    = NULL
features = rownames(flist)
for(i in ord){
  cat(i, ' ')
  train %<>% cbind(getFeatureValue.multiplicative(flist, features[i], dataset)) 
}
train %<>% as.data.frame
colnames(train) <- paste('F', ncol(train) %>% sequence, sep = '.')
# verify correlations again:
y = dataset[, 'Y']
colnames(train) %>% sapply(function(x) cor(train[, x], y))
model = new('STATS.LM')
model$fit(train, y)

model$get.performance.fit()

source('~/Documents/software/R/packages/maler/R/transformers.R')
# optimal binning?

names(dataset) <- gsub(x = names(dataset), pattern = "[.]", replacement = '_')
columns = names(dataset) %>% setdiff(c('Date', 'Y', 'Y2'))

ob = SMBINNING()
ob$fit(X = dataset[, columns], y = dataset[, 'Y2'])
X2 = ob$predict(dataset)
X3 = X2 %>% fastDummies::dummy_cols(names(X2)) %>% {.[, names(X2)]}

X  = cbind(dataset[, columns], X3)


# scikit.lr?
source('~/Documents/software/R/packages/maler/R/classifiers.R')

lr = SCIKIT.LR()
columns = names(dataset) %>% setdiff(c('Date', 'Y', 'Y2'))

lr$fit(X = dataset[, columns], y = dataset[, 'Y2'])
lr$get.performance.cv(X = dataset[, columns], y = dataset[, 'Y2'])
