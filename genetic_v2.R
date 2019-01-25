library(magrittr)
library(gener)
library(smbinning)

dataset = read.table('~/Documents/data/uci_repo/emg_data_7Jan19/EMG_data_for_gestures-master/01/1_raw_data_13-12_22.03.16.txt', header = T)
dataset$class %<>% as.integer
dataset$Y0 = (dataset$class == 0) %>% as.integer

sm = SMBINNING()

columns = names(dataset) %>% setdiff(c('time', 'class', 'Y0'))
dataset[, columns] %<>% {.*100000}

# run genetic algorithm for classification:
# convert all numeric columns to categorical by optimal binning:

sm$fit(X = dataset[, columns], y = dataset[, 'Y0'])
bins = sm$predict(dataset)


columns = names(dataset) %>% setdiff(c('time', 'class', 'Y0', columns))

X = dataset[, columns] %>% fastDummies::dummy_cols(columns)

# only keep binaries:
columns = names(X) %>% setdiff(columns)
X = X[, columns]
y = dataset[, 'Y0']

# consider a desired performance metric
correl = function(v1, v2){
  N = length(v2)
  if(is.null(dim(v1)) & inherits(v1, c('logical', 'integer', 'numeric'))){
    a = (v1 %>% xor(v2) %>% sum)/N
  } else if (nrow(v1) == N){
    a = (v1 %>% xor(v2) %>% colSums)/N
  } else {
    stop('Something is wrong!')
  }
  return(a %>% sapply(function(x) max(x, 1-x)))
}

flist  = data.frame(name = columns, father = NA, mother = NA, action = NA, correlation = correl(X[, columns], y) %>% as.numeric %>% abs, safety = 0) %>% column2Rownames('name')

# nf features are born by random parents:
i = 0
while(max(flist$correlation) < 0.9){
  i = i + 1
  flist = createFeatures.logical(flist, 2000)
  flist %<>% evaluateFeatures.logical(X, y, cor_fun = correl)
  
  cat('Iteration: ', i, ': Best Correlation = ', 100*max(flist$correlation), ' nrow(flist) = ', nrow(flist), '\n')
}

