library(gener)
dataset  = read.csv('~/Documents/data/forex/forexTrain.csv')
features = names(dataset) %>% setdiff(c('Date', 'Y', 'Y2'))
soc      = features %>% as.list
names(soc) <- features

mdl = STATS.LM()
soc = build_expert(soc, name = 'LR002', parents = features, school = mdl, label = 'Y')
