# Faster chi-squared statistics for binary categories

dataset = read.csv('C:/Users/nima_/OneDrive/Documents/data/uci/abalone/abalone.data', header = F)
columns = c('Sex', 'Length', 'Diameter', 'Height', 'wholeWeight', 'shuckledWeight', 'visceraWeight', 'shellWeight')
names(dataset) <- c(columns, 'Age')
#Sex / nominal / -- / M, F, and I (infant) 
#Length / continuous / mm / Longest shell measurement 
#Diameter	/ continuous / mm / perpendicular to length 
#Height / continuous / mm / with meat in shell 
#Whole weight / continuous / grams / whole abalone 
#Shucked weight / continuous	/ grams / weight of meat 
#Viscera weight / continuous / grams / gut weight (after bleeding) 
#Shell weight / continuous / grams / after being dried 
#Rings / integer / -- / +1.5 gives the age in years 
dataset$Y0 = dataset$Age > 10

ord = order(dataset$Length, decreasing = T)
x = dataset$Length[ord]
y = dataset$Y0[ord]
N = length(x)

i = 150
X = x >= x[i]
chisq.test(X, y, correct = F)


binchisq = function(x, y){
  a   = mean(x)
  b   = mean(y)
  c   = mean(x & y)
  den = a*b*(1-a)*(1-b)
  return((c- a*b)^2/den)
} 

N*binchisq(X, y)

I  = 2:(N-1)

system.time({cs1 = I %>% sapply(function(i) N*binchisq(x > x[i], y))})
system.time({cs2 = I %>% sapply(function(i) chisq.test(x > x[i], y, correct = F)$statistic)})

sum((cs1-cs2)^2)

plot(dataset$Length[ord[I]], cs1)
