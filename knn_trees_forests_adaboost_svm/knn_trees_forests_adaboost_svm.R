list.of.packages = c("ggplot2",
                     "xtable",
                     "MASS",
                     "class",
                     "rpart",
                     "rpart.plot",
                     "maboost",
                     "e1071",
                     "randomForest")
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) {
  install.packages(new.packages)
}
for (i in 1:length(list.of.packages)) {
  require(list.of.packages[i], character.only=TRUE)
}

#read in raw data
olive = read.table('olive.txt', header=T, sep="")

index = sort.int(olive$Area, index.return=T)$ix
olive = olive[index,]

#make region and area variables factors
olive$Region = as.factor(olive$Region)
olive$Area = as.factor(olive$Area)

#separate into training and test sets
n_obs = dim(olive)[1]
n_cols = dim(olive)[2]
M = array(0, c(9, 1))
for (i in seq(1, n_obs))
{
  t = olive$Area[i]
  M[t] = M[t]+1
}
L = floor(M*0.9)
M_cumul = c(0, cumsum(M))
select = NULL
for (i in seq(1,9))
{
  a = M_cumul[i]+1
  b = a+L[i]-1
  select = c(select, seq(a, b))
}
train = olive[select,]
test = olive[-select,]
n_train = dim(train)[1]
n_test = dim(test)[1]

n_k = 20
knn_error_rates = matrix(rep(0, n_k*3), nrow=n_k)
for (iii in 1:n_k)
{
  knn_train = knn(train = train[,3:10], 
                  test=train[,3:10],
                  cl=train[,1], k=iii)
  tab = table(train$Region, knn_train)
  knn_error_rates[iii,1] = 1 - sum(diag(tab))/sum(tab)
  
  knn_test = knn(train=train[,3:10], 
                 test=test[,3:10],
                 cl=train[,1],
                 k=iii)
  tab = table(test$Region, knn_test)
  knn_error_rates[iii,2] = 1 - sum(diag(tab))/sum(tab)
  
  knn_cv = knn.cv(train=train[,3:10],
                  cl=train[,1],
                  k=iii)
  tab = table(train$Region, knn_cv)
  knn_error_rates[iii,3] = 1 - sum(diag(tab))/sum(tab)
}

df_temp = data.frame(x=rep(seq(1:n_k), 3),
                     errors=c(knn_error_rates[,1], knn_error_rates[,2], knn_error_rates[,3]),
                     group=c(rep('train', n_k), rep('test', n_k), rep('CV', n_k)))
ggplot(df_temp, aes(x=x, y=errors)) +
  geom_line(aes(color=group)) + 
  xlim(1, n_k) +
  labs(title='kNN Error Rates', x='k', y='error rate') +
  theme_bw()

knn_train = knn(train = train[,3:10], 
                test=train[,3:10],
                cl=train[,1], 
                k=1)
tab = table(train$Region, knn_train)
xtable_temp = xtable(tab)
colnames(xtable_temp) = c("Region 1 Predicted", "Region 2 Predicted", "Region 3 Predicted")
rownames(xtable_temp) = c("Region 1 Actual", "Region 2 Actual", "Region 3 Actual")
align(xtable_temp) = c('l', rep('c', 3))
print(xtable_temp, type='html', sanitize.colnames.function = function(x) gsub('\\.', ' ', x),
      sanitize.rownames.function = function(x) gsub('\\.', ' ', x))

knn_test = knn(train = train[,3:10], 
               test=test[,3:10],
               cl=train[,1], 
               k=1)
tab = table(test$Region, knn_test)
xtable_temp = xtable(tab)
colnames(xtable_temp) = c("Region 1 Predicted", "Region 2 Predicted", "Region 3 Predicted")
rownames(xtable_temp) = c("Region 1 Actual", "Region 2 Actual", "Region 3 Actual")
align(xtable_temp) = c('l', rep('c', 3))
print(xtable_temp, type='html', sanitize.colnames.function = function(x) gsub('\\.', ' ', x),
      sanitize.rownames.function = function(x) gsub('\\.', ' ', x))

region_tree = rpart(Region~., data=train[,-2])
prp(region_tree, type=4, extra=1, clip.right.labs=FALSE)

train_tree_predict = predict(region_tree, train[,3:10], type='class')
tab = table(train$Region, train_tree_predict)
xtable_temp = xtable(tab)
colnames(xtable_temp) = c("Region 1 Predicted", "Region 2 Predicted", "Region 3 Predicted")
rownames(xtable_temp) = c("Region 1 Actual", "Region 2 Actual", "Region 3 Actual")
align(xtable_temp) = c('l', rep('c', 3))
print(xtable_temp, type='html', sanitize.colnames.function = function(x) gsub('\\.', ' ', x),
      sanitize.rownames.function = function(x) gsub('\\.', ' ', x))

test_tree_predict = predict(region_tree, newdata=test[,3:10], type='class')
tab = table(test$Region, test_tree_predict)
xtable_temp = xtable(tab)
colnames(xtable_temp) = c("Region 1 Predicted", "Region 2 Predicted", "Region 3 Predicted")
rownames(xtable_temp) = c("Region 1 Actual", "Region 2 Actual", "Region 3 Actual")
align(xtable_temp) = c('l', rep('c', 3))
print(xtable_temp, type='html', sanitize.colnames.function = function(x) gsub('\\.', ' ', x),
      sanitize.rownames.function = function(x) gsub('\\.', ' ', x))

rf_region = randomForest(Region~.-Area, data=train, mtry=3, ntree=100)

train_forest_predict = predict(rf_region, newdata=train, type='class')
tab = table(train$Region, train_forest_predict)
xtable_temp = xtable(tab)
colnames(xtable_temp) = c("Region 1 Predicted", "Region 2 Predicted", "Region 3 Predicted")
rownames(xtable_temp) = c("Region 1 Actual", "Region 2 Actual", "Region 3 Actual")
align(xtable_temp) = c('l', rep('c', 3))
print(xtable_temp, type='html', sanitize.colnames.function = function(x) gsub('\\.', ' ', x),
      sanitize.rownames.function = function(x) gsub('\\.', ' ', x))

test_forest_predict = predict(rf_region, newdata=test, type='class')
tab = table(test$Region, test_forest_predict)
xtable_temp = xtable(tab)
colnames(xtable_temp) = c("Region 1 Predicted", "Region 2 Predicted", "Region 3 Predicted")
rownames(xtable_temp) = c("Region 1 Actual", "Region 2 Actual", "Region 3 Actual")
align(xtable_temp) = c('l', rep('c', 3))
print(xtable_temp, type='html', sanitize.colnames.function = function(x) gsub('\\.', ' ', x),
      sanitize.rownames.function = function(x) gsub('\\.', ' ', x))

varImpPlot(rf_region, pch=21, bg="blue", main="Random Forest Variable Importance Plot")

ada_region = maboost(Region~.-Area, data=train, iter=100, rpart.control(maxdepth=1))

train_boost_predict = predict(ada_region, newdata=train, type='class')
tab = table(train$Region, train_boost_predict)
xtable_temp = xtable(tab)
colnames(xtable_temp) = c("Region 1 Predicted", "Region 2 Predicted", "Region 3 Predicted")
rownames(xtable_temp) = c("Region 1 Actual", "Region 2 Actual", "Region 3 Actual")
align(xtable_temp) = c('l', rep('c', 3))
print(xtable_temp, type='html', sanitize.colnames.function = function(x) gsub('\\.', ' ', x),
      sanitize.rownames.function = function(x) gsub('\\.', ' ', x))

test_boost_predict = predict(ada_region, newdata=test, type='class')
tab = table(test$Region, test_boost_predict)
xtable_temp = xtable(tab)
colnames(xtable_temp) = c("Region 1 Predicted", "Region 2 Predicted", "Region 3 Predicted")
rownames(xtable_temp) = c("Region 1 Actual", "Region 2 Actual", "Region 3 Actual")
align(xtable_temp) = c('l', rep('c', 3))
print(xtable_temp, type='html', sanitize.colnames.function = function(x) gsub('\\.', ' ', x),
      sanitize.rownames.function = function(x) gsub('\\.', ' ', x))

varplot.maboost(ada_region, pch=21, bg="blue", main="Adaboost Variable Importance Plot")

svm_region = svm(Region~.-Area, data=train, kernel='radial', cost=10000)

train_svm_predict = predict(svm_region, newdata=train, type='class')
tab = table(train$Region, train_svm_predict)
xtable_temp = xtable(tab)
colnames(xtable_temp) = c("Region 1 Predicted", "Region 2 Predicted", "Region 3 Predicted")
rownames(xtable_temp) = c("Region 1 Actual", "Region 2 Actual", "Region 3 Actual")
align(xtable_temp) = c('l', rep('c', 3))
print(xtable_temp, type='html', sanitize.colnames.function = function(x) gsub('\\.', ' ', x),
      sanitize.rownames.function = function(x) gsub('\\.', ' ', x))

test_svm_predict = predict(svm_region, newdata=test, type='class')
tab = table(test$Region, test_svm_predict)
xtable_temp = xtable(tab)
colnames(xtable_temp) = c("Region 1 Predicted", "Region 2 Predicted", "Region 3 Predicted")
rownames(xtable_temp) = c("Region 1 Actual", "Region 2 Actual", "Region 3 Actual")
align(xtable_temp) = c('l', rep('c', 3))
print(xtable_temp, type='html', sanitize.colnames.function = function(x) gsub('\\.', ' ', x),
      sanitize.rownames.function = function(x) gsub('\\.', ' ', x))

