list.of.packages = c("ggplot2",
                     "gridExtra",
                     "xtable",
                     "nnet",
                     "MASS",
                     "scales",
                     "GGally")
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

#Region linear discriminant analysis
lda_region_fit = lda(Region~Oleic+Eicosenoic, data=train)
region_train_lda_preds = predict(lda_region_fit)$class
region_train_actual = train[,1]
region_test_lda_preds = predict(lda_region_fit, newdata=test)$class
region_test_actual = test[,1]

#Region quadratic discriminant analysis
qda_region_fit = qda(Region~Oleic+Eicosenoic, data=train)
region_train_qda_preds = predict(qda_region_fit)$class
region_test_qda_preds = predict(qda_region_fit, newdata=test)$class

#Region multivariate logisitc regression analysis
log_region_fit = multinom(Region~Oleic+Eicosenoic, data=train)
region_train_log_preds = predict(log_region_fit, newdata=train, "class")
region_test_log_preds = predict(log_region_fit, newdata=test, "class")

#Area linear discriminant analysis
lda_area_fit = lda(Area~.-Region, data=train)
area_train_lda_preds = predict(lda_area_fit)$class
area_train_actual = train[,2]
area_test_lda_preds = predict(lda_area_fit, newdata=test)$class
area_test_actual = test[,2]

#Area quadratic discriminant analysis
qda_area_fit = qda(Area~.-Region, data=train)
area_train_qda_preds = predict(qda_area_fit)$class
area_test_qda_preds = predict(qda_area_fit, newdata=test)$class

#Area multivariate logistic regression
log_area_fit = multinom(Area~.-Region, data=train)
area_train_log_preds = predict(log_area_fit, newdata=train, "class")
area_test_log_preds = predict(log_area_fit, newdata=test, "class")

region_lda_train_error = length(region_train_actual[region_train_actual != region_train_lda_preds])/n_train
region_lda_test_error = length(region_test_actual[region_test_actual != region_test_lda_preds])/n_test

region_qda_train_error = length(region_train_actual[region_train_actual != region_train_qda_preds])/n_train
region_qda_test_error = length(region_test_actual[region_test_actual != region_test_qda_preds])/n_test

region_log_train_error = length(region_train_actual[region_train_actual != region_train_log_preds])/n_train
region_log_test_error = length(region_test_actual[region_test_actual != region_test_log_preds])/n_test

area_lda_train_error = length(area_train_actual[area_train_actual != area_train_lda_preds])/n_train
area_lda_test_error = length(area_test_actual[area_test_actual != area_test_lda_preds])/n_test

area_qda_train_error = length(area_train_actual[area_train_actual != area_train_qda_preds])/n_train
area_qda_test_error = length(area_test_actual[area_test_actual != area_test_qda_preds])/n_test

area_log_train_error = length(area_train_actual[area_train_actual != area_train_log_preds])/n_train
area_log_test_error = length(area_test_actual[area_test_actual != area_test_log_preds])/n_test

df_temp = data.frame("Region Training"=percent(round(c(region_lda_train_error, region_qda_train_error,region_log_train_error), 4)),
                     "Region Test" = percent(round(c(region_lda_test_error, region_qda_test_error, region_log_test_error), 4)),
                     "Area Training" = percent(round(c(area_lda_train_error, area_qda_train_error, area_log_train_error), 4)),
                     "Area Test" = percent(round(c(area_lda_test_error, area_qda_test_error, area_log_test_error), 4)))
rownames(df_temp) = c("Linear Discriminant Analysis", "Quadratic Discriminant Analysis", "Logistic Regression")

xtable_temp = xtable(df_temp)
digits(xtable_temp)[2:5] = 3
align(xtable_temp) = c('l', rep('c', 4))
print(xtable_temp, type='html', sanitize.colnames.function = function(x) gsub('\\.', ' ', x))

ggpairs(train, 
        columns=3:n_cols, 
        diag='blank', 
        colour='Region', 
        axisLabels='internal',
        lower=list(continuous='points'), 
        upper=list(continuous='blank')) + theme_bw()
ggparcoord(train, scale='globalminmax', columns=3:n_cols, groupColumn=1) +
  xlab('acid') +
  theme_bw()

plot1 = ggparcoord(train, scale='globalminmax', columns=c(3:7), groupColumn=1) +
  xlab('acid') +
  theme_bw()
plot2 = ggparcoord(train, scale='globalminmax', columns=c(4:5, 8:10), groupColumn=1) +
  xlab('acid') +
  theme_bw()
grid.arrange(plot1, plot2, nrow=1, ncol=2)

prior_probs = c(round(length(region_train_actual[region_train_actual==1])/n_train, 2),
                round(length(region_train_actual[region_train_actual==2])/n_train, 2),
                round(length(region_train_actual[region_train_actual==3])/n_train, 2))
oleic_mean = c(round(mean(train$Oleic[train$Region==1]), 2),
               round(mean(train$Oleic[train$Region==2]), 2),
               round(mean(train$Oleic[train$Region==3]), 2))
eicosenoic_mean = c(round(mean(train$Eicosenoic[train$Region==1]), 2),
                    round(mean(train$Eicosenoic[train$Region==2]), 2),
                    round(mean(train$Eicosenoic[train$Region==3]), 2))
df_temp = t(data.frame("Prior Probability" = prior_probs, 
                       "Oleic Mean" = oleic_mean, 
                       "Eicosenoic Mean" = eicosenoic_mean))
colnames(df_temp) = c("Region 1", "Region 2", "Region 3")
xtable_temp = xtable(df_temp)
align(xtable_temp) = c('l', rep('r', 3))
print(xtable_temp, type='html', sanitize.colnames.function = function(x) gsub('\\.', ' ', x),
      sanitize.rownames.function = function(x) gsub('\\.', ' ', x))

xtable_temp = xtable(table(region_train_lda_preds, region_train_actual))
colnames(xtable_temp) = c("Region 1 Actual", "Region 2 Actual", "Region 3 Actual")
rownames(xtable_temp) = c("Region 1 Predicted", "Region 2 Predicted", "Region 3 Predicted")
align(xtable_temp) = c('l', rep('c', 3))
print(xtable_temp, type='html', sanitize.colnames.function = function(x) gsub('\\.', ' ', x),
      sanitize.rownames.function = function(x) gsub('\\.', ' ', x))

xtable_temp = xtable(table(region_test_lda_preds, region_test_actual))
colnames(xtable_temp) = c("Region 1 Actual", "Region 2 Actual", "Region 3 Actual")
rownames(xtable_temp) = c("Region 1 Predicted", "Region 2 Predicted", "Region 3 Predicted")
align(xtable_temp) = c('l', rep('c', 3))
print(xtable_temp, type='html', sanitize.colnames.function = function(x) gsub('\\.', ' ', x),
      sanitize.rownames.function = function(x) gsub('\\.', ' ', x))

LD_1_train = predict(lda_region_fit)$x[,1]
LD_2_train = predict(lda_region_fit)$x[,2]
plot1 = ggplot(data=train, aes(x=LD_1_train, y=LD_2_train, color=Region)) +
  geom_point(size=2) +
  labs(x='LD 1', y='LD 2', title='LDA Region - Training') +
  theme_bw()

LD_1_test = predict(lda_region_fit, newdata=test)$x[,1]
LD_2_test = predict(lda_region_fit, newdata=test)$x[,2]
plot2 = ggplot(data=test, aes(x=LD_1_test, y=LD_2_test, color=Region)) +
  geom_point(size=2) +
  labs(x='LD 1', y='LD 2', title='LDA Region - Test') +
  theme_bw()
grid.arrange(plot1, plot2, nrow=1, ncol=2)

xtable_temp = xtable(table(region_train_qda_preds, region_train_actual))
colnames(xtable_temp) = c("Region 1 Actual", "Region 2 Actual", "Region 3 Actual")
rownames(xtable_temp) = c("Region 1 Predicted", "Region 2 Predicted", "Region 3 Predicted")
align(xtable_temp) = c('l', rep('c', 3))
print(xtable_temp, type='html', sanitize.colnames.function = function(x) gsub('\\.', ' ', x),
      sanitize.rownames.function = function(x) gsub('\\.', ' ', x))

xtable_temp = xtable(table(region_test_qda_preds, region_test_actual))
colnames(xtable_temp) = c("Region 1 Actual", "Region 2 Actual", "Region 3 Actual")
rownames(xtable_temp) = c("Region 1 Predicted", "Region 2 Predicted", "Region 3 Predicted")
align(xtable_temp) = c('l', rep('c', 3))
print(xtable_temp, type='html', sanitize.colnames.function = function(x) gsub('\\.', ' ', x),
      sanitize.rownames.function = function(x) gsub('\\.', ' ', x))

xtable_temp = xtable(table(region_train_log_preds, region_train_actual))
colnames(xtable_temp) = c("Region 1 Actual", "Region 2 Actual", "Region 3 Actual")
rownames(xtable_temp) = c("Region 1 Predicted", "Region 2 Predicted", "Region 3 Predicted")
align(xtable_temp) = c('l', rep('c', 3))
print(xtable_temp, type='html', sanitize.colnames.function = function(x) gsub('\\.', ' ', x),
      sanitize.rownames.function = function(x) gsub('\\.', ' ', x))

xtable_temp = xtable(table(region_test_log_preds, region_test_actual))
colnames(xtable_temp) = c("Region 1 Actual", "Region 2 Actual", "Region 3 Actual")
rownames(xtable_temp) = c("Region 1 Predicted", "Region 2 Predicted", "Region 3 Predicted")
align(xtable_temp) = c('l', rep('c', 3))
print(xtable_temp, type='html', sanitize.colnames.function = function(x) gsub('\\.', ' ', x),
      sanitize.rownames.function = function(x) gsub('\\.', ' ', x))

ggpairs(train,
        columns=3:n_cols,
        diag='blank',
        color='Area',
        axisLabels='internal',
        lower=list(continuous='points'),
        upper=list(continuous='blank')) + theme_bw()
ggparcoord(train,
           scale='globalminmax',
           columns=3:n_cols,
           groupColumn=2) + xlab('acid') + theme_bw()

plot1 = ggparcoord(train,
                   scale='globalminmax',
                   columns=3:n_cols,
                   groupColumn=2) + xlab('acid') + theme_bw()
plot2 = ggparcoord(train,
                   scale='globalminmax',
                   columns=c(4:5, 8:10),
                   groupColumn=2) + xlab('acid') + theme_bw()
grid.arrange(plot1, plot2, nrow=1, ncol=2)

xtable_temp = xtable(table(area_train_lda_preds, area_train_actual))
colnames(xtable_temp) = c('Area 1', 'Area 2', 'Area 3', 'Area 4', 'Area 5', 'Area 6', 'Area 7', 'Area 8', 'Area 9')
rownames(xtable_temp) = c('Area 1', 'Area 2', 'Area 3', 'Area 4', 'Area 5', 'Area 6', 'Area 7', 'Area 8', 'Area 9')
align(xtable_temp) = c('l', rep('c', 9))
print(xtable_temp, type='html', sanitize.colnames.function = function(x) gsub('\\.', ' ', x),
      sanitize.rownames.function = function(x) gsub('\\.', ' ', x))

xtable_temp = xtable(table(area_test_lda_preds, area_test_actual))
colnames(xtable_temp) = c('Area 1', 'Area 2', 'Area 3', 'Area 4', 'Area 5', 'Area 6', 'Area 7', 'Area 8', 'Area 9')
rownames(xtable_temp) = c('Area 1', 'Area 2', 'Area 3', 'Area 4', 'Area 5', 'Area 6', 'Area 7', 'Area 8', 'Area 9')
align(xtable_temp) = c('l', rep('c', 9))
print(xtable_temp, type='html', sanitize.colnames.function = function(x) gsub('\\.', ' ', x),
      sanitize.rownames.function = function(x) gsub('\\.', ' ', x))

LD_1_train = predict(lda_area_fit)$x[,1]
LD_2_train = predict(lda_area_fit)$x[,2]
plot1 = ggplot(data=train, aes(x=LD_1_train, y=LD_2_train, color=Area)) +
  geom_point(size=2) +
  labs(x='LD 1', y='LD 2', title='LDA Area = Training') +
  theme_bw()
LD_1_test = predict(lda_area_fit, newdata=test)$x[,1]
LD_2_test = predict(lda_area_fit, newdata=test)$x[,2]
plot2 = ggplot(data=test, aes(x=LD_1_test, y=LD_2_test, color=Area)) +
  geom_point(size=2) +
  labs(x='LD 1', y='LD 2', title='LDA Area = Test') +
  theme_bw()
grid.arrange(plot1, plot2, nrow=1, ncol=2)

xtable_temp = xtable(table(area_train_qda_preds, area_train_actual))
colnames(xtable_temp) = c('Area 1', 'Area 2', 'Area 3', 'Area 4', 'Area 5', 'Area 6', 'Area 7', 'Area 8', 'Area 9')
rownames(xtable_temp) = c('Area 1', 'Area 2', 'Area 3', 'Area 4', 'Area 5', 'Area 6', 'Area 7', 'Area 8', 'Area 9')
align(xtable_temp) = c('l', rep('c', 9))
print(xtable_temp, type='html', sanitize.colnames.function = function(x) gsub('\\.', ' ', x),
      sanitize.rownames.function = function(x) gsub('\\.', ' ', x))

xtable_temp = xtable(table(area_test_qda_preds, area_test_actual))
colnames(xtable_temp) = c('Area 1', 'Area 2', 'Area 3', 'Area 4', 'Area 5', 'Area 6', 'Area 7', 'Area 8', 'Area 9')
rownames(xtable_temp) = c('Area 1', 'Area 2', 'Area 3', 'Area 4', 'Area 5', 'Area 6', 'Area 7', 'Area 8', 'Area 9')
align(xtable_temp) = c('l', rep('c', 9))
print(xtable_temp, type='html', sanitize.colnames.function = function(x) gsub('\\.', ' ', x),
      sanitize.rownames.function = function(x) gsub('\\.', ' ', x))

xtable_temp = xtable(table(area_train_log_preds, area_train_actual))
colnames(xtable_temp) = c('Area 1', 'Area 2', 'Area 3', 'Area 4', 'Area 5', 'Area 6', 'Area 7', 'Area 8', 'Area 9')
rownames(xtable_temp) = c('Area 1', 'Area 2', 'Area 3', 'Area 4', 'Area 5', 'Area 6', 'Area 7', 'Area 8', 'Area 9')
align(xtable_temp) = c('l', rep('c', 9))
print(xtable_temp, type='html', sanitize.colnames.function = function(x) gsub('\\.', ' ', x),
      sanitize.rownames.function = function(x) gsub('\\.', ' ', x))

xtable_temp = xtable(table(area_test_log_preds, area_test_actual))
colnames(xtable_temp) = c('Area 1', 'Area 2', 'Area 3', 'Area 4', 'Area 5', 'Area 6', 'Area 7', 'Area 8', 'Area 9')
rownames(xtable_temp) = c('Area 1', 'Area 2', 'Area 3', 'Area 4', 'Area 5', 'Area 6', 'Area 7', 'Area 8', 'Area 9')
align(xtable_temp) = c('l', rep('c', 9))
print(xtable_temp, type='html', sanitize.colnames.function = function(x) gsub('\\.', ' ', x),
      sanitize.rownames.function = function(x) gsub('\\.', ' ', x))

