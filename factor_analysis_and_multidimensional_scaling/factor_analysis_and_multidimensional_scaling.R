list.of.packages = c("ggplot2",
                     "gridExtra",
                     "xtable",
                     "cluster",
                     "stats")
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) {
  install.packages(new.packages)
}
for (i in 1:length(list.of.packages)) {
  require(list.of.packages[i], character.only=TRUE)
}

wine = read.table('wine.txt', header=TRUE, sep="")
wine_cov = round(var(wine), 2)
wine_cor = round(cor(wine), 2)
df_temp = data.frame(wine_cor)
df_temp[upper.tri(df_temp)] = NA
xtable_temp = xtable(df_temp)
digits(xtable_temp)[2:14] = 2
align(xtable_temp) = c('l', rep('c', 13))
print(xtable_temp, type='html', sanitize.colnames.function = function(x) seq(1:13))

eig_vals = eigen(wine_cor)$values[1:8]
scree_plot = ggplot(data.frame(eig_vals=eig_vals, factors=seq(1:8)), aes(x=factors, y=eig_vals)) +
  geom_point(shape=16, size=3, col="blue") +
  geom_line(col="darkgreen") + 
  labs(x='Factor', y='Eigenvalues', title='Scree Plot') +
  theme_bw() +
  scale_x_discrete(breaks=seq(1:8)) +
  scale_y_continuous(breaks=seq(0,5,0.5))
scree_plot

wine_FA_5 = factanal(factors=5, x=wine)
wine_FA_6 = factanal(factors=6, x=wine)
df_temp = wine_FA_6$loading[1:13, 1:6]
xtable_temp = xtable(df_temp)
digits(xtable_temp)[2:7] = 2
align(xtable_temp) = c('l', rep('c', 6))
print(xtable_temp, type='html', sanitize.colnames.function = function(x) gsub('r', 'r\ ', x))

wine_FA_6 = factanal(factors=6, x=wine, rotation='promax')
df_temp = wine_FA_6$loading[1:13, 1:6]
xtable_temp = xtable(df_temp)
digits(xtable_temp)[2:7] = 2
align(xtable_temp) = c('l', rep('c', 6))
print(xtable_temp, type='html', sanitize.colnames.function = function(x) gsub('r', 'r\ ', x))

df_temp = data.frame(matrix(c(NA, 'x', 'x', NA, NA, NA,
                              NA, NA, NA, NA, 'x', NA,
                              NA, NA, NA, 'x', NA, NA,
                              NA, 'x', NA, 'x', NA, NA,
                              NA, NA, NA, NA, NA, 'x',
                              'x', NA, NA, NA, NA, NA,
                              'x', NA, NA, NA, NA, NA,
                              'x', NA, NA, 'x', NA, NA,
                              'x', NA, NA, NA, NA, NA,
                              NA, 'x', 'x', NA, NA, NA,
                              NA, NA, 'x', NA, 'x', NA,
                              'x', NA, 'x', NA, NA, NA,
                              NA, 'x', NA, NA, NA, NA),
                            nrow=13, ncol=6, byrow=TRUE))
colnames(df_temp) = c('Factor1', 'Factor2', 'Factor3', 'Factor4', 'Factor5', 'Factor6')
rownames(df_temp) = colnames(wine)
xtable_temp = xtable(df_temp)
align(xtable_temp) = c('l', rep('c', 6))
print(xtable_temp, type='html', sanitize.colnames.function = function(x) gsub('r', 'r\ ', x))

df_temp = data.frame(matrix(c(0.26, 0.14, 0.10, 0.10, 0.09, 0.08,
                              0.26, 0.40, 0.50, 0.60, 0.69, 0.77),
                            nrow=2, ncol=6))
colnames(df_temp) = c('Factor1', 'Factor2', 'Factor3', 'Factor4', 'Factor5', 'Factor6')
rownames(df_temp) = c('Proportion of Variation Explained', 'Cumulative Proportion of Variation Explained')
xtable_temp = xtable(df_temp)
align(xtable_temp) = c('l', rep('c', 6))
print(xtable_temp, type='html', sanitize.colnames.function = function(x) gsub('r', 'r\ ', x))

pve = c(0.26, 0.14, 0.10, 0.10, 0.09, 0.08)
df_temp = data.frame(factors=seq(1:6), pve=pve, cpve=cumsum(pve))
plot1 = ggplot(df_temp, aes(x=factors, y=pve)) +
  ylim(0,1) +
  labs(x='Factors', y='Proportion of Variation Explained') +
  geom_point(shape=16, size=3, col="blue") +
  geom_line(col="darkgreen") +
  theme_bw() +
  scale_x_discrete(breaks=seq(1:6))
plot2 = ggplot(df_temp, aes(x=factors, y=cpve)) +
  ylim(0,1) +
  labs(x='Factors', y='Cumulative Proportion of Variation Explained') +
  geom_point(shape=16, size=3, col="blue") +
  geom_line(col="darkgreen") +
  theme_bw() +
  scale_x_discrete(breaks=seq(1:6))
grid.arrange(plot1, plot2, ncol=2)

wine_dist = daisy(wine, stand=TRUE)

wine_mds_1 = cmdscale(wine_dist, k=1)
fitted_dist_1 = daisy(wine_mds_1)
fit_qual_1 = sum((wine_dist - fitted_dist_1)^2)/sum(fitted_dist_1^2)

wine_mds_2 = cmdscale(wine_dist, k=2)
fitted_dist_2 = daisy(wine_mds_2)
fit_qual_2 = sum((wine_dist - fitted_dist_2)^2)/sum(fitted_dist_2^2)

wine_mds_3 = cmdscale(wine_dist, k=3)
fitted_dist_3 = daisy(wine_mds_3)
fit_qual_3 = sum((wine_dist - fitted_dist_3)^2)/sum(fitted_dist_3^2)

wine_mds_4 = cmdscale(wine_dist, k=4)
fitted_dist_4 = daisy(wine_mds_4)
fit_qual_4 = sum((wine_dist - fitted_dist_4)^2)/sum(fitted_dist_4^2)

wine_mds_5 = cmdscale(wine_dist, k=5)
fitted_dist_5 = daisy(wine_mds_5)
fit_qual_5 = sum((wine_dist - fitted_dist_5)^2)/sum(fitted_dist_5^2)

wine_mds_6 = cmdscale(wine_dist, k=6)
fitted_dist_6 = daisy(wine_mds_6)
fit_qual_6 = sum((wine_dist - fitted_dist_6)^2)/sum(fitted_dist_6^2)

fit = c(fit_qual_1, fit_qual_2, fit_qual_3, fit_qual_4, fit_qual_5, fit_qual_6)

qplot(x=seq(1:6), y=fit) + 
  labs(x='Dimensions', y='Fit Quality') +
  geom_point(shape=16, size=3, col="blue") +
  geom_line(col="darkgreen") +
  ylim(0, 1) +
  theme_bw()

qplot(x=wine_mds_2[,1], y=wine_mds_2[,2]) +
  geom_point(col="blue") +
  labs(x='First Dimension', y='Second Dimension') +
  theme_bw()