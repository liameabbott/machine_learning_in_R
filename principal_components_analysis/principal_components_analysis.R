list.of.packages = c("ggplot2",
                     "gridExtra",
                     "xtable",
                     "devtools",
                     "Rcpp")
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) {
  install.packages(new.packages)
}
for (i in 1:length(list.of.packages)) {
  require(list.of.packages[i], character.only=TRUE)
}
install_github("vqv/ggbiplot")
require(ggbiplot)

wine = read.table('wine.txt', header=T, sep="")
plot1 = ggplot(wine, aes(x=Falavanoids, y=Total.phenols)) + 
  geom_point(col="blue") + 
  labs(x='Flavanoids', y='Total Phenols',title='Total Phenols vs. Flavanoids \n r = 0.8680') +
  theme_bw()
plot2 = ggplot(wine, aes(x=OD280.OD315, y=Total.phenols)) + 
  geom_point(col="blue") + 
  labs(x='OD280/OD315', y='Total Phenols',title='Total Phenols vs. OD280/OD315 \n r = 0.7056') +
  theme_bw()
plot3 = ggplot(wine, aes(x=Proline, y=Alcohol.content)) + 
  geom_point(col="blue") +
  labs(x='ProlineFGH', y='Alcohol', title='Alcohol vs. ProlineFGH \n r = 0.6449') +
  theme_bw()
plot4 = ggplot(wine, aes(x=OD280.OD315, y=Falavanoids)) +
  geom_point(col="blue") +
  labs(x='OD280/OD315',y='Flavanoids',title='Flavanoids vs. OD280/OD315 \n r = 0.7912') +
  theme_bw()
grid.arrange(plot1, plot2, ncol=2)
grid.arrange(plot3, plot4, ncol=2)

df_table = data.frame("Variable" = colnames(wine), "Mean" = apply(wine, 2, mean), "Standard Deviation" = apply(wine, 2, sd))
xtable_temp = xtable(df_table)
digits(xtable_temp)[c(1, 3)] = 2
print(xtable_temp, type='html', include.rownames=FALSE)

pca_out = princomp(wine, cor=TRUE)
eig_vals = pca_out$sdev^2
pve = eig_vals/sum(eig_vals)
df_table = data.frame("Principal Component" = seq(1:13), "Percentage of Variation Explained" = pve*100)
xtable_temp = xtable(df_table)
digits(xtable_temp)[2] = 2
align(xtable_temp) = rep("c", 3)
print(xtable_temp, type='html', include.rownames=FALSE,
      sanitize.colnames.function = function(x) gsub("\\.", " ", x))

scree_plot = ggplot(data.frame(eig_vals = eig_vals, pc = seq(1:13)), aes(x=pc,y=eig_vals)) +
  geom_point(shape=16, size=3, col="blue") + 
  geom_line(col="darkgreen") +
  labs(x='Principal Component', y='Variance of Component', title='Scree Plot') +
  scale_x_continuous(breaks = seq(1, 13, 1)) +
  scale_y_continuous(breaks = seq(0, 5, 0.5)) +
  theme_bw()
scree_plot

df_temp = data.frame(pc=seq(1:13), pve=pve, cpve=cumsum(pve))
plot1 = ggplot(df_temp, aes(x=pc, y=pve)) +
  geom_point(shape=16, size=3, col="blue") + 
  geom_line(col="darkgreen") +
  ylim(0,1) +
  labs(x='Principal Component', y='Proportion of Variation Explained') +
  theme_bw()
plot2 = ggplot(df_temp, aes(x=pc, y=cpve)) + 
  geom_point(shape=16,size=3, col="blue") +
  geom_line(col="darkgreen") +
  ylim(0,1) +
  labs(x='Principal Component', y='Cumulative Proportion of Variation Explained') +
  theme_bw()
grid.arrange(plot1, plot2, ncol=2)

loadings = data.frame(round(pca_out$loadings[,1:5],4))
colnames(loadings) = c("Prin. . 1", "Principal Component 2", 
                       "Principal Component 3", "Principal Component 4",
                       "Principal Component 5")
xtable_loadings = xtable(loadings)
digits(xtable_loadings)[2:6] = 4
align(xtable_loadings) = c('l', rep("c", 5))
print(xtable_loadings, type='html')

qplot(x=pca_out$scores[,1], y=pca_out$scores[,2], data=wine, label=row.names(wine), geom='text') +
  theme_bw() +
  labs(x='Principal Component 1', y='Principal Component 2')

ggbiplot(pca_out, labels=rownames(wine)) + theme_bw()