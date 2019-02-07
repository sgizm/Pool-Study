######################################################################
# Imports and constants
######################################################################

library(foreign)
library(ggplot2)
library(GGally)
library(data.table)
library(gmodels)
library(Hmisc)
library(corrplot)
library(magrittr)
library(reshape2)
library(scales)
library(readr)
library(car)
library(rgl)
library(nFactors)
library(cluster)
library(pvclust)
library(plyr)
#library(dplyr)

clus_data <- read.csv("clus_data.csv")
clus_data <- data.frame(clus_data)

### CORRELATIONS ###

cols <- c(7:20, 26:35, 37:42) # Selecting the numerical columns
clus_data_selected <- clus_data[, cols]
# and a scaled version of that with NA's removed
clus_data_scaled <- scale(na.omit(clus_data_selected))

## Correlation matrix ##

cor_matrix <- cor(clus_data_selected, use = "pairwise.complete.obs")
print(cor_matrix %>% round(2))
# add order = "hclust" as a parameter below for clustering of correlation coefficients
corrplot.mixed(cor_matrix, lower = "number", upper = "circle", order = "hclust")
# simpler view:
corrplot(cor_matrix, order = "hclust", addrect = 5)

# get the  most significant correlations (p > 0.05): 
correlations <- rcorr(as.matrix(clus_data_scaled))
for (i in 1:30){
  for (j in 1:30){
    if ( !is.na(correlations$P[i,j])){
      if ( correlations$P[i,j] < 0.05 ) {
        print(paste(rownames(correlations$P)[i], "-" , colnames(correlations$P)[j], ": ", correlations$P[i,j]))
      }}}}

# highest correlations in each column:
library(data.table)
setDT(melt(cor_matrix))[Var1 != Var2, .SD[which.max(value)], keyby=Var1]
setDT(melt(cor_matrix))[Var1 != Var2, .SD[which.min(value)], keyby=Var1]

# heatmap
col <- colorRampPalette(c("darkblue", "white", "darkorange"))(20) # get some colors
heatmap(x = cor_matrix, col=col, symm = TRUE)

#alternative views
library("PerformanceAnalytics")
chart.Correlation(cor_matrix, histogram = TRUE, pch = 19)

# Hierarchical clustering of observations with company identifiers
d2 <- dist(clus_data_scaled, method="euclidean")
hcl2 <- hclust(d2, method="ward.D2")
plot(hcl2, cex=.5)
groups2 <- cutree(hcl2, k=3)

# to look at the clusters a bit, e.g.: 
table(groups2)
summary(clus_data$ROLE[groups2 == 1])
rect.hclust(hcl2, k=3, border="red")
# shows each clusters' roles
sapply(unique(groups2),function(g)clus_data$ROLE[groups2== g])


# another heatmap
#heatmap with company identifiers
library(gplots)
# get a color palette equal to the number of clusters
clusterCols <- rainbow(length(unique(groups2)))
# create vector of colors for side bar
myClusterSideBar <- clusterCols[groups2]
# choose a color palette for the heat map
myheatcol <- rev(redgreen(75))
# draw the heat map
heatmap.2(clus_data_scaled, main="Hierarchical Cluster", Rowv=as.dendrogram(hcl2), Colv=NA, dendrogram="row", scale="row", col=myheatcol, density.info="none", trace="none", RowSideColors= myClusterSideBar)


## Principal components analysis
pc <- princomp(cor_matrix, cor=TRUE)
summary(pc)
loadings(pc)
plot(pc, type="lines") # indicates 3 main components
print(pc$scores)
biplot(pc)
# to look at the eigenvalues:
library("factoextra")
get_eigenvalue(pc)

## Factor analysis

# How many factors?
library(nFactors)
ev <- eigen(cor_matrix)
ap <- parallel(subject = nrow(na.omit(clus_data_selected)), var = ncol(na.omit(clus_data_selected)), rep = 100, cent = .05)
nS <- nScree(x = ev$values, aparallel = ap$eigen$qevpea)
plotnScree(nS) 