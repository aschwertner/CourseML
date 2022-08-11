rm(list = ls())
save_figure = FALSE

# Libraries ####################################################################

library(tidyr)
library(tidyverse)
library(dplyr)
library(dtw)
library(cluster)
library(factoextra)

# Data #########################################################################

# Loading data
Drca = readRDS("./dataderived/Drca.RDS")

# Taking a sample with 100 time series
CELLS = unique(Drca$CellID)
set.seed(123)
cells = sample(CELLS, 100)
drca = Drca %>% 
  filter(is.element(CellID, cells))

head(drca)

# Reshaping data to employ clustering algorithms: 'hclust' and 'agnes'
drca_reshaped = spread(drca, key = Month, value = DOAVEG_avg)

# First test using 'hcluster' #################################################

# Computing the dissimilarity matrix
cluster_dist <- dist(drca_reshaped, method = "DTW")

# Hierarchical clustering using complete linkage
hc <- hclust(cluster_dist, method = "complete")

# Cut the hierarchical tree in 4 groups
subgroups <- cutree(hc, k = 4)

# Plot the dendrogram
if(save_figure){
  png(file="./figures/hc_dtw_complete.png", width=1920, height=1080)
}
plot(hc, lwd = 2, hang = -1, main = "Cluster Dendrogram of DOAVEG_avg (Complete, DTW)")
par(lwd=4)
rect.hclust(hc, k = 4, border = 2:5)
if(save_figure){
  dev.off()
}

# Selecting linkage type #######################################################

# Create a vector with the linkage types
linkages <- c( "average", "single", "complete", "ward")
names(linkages) <- c( "average", "single", "complete", "ward")

# Function to compute agglomerative coefficient
ac <- function(x) {
  agnes(drca, method = x)$ac
}

# Agglomerative coefficient
map_dbl(linkages, ac)

# Results (values closer to 1 suggest strong clustering structure):
# average   single    complete  ward 
# 0.9996731 0.9977773 0.9998462 0.9999864 

# Note that Ward linkage give us the best result.

# Hierarchical clustering using Ward linkage
hc2 = hclust(cluster_dist, method = "ward.D2")

# Plot the dendrogram
if(save_figure){
  png(file="./figures/hc_dtw_ward.png", width=1920, height=1080)
}
plot(hc2, lwd = 2, hang = -1, main = "Cluster Dendrogram of DOAVEG_avg (Ward, DTW)")
par(lwd = 4)
rect.hclust(hc2, k = 4, border = 2:5)
if(save_figure){
  dev.off()
}

# Determining Optimal Clusters #################################################

# Plot of the Elbow method
if(save_figure){
  png(file="./figures/hc_elbow_method.png", width=800, height=600)
}
fviz_nbclust(drca, FUN = hcut, method = "wss") + 
  geom_vline(xintercept = 2, linetype = 2)
if(save_figure){
  dev.off()
}
# The largest decrease occurs with k=2 clusters

# Final solution ###############################################################

# Cut the hierarchical tree in 2 groups
subgroups <- cutree(hc2, k = 2)

# Plot the dendrogram
if(save_figure){
  png(file="./figures/hc_solution.png", width=1920, height=1080)
}
plot(hc2, lwd = 2, hang = -1, main = "Cluster Dendrogram of DOAVEG_avg (Ward, DTW, k = 2)")
par(lwd = 4)
rect.hclust(hc2, k = 2, border = 2:3)
if(save_figure){
  dev.off()
}
