#import Libraries
library(xlsx)
library(factoextra)
library(NbClust)
library(fpc)
library(cluster)

#Load the data set
loadfile <- read.xlsx(file = 'vehicles.xlsx', sheetIndex = 1, header = TRUE)
Dataset <- loadfile[2:20]

#Detect Missing values
sum(is.na(Dataset))
#No missing data

#Detect outliers
boxplot(Dataset[1:18],horizontal = TRUE,las = 1 )

#Outliers founded columns
OutlierList <- c(
  "Skew.maxis",
  "Pr.Axis.Ra",
  "Sc.Var.Maxis",
  "Rad.Ra",
  "Kurt.maxis",
  "Skew.maxis",
  "Max.L.Ra",
  "Sc.Var.maxis"
)

#removing outliers
for (Var in names(Dataset)) {
  if (Var %in% names(Dataset[, OutlierList])) {
    outliers <- boxplot.stats(Dataset[,Var])$out
    Dataset <- subset(Dataset, !Dataset[,Var] %in% outliers)
  }
}

#Check still there is any outlier
boxplot(Dataset[1:18],horizontal = TRUE,las = 1 )

#there is out liers still and redo the process
#new outlier list
OutlierList2 <- c(
  "Skew.Maxis",
  "Sc.Var.maxis"
)

#removing remaining outliers
for (Var in names(Dataset)) {
  if (Var %in% names(Dataset[, OutlierList2])) {
    outliers <- boxplot.stats(Dataset[,Var])$out
    Dataset <- subset(Dataset, !Dataset[,Var] %in% outliers)
  }
}
#Check still there is any outlier
boxplot(Dataset[1:18],horizontal = TRUE,las = 1 )

dataset.cleared <- Dataset[1:18]
label <- Dataset$Class

#Scaling the Dataset
dataset.scaled <- scale(dataset.cleared)


#scaled dataset visualization
boxplot(dataset.scaled)

#Find the optimal number of clusters

#NbClust
set.seed(23)
no_of_Cluster <- NbClust(dataset.scaled,distance = 'euclidean',min.nc = 2, max.nc = 10, method = 'kmeans', index = 'all')
no_of_Cluster1 <- NbClust(dataset.scaled,distance = 'manhattan',min.nc = 2, max.nc = 10, method = 'kmeans', index = 'all')

#Elbow method
fviz_nbclust(dataset.scaled, kmeans, method = 'wss')
#k =3

#silhouette method
fviz_nbclust(dataset.scaled, kmeans, method = 'silhouette')
#k =2

#Gap statistics
fviz_nbclust(dataset.scaled, kmeans, method = 'gap_stat')
#k =3


### Clustering Attempt k=3
cluster_output <- kmeans(dataset.scaled,3 , nstart = 10)
cluster_output

#total WSS
wss <- cluster_output$tot.withinss
wss

#BSS
bss <- cluster_output$betweenss
bss

totss <- cluster_output$totss
totss

#Clusters
table(label,cluster_output$cluster)

#Centers
cluster_output$centers


plotcluster(dataset.scaled,cluster_output$cluster)

#silhouette plot
sil <- silhouette(cluster_output$cluster, dist(dataset.scaled))
fviz_silhouette(sil)
#silhouette average width is 0.29


#PCA
#correlation between variables
cor_val <- cor(dataset.cleared)
cor_val
mean(cor_val)

#perform PCA
pca <- prcomp(dataset.cleared, center = TRUE, scale = TRUE)
summary(pca)

#Eigenvalues
eigenvalues <- pca$sdev^2

#Eigenvectors
eigenvectors <- pca$rotation
eigenvalues
eigenvectors


#cumlative score
cumulative_scores <- cumsum(eigenvalues / sum(eigenvalues) * 100)

vehicles_transformed <- predict(pca, dataset.cleared)

colnames(vehicles_transformed) <- paste0("PC", 1:ncol(vehicles_transformed))
vehicles_transformed

pc_index <- min(which(cumulative_scores > 92))
pcs <- 1:pc_index
pcs

dataset.transformed <- as.data.frame(-pca$x[,1:6])
#NbClust
set.seed(23)
no_of_Cluster2 <- NbClust(dataset.transformed,distance = 'euclidean',min.nc = 2, max.nc = 10, method = 'kmeans', index = 'all')
no_of_Cluster3 <- NbClust(dataset.transformed,distance = 'manhattan',min.nc = 2, max.nc = 10, method = 'kmeans', index = 'all')
#k =2

#Elbow method
fviz_nbclust(dataset.transformed, kmeans, method = 'wss')
#k =2

#silhouette method
fviz_nbclust(dataset.transformed, kmeans, method = 'silhouette')
#k =2

#Gap statistics
fviz_nbclust(dataset.transformed, kmeans, method = 'gap_stat')
#k =3

### Clustering Attempt for new datasetk=2
new_kmeans <- kmeans(dataset.transformed,2,nstart = 10)
new_kmeans


#total WSS
new_wss <- new_kmeans$tot.withinss
new_wss

new_tss <- new_kmeans$totss
new_tss

#BSS
new_bss <- new_kmeans$betweenss
new_bss

#Clusters
table(label,new_kmeans$cluster)

#Centers
new_kmeans$centers

plotcluster(dataset.transformed,new_kmeans$cluster)

#silhouette plot
new_sil <- silhouette(new_kmeans$cluster, dist(dataset.transformed))
fviz_silhouette(new_sil)
#silhouette average width is 0.4

#calinski_harabasz
calinhara(dataset.transformed, new_kmeans$cluster,3)

ch_index <- sapply(2:10, function(k) {
  km <- kmeans(dataset.transformed, k, nstart = 10)
  return(calinhara(dataset.transformed, km$cluster))
})

# Plot the Calinski-Harabasz index
fviz_nbclust(dataset.transformed, kmeans, method = "wss", k.max = 10, nstart =10) +
  geom_vline(xintercept = which.max(ch_index) + 1, linetype = "dashed")
