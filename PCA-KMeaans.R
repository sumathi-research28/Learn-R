
df<-read.csv("wine.csv")

df<-scale(df[,-1])
pca<-princomp(df,scores = TRUE)

summary(pca)
plot(pca)

pca$scores
View(pca)

new_data<-pca$scores[,1:2]
#pca$loadings
loadings(pca)
plot(pca$scores[,1:2],col="Blue",cex = 0.2)
text(pca$scores[,1:2], labels=c(1:25), cex= .7)

install.packages('factoextra')

library(factoextra)
library(cluster)


# Elbow method
pca$scores[,1:3]

###Cluster algorithm building
fviz_nbclust(pca$scores[,1:3], kmeans, method = "wss") +
  labs(subtitle = "Elbow method")
km <- kmeans(pca$scores[,1:3],3) 
km$centers
km$cluster
clust<-data.frame("Wine Dataset"=pca$scores[,1:3],"cluster"=km$cluster)

##Animation
install.packages("animation")
library(animation)
ani.options(interval=1)
km <- kmeans.ani(pca$scores[,1:3], 3)
cl<-data.frame("Wine" =pca$scores[,1:3])

d <- dist(pca$scores[,1:3], method = "euclidean") #Computing the distance natrix
as.matrix(d)[1:6, 1:6]

fit <- hclust(d, method="average") # Building the algorithm # try with 'centroid'
plot(fit) # display dendogram
clusters <- cutree(fit, k=3) # cut tree into3 clusters
table(clusters)
# draw dendogram with red borders around the 3 clusters 
rect.hclust(fit, k=3, border="red")
#Attach the cluster numbers to Uni
Final_output=data.frame('PCA'=pca$scores[,1:3],'Cluster' =clusters)




