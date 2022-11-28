#########################################################
#
# Example: PCA and Cluster Analysis with NCI60 data
#
#########################################################

## Data
     
library(ISLR) #install.packages("ISLR") 
nci.labs=NCI60$labs 
nci.data=NCI60$data

head(nci.data)
dim(nci.data) #The data has 64 rows and 6,830 columns.
nci.labs  #Each cell line is labeled with a cancer type.
table(nci.labs)


## Perform principal components analysis

pr.out = prcomp(nci.data, scale=TRUE)
names(pr.out)
summary(pr.out)


## To assign a color to each of the lines,

Cols=function(vec){
 cols=rainbow(length(unique(vec)))
 return(cols[as.numeric(as.factor(vec))])
}

par(mfrow =c(1,2))
plot(pr.out$x[,1:2],    col=Cols(nci.labs), pch=19, xlab="Z1",ylab="Z2")
plot(pr.out$x[,c(1,3)], col=Cols(nci.labs), pch=19, xlab="Z1",ylab="Z3")


## PVC
summary(pr.out)
par(mfrow=c(1,1))
plot(pr.out) #plot the variance explained by the first few principal components

pve = 100*pr.out$sdev^2/sum(pr.out$sdev^2)
par(mfrow=c(1,2))
plot(pve, type ="o", ylab="PVE", xlab=" Principal Component ", col="blue")
plot(cumsum(pve), type="o", ylab="Cumulative PVE", xlab="Principal Component", col="brown3")
#the first seven principal components explain around 40% of the variance in the data



## Cluster Analysis

sd.data=scale(nci.data) #scaling

par(mfrow=c(1,1))
data.dist=dist(sd.data)
plot(hclust(data.dist), labels=nci.labs, main="Complete Linkage", xlab="", sub="", ylab="")
plot(hclust(data.dist, method="average"), labels=nci.labs, main="Average Linkage", xlab="", sub="", ylab ="")
plot(hclust(data.dist, method="single"), labels =nci.labs,main=" Single Linkage ", xlab="", sub ="", ylab ="")

hc.out = hclust(dist(sd.data))
hc.clusters = cutree(hc.out,4) #cut at 4
table(hc.clusters, nci.labs) #All the leukemia cell lines fall in cluster 3

par(mfrow=c(1,1))
plot(hc.out, labels=nci.labs)
abline(h=139, col="red")

hc.out #Printing the output of hclust gives a useful brief summary of the object



## K-means Clustering

set.seed(2)
km.out = kmeans(sd.data, 4, nstart=20)
km.clusters = km.out$cluster
table(km.clusters, hc.clusters) #K-means clustering vs Hierachical clustering



## Hierarchical clustering on the first few principal component score vectors

hc.out = hclust(dist(pr.out$x[,1:5]))
plot(hc.out, labels=nci.labs, main="Hier. Clust . on First Five Score Vectors")
table(cutree(hc.out,4), nci.labs)



## K-means Clustering on the first few principal component score vectors

set.seed(2)
km.out = kmeans(pr.out$x[,1:5], 4, nstart=20)
km.clusters = km.out$cluster
km.clusters

#END