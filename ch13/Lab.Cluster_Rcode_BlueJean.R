###############################################
#
# Example: Cluster Analysis with BlueJean Data 
#
###############################################

## Install packages

install.packages("sas7bdat")      #for reading SAS datasets
install.packages("scatterplot3d") #for plotting
install.packages("rgl")           #for plotting
install.packages("dummies")       #for creating dummay variables


## Reading data

library(sas7bdat)

zdungar = read.sas7bdat("zdungar.sas7bdat")
head(zdungar)
dim(zdungar)

## Creating new variables

zdungar$FA_RATIO = log(zdungar$FASHION/zdungar$ORIGINAL)
zdungar$LE_RATIO = log(zdungar$LEISURE/zdungar$ORIGINAL)
zdungar$ST_RATIO = log(zdungar$STRETCH/zdungar$ORIGINAL)
zdungar$totsales = zdungar$FASHION+zdungar$LEISURE+zdungar$STRETCH


## NOTE: Create noise variables for demonstration !!!

n = nrow(zdungar)
zdungar$x1 = rnorm(n, 0, 1)
zdungar$x2 = factor(sample(c("A","B","C","D"), size=n, replace=TRUE))
head(zdungar)


## Creating dummy variables

library(dummies)

x2.dum = dummy(zdungar$x2)

head(x2.dum)

zdungar = cbind(zdungar, x2.dum)
head(zdungar)

zdungar2 = zdungar[,-c(1:5,11)]
head(zdungar2)


## Standardization

zdungar2 = as.data.frame(scale(zdungar2))
head(zdungar2)

attach(zdungar2)


## 3D plots

library(scatterplot3d)

scatterplot3d(FA_RATIO, LE_RATIO, ST_RATIO)
scatterplot3d(FA_RATIO, LE_RATIO, totsales)


## Rotating plots

library(rgl)

plot3d(FA_RATIO,LE_RATIO, ST_RATIO)
plot3d(FA_RATIO,LE_RATIO, totsales)




#############################
##### Hierarchical clustering

h.clust = hclust(dist(zdungar2), method="complete")
plot(h.clust)

h.clust2 = hclust(dist(zdungar2[1:50,]), method="complete")
plot(h.clust2)


## Cut the dendrograms

h.cut = cutree(h.clust, k=3)
h.cut

h.cut2 = cutree(h.clust, h=7)
h.cut2


## Showing the results

h.seg1 = zdungar2[h.cut==1,]
h.seg2 = zdungar2[h.cut==2,]
h.seg3 = zdungar2[h.cut==3,]

pie(table(h.cut),main="number of observations in segment")
h.mean = rbind(apply(h.seg1,2,mean),apply(h.seg2,2,mean),apply(h.seg3,2,mean))
rownames(h.mean) = c(1,2,3)
h.mean
dist(h.mean, method = "euclidean", diag = TRUE)


## 3D plot
plot3d(FA_RATIO,LE_RATIO,ST_RATIO,col=h.cut)
scatterplot3d(FA_RATIO,LE_RATIO,ST_RATIO,color=h.cut)


## Boxplot
par(mfrow=c(2,2))
boxplot(h.seg1[,1],h.seg2[,1],h.seg3[,1],ylab=names(h.seg1)[1],xlab="segment",col="blue",names=c(1,2,3))
boxplot(h.seg1[,2],h.seg2[,2],h.seg3[,2],ylab=names(h.seg1)[2],xlab="segment",col="blue",names=c(1,2,3))
boxplot(h.seg1[,3],h.seg2[,3],h.seg3[,3],ylab=names(h.seg1)[3],xlab="segment",col="blue",names=c(1,2,3))
boxplot(h.seg1[,4],h.seg2[,4],h.seg3[,4],ylab=names(h.seg1)[4],xlab="segment",col="blue",names=c(1,2,3))





########################
#### K-means clustering

k.clust = kmeans(zdungar2[,1:4], centers=3, nstart=20)
k.clust$cluster
k.clust$tot.withinss


k.clust2 = kmeans(zdungar2[,1:4], centers=3)
k.clust2$tot.withinss


## Showing the results

pie(k.clust$size, main="number of observations in segment")
k.clust$centers
dist(k.clust$centers, method = "euclidean", diag = TRUE)


plot3d(FA_RATIO, LE_RATIO, ST_RATIO, col=k.clust$cluster)
scatterplot3d(FA_RATIO, LE_RATIO, ST_RATIO, color=k.clust$cluster)


## Boxplot

seg1 = zdungar2[k.clust$cluster==1,]
seg2 = zdungar2[k.clust$cluster==2,]
seg3 = zdungar2[k.clust$cluster==3,]

par(mfrow=c(2,2))
boxplot(seg1[,1],seg2[,1],seg3[,1],ylab=names(seg1)[1],xlab="segment",col="blue",names=c(1,2,3))
boxplot(seg1[,2],seg2[,2],seg3[,2],ylab=names(seg1)[2],xlab="segment",col="blue",names=c(1,2,3))
boxplot(seg1[,3],seg2[,3],seg3[,3],ylab=names(seg1)[3],xlab="segment",col="blue",names=c(1,2,3))
boxplot(seg1[,4],seg2[,4],seg3[,4],ylab=names(seg1)[4],xlab="segment",col="blue",names=c(1,2,3))



#END