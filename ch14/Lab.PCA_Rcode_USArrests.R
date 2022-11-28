################################################################
#
# Example: Principal Cmponents Aalysis(PCA) with USArrests data
#
################################################################

## Data
     
library(ISLR) #install.packages("ISLR") 
states = row.names(USArrests)
states
head(USArrests)

apply(USArrests, 2, mean) #compute means for each row
apply(USArrests, 2, var) #compute var for each row


## Perform principal components analysis

pr.out = prcomp(USArrests, scale=TRUE)

names(pr.out)
pr.out$center #means used for scaling
pr.out$scale  #standard deviations used for scaling

pr.out$rotation #principal component loadings


## Plotting the first two principal components
biplot(pr.out, scale=0) 


## PVE and cumulative PVE explained by each component

pr.out$sdev #the standard deviation of each principal component
pr.var = pr.out$sdev^2 #The variance explained by each principal component
pr.var
 
pve=pr.var/sum(pr.var) #the proportion of variance explained by each principal component
pve

par(mfrow=c(2,2))
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1), type='b')
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')


#END