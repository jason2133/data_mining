###############################################
#                        
# Lab: Decision Trees with German Credit Data 
#
###############################################

## Calling packages

library(rpart) #install.packages("rpart")


## Data reading

german = read.table("germandata.txt",header=T) 
german$y = ifelse(german$y=="good", 1, 0)
german$y = factor(german$y)

sum(is.na(german))

#german = na.omit(german) #can remove the cases with missing values if any


## Growing a tree

set.seed(1)
fit = rpart(y ~., data=german, method="class") #cp=0.01 (default), 
#NOTE: parms=list(split='gini') is the default, parms=list(split='information') is optional

fit
summary(fit)
plot(fit);  text(fit)


fit2 = rpart(y ~., data=german, method="class", 
             control = rpart.control(xval=10, cp=0)) #cp=0 (maximal tree)
fit2
summary(fit2)
plot(fit2);  text(fit2)


## Pruning the tree and selecting the tree with min error

tmp = printcp(fit2)
k = which.min(tmp[,"xerror"])
cp.tmp = tmp[k,"CP"]
fit.pruned = prune(fit2, cp=cp.tmp)
plot(fit.pruned, margin = 0.1);text(fit.pruned, use.n=TRUE)


## Alternatively, Pruning the tree and selecting the smallest tree within min error + 1 se)

tmp = printcp(fit2)
k = which.min(tmp[,"xerror"])
err = tmp[k,"xerror"]; se = tmp[k,"xstd"]; k = which(tmp[,"xerror"] <= err+se)[1]
cp.tmp = tmp[k,"CP"]
fit.pruned = prune(fit2, cp=cp.tmp)
plot(fit.pruned, margin = 0.1);text(fit.pruned, use.n=TRUE)


## Predicting

cutoff = 0.5
pred = predict(fit.pruned, newdata=german, type="prob") #prediction
yhat = ifelse(pred[,2] > cutoff, 1, 0)
ctable = table(german$y, yhat, dnn=c("Actual", "Predicted")); ctable #classification table


## Evaluating

miss.err = 1-sum(diag(ctable))/sum(ctable); miss.err # Misclassification Rate
pred.acc = 1 - miss.err; pred.acc #Prediction Accuracy
diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity


## ROC and AUC

library(ROCR) #install.packages("ROCR")

pred2 = predict(fit.pruned, newdata=german, type="prob") #prediction
pred = prediction(pred2[,2], german$y)
perf = performance(pred, "tpr","fpr")

plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.6, 0.3, legend = c("Tree","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC





###########################################
# Computing the test error by paritioning


## Data Partitioning 

set.seed(12)
V = 2
n =  NROW(german)
id = sample(1:V, n, prob = c(0.7,0.3), replace = T) # Partitioning 7:3
ii = which(id==1)
german.train = german[ii,]
german.test  = german[-ii,]


## Growing a tree

fit = rpart(y ~., data=german.train, method="class", control = rpart.control(xval=10, cp=0))
fit
summary(fit)
plot(fit);  text(fit)


## Pruning

tmp = printcp(fit)
k = which.min(tmp[,"xerror"])
cp.tmp = tmp[k,"CP"]
fit.pruned = prune(fit, cp=cp.tmp)
plot(fit.pruned, margin = 0.1);text(fit.pruned, use.n=TRUE, all = T)


## Predicting and Evaluating

cutoff = 0.5
pred = predict(fit.pruned, newdata=german.test, type="prob") #prediction
yhat = ifelse(pred[,2] > cutoff, 1, 0)
ctable = table(german.test$y, yhat, dnn=c("Actual", "Predicted")); ctable #classification table


miss.err = 1-sum(diag(ctable))/sum(ctable); miss.err # Misclassification Rate
pred.acc = 1 - miss.err; pred.acc #Prediction Accuracy
diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity


## ROC and AUC

library(ROCR)
par(mfrow = c(2,2))

pred2 = predict(fit.pruned, newdata=german.train, type="prob") #prediction
pred = prediction(pred2[,2], german.train$y)
perf = performance(pred, "tpr","fpr")

plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Train)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.5, 0.3, legend = c("Tree","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC


pred2 = predict(fit.pruned, newdata=german.test, type="prob") #prediction
pred = prediction(pred2[,2], german.test$y)
perf = performance(pred, "tpr","fpr")

plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Test)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.5, 0.3, legend = c("Tree","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC





##########################
# Computing the CV error


V = 10 #V-fold CV
miss.err.train = 0
miss.err.test = 0
cutoff = 0.5

set.seed(123)
id = sample(1:V, nrow(german), replace = T)

for(i in 1:V) {
 
  print(i)
  
  ## Data Partitioning 

  german.train = german[id != i,] 
  german.test  = german[id == i,] 
  
  ## Growing

  fit = rpart(y ~., data=german.train, method="class", 
              control = rpart.control(xval=10, cp=0)) #maximal tree
  
  ## Pruning

  tmp = printcp(fit)
  k = which.min(tmp[,"xerror"])
  cp.tmp = tmp[k,"CP"]
  fit.pruned = prune(fit, cp=cp.tmp) #tree with min CV error
  
  ## Predicting and Evaluating

  pred = predict(fit.pruned, newdata=german.test, type="prob") #prediction for test data
  yhat = ifelse(pred[,2] > cutoff, 1, 0)
  miss.err.test = miss.err.test + mean(german.test$y != yhat) 
  
}

cv.err.test = miss.err.test/V; cv.err.test # CV test error



### END
