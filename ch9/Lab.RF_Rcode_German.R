###############################################
#
# Lab: Random Forests with German Credit Data 
#
###############################################


## Calling packages

library(randomForest) #install.packages("randomForest")

help(randomForest)


## Foresting

set.seed(1234)
fit = randomForest(y~., data=german, ntree=100, mtry=5, importance=T)

plot(fit, type="l")
importance(fit)


## Predicting

pred = predict(fit, newdata=german, type="prob")
cutoff = 0.5
yhat = ifelse(pred[,2] > cutoff, 1, 0)
ctable = table(german$y, yhat, dnn=c("Actual", "Predicted")); ctable #classification table


## Evaluating

miss.err = 1-sum(diag(ctable))/sum(ctable); miss.err # Misclassification Rate
pred.acc = 1 - miss.err; pred.acc #Prediction Accuracy
diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity


## ROC and AUC

#install.packages("ROCR")
library(ROCR)

pred2 = predict(fit, newdata=german, type="prob")
pred = prediction(pred2[,2], german$y)
perf = performance(pred, "tpr","fpr")

par(mfrow=c(1,1))
plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.6, 0.3, legend = c("Random Forest","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC





###########################################
# Computing the test error by paritioning


## Data Partitioning

set.seed(123)
V = 2
n =  NROW(german)
id = sample(1:V, n, prob = c(0.7,0.3), replace = T) # Partitioning 7:3
ii = which(id==1)
german.train = german[ii,]
german.test  = german[-ii,]


## Foresting

set.seed(1234)
fit = randomForest(y~., data=german.train, ntree=100, mtry=5, importance=T)

plot(fit, type="l")
importance(fit)


## Predicting and Evaluating

pred = predict(fit, newdata=german.test, type="prob")
cutoff = 0.5
yhat = ifelse(pred[,2] > cutoff, 1, 0)
ctable = table(german.test$y, yhat, dnn=c("Actual", "Predicted")); ctable #classification table

miss.err = 1-sum(diag(ctable))/sum(ctable); miss.err # Misclassification Rate
pred.acc = 1 - miss.err; pred.acc #Prediction Accuracy
diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity


## ROC and AUC

library(ROCR)
par(mfrow = c(2,2))

pred2 = predict(fit, newdata=german.train, type="prob")
pred = prediction(pred2[,2], german.train$y)
perf = performance(pred, "tpr","fpr")

plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Train)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.5, 0.3, legend = c("Bagging","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC


pred2 = predict(fit, newdata=german.test, type="prob")
pred = prediction(pred2[,2], german.test$y)
perf = performance(pred, "tpr","fpr")

plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Test)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.5, 0.3, legend = c("Bagging","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC





##########################
# Computing the CV error


V = 10 #V-fold CV
miss.err.test = 0
cutoff = 0.5

set.seed(12345)
id = sample(1:V, nrow(german), replace = T)

for(i in 1:V) {
  
  print(i)
  
  ## Data Partitioning

  german.train = german[id != i,] 
  german.test  = german[id == i,] 
  
  ## Foresting

  fit = randomForest(y~., data=german.train, ntree=100, mtry=5, importance=T)

  ## Predicting and Evaluating

  pred = predict(fit, newdata=german.test, type="prob")
  yhat = ifelse(pred[,2] > cutoff, 1, 0)
  miss.err.test = miss.err.test + mean(german.test$y != yhat) 
  
}

cv.err.test = miss.err.test/ V;cv.err.test # CV test error



### END
