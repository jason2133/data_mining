#########################################################
#
# Lab: SVM with German Credit Data 
#
#########################################################


## Calling packages

library(e1071) #install.packages("e1071")


## Data reading

german = read.table("germandata.txt",header=T) 
german$y = ifelse(german$y=="good", 1, 0)
german$y = factor(german$y)

sum(is.na(german))

#german = na.omit(german) #can remove the cases with missing values if any


## Model fitting

fit = svm(y ~., data = german, kernel="radial", cost=1, decision.values=TRUE, probability=TRUE)
summary(fit)


## Predicting

fit.pred = predict(fit, newdata = german, decision.values=TRUE, probability=TRUE)
#pred = attributes(fit.pred)$decision.values
pred = attributes(fit.pred)$probabilities[,1] 


cutoff = 0.5
fit.yhat = ifelse(pred <= cutoff, 0, 1)

ctable = table(german$y, fit.yhat,  dnn = c("Actual", "Predicted"))  
ctable


## Evaluating

miss.err = 1-sum(diag(ctable))/sum(ctable) # Misclassification Rate
miss.err

pred.acc = 1 - miss.err #Prediction Accuracy
pred.acc  

diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity


## ROC and AUC

#install.packages("ROCR")
library(ROCR)

fit.pred = predict(fit, newdata = german,  decision.values=TRUE, probability=TRUE)
#pred = attributes(fit.pred)$decision.values
pred = attributes(fit.pred)$probabilities[,1] 
pred = prediction(pred, german$y)

perf = performance(pred, "tpr","fpr")
plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.6, 0.3, legend = c("SVM","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC



###########################################
# Computing the test error by paritioning


## Data partitioning

set.seed(123)
V = 2
n =  NROW(german)
id = sample(1:V, n, prob = c(0.7,0.3), replace = T) # Partitioning 7:3
ii = which(id==1)
german.train = german[ ii,]
german.test  = german[-ii,]


## Model fitting

fit = svm(y ~., data = german.train, kernel="radial", cost=1, decision.values=TRUE, probability=TRUE)
summary(fit)


## Predicting

fit.pred = predict(fit, newdata = german.test, decision.values=TRUE, probability=TRUE)
#pred = attributes(fit.pred)$decision.values
pred = attributes(fit.pred)$probabilities[,1]

cutoff = 0.5
fit.yhat = ifelse(pred <= cutoff, 0, 1)

ctable = table(german.test$y, fit.yhat,  dnn = c("Actual", "Predicted"))  
ctable


## Evaluating

miss.err = 1-sum(diag(ctable))/sum(ctable) # Misclassification Rate
miss.err

pred.acc = 1 - miss.err #Prediction Accuracy
pred.acc  

diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity


## ROC and AUC

library(ROCR)
par(mfrow = c(2,2))

fit.pred = predict(fit, newdata = german.train,   decision.values=TRUE, probability=TRUE)
#pred = attributes(fit.pred)$decision.values
pred = attributes(fit.pred)$probabilities[,1]
pred = prediction(pred, german.train$y)

perf = performance(pred, "tpr","fpr")
plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Train)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.5, 0.3, legend = c("Regression","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC


fit.pred = predict(fit, newdata = german.test, decision.values=TRUE, probability=TRUE)
#pred = attributes(fit.pred)$decision.values
pred = attributes(fit.pred)$probabilities[,1]
pred = prediction(pred, german.test$y)

perf = performance(pred, "tpr","fpr")
plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Test)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.5, 0.3, legend = c("SVM","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC



##########################
# Computing the CV error

cutoff = 0.5

V = 3 #V-fold CV
miss.err.train = 0
miss.err.test = 0

set.seed(1234)
id = sample(1:V, nrow(german), replace = T)

for(i in 1:V) {
  
  print(i)

  ## Data Partitioning 

  german.train = german[id != i,] 
  german.test  = german[id == i,] 
  
  ## Fitting

  fit = svm(y ~., data = german.train, kernel="linear", cost=1, decision.values=TRUE, probability=TRUE)
  
  ## Predicting and Evaluating
  
  #fit.pred = predict(fit, newdata = german.test, decision.values=TRUE, probability=TRUE)
  ##pred.test = attributes(fit.pred)$decision.values
  #pred.test = attributes(fit.pred)$probabilities[,1]
  #yhat.test = ifelse(pred.test <= cutoff, 0, 1)

  yhat.test = predict(fit, newdata = german.test) 
  miss.err.test = miss.err.test + mean(german.test$y != yhat.test)
  
}

cv.err.test = miss.err.test/V; cv.err.test # CV test error


#END

