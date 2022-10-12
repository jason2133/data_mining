################################################
# Lab: Neural Networks with German Credit Data #
################################################


## Calling the libraries

library(neuralnet) #install.packages("neuralnet")
library(dummy) #install.packages("dummy")


## Data reading

german = read.table("germandata.txt",header=T) 
#german$y = ifelse(german$y=="good", 1, 0)
german$y = factor(german$y)

sum(is.na(german))

#german = na.omit(german) #can remove the cases with missing values if any


## Data handling

#when taking only numerical variables
#german2 = german[,c(2,5,8,11,13,16,18,21)] 

#when taking both numerical and categorical variables
dvar = c(1,3,4,6,7,9,10,12,14,15,17,19,20) #find nominal variables
german2 = dummy(x=german[,dvar]) # transform nominal variables into dummy variables
german2 = german2[,-c(4,9,19,24,29,33,36,40,43,46,50,52,54)] # delete redundant dummy variables
german2 = cbind(german[,-dvar], german2) # combine them
for(i in 1: ncol(german2)) if(!is.numeric(german2[,i])) german2[,i] = as.numeric(german2[,i])


## Scaling

max1 = apply(german2, 2, max) 
min1 = apply(german2, 2, min)

gdat = scale(german2, center = min1, scale = max1 - min1) #Standaization
gdat = as.data.frame(gdat)


## Model fitting

german2 = gdat
gn = names(german2)
f = as.formula(paste("y ~", paste(gn[!gn %in% "y"], collapse = " + "))) #y: target variable
fit.nn = neuralnet(f, data = german2, hidden=c(3,2), linear.output=F) 
#fit neural networks with 1 hidden layer consisting of 3 hidden neurons.

plot(fit.nn)


## Predicting

cutoff = 0.5
p.nn = predict(fit.nn, german2)
yhat.nn = ifelse(p.nn > cutoff, 1, 0)

ctable = table(german2$y, yhat.nn, dnn=c("Actual","Predicted"))
print(ctable) # classification table  


## Evaluating

miss.err = 1-sum(diag(ctable))/sum(ctable) # Misclassification Rate
miss.err

pred.acc = 1 - miss.err #Prediction Accuracy
pred.acc  

diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity


## ROC and AUC

library(ROCR) #install.packages("ROCR")

pred = prediction(p.nn, german2$y)

perf = performance(pred, "tpr","fpr")
plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.6, 0.3, legend = c("Neural Network","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC




###########################################
# Computing the test error by paritioning


## Data partitioning 

set.seed(123)
V = 2
n =  NROW(gdat)
id = sample(1:V, n, prob = c(0.7,0.3), replace = T) # Partitioning 7:3
ii = which(id==1)
german2.train = gdat[ii,]
german2.test  = gdat[-ii,]


## Model fitting

gn = names(german2.train)
f = as.formula(paste("y ~", paste(gn[!gn %in% "y"], collapse = " + ")))
fit.nn = neuralnet(f, data = german2.train, hidden=c(3,2), linear.output=F) #3 hidden neurons

plot(fit.nn)


## Predicting

cutoff = 0.5
p.test.nn = predict(fit.nn, german2.test)
yhat.test.nn = ifelse(p.test.nn > cutoff, 1, 0)

ctable = table(german2.test$y, yhat.test.nn, dnn=c("Actual","Predicted"))
print(ctable) # classification table  


## Evaluating

miss.err = 1-sum(diag(ctable))/sum(ctable) # Misclassification Rate
miss.err

pred.acc = 1 - miss.err #Prediction Accuracy
pred.acc  

diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity


## ROC and AUC

library(ROCR)
cutoff = 0.5
par(mfrow = c(2,2))

#Train 
p.train.nn = predict(fit.nn, german2.train)
yhat.train.nn = ifelse(p.train.nn > cutoff, 1, 0)
pred = prediction(p.train.nn, german2.train$y)

perf = performance(pred, "tpr","fpr")
plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Train)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.4, 0.3, legend = c("Neural Networks","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC


#Test
p.test.nn = predict(fit.nn, german2.test)
yhat.test.nn = ifelse(p.test.nn > cutoff, 1, 0)
pred = prediction(p.test.nn, german2.test$y)

perf = performance(pred, "tpr","fpr")
plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Test)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.4, 0.3, legend = c("Neural Networks","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC





##########################
# Computing the CV errors

cutoff = 0.5

V = 10 #V-fold CV
miss.err.train = 0
miss.err.test = 0

set.seed(1234)
id = sample(1:V, nrow(german), replace = T)

for(i in 1:V) {
  
  print(i)

  ## Data partitioning 

  german2.train = gdat[id != i,] 
  german2.test  = gdat[id == i,] 

  ## Model fitting

  gn = names(german2.train)
  f = as.formula(paste("y ~", paste(gn[!gn %in% "y"], collapse = " + ")))
  fit.nn = neuralnet(f, data = german2.train, hidden=1, linear.output=F) #1 hidden neuron

  ## Predicting and Evaluating

  p.test.nn = predict(fit.nn, german2.test)  
  yhat.test.nn = ifelse(p.test.nn > cutoff, 1, 0)
  miss.err.test = miss.err.test + mean(german2.test$y != yhat.test.nn)
  
}

cv.err.test = miss.err.test/ V; cv.err.test # CV test error



### END
