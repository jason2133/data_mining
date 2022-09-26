rm(list=ls())

data <- read.csv('C:/Users/jason/바탕 화면/coding1/data_mining/Assignment/assignment2/kyungsang_univ_2.csv', header=T)
head(data)
tail(data)

## Here we do!!!
train_data <- data[17521:26280, 3:23]
head(train_data)
tail(train_data)

test_data <- data[26281:35064, 3:23]
head(test_data)
tail(test_data)
summary(test_data)

data_train_plus_test <- data[17521:35064, 3:23]
summary(data_train_plus_test)

# Model Fitting
fit.all <- lm(발전량 ~., data=train_data)
fit.step = step(fit.all, direction='both')
fit.step$anova
summary(fit.step)

# Predicting
yhat = predict(fit.step, newdata = test_data, type='response')
head(yhat)

plot(test_data$발전량, yhat, xlim=c(0, 750), ylim=c(0, 750))
abline(a=0, b=1)

# Evaluating
mean((test_data$발전량 - yhat)^2) # MSE
sqrt(mean((test_data$발전량 - yhat)^2)) # RMSE
mean(abs(test_data$발전량 - yhat)) # MAE
cor(test_data$발전량, yhat) # PCC

### Computing the CV error
V = 10
mse.train = 0
mse.test = 0
mae.train = 0
mae.test = 0

set.seed(2017)
id = sample(1:V, nrow(data_train_plus_test), replace=T)

for(i in 1:V) {
  print(i)
  
  # Data Partitioning
  test.index = which(id==i)
  data.train = data_train_plus_test[-test.index,] # Train Data
  data.test = data_train_plus_test[test.index,] # Test Data
  
  # Fitting
  fit.reg = lm(발전량 ~., data=data.train)
  fit.step.reg = step(fit.reg, direction='both', trace=FALSE) # Stepwise variable selection
  
  # Predicting and Evaluating
  yhat.reg = predict(fit.step.reg, newdata = data.test, type='response')
  mse.test = mse.test + mean((test_data$발전량 - yhat)^2) # MSE
  mae.test = mae.test + mean(abs(test_data$발전량 - yhat)) # MAE
}

cv.mse.test = mse.test/V
cv.mae.test = mae.test/V

cv.mse.test
sqrt(cv.mse.test)
cv.mae.test

