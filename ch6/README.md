# Boston
~~~
> i = which(Boston$medv == 50) 
> library(MASS) 
> # 50th data: Outlier -> So Exclude this data
> i = which(Boston$medv == 50) 
> boston = Boston[-i,] # delete cases with medv=50
> boston$chas = factor(boston$chas)
> boston$rad = factor(boston$rad)
> # Factor: Qualitative 질적 변수로 적용하기 때문에 그렇다.
> boston$chas = factor(boston$chas)
> boston$rad = factor(boston$rad)
> ## Model fitting
> fit.all = lm(medv ~ ., data = boston) # fit a linear model with all variables
> fit.step = step(fit.all, direction="both") # stepwise variable selection
Start:  AIC=1295.03
medv ~ crim + zn + indus + chas + nox + rm + age + dis + rad + 
    tax + ptratio + black + lstat

          Df Sum of Sq    RSS    AIC
- chas     1      0.63 6321.5 1293.1
- indus    1     11.30 6332.2 1293.9
<none>                 6320.9 1295.0
- age      1     82.67 6403.5 1299.4
- tax      1    126.28 6447.1 1302.7
- nox      1    189.88 6510.7 1307.5
- black    1    192.42 6513.3 1307.7
- zn       1    203.44 6524.3 1308.5
- crim     1    228.82 6549.7 1310.5
- rad      8    721.85 7042.7 1332.0
- ptratio  1    706.41 7027.3 1344.9
- dis      1    860.51 7181.4 1355.6
- lstat    1    965.26 7286.1 1362.7
- rm       1   1330.92 7651.8 1386.7

Step:  AIC=1293.08
medv ~ crim + zn + indus + nox + rm + age + dis + rad + tax + 
    ptratio + black + lstat

          Df Sum of Sq    RSS    AIC
- indus    1     11.00 6332.5 1291.9
<none>                 6321.5 1293.1
+ chas     1      0.63 6320.9 1295.0
- age      1     82.48 6404.0 1297.4
- tax      1    130.45 6451.9 1301.1
- nox      1    189.27 6510.8 1305.5
- black    1    193.59 6515.1 1305.9
- zn       1    203.76 6525.2 1306.6
- crim     1    230.58 6552.1 1308.6
- rad      8    738.26 7059.8 1331.2
- ptratio  1    719.40 7040.9 1343.9
- dis      1    861.64 7183.1 1353.7
- lstat    1    965.11 7286.6 1360.7
- rm       1   1333.37 7654.9 1384.9

Step:  AIC=1291.93
medv ~ crim + zn + nox + rm + age + dis + rad + tax + ptratio + 
    black + lstat

          Df Sum of Sq    RSS    AIC
<none>                 6332.5 1291.9
+ indus    1     11.00 6321.5 1293.1
+ chas     1      0.32 6332.2 1293.9
- age      1     81.09 6413.6 1296.2
- tax      1    192.78 6525.3 1304.6
- black    1    196.55 6529.0 1304.9
- zn       1    220.63 6553.1 1306.7
- crim     1    225.50 6558.0 1307.1
- nox      1    239.09 6571.6 1308.1
- rad      8    791.09 7123.6 1333.6
- ptratio  1    732.81 7065.3 1343.6
- dis      1    857.27 7189.8 1352.1
- lstat    1    987.73 7320.2 1361.0
- rm       1   1380.21 7712.7 1386.5
> fit.step$anova
     Step Df   Deviance Resid. Df Resid. Dev      AIC
1         NA         NA       469   6320.865 1295.031
2  - chas  1  0.6261633       470   6321.491 1293.079
3 - indus  1 10.9964825       471   6332.487 1291.931
> summary(fit.step) # print the fitted model

Call:
lm(formula = medv ~ crim + zn + nox + rm + age + dis + rad + 
    tax + ptratio + black + lstat, data = boston)

Residuals:
    Min      1Q  Median      3Q     Max 
-9.5200 -2.2850 -0.4688  1.7535 15.3972 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  30.252522   4.329907   6.987 9.64e-12 ***
crim         -0.104568   0.025533  -4.095 4.96e-05 ***
zn            0.045510   0.011235   4.051 5.97e-05 ***
nox         -12.366882   2.932651  -4.217 2.97e-05 ***
rm            3.583130   0.353644  10.132  < 2e-16 ***
age          -0.025822   0.010514  -2.456 0.014412 *  
dis          -1.253903   0.157029  -7.985 1.08e-14 ***
rad2          2.387130   1.160735   2.057 0.040278 *  
rad3          4.644091   1.062157   4.372 1.51e-05 ***
rad4          2.608777   0.944668   2.762 0.005977 ** 
rad5          3.116933   0.960550   3.245 0.001258 ** 
rad6          1.422890   1.150280   1.237 0.216705    
rad7          4.868388   1.240114   3.926 9.94e-05 ***
rad8          5.872144   1.180865   4.973 9.26e-07 ***
rad24         6.420553   1.393304   4.608 5.24e-06 ***
tax          -0.010571   0.002792  -3.787 0.000172 ***
ptratio      -0.837356   0.113420  -7.383 7.08e-13 ***
black         0.007949   0.002079   3.823 0.000149 ***
lstat        -0.357576   0.041718  -8.571  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 3.667 on 471 degrees of freedom
Multiple R-squared:  0.7907,	Adjusted R-squared:  0.7827 
F-statistic: 98.83 on 18 and 471 DF,  p-value: < 2.2e-16

> yhat = predict(fit.step, newdata=boston, type="response") # predictions
> head(yhat)
       1        2        3        4        5        6 
26.59831 24.00195 28.99396 29.60018 29.07676 26.41636 
> plot(boston$medv, yhat, xlim=c(0,50), ylim=c(0,50), xlab="Observed Values", ylab="Fitted Values")
> abline(a=0, b=1)
> mean((boston$medv - yhat)^2)  # MSE
[1] 12.92344
> mean(abs(boston$medv - yhat)) # MAE
[1] 2.639724
~~~
