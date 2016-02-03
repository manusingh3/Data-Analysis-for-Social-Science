---
title: "Ordinal Logistic Regression"
output: html_document
---
# Ordinal Logistic Regression


For the purpose of this assignment I have chosen to work with data from the world values survey. I am trying to determine the probability of an individual feeling unsafe in the united states. As predictor variables I have considered factors such as 
1. Has the individual ever been attacked or been a victim of crime in the past one year . the responses have been recoded as 1- Yes and 0- No.
2. 
The next factor we is has the family of the respondent ever been attacked in the past year. the variable are again recoded as - 1- Yes and 0- No

3.The third predictor variable we use for the purpose of this study is - Does the respondent carry a weapon on him ( knives, guns etc) The variables are recoded to correspond to 1- Yes and 0 - No

The unsafe is the dependent variable and it has four levels- 
1# Often felt unsafe 
2# Sometimes felt unsafe
3# Rarely felt unsafe
4# Never felt unsafe.


```r
library(plm)
library(QMSS)
```

Some basic preprocessing steps and recoding of the variables

```r
u=subset(WV6_Data_v_2015_04_18, V2==840)
u$family <- ifelse((u$V180==1),1,0)
u$weapon <- ifelse((u$V189==1),1,0)
u$victim <- ifelse((u$V179==1),1,0)
a$unsafe[a$unsafe==-2 | a$unsafe==-1] <- NA
```


```r
ordlogit  <- vglm(unsafe ~ victim + family+ weapon, data= a, family=propodds)
summary(ordlogit)
```

```
## 
## Call:
## vglm(formula = unsafe ~ victim + family + weapon, family = propodds, 
##     data = a)
## 
## Pearson residuals:
##                  Min     1Q Median    3Q  Max
## logit(P[Y>=2]) -4.80  0.118  0.154 0.209 1.02
## logit(P[Y>=3]) -2.90  0.223  0.223 0.333 1.68
## logit(P[Y>=4]) -1.87 -0.697  0.647 0.647 2.26
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept):1   3.1814     0.0754   42.19   <2e-16 ***
## (Intercept):2   1.8611     0.0522   35.65   <2e-16 ***
## (Intercept):3   0.7300     0.0424   17.23   <2e-16 ***
## victim         -0.9866     0.0860  -11.47   <2e-16 ***
## family         -0.6813     0.0816   -8.35   <2e-16 ***
## weapon         -0.7673     0.0786   -9.76   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Number of linear predictors:  3 
## 
## Names of linear predictors: 
## logit(P[Y>=2]), logit(P[Y>=3]), logit(P[Y>=4])
## 
## Dispersion Parameter for cumulative family:   1
## 
## Residual deviance: 7830 on 11058 degrees of freedom
## 
## Log-likelihood: -3915 on 11058 degrees of freedom
## 
## Number of iterations: 4
```

The interpretation of the model is that if someone goes from 0- never been a victim to 1 been a victim the logit decreases by 1.1815, moving towards a lower category of unsafe as opposed to a higher category net of all other factors. (Remember 1- very unsafe and 4 is very safe).
This finding also makes intuitive sense because if a person has been a victim of a crime they are less likely to feel safe in their neighbourhood. The logit is also moving in the direction of lower safety in line with per expectations

The interpretation for family is also similar. In moving from 0- family never attacked to 1- family has been attacked . The logit decreases (in the direction of un-safety). Their logit decreases by 0.588 that they will be in a lower category as opposed to a higher category net of all other factors. 

The individuals who feel unsafe are more likely to carry a weapon .Our finding are in the direction of expectation. As an individual moves from 0- not carrying a weapon to 1- carrying a weapon - their logit decreases by 0.599 that they will belong to a lower category as opposed to a higher category. Net of all other factors.


```r
exp(coef(summary(ordlogit)))
```

```
##               Estimate Std. Error   z value Pr(>|z|)
## (Intercept):1  24.0804      1.078 2.096e+18        1
## (Intercept):2   6.4308      1.054 3.041e+15        1
## (Intercept):3   2.0751      1.043 3.033e+07        1
## victim          0.3728      1.090 1.047e-05        1
## family          0.5060      1.085 2.373e-04        1
## weapon          0.4642      1.082 5.772e-05        1
```
The above are the calculations for the odds ratio. 

The interpretation is similar to that above logically. But if an individual goes from not being a victim( 0) to being a victim - 1 the odds decrease by 69.3 percent that they will be in higher category of safety as opposed to  lower category net of all other factors. Similarly for family - if the family goes from not being attacked( 0) to being a attacked - 1 the odds decrease by 45.5 percent that they will be in higher category of safety as opposed to  lower category net of all other factors.


We predict the probabilities for some of the cases above:


```r
predict(ordlogit, type = "response", newdata = data.frame(victim = 0, family =0, weapon = 0 ))
```

```
##         1      2      3      4
## 1 0.03987 0.0947 0.1906 0.6748
```

In the first case we take a respondent who has never been a victim, whose family also hasn't been attacked and does not carry a weapon. The probability that such an individual feel very unsafe is 0.94 % and very safe is 76.13 %.


We try and compare these probabilities to another individual who has been attacked , whose family has been attacked and feels the need to carry a weapon. The probabilities for such an individual are :

```r
predict(ordlogit, type = "response", newdata = data.frame(victim = 1, family =1, weapon = 1 ))
```

```
##        1      2      3      4
## 1 0.3217 0.3181 0.2065 0.1538
```

For the second individual we observe the probability of belonging to the first category of very unsafe is 9.2 % which is almost ten times as large as the previous probability. The probability of such an individual feeling very safe is also much lower at 22.97 % 

The point of using ordinal logistic regression over OLS we assume that the distance between the categories is the same. But in real life the distance between often unsafe and sometimes unsafe may not always be the same as the distance between Sometimes unsafe and rarely unsafe. By using ordinal logistic regression we have not made that assumption.



