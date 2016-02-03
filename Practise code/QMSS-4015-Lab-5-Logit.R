
library(QMSS)

d = read.table("C:/Users/SONY/Desktop/Data Anlysis for Social Science/copen.dat")  ## choose the 2006 GSS ##

sub <- d[, c("xhaustn", "hrs1", "age", "prestg80", "babies", "wrkstat")]

sub <- na.omit(sub) ## get rid of all missings; necessary for predictions later ##

sub$exh = ifelse(sub$xhaustn==1, 1, 0) ## only look at "always exhausted" vs. everything else ##

table(sub$exh)

lm1 = lm(exh ~ hrs1 + age + prestg80 + babies, sub, subset= wrkstat==1)
summary(lm1)

## 2. Run a multiple (binary) logistic model.  (It can be the same as the above LPM or -- even better -- a new model.)  Tell me how you think your independent variables will affect your dependent variable.  Interpret your results in the logit scale.  Were your expectations correct?  Why or why not?

logit1 = glm(exh ~ hrs1 + age + prestg80 + babies, sub, subset= wrkstat==1, family=binomial)
summary(logit1)

## 3. Get odds ratios from your logit model in Question 2 and interpret some of them.  

exp(coef(logit1))

## 4. Get predicted probabilities from your logit model in Question 2 for some constellations of X values and interpret the results.  

predict(logit1, type = "response", newdata = data.frame(hrs1 = c(35,80), age = c(35, 35), prestg80 = c(50, 50), babies = c(0,0)))

predict(logit1,  type = "response", newdata = data.frame( hrs1 = c(45,45), age = c(35, 35), prestg80 = c(50, 50), babies = c(0,2)))

predict(logit1,  type = "response", newdata = data.frame( hrs1 = c(35,60), age = c(20, 50), prestg80 = c(40, 80), babies = c(0,2)))

## the below will get it for any combination of variables and everything else set to means ##

pred.dat <- with(sub, expand.grid( 
  hrs1 = sort(unique(hrs1)),
  age = mean(age),
  prestg80 = mean(prestg80),
  babies = sort(unique(babies))))


predProb(logit1, predData = pred.dat, ci = F)
