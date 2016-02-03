
##setwd("~/Downloads")
##g = read.csv("GSS2006.csv")

g = read.csv(file.choose())

## 1. Run a simple bivariate regression, and interpret your results.  (Did the results fit your expectations?  Why?  Why not?)  

g$ln.realrinc = log(g$realrinc)

plot(as.factor(g$replaceu), g$ln.realrinc)

lm1 = lm(replaceu ~ ln.realrinc , data=g,  subset = !is.na(big5d2) )
summary(lm1)

## 2. Add an additional variable that might mediate or partly "explain" the initial association from that simple regression above -- and explain your results.  Did it work out?  Yes?  No?  

plot(as.factor(g$big5d2), g$ln.realrinc)

lm2 = lm(replaceu ~ ln.realrinc + big5d2, data=g)
summary(lm2)

## install.packages("stargazer")
library(stargazer)
stargazer(lm1, lm2, type = "text")

## 3. Run another multiple regression.  Tell me how you expect your dependent variable to be affected by the independent variables.  Interpret your results.

## install.packages("plyr")
library(plyr)

d = read.csv(file.choose()) ## choose the World Values Survey ##

d = rename(d, c("V73"="spoil"))
d$rspoil = 7-d$spoil
d$rspoil.lab <- ordered(d$rspoil, levels = c(1,2,3,4,5,6), labels = c("not at all like me", "2", "3", "4", "5", "very much like me"))
table(d$rspoil.lab)

d = rename(d, c("V242"="age"))
d$married=ifelse(d$V57==1, 1,0)
d$female = ifelse(d$V240==2, 1, 0)
d = rename(d, c("V239"="ses"))

lm1 = lm(as.numeric(rspoil.lab) ~ age + female + married, d, subset=V2==36 & !is.na(ses))  ## This is for Australia ##
summary(lm1)

## 4. Now add another independent variable to that model in Question 3, preferably a set of dummy variables.  Tell me why you added that new set of variables and what effect you expected them to have.  Did they have an effect?  Interpret that new model.  ##

lm2 = lm(as.numeric(rspoil.lab) ~ age + female + married + ses, d, subset=V2==36)
summary(lm2)

## 5. Now run a partial F test comparing the model in Question 3 to the model in Question 4.  Does the F test support the idea of adding those new variables?  Why?  Why not? ##

anova(lm1, lm2)

## or ##

lm3 = lm(as.numeric(rspoil.lab) ~ age + female + married + as.factor(ses), d, subset=V2==36)
summary(lm3)

anova(lm1, lm3)

## or ##

d$ses.cat = cut(d$ses, breaks = c(-1, 4, 7, 10), label=c("poor","middle","rich")) 

lm4 = lm(as.numeric(rspoil.lab) ~ age + female + married + ses.cat, d, subset=V2==36)
summary(lm4)

anova(lm1, lm4)

## or ##

lm5 = lm(as.numeric(rspoil.lab) ~ age + female + married + as.numeric(ses.cat), d, subset=V2==36)
summary(lm5)