
##read.url <- function(url, ...){
  ##tmpFile <- tempfile()
  ##download.file(url, destfile = tmpFile, method = "curl")
  ##url.data <- read.csv(tmpFile, ...)
  ##return(url.data)
##}
##d = read.url("https:courseworks.columbia.edu/access/content/group/QMSSG4015_001_2015_3/Data/GSS2006.csv")

##setwd("D:/Documents/gme2101")

##d = read.csv("GSS2006.csv")

d = read.csv(file.choose())

## 1. Recode 1 *sort of* continuous variable into categories.  Tell me what you did and explain the variable(s).

## A. The simplest way to make a dummy variable:

d$hi.attend = ifelse((d$attend>4), 1, 0) ## binary recode ##

table(d$hi.attend, d$attend) ## check the recoding ##


## B. Making a dummy variable of categories, not numbers:

d$mar.rec <- ifelse(d$marital ==1, c("married"), c("not married")) ## binary recode into categories, not numbers ##

table(d$mar.rec, d$marital) ## check the recoding ##


## C. Breaking a variable into categories:

d$attend.cat = cut(d$attend, breaks = c(-1, 2, 5, 8), label=c("weak","moderate","strong")) ## create a number of categories ##

table(d$attend.cat, d$attend) ## check the recoding ##


## D. Coding multiple conditions:

d$bothft = ifelse((d$wrkstat==1 & d$spwrksta==1), 1, 0) ## an example with multiple conditions at once ##

table(d$bothft, d$wrkstat, d$spwrksta)

 
## E. Another way to apply multiple labels:  

d$oncemarried[d$marital==1 ] <- 0

d$oncemarried[d$marital==2 ] <- 1

d$oncemarried[d$marital==3 ] <- 1

d$oncemarried[d$marital==4 ] <- 1

d$oncemarried[d$marital==5 ] <- 0

table(d$oncemarried, d$marital)


## F. Another way to apply multiple categorical labels:

d$econ.new[d$econsci==1] <- "very"

d$econ.new[d$econsci==2] <- "some"

d$econ.new[d$econsci==3] <- "little"

d$econ.new[d$econsci==4] <- "none"

d$econ.new[d$econsci==5] <- NA

table(d$econsci, d$econ.new)


d$econ.newest = factor(d$econ.new,levels=c("none", "little", "some", "very"), ordered=TRUE)

table(d$econ.newest)


## 2. Recode 1 other variable and attach value labels.  Tell me what you did and explain the variable(s).

## A. Add labels to existing variables:

d$hi.attend.lab <- ordered(d$hi.attend, levels = c(0,1), labels = c("low", "high")) ## using the hi.attend variable from above ##

table(d$hi.attend.lab)

## B. Reverse code a variable and then add labels and make it ordered:

d$rhappy = 4-d$happy ## to reverse code a variable, do this ... (highest category + 1) - orginal_variable ##

d$rhappy.fact = as.factor(d$rhappy) ## make this new numeric variable into a factor ##

d$lab.rhappy <- ordered(d$rhappy, levels = c(1,2,3), labels = c("unhappy", "so-so", "happy")) ## make that factor variable into an ORDERED factor, with value labels ##

table(d$lab.rhappy, d$rhappy)


mean(d$happy, na.rm=T) ## the original variable, happy, was numeric, so we can get the mean ##

mean(as.numeric(d$lab.rhappy), na.rm=T) ## the new variable, lab.rhappy, is an ordered factor -- and we need to tell R to treat it like a number, hence, the as.numeric) ##


## 3. Use one (or both) of your recoded variables to do a cross-tabulation (like last week, with prop.table, doBy, or ddply). Explain your results.

d$hi.attend = ifelse((d$attend>4), 1, 0) ## as before ##

d$hi.thorough = ifelse((d$big5c1<2), 1, 0)

## install.packages("gmodels")

library(gmodels)

CrossTable(d$hi.attend, d$hi.thorough, prop.r=F, prop.c=T, prop.t=F, prop.chisq=F, format="SPSS")  


## 4. Tell me two theories/ideas you might want to test in this course. Do you have a dataset for these ideas/theories already? Do you have it in R-readable format already? What is your main independent variable? What is your main dependent variable?

 
## 5. Run a linear regression with 1 independent and 1 dependent variable; make all of the recodes necessary to make the model as easy to interpret as possible; and explain your results.

## install.packages("psych")

library(psych)

describe(d$hrs1) ## respondent's work hours ##

describe(d$sphrs1) ## respondent's spouse's work hours ##


lm1 = lm(hrs1 ~ sphrs1, data=d) ## predicting one's hours from their spouse's ##
summary(lm1) ## examine the results ##

##  -- or --

d$hisp = ifelse(d$hispanic!=1, 1, 0) ## recode people to hispanic or not ##

d$rmarhisp = 6-d$marhisp ## reverse code acceptance of marriage to a hispanic person ##
d$rmarhisp.lab = ordered(d$rmarhisp, levels = c(1,2,3,4,5), labels = c("strongly disapprove", "somewhat disapprove", "neutral", "somewhat approve", "strongly approve" )) ## add labels ##

lm2 = lm(as.numeric(rmarhisp.lab) ~ hisp , data=d) 
summary(lm2) 


## 6. Plot two variables, either as a scatter plot or boxplot; add in trend/regression lines; and explain your results.


plot(d$sphrs1, d$hrs1, main="Scatterplot Example", 
  xlab="Spouse Hours", ylab="Hours", pch=19) ## scatter X and Y ##

abline(lm(hrs1 ~ sphrs1, data=d), col="blue") ## add in a regression line ##

## -- or ---

plot(jitter(d$sphrs1), jitter(d$hrs1), main="Scatterplot Example", 
  xlab="Spouse Hours", ylab="Hours", pch=19) ## same as above, but jitter the points ##

## -- or (for a boxplot) ---

plot(d$sex, d$hrs1, main="Scatterplot Example", 
  xlab="Sex", ylab="Hours", pch=19) ## this creates a scatter ##

plot(as.factor(d$sex), d$hrs1, main="Scatterplot Example", 
  xlab="Sex", ylab="Hours", pch=19) ## this creates a box plot ##

mean(d[d$sex == 1, 'hrs1'], na.rm=T)
describe(d[d$sex == 1, 'hrs1']) ## respondent's work hours ##

## ----- other useful graphing codes ----

hist(d$hrs1) ## draws a histogram ##

dense <- density(d$hrs1, na.rm=T) # returns the density data 
plot(dense) # plots the results as a kernel density plot

## install.packages("ggplot2")
library(ggplot2)
ggplot(d, aes(x=d$sphrs1, y=d$hrs1)) + ## Another scatter plot
   geom_point(shape=1)      +    # Use hollow circles
   geom_smooth(method=lm)   # Add linear regression line



