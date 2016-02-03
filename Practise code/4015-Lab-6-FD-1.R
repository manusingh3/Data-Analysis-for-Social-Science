

d=read.csv(file.choose()) ## choose "panel-for-R.csv" and more information on variables are here: http://sda.berkeley.edu/sdaweb/analysis/?dataset=gss06panelw3 ##

vars <- c("idnum","panelwave","affrmact","race", "intrace1")
pd.sub <- d[, vars]

pd.sub$black = ifelse(pd.sub$race==2, 1, 0)

pd.sub$intblack = ifelse(pd.sub$intrace1==2, 1, 0)

pd.sub$r.affact = 5-pd.sub$affrmact

pd.sub$year= ifelse(pd.sub$panelwave==3, 1, 0)

## 1. Run a naive ("pooled") OLS regression on the panel data.  Tell we how you expect your Xs to affect your Y and why.  Apply clustered standard errors too.  Interpret your results.

lm1 <- lm(r.affact ~ black + intblack + as.factor(panelwave),  data = pd.sub)

summary(lm1)

clusterSE(fit = lm1, cluster.var = "idnum", data=pd.sub)

## 2. Run a first differences regression on the same model in Question 1.  Interpret your results.  Do you draw a different conclusion than in Question 1?  Explain.

plm1 <- plm(r.affact ~ black + intblack + year,  index = c("idnum", "panelwave"),  model = "fd", data = pd.sub)

summary(plm1)

clusterSE(fit = plm1, cluster.var = "idnum", data=pd.sub)

##---- Extra stuff you could do ----
  
pd.sub$fourvsall= ifelse(pd.sub$r.affact==4, 1, 0)

pd.sub$fourthreevsall= ifelse(pd.sub$r.affact>=3, 1, 0)

pd.sub$fourthreetwovsone= ifelse(pd.sub$r.affact>=2, 1, 0)

plm2 <- plm(fourvsall ~ black + intblack + year,  index = c("idnum", "panelwave"),  model = "fd", data = pd.sub)

plm3 <- plm(fourthreevsall ~ black + intblack + year,  index = c("idnum", "panelwave"),  model = "fd", data = pd.sub)

plm4 <- plm(fourthreetwovsone ~ black + intblack + year,  index = c("idnum", "panelwave"),  model = "fd", data = pd.sub)

summary(plm4)

## install.packages("stargazer")
library(stargazer)
stargazer(plm1, plm2, plm3, plm4, type = "text")

## -- a few more checks -----

pd.sub$d.intblack = firstD(intblack, idnum, pd.sub )
table(pd.sub$d.intblack)
pd.sub$bw=ifelse(pd.sub$d.intblack==-1,1,0)
pd.sub$wb=ifelse(pd.sub$d.intblack==1,1,0)
pd.sub$d.r.affact=firstD(r.affact, idnum, pd.sub )
summary(lm(intblack ~ black, pd.sub))

summary(lm(d.r.affact ~ bw, pd.sub, subset=black==0))