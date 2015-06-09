####################################################################################
## Data Analysis for ESRC Transformative project
## - Using the Commission for Energy Regulation (CER)'s Irish Smart Meter Trial data
##   - http://www.ucd.ie/issda/data/commissionforenergyregulationcer/

## midweek & weekend household regressions using:
## - standardised 24 hour power data derived clusters
## - AR coefficients (lag = 35 & 70)
## - power variables

## For the latest version of this code go to: https://github.com/dataknut/Census2022

## This work was funded by RCUK through the ESRC's Transformative Social Science Programme via the
## "Census 2022: Transforming Small Area Socio-Economic Indicators through 'Big Data'" Project 
## - http://gtr.rcuk.ac.uk/project/2D2CD798-4F04-4399-B1AF-D810A233DD21
## - http://www.energy.soton.ac.uk/tag/census2022/
 
## Copyright (C) 2014  University of Southampton

## Author: Sharon Lin (X.Lin@soton.ac.uk) 
##  [Energy & Climate Change, Faculty of Engineering & Environment, University of Southampton]

## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License 
## (http://choosealicense.com/licenses/gpl-2.0/), or (at your option) any later version.

## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## #YMMV - http://en.wiktionary.org/wiki/YMMV

rm(list=ls())
## install.packages("psych")
library("psych")
library(lme4)
require(plyr)
library("lmtest")
library(xlsx)
library(nnet)


## data load and preparation
setwd("//soton.ac.uk/ude/PersonalFiles/Users/xl25g12/mydocuments/census2022/CER_analysis/data")
HHsurvey = read.table("HH09_pretrial.txt",header=T,stringsAsFactors=FALSE,sep="\t")   ## read in txt file, need sep="t", HHpresurvey data, the income column is from Q4021 only
HH09FA = read.table("HH_floor_pretrial_survey.txt",header=T,stringsAsFactors=FALSE,sep="\t")   ## read in txt file, need sep="t"; pretrial floor area data
## income category 6 has been taken out of the sample.
HH09income = read.xlsx("CER_income09.xlsx",3, header=T, stringsAsFactors=FALSE) ## 4232 obs of 13 variables  ## note in excel file, use space to replace "NA"
tmp1 = HHsurvey[,1:11]
tmp2 = HH09FA[,-2:-3]
tmp3 = merge(tmp1,tmp2)
tmp4 = merge(tmp3,HH09income)
describe(tmp4)
svdata = tmp4
## make a dummy variable with presence of chilren 0, no children, 1 with children
svdata["dum_child"] = NA
svdata= within(svdata,dum_child[HHtype_y09==3]<-1)   ##HHtype = 3, living with children (< 15 years)
svdata= within(svdata,dum_child[HHtype_y09==2]<-0)   ##HHtype = 2, living with adults(> 15years)
svdata= within(svdata,dum_child[HHtype_y09==1]<-0)   ##HHtype = 1, living alone 

## make a dummy variable for no. of residents,  1 or 2 residents, dum_people = 0, 3 or above residents, dum_people = 1 
svdata["dum_people"]= NA
svdata = within(svdata,dum_people[HHtype_y09==1]<-0)   
svdata = within(svdata,dum_people[HHtype_y09==3&HHchild_y09==1]<-0)   
svdata = within(svdata,dum_people[HHtype_y09==2&HHadult_y09<3]<-0)   
svdata = within(svdata,dum_people[HHchild_y09>1]<-1) 
svdata = within(svdata,dum_people[HHadult_y09>2]<-1)   

## make a dummy variable for employment status,  1-3 as employment (0), 4-7 as unemployment (1)
svdata["dum_em"]= NA
svdata = within(svdata,dum_em[employment_y09==1]<-0)   
svdata = within(svdata,dum_em[employment_y09==2]<-0)   
svdata = within(svdata,dum_em[employment_y09==3]<-0)   
svdata = within(svdata,dum_em[employment_y09>3]<-1)   



## careful with weekend or midweek data


## 8 LHS indicators: 1) peak time, 2) peak load, 3) baseload, 4) mean demand, 5) sum, 6) 97.5%, 7) ECF, 8) LF    
## energy data
setwd("//soton.ac.uk/ude/PersonalFiles/Users/xl25g12/mydocuments/census2022/CER_analysis/data")

## weekend data
  cluster_48 = read.table("CER_Oct09HH_48obs_weekend.txt", header=T, stringsAsFactors=FALSE)        ## read in text files
  energydata = read.table("CER_OctHH_wkend_long", header=T, stringsAsFactors=FALSE)        
  ARlong = read.csv("CER_Oct2009_archr-all_hubids_Weekends_v2.csv", header=T, stringsAsFactors=FALSE) ## 4232 obs of 13 variables  ## note in excel file, use space to replace "NA"
# 
# 


## midweek data
#     cluster_48 = read.table("CER_Oct09HH_48obs_midweek.txt", header=T, stringsAsFactors=FALSE)        ## read in text files
#     energydata = read.table("CER_OctHH_midwk_long", header=T, stringsAsFactors=FALSE) 
#     ARlong = read.csv("CER_Oct2009_archr-all_hubids_Midweek_v2.csv", header=T, stringsAsFactors=FALSE) ## 4232 obs of 13 variables  ## note in excel file, use space to replace "NA"

## processing AR from long to shor data form
ARlong = ARlong[,1:3]
AR.sort= ARlong[order(ARlong$ID),]    ## unique IDs: 3486
AR = reshape(AR.sort, timevar="lag",idvar="ID", direction = "wide")    ## checked, done correctly, 3486 IDs

tmp = energydata[energydata$DateOct<365,]
regdata = ddply(tmp, .(ID), summarize,
                octpeaktime = round(mean(dailypktime), 4),
                octmax = round(mean(dailymax), 4),
                octammean = round(mean(dailyammean), 4),
                octbase = round(mean(dailybase), 4),
                octmean = round(mean(dailymean), 4),
                octsum = round(mean(dailysum), 4),
                octq975= round(mean(dailyq975),4),
                octECF = round(mean(ECF), 4),
                octLF = round(mean(LF), 4),
                na.rm=TRUE)  

## merge data together
tmp = merge(svdata,AR)           ## AR35 is the autocorrleation of 35 lags, or a day.  AR70 is the autocorrelation of 70 lags, or two days
tmp1 = merge(tmp,regdata)
mydatam = merge(tmp1,cluster_48)

######### standardised clustering  ############
stddata = mydatam[,177:224]
stddata = na.omit(stddata)
stddata = scale(stddata)   ## make mean 0, stdev 1

## k means clustering
## partitioning by a plot of the within groups sum of squares (WSS) by number of clusters extracted (looking for a bend in the plot, http://www.statmethods.net/advstats/cluster.html)
## wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
wss <- (nrow(stddata)-1)*sum(apply(stddata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(stddata,centers=i,iter.max=20,nstart=1)$withinss)   ## allow 20 iterations
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="CER_midweek_clusters (30min profile")

### fit kmeans analysis for 6 clusters ##########
set.seed(1)
fit <- kmeans(stddata, 6) # no. of cluster solution
ckmean = aggregate(stddata,by=list(fit$cluster),FUN=mean)    # get cluster means 
# append cluster assignment
stddataf <- data.frame(mydatam$ID,stddata, fit$cluster)    ## standardized clusters with ID column
mydata = stddataf
mydata["dum_ck1"] = 0
mydata = within(mydata, dum_ck1[fit.cluster==1]<-1)
mydata["dum_ck2"] = 0
mydata = within(mydata, dum_ck2[fit.cluster==2]<-1)
mydata["dum_ck3"] = 0
mydata = within(mydata, dum_ck3[fit.cluster==3]<-1)
mydata["dum_ck4"] = 0
mydata = within(mydata, dum_ck4[fit.cluster==4]<-1)
mydata["dum_ck5"] = 0
mydata = within(mydata, dum_ck5[fit.cluster==5]<-1)
mydata["dum_ck6"] = 0
mydata = within(mydata, dum_ck6[fit.cluster==6]<-1)
names(mydata)[names(mydata)=="mydatam.ID"]<-"ID"

############## end of standardizing clusters,  mydata has 48 obs of standardized means and dummy variables for 6 clusters  ################

## merge energy, HH survey, AR, cluster data together
datam = merge(mydatam[,1:175],mydata)   ### data in daily 48 obs are standardized (mean 0, std = 1)
## write.table(mydata,"test_wkend_cluster")
mydata = as.data.frame(datam)
## test=do.call(data.frame,lapply(mydata, function(x) replace(x, is.infinite(x),NA)))

## change data structure
mydata$dum_em = as.factor(mydata$dum_em)
mydata$dum_people = as.factor(mydata$dum_people)
mydata$dum_child = as.factor(mydata$dum_child)
mydata$dum_ck1 = as.factor(mydata$dum_ck1)
mydata$dum_ck2 = as.factor(mydata$dum_ck2)
mydata$dum_ck3 = as.factor(mydata$dum_ck3)
mydata$dum_ck4 = as.factor(mydata$dum_ck4)
mydata$dum_ck5 = as.factor(mydata$dum_ck5)
mydata$dum_ck6 = as.factor(mydata$dum_ck6)



##### logit regresssion  models  

## Wkend
# logit(dum_resident) ~ ampkmean + mean
# logit(dum_child) ~ ampkmean + mean 



# ## dummy people    1/2   v 3/3+
tmp = mydata
tmp1 = tmp[!is.na(tmp$dum_people),]
mydata_r = tmp1
### weekend regresssion 
D1_logit = glm(dum_people ~ octammean + octmean, na.action="na.exclude", data=mydata_r,family="binomial")    ## linear regression 
D1a_logit = glm(dum_people ~ octammean + octmean + dum_ck2 + dum_ck3 + dum_ck4 + dum_ck5 + dum_ck6, na.action="na.exclude", data=mydata_r,family="binomial")    ## linear regression 
D1b_logit = glm(dum_people ~ octammean + octmean + dum_ck2 + dum_ck3 + dum_ck4 + dum_ck5 + dum_ck6+ archr.35+archr.70, na.action="na.exclude", data=mydata_r,family="binomial")    ## linear regression 

lrtest(D1_logit,D1a_logit)      ## 4.297e-09 ***
lrtest(D1a_logit,D1b_logit)    ## 0.002879 **
# 
# ## dummy children     0 or 1 (presence of chilidren)
tmp = mydata
tmp1 = tmp[!is.na(tmp$dum_child),]
mydata_r = tmp1

D2_logit = glm(dum_child ~ octammean + octmean, na.action="na.exclude", data=mydata_r,family="binomial")    ## linear regression 
 D2a_logit = glm(dum_child ~ octammean + octmean + dum_ck2 + dum_ck3 + dum_ck4 + dum_ck5 + dum_ck6, na.action="na.exclude", data=mydata_r,family="binomial")
 D2b_logit = glm(dum_child ~ octammean + octmean + dum_ck2 + dum_ck3 + dum_ck4 + dum_ck5 + dum_ck6 + archr.35+archr.70, na.action="na.exclude", data=mydata_r,family="binomial")
 lrtest(D2_logit,D2a_logit)      ## 2.258e-15 ***
 lrtest(D2a_logit,D2b_logit)      ##  0.08583


## Midweek regression
# logit(dum_people) ~ ampkmean + base + mean
# logit(dum_child) ~ ampkmean + mean + ECF
# logit(employment_y09) ~ ECF + LF

# dummy people as LHV
tmp = mydata
tmp1 = tmp[!is.na(tmp$dum_people),]
mydata_r = tmp1
C1_logit = glm(dum_people ~ octammean + octbase + octmean, na.action="na.exclude", data=mydata_r,family="binomial")    ## linear regression 
C1a_logit = glm(dum_people ~ octammean + octbase + octmean + dum_ck2 + dum_ck3 + dum_ck4 + dum_ck5 + dum_ck6, na.action="na.exclude", data=mydata_r,family="binomial")    ## linear regression 
C1b_logit = glm(dum_people ~ octammean + octbase + octmean + dum_ck2 + dum_ck3 + dum_ck4 + dum_ck5 + dum_ck6 +archr.35+archr.70 , na.action="na.exclude", data=mydata_r,family="binomial")    ## linear regression 
lrtest(C1_logit,C1a_logit)     ## 4.393e-12 ***
lrtest(C1a_logit,C1b_logit)   ## 0.4109 .
# 
# ## dummy child as LHV
tmp = mydata
tmp1 = tmp[!is.na(tmp$dum_child),]
mydata_r = tmp1

C2_logit = glm(dum_child ~ octmax + octammean + octmean + octECF, na.action="na.exclude", data=mydata_r,family="binomial")    ## linear regression 
C2a_logit = glm(dum_child ~ octmax +  octammean + octmean + octECF  + dum_ck2 + dum_ck3 + dum_ck4 + dum_ck5 + dum_ck6, na.action="na.exclude", data=mydata_r,family="binomial")    ## linear regression 
C2b_logit = glm(dum_child ~ octmax + octammean + octmean + octECF+ dum_ck2 + dum_ck3 + dum_ck4 + dum_ck5 + dum_ck6 +archr.35+archr.70, na.action="na.exclude", data=mydata_r,family="binomial")    ## linear regression 

lrtest(C2_logit,C2a_logit)     ##  2.973e-09 *** midweek
lrtest(C2a_logit,C2b_logit)    ##   0.00177 ** midweek

# 

### dum_employment as LHV, midweek only
#  tmp = mydata
#  tmp1 = tmp[!is.na(tmp$dum_em),]
#  mydata_r = tmp1
# 
# S5_logit = glm(dum_em ~ octECF + octLF, na.action="na.exclude", data=mydata_r,family="binomial")
# S5a_logit = glm(dum_em ~ octECF + octLF + dum_ck2 + dum_ck3 + dum_ck4 + dum_ck5 + dum_ck6 , na.action="na.exclude", data=mydata_r,family="binomial")
# S5b_logit = glm(dum_em ~ octECF + octLF + dum_ck2 + dum_ck3 + dum_ck4 + dum_ck5 + dum_ck6 + archr.35 + archr.70, na.action="na.exclude", data=mydata_r,family="binomial")
# S5c_logit = glm(dum_em ~ dum_child+ dum_people + octECF + octLF + dum_ck2 + dum_ck3 + dum_ck4 + dum_ck5 + dum_ck6 + archr.35 + archr.70, na.action="na.exclude", data=mydata_r,family="binomial")
# lrtest(S5_logit,S5a_logit)     ## 2.2e-16
# lrtest(S5a_logit,S5b_logit)    ## 0.00013
# lrtest(S5b_logit,S5c_logit)    ## 2.2e-16 

####### classification
## input output regression
logit = C2b_logit

## input depedent variable
true_dum = as.numeric(as.character(mydata_r$dum_child))   ## put dummy back to 0 and 1; dummy for employed is 1

## run the following, 
## extract fitted values
fitted = fitted(logit)
predited_dum = 0
logit_out= as.data.frame(cbind(true_dum,fitted,predited_dum))      ## 
colnames(logit_out) = c("true_dum", "fitted","predited_dum")       ## 
logit_out = within(logit_out, predited_dum[fitted>0.5]<-1)

## fill in the correct "+" preditions and "-" predictions
## 
TrueD = as.data.frame(count(logit_out,"true_dum"))
Total = TrueD[1,2] + TrueD[2,2]
# true_dum freq
# 1        0 2089
# 2        1 1399

correctP = sum(logit_out[,1]==0&logit_out[,3]==0)  ## corret negative prediction  , eg. 1757
correctN= sum(logit_out[,1]==1&logit_out[,3]==1)  ## correct positive prediction  ,  e.g. 763
(sum(logit_out[,1]==0&logit_out[,3]==0) + sum(logit_out[,1]==1&logit_out[,3]==1)) /(TrueD[1,2]+TrueD[2,2])  
## correct predictions (both positive and negative)
(correctP + correctN) /Total  ## 0.7225
###### end of classification test ##############





## regression ##  
## income band
## multinomail income band regression as LHV
 tmp = mydata
 tmp1 = tmp[!is.na(tmp$HH_income),]
 mydata_r = tmp1
# midweek
#  test = multinom(HH_income ~ octammean + octbase + octmean,na.action="na.omit",data=mydata_r )   ## 2LL = 2*2179 (residual deviance)
#  test_cluster = multinom(HH_income ~ octammean + octbase + octmean + dum_ck3+ dum_ck4+ dum_ck5+dum_ck6 ,na.action="na.omit",data=mydata_r )   ## 2LL = 2*2166 (residual deviance)
#  test_clusterar = multinom(HH_income ~ octammean + octbase + octmean  + dum_ck2 + dum_ck3+ dum_ck4+ dum_ck5+dum_ck6 + archr.35 + archr.70 + archr.105,na.action="na.omit",data=mydata_r )   ## 2LL = 2*2163 (residual deviance)
#  test_clusterar_admin = multinom(HH_income ~ dum_people + dum_child + octammean + octbase + octmean  + dum_ck2 + dum_ck3+ dum_ck4+ dum_ck5+dum_ck6 + archr.35 + archr.70 + archr.105,na.action="na.omit",data=mydata_r)   ## 2LL = 2*2163 (residual deviance)

##weekend 
test = multinom(HH_income ~ octmax + octbase + octmean,na.action="na.omit",data=mydata_r )   ## 2LL = 2*2179 (residual deviance)
test_cluster = multinom(HH_income ~ octmax + octbase + octmean + dum_ck3+ dum_ck4+ dum_ck5+dum_ck6 ,na.action="na.omit",data=mydata_r )   ## 2LL = 2*2166 (residual deviance)
test_clusterar = multinom(HH_income ~ octmax + octbase + octmean  + dum_ck2 + dum_ck3+ dum_ck4+ dum_ck5+dum_ck6 + archr.35 + archr.70 + archr.105,na.action="na.omit",data=mydata_r )   ## 2LL = 2*2163 (residual deviance)
test_clusterar_admin = multinom(HH_income ~ dum_people + dum_child + octmax + octbase + octmean  + dum_ck2 + dum_ck3+ dum_ck4+ dum_ck5+dum_ck6 + archr.35 + archr.70 +  archr.105,na.action="na.omit",data=mydata_r)

 lrtest(test,test_cluster)                     ### 0.005248 (midweek)         ### 0.5166 (weekend)
 lrtest(test_cluster,test_clusterar)           ## 0.002872 (midweek)         ##  0.9379 (weekend)
 lrtest(test_clusterar,test_clusterar_admin)   ## 2.483e-13  (midweek)       ##  1.403e-11 (weekend) 
# 
test = test_clusterar_admin
summary(test)
z <- summary(test)$coefficients/summary(test)$standard.errors   ## z-score
z
p <- (1 - pnorm(abs(z), 0, 1)) * 2    ## p-value
p
exp(coef(test))    ## relative risk ratio
pp <- fitted(test)   ## 
head(pp)

# ## floor space weekend
#  FL = lm(M2 ~ octbase + octLF, ,na.action="na.exclude",data=mydata)    ## working order
#  FLa = lm(M2 ~ octbase + octLF + dum_ck2 + dum_ck3+ dum_ck4+ dum_ck5+dum_ck6, ,na.action="na.exclude",data=mydata)    ## working order
#  FLb = lm(M2 ~ octbase + octLF + dum_ck2 + dum_ck3+ dum_ck4+ dum_ck5+dum_ck6 + archr.35 + archr.70 + archr.105, ,na.action="na.exclude",data=mydata)    ## working order
#  FLc = lm(M2 ~ octbase + dum_people + dum_child + octLF + dum_ck2 + dum_ck3+ dum_ck4+ dum_ck5+dum_ck6 + archr.35 + archr.70 + archr.105, ,na.action="na.exclude",data=mydata)    ## working order
#  lrtest(FL,FLa)    ## 0.0843,  0.2686 (weekend)
#  lrtest(FLa,FLb)   ## 0.546,   0.7867 (weekend)
#  lrtest(FLb,FLc)   ## 0.0024,  0.008342 (weekend)  


######### output cluster data  ###################
# test = mydata[,-2:-223]
# write.xlsx(test, "weekend_cluster.xlsx")

# test = mydata[,-2:-223]
# write.xlsx(test, "midweek_cluster.xlsx")
