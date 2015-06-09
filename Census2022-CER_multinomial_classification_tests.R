####################################################################################
## Data Analysis for ESRC Transformative project
## - Using the Commission for Energy Regulation (CER)'s Irish Smart Meter Trial data
##   - http://www.ucd.ie/issda/data/commissionforenergyregulationcer/

## evaluating usefulness for logistic models: the benchmark that we will use to charaterize a multinomial logistice regression model as useful is a 25% improvement over the rate of accuracy chievable by change alone.
## ppt from Utexas, using google search "R multinomial logistic regression interpretation"

## This file is used to generate forecasts on banded info.
## LHV: 4 categories on number of children:  0, 1, 2, 3+
## LHV: 4 categories on number of adults:  1, 2, 3, 4+
## LHV: 5 categories on income band: 1,2,3,4,5

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
install.packages("mlogit")
library(mlogit)


## load and prepare data
## HH survey data
setwd("//soton.ac.uk/ude/PersonalFiles/Users/xl25g12/mydocuments/census2022/CER_analysis/data")
HHsurvey = read.table("HH09_pretrial.txt",header=T,stringsAsFactors=FALSE,sep="\t")   ## read in txt file, need sep="t", HHpresurvey data, the income column is from Q4021 only
HH09FA = read.table("HH_floor_pretrial_survey.txt",header=T,stringsAsFactors=FALSE,sep="\t")   ## read in txt file, need sep="t"; pretrial floor area data
## income category 6 has been taken out of the sample as it is a non-response code.
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

## track number of children, residents
num_children = unique(svdata$HHchild_y09)
num_resident = unique(svdata$HHadult_y09)
## ddply(svdata,.(HHchild_y09),nrow)    NA = 3003
## ddply(svdata, .(HHadult_y09),nrow)
svdata["num_children"]= NA
svdata$num_children = svdata$HHchild_y09
svdata = within(svdata,num_children[HHtype_y09==1]<-0)   
svdata = within(svdata,num_children[HHtype_y09==2]<-0)   
svdata = within(svdata,num_children[num_children>3]<-3)   
ddply(svdata,.(num_children),nrow)   ##

svdata["num_adult"] = NA
svdata$num_adult = svdata$HHadult_y09
svdata = within(svdata,num_adult[num_adult>4]<-4)
ddply(svdata,.(HHadult_y09),nrow)   ##
ddply(svdata,.(num_adult),nrow)   ##

## careful with weekend or midweek data

## 8 LHS indicators: 1) peak time, 2) peak load, 3) baseload, 4) mean demand, 5) sum, 6) 97.5%, 7) ECF, 8) LF    
## energy data
setwd("//soton.ac.uk/ude/PersonalFiles/Users/xl25g12/mydocuments/census2022/CER_analysis/data")

## weekend data
## energydata = read.table("CER_OctHH_wkend_long", header=T, stringsAsFactors=FALSE)        
## ARlong = read.csv("CER_Oct2009_archr-all_hubids_Weekends_v1.csv", header=T, stringsAsFactors=FALSE) ## 4232 obs of 13 variables  ## note in excel file, use space to replace "NA"

## midweek data
energydata = read.table("CER_OctHH_midwk_long", header=T, stringsAsFactors=FALSE) 
ARlong = read.csv("CER_Oct2009_archr-all_hubids_Midweek_v2.csv", header=T, stringsAsFactors=FALSE) ## 4232 obs of 13 variables  ## note in excel file, use space to replace "NA"

## read AR ready from long form to short form
## head(ARlong)
AR.sort= ARlong[order(ARlong$ID),]    ## unique IDs: 3486
AR = reshape(AR.sort, timevar="lag",idvar="ID", direction = "wide")    ## checked, done correctly, 3486 IDs

tmp = energydata[energydata$DateOct<365,]
regdata = ddply(tmp, .(ID), summarize,
                octpeaktime = round(mean(dailypktime), 4),
                octmax = round(mean(dailymax), 4),
                octbase = round(mean(dailybase), 4),
                octmean = round(mean(dailymean), 4),
                octsum = round(mean(dailysum), 4),
                octq975= round(mean(dailyq975),4),
                octECF = round(mean(ECF), 4),
                octLF = round(mean(LF), 4),
                na.rm=TRUE)  


ARtmp = as.data.frame(cbind(AR$ID,AR$archr.35,AR$archr.70,AR$archr.105))
colnames(ARtmp) = c("ID","AR35","AR70","AR105")

tmp = merge(svdata,ARtmp) 
datam = merge(tmp,regdata)      ## 3486 IDs
names(datam)
datam$dum_people = as.factor(datam$dum_people)
datam$dum_child = as.factor(datam$dum_child)
datam$HH_income = as.factor(datam$HH_income)



## important! take out NAs from variables used in logit model, If not, observations and fitted values not matched, problems for classification!

## Take out NAs from HH_income, ## Total obs of HH_income is 1481
datam1 = datam[!is.na(datam$HH_income),]
datam2 = datam1[!is.na(datam1$dum_people),]
datam3 = datam2[!is.na(datam2$dum_child),]
datam4 = datam3[!is.na(datam3$octbase),]
datam5 = datam4[!is.na(datam4$octECF),]

datamf = datam5

testest = multinom(HH_income ~ dum_people + dum_child + octbase + octECF,na.action="na.omit", data=datamf)
summary(testest)

test = testest
summary(test)
z <- summary(test)$coefficients/summary(test)$standard.errors   ## z-score
z
p <- (1 - pnorm(abs(z), 0, 1)) * 2    ## p-value
p
exp(coef(test))    ## relative risk ratio
pp <- fitted(test)   ## 
head(pp)



####### classification #######
## Give the highest prediction on the category as the predicted category
## give the HH predicted category = true category as "1", 
## Table of true classification given by income data ### 
# 

## ddply(datamf,.(HH_income),nrow)
# HH_income  V1
# 1         1 153
# 2         2 222
# 3         3 343
# 4         4 475
# 5         5 285

### classification test
####### classification codes #######################
fitted = as.data.frame(pp)
colnames(fitted) = c("pp1","pp2","pp3","pp4","pp5")
names(fitted)
true_income = as.numeric(as.character(datamf$HH_income))    ## put dummy back to 0 and 1; dummy for employed is 1
predited_cat = 0
predited_dum = 0
out= as.matrix(cbind(true_income,fitted,predited_cat,predited_dum))      ## 

##find the max row value, max.col(), stored in predited_cat
tmp1 = max.col(out[,2:6],"first")
out = as.data.frame(out)
out$predited_cat = as.numeric(tmp1)

## if predited_cat equal to true income band, put 1 in predited_dum
out$predited_dum = ifelse(out$predited_cat==out$true_income,1,0)
# ddply(out,.(predited_dum),nrow)
# predited_dum   V1
# 1            0 1007
# 2            1  471

## examples of probabilities calculated against observed & predicted categories using this method
## essentially the majority are allocated to band 4
# head(out)
# true_income         pp1        pp2       pp3       pp4       pp5       predited_cat predited_dum
# 1            1 0.225094742 0.23912560 0.1712665 0.2564000 0.1081132            4            0
# 2            3 0.020366367 0.09399776 0.2354826 0.4401418 0.2100115            4            0
# 8            3 0.037214194 0.12067370 0.2321019 0.3690928 0.2409174            4            0
# 18           4 0.009786489 0.05460458 0.2638757 0.3738515 0.2978817            4            1
# 24           4 0.190800918 0.22274730 0.1890820 0.2766414 0.1207283            4            1
# 26           1 0.031743912 0.09372484 0.2716323 0.3721229 0.2307760            4            0
           
# ddply(out,.(predited_dum,predited_cat),nrow)
#         predited_dum predited_cat  V1
# 1             0            1  23
# 2             0            2  23
# 3             0            3  21
# 4             0            4 912
# 5             0            5  28
# 6             1            1  12
# 7             1            2   8
# 8             1            3   9
# 9             1            4 430
# 10            1            5  12

## classification table: income band from 1 - 5, 10 entries as follows: 
table =  ddply(out,.(predited_dum,predited_cat),nrow)
table_T = ddply(out,.(true_income),nrow)
cltable =table_T
colnames(cltable)= c("income_range", "true_income")
cltable$correctPre = table[6:10,3]
cltable$incorrectPre = table[1:5,3]
cltable$correctPercent =  cltable$correctPre/cltable$true_income                   ## correct prediction given all real outcome
cltable$overallPercent =  cltable$correctPre/(cltable$correctPre + cltable$incorrectPre)
## grand correct percentage
F = sum((cltable$correctPercent*cltable$overallPercent)^2)    ## 0.085167
F

### classification by random as a comparison


# cltable
#        income_range true_income correctPre incorrectPre correctPercent overallPercent
# 1            1         153         12           23     0.07843137      0.3428571
# 2            2         222          8           23     0.03603604      0.2580645
# 3            3         343          9           21     0.02623907      0.3000000
# 4            4         475        430          912     0.90526316      0.3204173
# 5            5         285         12           28     0.04210526      0.3000000
# 
# The proportional by chance accuracy rate is
## CA = sum((cltable$true_income/sum(cltable$true_income))^2)
## CA     ## 0.2276
## The proportional by chance accuracy criteria,   CA * 1.25 = 0.2276*1.25 = 0.2845   (not good!)
