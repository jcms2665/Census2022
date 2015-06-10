####################################################################################
## Data Analysis for ESRC Transformative project
## - Using the Commission for Energy Regulation (CER)'s Irish Smart Meter Trial data
##   - http://www.ucd.ie/issda/data/commissionforenergyregulationcer/

## This file is used to produce midweek (Tues-Thurs) and weekend (Sat-Sun) mixed models which test the ability of 
## various power (electricity consumption) variables to predict the household attributes of interest 

## The energy data is for 28 days (4 weeks) of Oct 2009 & 2010
## October 2009 was before the smart meter trials started and
## was also close to the pre-trial survey date

## Comparativ emodels also run for 2010

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
library(lme4)



## load in energy data
setwd("//soton.ac.uk/ude/PersonalFiles/Users/xl25g12/mydocuments/census2022/CER_analysis/data")   ## the file is in working order
## egdata = read.table("OctHH_midwk_long", header=T, stringsAsFactors=FALSE)   ## read in R exported txt file, no need sep="t"; long form of mid week data for all Oct 2009 and 2010 data
egdata = read.table("CER_OctHH_wkend_long", header=T, stringsAsFactors=FALSE)   ## read in R exported txt file, no need sep="t"; long form of mid week data for all Oct 2009 and 2010 data
eg09 = egdata[egdata$DateOct<365,] 
eg10 = egdata[egdata$DateOct>365,] 

## load in HH survey data, note survey content for pre-trial and post-trial are different
data1 = read.table("HH09_pretrial.txt",header=T,stringsAsFactors=FALSE,sep="\t")   ## read in txt file, need sep="t", HHpresurvey data
HH09FA = read.table("HH_floor_pretrial_survey.txt",header=T,stringsAsFactors=FALSE,sep="\t")   ## read in txt file, need sep="t"; pretrial floor area data
## HH10 = read.table("HH10_posttrial.txt",header=T,stringsAsFactors=FALSE,sep="\t")   ## read in txt file, need sep="t", HHpresurvey data
HH09 = merge(data1,HH09FA)

## merge energy and HH data together
## data for 09 and 10 separately
mydata09=merge(eg09,HH09)
##mydata10=merge(eg10,HH10)





########## mixed effects models are run on these separate files  ###################
## check on mixed model with income variables using 09 data
## make a comparison with 0910 data results using 0910

mydata09$dummy_HHchild = NA
mydata09 = within(mydata09,dummy_HHchild[HHtype_y09==1]<-0)   
mydata09 = within(mydata09,dummy_HHchild[HHtype_y09==2]<-0)   
mydata09 = within(mydata09,dummy_HHchild[HHtype_y09==3]<-1)   
ddply(mydata09, .(dummy_HHchild),nrow)
ddply(mydata09, .(HHtype_y09),nrow)

## make a new dummy vriable column D_unemployed 
mydata09$dummy_unemp_R = NA
mydata09 = within(mydata09,dummy_unemp_R[employment_y09==1]<-0)   
mydata09 = within(mydata09,dummy_unemp_R[employment_y09==4]<-1)   
mydata09 = within(mydata09,dummy_unemp_R[employment_y09==5]<-1)   
mydata09 = within(mydata09,dummy_unemp_R[employment_y09==2]<-NA)   
mydata09 = within(mydata09,dummy_unemp_R[employment_y09==3]<-NA)   
mydata09 = within(mydata09,dummy_unemp_R[employment_y09==6]<-NA)

ddply(mydata09, .(dummy_unemp_R),nrow)

## make a second new dummy vriable column dum_em_mixed,  (1-3)



## prepare dummy variable for "number of residents"
mydata09["dum_noResident"] = NA
mydata09 = within(mydata09,dum_noResident[HHtype_y09==1]<-0)   
mydata09 = within(mydata09,dum_noResident[HHtype_y09==3&HHchild_y09==1]<-0)   
mydata09 = within(mydata09,dum_noResident[HHtype_y09==2&HHadult_y09==1]<-0)   
mydata09 = within(mydata09,dum_noResident[HHadult_y09>1]<-1)   
mydata09 = within(mydata09,dum_noResident[HHchild_y09>1]<-1)   
ddply(mydata09, .(dum_noResident),nrow)
names(mydata09)


## change income to approximated mid ranged numbers
mydata09$income_num = mydata09$income   ## add a new column
mydata09 = within(mydata09,income_num[income_num==1]<-7500)   
mydata09 = within(mydata09,income_num[income_num==2]<-22500)   
mydata09 = within(mydata09,income_num[income_num==3]<-40000)
mydata09 = within(mydata09,income_num[income_num==4]<-62500)
mydata09 = within(mydata09,income_num[income_num==5]<-92500)   
mydata09 = within(mydata09,income_num[income_num==6]<-NA)   

## change dataframe structure
mydata09 = within(mydata09,HHtype_y09<-as.factor(HHtype_y09))
mydata09 = within(mydata09,dummy_unemp_R<-as.factor(dummy_unemp_R))   
mydata09 = within(mydata09,dummy_HHchild<-as.factor(dummy_HHchild))
mydata = mydata09


## the following code are used for weekend data 
final =0
for (i in c(11,9,12, 3, 5, 7,16,17) )
{
  energy  = mydata[,i]
  
  ## C1:  test = lmer(energy  ~ 1 + (1|HHID) + num_resident + log(income_num) + emp_dummy, data=mydata, na.action="na.omit")  ## RE on intercept only   
  ## C2:  test = lmer(energy  ~ 1 + (1|HHID) + num_resident + log(income_num), data=mydata, na.action="na.omit")  ## RE on intercept only 
  ## C3: test = lmer( energy  ~ 1 + (1|HHID) + num_resident + log(income_num) + HHtype , data=mydata, na.action="na.omit")  ## RE on intercept only 
  ## C4: test = lmer( energy  ~ 1 + (1|HHID) + num_resident + log(income_num) + dummy_HHchild , data=mydata, na.action="na.omit")  ## RE on intercept only 
  ## C5: test = lmer(energy ~ 1 + (1|HHID) + num_resident + log(income_num) + dummy_HHchild + dummy_unemp_R , data=mydata, na.action="na.omit")  ## RE on intercept only 
  ## C4: test = lmer(energy  ~ 1 + (1|ID) + dum_noResident + log(income_num) + dummy_HHchild, data=mydata, na.action="na.omit")  ## RE on intercept only   
  ## C3: test = lmer(energy  ~ 1 + (1|ID) + dum_noResident + log(income_num), data=mydata, na.action="na.omit")  ## RE on intercept only   
  ## C1: 
  test = lmer(energy  ~ 1 + (1|ID) + dum_noResident + log(income_num)+dummy_unemp_R, data=mydata, na.action="na.omit")  ## RE on intercept only   
   
  ## Extract fixed effect coefficients
  out_FE = as.data.frame(cbind(as.data.frame(coef(summary(test)))[,1],as.data.frame(coef(summary(test)))[,3]))
  colnames(out_FE)[1] = c(get("i"))
  colnames(out_FE)[2] = c("zsta")
  
  ## for 1 RE only: Extract random effect varaince and percentage of ICC (intercorrelation between RE in intercept and residuals)
    RE1 = as.data.frame(VarCorr(test))$vcov[1]    ## variance of HHID ##  as.data.frame(VarCorr(test))$sdcor[1]  for std. dev.
    RE2 = as.data.frame(VarCorr(test))$vcov[2]    ## varaince of residual ## 
  # alternative code
  ##  as.numeric(summary(test)@REmat[1,3])    ## variance of HHID;  
  ##  as.numeric(summary(test)@REmat[1,4])    ## std error of HHID
  ## as.numeric(summary(test)@REmat[2,4])    ## std error of residuals
  ## k = 2
  ## tmp11 = RE1 / (RE1 + RE2)    ## % of RE effects in HH
  ## RE3 = as.numeric(format(round(tmp11,k),nsmall=k))     ## % of RE effects in HH      
  ## RE = NA   ## REtmp and out are of different length
  ## out = cbind(out_FE,RE)
  ## out$RE[1]=RE1
  ## out$RE[2]=RE2
  ##  out$RE[3]=RE3
  
  testlmer = as.vector(test@pp$X %*% fixef(test))   ## fitted values    ## length of fitted values is 4099, no adjustment is made
   VarF=var(testlmer)
  ## VarV = variance (intercept) + varaince(Slope) + 2 cov.correlation * stdev(intercept)stdev(slope)   ## variation generated from RE
  ##  RE_sdcor =  as.data.frame(VarCorr(test))$sdcor  ## stdev and correlation of two RE effects, and stdev of residuals
  ## VarV = RE_sdcor[1]^2 
  ##  VarR = RE_sdcor[2]^2      ## variation from residuals
    VarV=RE1
    VarR = RE2
    R2.m = VarF/(VarF + VarV + VarR)   ## Var(FE)/[Var(FE)+Var(RE) + Var(Res)]
    R2.c = (VarF+VarV)/(VarF + VarV + VarR)
    R2.r = VarR / (VarF + VarV + VarR) 
    R2 = NA   ## REtmp and out are of different length
    out = cbind(out_FE,R2)
    out$R2[1]=R2.m
    out$R2[2]=R2.c
    out$R2[3]=R2.r
  
  
  
#   ## for 2 REs in the model: extract RE effects from the model
#   RE_sdcor =  as.data.frame(VarCorr(test))$sdcor  ## stdev and correlation of two RE effects, and stdev of residuals
#   ##  final_FE = cbind(final_FE,out_FE)
#   ## ## final_RE = cbind(final_RE,RE_sdcor)
#   ## R squared for D1-D5 models,  for mixed with RE in intercept and slope
#   testlmer = as.vector(test@pp$X %*% fixef(test))   ## fitted values    ## length of fitted values is 4099, no adjustment is made
#   VarF=var(testlmer)
#   ##  VarV = variance (intercept) + varaince(Slope) + 2 cov.correlation * stdev(intercept)stdev(slope)   ## variation generated from RE
#   ##  or VarV = RE_sdcor[1]^2 + 2*RE_sdcor[1]*RE_sdcor[2]*RE_sdcor[3] + RE_sdcor[2]^2   ## variation generated from RE
#   VarR = RE_sdcor[4]^2      ## variation from residuals
#   R2.m = VarF/(VarF + VarV + VarR)   ## Var(FE)/[Var(FE)+Var(RE) + Var(Res)]
#   R2.c = (VarF+VarV)/(VarF + VarV + VarR)
#   R2.r = VarR / (VarF + VarV + VarR) 
#   R2 = NA   ## REtmp and out are of different length
#   out = cbind(out_FE,R2)
#   out$R2[1]=R2.m
#   out$R2[2]=R2.c
#   out$R2[3]=R2.r
#   
  final = cbind(final,out) 
}


## RE1 working codes are as above.














###### models with T1 - T2 (without income variables) 

final=0
for (i in c(3,7,9,11, 12, 16, 17) )    ## mean, q975, ammean, pktime, base, LF  (ECF has been consistent not converging, taken out)
  ## i = 16, ECF does not converge
{
  energy  = (mydata09[,i])
  ## D1: test = lmer(energy ~ num_resident + income_num.c + dummy_unemp_R + (1+income_num.c|HHID) , data=mydata, na.action="na.omit")  ## RE on intercept and slope   
  ## D2 test = lmer(energy ~ num_resident + income_num.c + (1+income_num.c|HHID), data=mydata, na.action="na.omit")  ## RE on intercept and slope   
  ## D3: test = lmer(energy ~ num_resident + income_num.c + HHtype + (1+income_num.c|HHID) , data=mydata, na.action="na.omit")  ## RE on intercept and slope   
  ##  D4: test = lmer(energy ~ num_resident + income_num.c + dummy_HHchild + (1+income_num.c|HHID) , data=mydata, na.action="na.omit")  ## RE on intercept and slope   
  
   ## T1 =  lmer(energy  ~ 1 + dum_noResident + dum_em13_46 + (1|ID),  data=mydata0910, na.action="na.omit")  ## RE on intercept only 
  
   T2 = lmer(energy  ~ 1 + dum_noResident + (1|ID),  data=mydata09, na.action="na.omit")  ## RE on intercept only 
   ## T2a = lmer( energy  ~ 1 + dum_noResident + dum_em + (1|ID),  data=mydata09, na.action="na.omit")  ## RE on intercept only 
   ## T3 = lmer(energy  ~ 1 + dum_noResident + dum_HHtype2 + dum_HHtype3 + (1|ID),  data=mydata09, na.action="na.omit") 
  
  ## T4 = lmer( energy  ~ 1 + dum_noResident + dum_child + (1|ID),  data=mydata09, na.action="na.omit")  ## RE on intercept only 
  ## T4a = lmer( energy  ~ 1 + dum_child + (1|ID),  data=mydata09, na.action="na.omit")  ## RE on intercept only 
  ## T5 = lmer( energy  ~ 1 + dum_noResident + dum_child + dum_em + (1|ID),  data=mydata09, na.action="na.omit")  ## RE on intercept only 
  ## T6 = lmer( energy  ~ 1 + dum_child + dum_noResident + dum_child*dum_noResident + (1|ID),  data=mydata09, na.action="na.omit")  ## RE on intercept only 
  
  ## T7 = lmer( energy  ~ 1 + dum_child + dum_noResident + dum_child*dum_noResident + dum_em + (1|ID),  data=mydata09, na.action="na.omit")  ## RE on intercept only 
  
  T2 = T2
  
  ## Extract fixed effect coefficients
  out = as.data.frame(cbind(as.data.frame(coef(summary(T2)))[,1],as.data.frame(coef(summary(T2)))[,3]))
  colnames(out)[1] = c(get("i"))
  colnames(out)[2] = c("zsta")
  
  ## for 1 RE only: Extract random effect varaince and percentage of ICC (intercorrelation between RE in intercept and residuals)
  ##   command:  as.data.frame(VarCorr(test)) gives  variance-covariance of the model: as.data.frame(VarCorr(test))$vcov;  std.error of the model: as.data.frame(VarCorr(test))$sdcor
  RE1 = as.data.frame(VarCorr(T2))$vcov[1]    ## variance of RE in the intercept "ID" 
  RE2 = as.data.frame(VarCorr(T2))$vcov[2]    ## varaince of residual ## 
  
  testlmer = as.vector(T2@pp$X %*% fixef(T2))   ## fitted values    ## length of fitted values is 4099, no adjustment is made
  VarF=var(testlmer)
  ## VarV = variance (intercept) + varaince(Slope) + 2 cov.correlation * stdev(intercept)stdev(slope)   ## variation generated from RE
  VarV =RE1
  VarR = RE2      ## variation from residuals
  R2.m = VarF/(VarF + VarV + VarR)   ## Var(FE)/[Var(FE)+Var(RE) + Var(Res)]
  R2.c = (VarF+VarV)/(VarF + VarV + VarR)
  R2.r = VarR / (VarF + VarV + VarR) 
  out["R2"]=NA
  out$R2[1] = R2.m
  out$R2[2] = R2.c
  out$R2[3] = R2.r
  final= cbind(final,out)
}








########  end of mixed effects models on sepearte 09 and 10 periods ###################


















#################  beginning of mixed models on both Oct 09 and Oct 10 data  #######################

######## data for 09 and 10 together  #############
HH0910 = read.table("HH_both_survey", header=T, stringsAsFactors=FALSE)    ## HH info.
tmpHH09=HH0910[HH0910$index==0,]     ## HH info in Oct09
tmpHH10=HH0910[HH0910$index==1,]     ## HH info in Oct10

tdata09=merge(eg09,tmpHH09)   ## HH and enregy infoin Oct 09 where HH has both info (HH participated in both pre and post-survey)
tdata10=merge(eg10,tmpHH10)   ## HH and enregy infoin Oct 10 where HH has both info (HH participated in both pre and post-survey)
mydata0910 = rbind(tdata09,tdata10)

mydata0910["dum_em"]=NA    ## in survey data, 1 is employed; 4 is unemployed
mydata0910 = within(mydata0910,dum_em[employment==1]<-0)  
mydata0910 = within(mydata0910,dum_em[employment==4]<-1)  
ddply(mydata0910, .(dum_em),nrow)
names(mydata0910)

mydata0910["dum_child"] = NA
mydata0910 = within(mydata0910,dum_child[HHtype==1]<-0)   
mydata0910 = within(mydata0910,dum_child[HHtype==2]<-0)   
mydata0910 = within(mydata0910,dum_child[HHtype==3]<-1)   
ddply(mydata0910, .(dum_child),nrow)
ddply(mydata0910, .(HHtype),nrow)
names(mydata0910)

## prepare the new columne,  count for the number of resdients (0/1), 
mydata0910["dum_noResident"] = NA
mydata0910 = within(mydata0910,dum_noResident[HHtype==1]<-0)   
mydata0910 = within(mydata0910,dum_noResident[HHtype==3&HHchild==1]<-0)   
mydata0910 = within(mydata0910,dum_noResident[HHtype==2&HHadult==1]<-0)   
mydata0910 = within(mydata0910,dum_noResident[HHadult>1]<-1)   
mydata0910 = within(mydata0910,dum_noResident[HHchild>1]<-1)   
ddply(mydata0910, .(dum_noResident),nrow)
names(mydata0910)


##### prepare for dummy_em, with 1-3 value of employment variable as 0, 4-6 as 1  ######
mydata0910["dum_em13_46"] = NA
mydata0910 = within(mydata0910,dum_em13_46[employment<4]<-0) ## 0 for employed people: 1: employee, 2:sel-employed with employees, 3:self-employed without employees;   
mydata0910 = within(mydata0910,dum_em13_46[employment>3]<-1)  ## 1 for unemployed people: , 4: unemployed actively seeking work; 5 unemployed not seeking work, 6: retired, 7: carer: looking after relative family)
ddply(mydata0910, .(dum_em13_46),nrow)


## prepare dummy for HHtype, HHtype2 = 1 if HHtype = 2, otherwise 0; HHtype3 = 1 if HHtype = 3, otherwise 0; 
mydata0910["dum_HHtype2"] = NA
mydata0910 = within(mydata0910,dum_HHtype2[HHtype==2]<-1) ## 0 for employed people: 1: employee, 2:sel-employed with employees, 3:self-employed without employees;   
mydata0910 = within(mydata0910,dum_HHtype2[HHtype==1]<-0) ## 0 for employed people: 1: employee, 2:sel-employed with employees, 3:self-employed without employees;   
mydata0910 = within(mydata0910,dum_HHtype2[HHtype==3]<-0) ## 0 for employed people: 1: employee, 2:sel-employed with employees, 3:self-employed without employees;   
ddply(mydata0910, .(dum_HHtype2),nrow)


mydata0910["dum_HHtype3"] = NA
mydata0910 = within(mydata0910,dum_HHtype3[HHtype==3]<-1) ## 0 for employed people: 1: employee, 2:sel-employed with employees, 3:self-employed without employees;   
mydata0910 = within(mydata0910,dum_HHtype3[HHtype==1]<-0) ## 0 for employed people: 1: employee, 2:sel-employed with employees, 3:self-employed without employees;   
mydata0910 = within(mydata0910,dum_HHtype3[HHtype==2]<-0) ## 0 for employed people: 1: employee, 2:sel-employed with employees, 3:self-employed without employees;   
ddply(mydata0910, .(dum_HHtype3),nrow)


## change dataframe structure
mydata0910 = within(mydata0910,MF<-as.factor(MF))   
mydata0910 = within(mydata0910,HHtype<-as.factor(HHtype))
mydata0910 = within(mydata0910,age<-as.factor(age))
mydata0910 = within(mydata0910,employment<-as.factor(employment))   
mydata0910 = within(mydata0910,socialclass<-as.factor(socialclass))
mydata0910 = within(mydata0910,HHadult<-as.factor(HHadult))
mydata0910 = within(mydata0910,dayault<-as.factor(dayault))
mydata0910 = within(mydata0910,dum_child<-as.factor(dum_child))
mydata0910 = within(mydata0910,daychild<-as.factor(daychild))
## mydata0910 = within(mydata0910,bedroom<-as.factor(bedroom))
## mydata0910 = within(mydata0910,heating<-as.factor(heating))
## mydata0910 = within(mydata0910,income<-as.factor(income))
mydata0910 = within(mydata0910,dum_em<-as.factor(dum_em))
mydata0910 = within(mydata0910,dum_HHtype2<-as.factor(dum_HHtype2))
mydata0910 = within(mydata0910,dum_HHtype3<-as.factor(dum_HHtype3))
mydata0910 = within(mydata0910,ID<-as.factor(ID))
mydata0910 = within(mydata0910,dum_child<-as.factor(dum_child))
mydata0910 = within(mydata0910,dum_noResident<-as.factor(dum_noResident))
mydata0910 = within(mydata0910,dum_em13_46<-as.factor(dum_em13_46))


########## mixed models for RE effect in the intercept   ############################
mydata = mydata0910[order(mydata0910$ID),]   ## sort data accoring to HH ID.
A1 = lmer( dailyammean  ~ 1 + (1|ID),  data=mydata0910, na.action="na.omit")  ## RE on intercept only 
A2 = lmer( dailyammean  ~ 1 + dum_noResident + (1|ID),  data=mydata0910, na.action="na.omit")  ## RE on intercept only 
A3 = lmer( dailyammean  ~ 1 + dum_noResident + dum_em + (1|ID),  data=mydata0910, na.action="na.omit")  ## RE on intercept only 
A4 = lmer( dailyammean  ~ 1 + dum_child + (1|ID),  data=mydata0910, na.action="na.omit")  ## RE on intercept only 
A5 = lmer( dailyammean  ~ 1 + dum_child + dum_em + (1|ID),  data=mydata0910, na.action="na.omit")  ## RE on intercept only 
A6 = lmer( dailyammean  ~ 1 + dum_child + dum_noResident + dum_child*dum_noResident + (1|ID),  data=mydata0910, na.action="na.omit")  ## RE on intercept only 
A7 = lmer( dailyammean  ~ 1 + dum_child + dum_noResident + dum_child*dum_noResident + dum_em + (1|ID),  data=mydata0910, na.action="na.omit")  ## RE on intercept only 


###############  automation to extract tables from RE  ########################## 
########## loop through one energy variable at a time ##########
final=0
for (i in c(3,7,9,11, 12, 17) )    ## mean, q975, ammean, pktime, base, LF  (ECF has been consistent not converging, taken out)
  ## i = 16, ECF does not converge
  {
  energy  = (mydata0910[,i])
  ## D1: test = lmer(energy ~ num_resident + income_num.c + dummy_unemp_R + (1+income_num.c|HHID) , data=mydata, na.action="na.omit")  ## RE on intercept and slope   
  ## D2 test = lmer(energy ~ num_resident + income_num.c + (1+income_num.c|HHID), data=mydata, na.action="na.omit")  ## RE on intercept and slope   
  ## D3: test = lmer(energy ~ num_resident + income_num.c + HHtype + (1+income_num.c|HHID) , data=mydata, na.action="na.omit")  ## RE on intercept and slope   
  ##  D4: test = lmer(energy ~ num_resident + income_num.c + dummy_HHchild + (1+income_num.c|HHID) , data=mydata, na.action="na.omit")  ## RE on intercept and slope   
  
  ## T1 =  lmer(energy  ~ 1 + dum_noResident + dum_em13_46 + (1|ID),  data=mydata0910, na.action="na.omit")  ## RE on intercept only 
  
  ## T2 = lmer(energy  ~ 1 + dum_noResident + (1|ID),  data=mydata0910, na.action="na.omit")  ## RE on intercept only 
  ## T2a = lmer( energy  ~ 1 + dum_noResident + dum_em + (1|ID),  data=mydata0910, na.action="na.omit")  ## RE on intercept only 
  ## T3 = lmer(energy  ~ 1 + dum_noResident + dum_HHtype2 + dum_HHtype3 + (1|ID),  data=mydata0910, na.action="na.omit") 
  
  T4 = lmer( energy  ~ 1 + dum_noResident + dum_child + (1|ID),  data=mydata0910, na.action="na.omit")  ## RE on intercept only 
  ## T4a = lmer( energy  ~ 1 + dum_child + (1|ID),  data=mydata0910, na.action="na.omit")  ## RE on intercept only 
  T5 = lmer( energy  ~ 1 + dum_noResident + dum_child + dum_em + (1|ID),  data=mydata0910, na.action="na.omit")  ## RE on intercept only 
  ## T6 = lmer( energy  ~ 1 + dum_child + dum_noResident + dum_child*dum_noResident + (1|ID),  data=mydata0910, na.action="na.omit")  ## RE on intercept only 
  
  ## T7 = lmer( energy  ~ 1 + dum_child + dum_noResident + dum_child*dum_noResident + dum_em + (1|ID),  data=mydata0910, na.action="na.omit")  ## RE on intercept only 
  
  T2 = T5
  
  ## Extract fixed effect coefficients
  out = as.data.frame(cbind(as.data.frame(coef(summary(T2)))[,1],as.data.frame(coef(summary(T2)))[,3]))
  colnames(out)[1] = c(get("i"))
  colnames(out)[2] = c("zsta")
  
  ## for 1 RE only: Extract random effect varaince and percentage of ICC (intercorrelation between RE in intercept and residuals)
  ##   command:  as.data.frame(VarCorr(test)) gives  variance-covariance of the model: as.data.frame(VarCorr(test))$vcov;  std.error of the model: as.data.frame(VarCorr(test))$sdcor
  RE1 = as.data.frame(VarCorr(T2))$vcov[1]    ## variance of RE in the intercept "ID" 
  RE2 = as.data.frame(VarCorr(T2))$vcov[2]    ## varaince of residual ## 
      
  testlmer = as.vector(T2@pp$X %*% fixef(T2))   ## fitted values    ## length of fitted values is 4099, no adjustment is made
  VarF=var(testlmer)
  ## VarV = variance (intercept) + varaince(Slope) + 2 cov.correlation * stdev(intercept)stdev(slope)   ## variation generated from RE
  VarV =RE1
  VarR = RE2      ## variation from residuals
  R2.m = VarF/(VarF + VarV + VarR)   ## Var(FE)/[Var(FE)+Var(RE) + Var(Res)]
  R2.c = (VarF+VarV)/(VarF + VarV + VarR)
  R2.r = VarR / (VarF + VarV + VarR) 
  out["R2"]=NA
  out$R2[1] = R2.m
  out$R2[2] = R2.c
  out$R2[3] = R2.r
  final= cbind(final,out)
}


