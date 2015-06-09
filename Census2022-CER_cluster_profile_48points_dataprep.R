####################################################################################
## Data Exploration for ESRC Transformative project
## - Using the Commission for Energy Regulation (CER)'s Irish Smart Meter Trial data
##   - http://www.ucd.ie/issda/data/commissionforenergyregulationcer/

## data prep to extract Oct 2009 sub-sample

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
require(plyr) 

## read in the tables of data
setwd("//soton.ac.uk/ude/PersonalFiles/Users/xl25g12/mydocuments/census2022/CER_data/unzipped/CER_Electricity_Revised_March_2012")
real1 <- read.table("file1.txt", header=F, stringsAsFactors=FALSE)
real2 <- read.table("file2.txt", header=F, stringsAsFactors=FALSE)
real3 <- read.table("file3.txt", header=F, stringsAsFactors=FALSE)
real4 <- read.table("file4.txt", header=F, stringsAsFactors=FALSE)
real5 <- read.table("file5.txt", header=F, stringsAsFactors=FALSE)
real6 <- read.table("file6.txt", header=F, stringsAsFactors=FALSE)

setwd("//soton.ac.uk/ude/PersonalFiles/Users/xl25g12/mydocuments/census2022/CER_analysis/data")
ResID = read.csv("CER_HHID.csv", header = F, dec="." )   ## file with residential ID from survey file
ID = as.vector(as.numeric(ResID$V1))   ## transfer the ResID as a vector

## midweek dates (days start from 1 = 1/1/2009)
## DateOct = c(272,273,274,279,280,281,286,287,288,293,294,295)   

## weekend dates
DateOct = c(270,276,277,283,284,290,291,297)   

Fun1 = function (x) {
  x = x[is.element(x$V1,ID),]
  x["Dateoct"]=NA  
  x$Dateoct = substring(x$V2,1,3) 
  x = x[is.element(x$Dateoct,DateOct),]
  x}

data1 = Fun1(real1)  
data2 = Fun1(real2)    
data3 = Fun1(real3)    
data4 = Fun1(real4)    
data5 = Fun1(real5)    
data6 = Fun1(real6) 

dataOct = rbind(data1,data2)
dataOct = rbind(dataOct,data3)
dataOct = rbind(dataOct,data4)
dataOct = rbind(dataOct,data5)
dataOct = rbind(dataOct,data6)
colnames(dataOct) <- c("ID","DS","KW","DateOct")

dataOct["time"] = NA  
dataOct$time = substring(dataOct$DS,4,5) 

tmp = ddply(dataOct, .(ID,time), summarize,
            time_mean = round(mean(KW), 4),
            na.rm=TRUE)  
mydata = tmp[,1:3]
## convert data to wide form
l.sort <- mydata[order(mydata$ID,mydata$time),]

w <- reshape(l.sort, 
             timevar = "time",
             idvar = c("ID"),
             direction = "wide")

setwd("//soton.ac.uk/ude/PersonalFiles/Users/xl25g12/mydocuments/census2022/CER_analysis/data")
# write.table(w, file = "CER_Oct09HH_48obs_midweek.txt", append = FALSE, sep = "\t",
#                         eol = "\n", na = "NA", dec = ".", row.names = F,
#                        col.names = TRUE, qmethod = c("escape", "double"),
#                        fileEncoding = "")
# 
# 
# write.table(w, file = "CER_Oct09HH_48obs_weekend.txt", append = FALSE, sep = "\t",
#             eol = "\n", na = "NA", dec = ".", row.names = F,
#             col.names = TRUE, qmethod = c("escape", "double"),
#             fileEncoding = "")




