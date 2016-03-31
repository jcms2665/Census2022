# Header ####################################################################
# Data Analysis for paper for Computers, Environment & Urban Systems
# Uses the Commission for Energy Regulation (CER)'s Irish Smart Meter Trial data
#   - http://www.ucd.ie/issda/data/commissionforenergyregulationcer/
#
# For the latest version of this code go to: https://github.com/dataknut/Census2022

# Script uses CER data pre-processed using:
# https://github.com/dataknut/CER/blob/master/CER-data-processing-electricity.R

# Script uses autocorrelation results calculated using:
# https://github.com/dataknut/CER/blob/master/Census2022-CER-calculate-AR.do

# In addition the paper uses:

# Multilevel regression results (Table 7) calculated using:
# https://github.com/dataknut/CER/blob/master/Census2022-CER-mixed_model_0910.R

# Logistic regression results (Table 8 & 9) calculated using:
# https://github.com/dataknut/CER/blob/master/Census2022-CER_regP_48_CLUSTER_std-1-5-15.R

# This work was funded by RCUK through the ESRC's Transformative Social Science Programme via the
# "Census 2022: Transforming Small Area Socio-Economic Indicators through 'Big Data'" Project 
# - http://gtr.rcuk.ac.uk/project/2D2CD798-4F04-4399-B1AF-D810A233DD21
# - http://www.energy.soton.ac.uk/tag/census2022/
#
# Copyright (C) 2014-2016  University of Southampton
#
# Authors: Ben Anderson (b.anderson@soton.ac.uk, @dataknut), Sharon Lin (X.Lin@soton.ac.uk) 
#  [Energy & Climate Change, Faculty of Engineering & Environment, University of Southampton]
#
# This program is free software; you can redistribute it and/or modify it under the terms of the 
# GNU General Public License as published by the Free Software Foundation; either version 2 of the License 
# (http://choosealicense.com/licenses/gpl-2.0/), or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
# without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
# See the GNU General Public License or the LICENSE file in this repository for more details.
#
# YMMV - http://en.wiktionary.org/wiki/YMMV
#
# End Header ##########################################

# Housekeeping ----
# clear out all old objects etc to avoid confusion

rm(list = ls()) 

library(data.table) # for super fast tables
library(foreign)  # to import the survey data - we've cheated and used STATA to read in and save out the .xlsx file as it 
    # converts q names and is a lot quicker!
library(psych) # useful for skew etc
library(ggplot2) # for much nicer graphs

###
# Set paths and files ----
cerPath <- "~/Documents/Work/Data/CER_Smart_Metering_Project/"
samplePath <- "data/original/CER_both/CER Electricity Revised March 2012/"
dPath <- "data/processed/"
census2022Path <- "~/Documents/Work/Projects/ESRC-Transformative-Census2022/data/CER-Irish-SM-Trial/CER_OctHH_data/"

preTrialSurveyFile <- "Smart meters Residential pre-trial survey data.dta" # use STATA format file (question names de-spaced)
postTrialSurveyFile <- "Smart meters Residential post-trial survey data.dta" # use STATA format file (question names de-spaced)
consDataCensus2022 <- "CER_Census2022_Autumn_2009.csv.gz" #use the specific sub-sample 
              # 27th Sept 2009 (Sun) to 24th Oct 2009 (Sat) inclusive with the duration of 4 weeks
archrCensus2022 <- "CER_Oct2009_both_archr_all_hubids_v1.dta" # the autocorrelation coefficients per household per lag

rPath <- "~/OneDrive - University of Southampton/Papers/CEUS-2015-special-issue/v3/results/" # where to put results


#### Functions ----

loadConsumptionData <- function(){
  cerOct09 <- paste0(
    cerPath,dPath,consDataCensus2022 
  )
  # load from gzipped file
  cmd <- paste0("gunzip -c ",cerOct09)
  cerOct09DT <- fread(cmd)
  setkey(cerOct09DT, ID)
  
  # Check we just have residential ----
  # Use Code on consumption data
  with(cerOct09DT,
       table(AllocCode, useNA = "always")
  )
  
  # Create a useful date/time in the consumption data ----
  summary(cerOct09DT)
  head(cerOct09DT)
  
  print("# check first few rows")
  print(head(cerOct09DT))
  
  print("# extract useful time elements: date")
  cerOct09DT$r_date <- as.Date(cerOct09DT$r_datetime)
  print("# extract useful time elements: year")
  cerOct09DT$r_year <- as.POSIXlt(cerOct09DT$r_datetime)$year # since 1900
  print("# extract useful time elements: day of month")
  cerOct09DT$r_mday <- as.POSIXlt(cerOct09DT$r_datetime)$mday
  print("# extract useful time elements: day of week")
  cerOct09DT$r_wday <- as.POSIXlt(cerOct09DT$r_datetime)$wday # Sunday = 0
  print("# extract useful time elements: hour")
  cerOct09DT$r_hour <- as.POSIXlt(cerOct09DT$r_datetime)$hour
  
  print("# create weekday, mid-week and weekend indicator")
  cerOct09DT$weekend <- ifelse(cerOct09DT$r_wday == 0 | cerOct09DT$r_wday == 6, 
                               1, # if weekend
                               0 # if not
  )
  cerOct09DT$weekday <- ifelse(cerOct09DT$r_wday > 0 & cerOct09DT$r_wday < 6, 
                               1, # if weekday
                               0 # if not
  )
  cerOct09DT$mid_week <- ifelse(cerOct09DT$r_wday > 1 & cerOct09DT$r_wday < 5, 
                                1, # if Tues - Thurs
                                0 # if not
  )
  print("# check day of week indictaors")
  print(
    table(cerOct09DT$weekend, cerOct09DT$r_wday, useNA = "always")
  )
  print(
    table(cerOct09DT$weekday, cerOct09DT$r_wday, useNA = "always")
  )
  print(
    table(cerOct09DT$mid_week, cerOct09DT$r_wday, useNA = "always")
  )
  # globalise
  cerOct09DT <<- cerOct09DT
}

loadPreSurvey <- function() {
  cerResPreSurvey <- paste0(
    cerPath,dPath,preTrialSurveyFile
  )
  cerResPreSurveyDT <- as.data.table(
    read.dta(cerResPreSurvey)
  )
  setkey(cerResPreSurveyDT, ID)
  cerResPreSurveyDT$baCompletedPreSurvey <- 1
  cerResPreSurveyDT$baCompletedPreSurvey <- factor(cerResPreSurveyDT$baCompletedPreSurvey,
                                                   labels = c(
                                                     "Completed pre-trial survey"
                                                   )
  )
  
  print("# check for NAs")
  print(
    with(cerResPreSurveyDT,
       table(baCompletedPreSurvey,question200pleaserecordsexf, useNA = "always")
    )
  )
  # Process data ----
  
  print("# create heating variable - NB original = multi choice")
  cerResPreSurveyDT$baHeat <- ifelse(cerResPreSurveyDT$question470whichofthefollow == 1,
                                     "Electricity_storage",
                                     NA)
  cerResPreSurveyDT$baHeat <- ifelse(cerResPreSurveyDT$aq == 1,
                                     "Electricity_plug_in",
                                     cerResPreSurveyDT$baHeat)
  cerResPreSurveyDT$baHeat <- ifelse(cerResPreSurveyDT$ar == 1,
                                     "Gas",
                                     cerResPreSurveyDT$baHeat)
  cerResPreSurveyDT$baHeat <- ifelse(cerResPreSurveyDT$as == 1,
                                     "Oil",
                                     cerResPreSurveyDT$baHeat)
  cerResPreSurveyDT$baHeat <- ifelse(cerResPreSurveyDT$at == 1,
                                     "Solid_fuels",
                                     cerResPreSurveyDT$baHeat)
  cerResPreSurveyDT$baHeat <- ifelse(cerResPreSurveyDT$au == 1,
                                     "Renewables",
                                     cerResPreSurveyDT$baHeat)
  cerResPreSurveyDT$baHeat <- ifelse(cerResPreSurveyDT$av == 1,
                                     "Other",
                                     cerResPreSurveyDT$baHeat)
  
  print("# create hot water variable - NB original = multi choice")
  cerResPreSurveyDT$baHotWater <- ifelse(cerResPreSurveyDT$question4701whichofthefollo == 1,
                                         "via central heating",
                                         NA)
  cerResPreSurveyDT$baHotWater <- ifelse(cerResPreSurveyDT$ba == 1,
                                         "Gas",
                                         cerResPreSurveyDT$baHotWater)
  cerResPreSurveyDT$baHotWater <- ifelse(cerResPreSurveyDT$bb == 1,
                                         "Oil",
                                         cerResPreSurveyDT$baHotWater)
  cerResPreSurveyDT$baHotWater <- ifelse(cerResPreSurveyDT$bc == 1,
                                         "Solid_fuel",
                                         cerResPreSurveyDT$baHotWater)
  cerResPreSurveyDT$baHotWater <- ifelse(cerResPreSurveyDT$bd == 1,
                                         "Renewables",
                                         cerResPreSurveyDT$baHotWater)
  cerResPreSurveyDT$baHotWater <- ifelse(cerResPreSurveyDT$be == 1,
                                         "Other",
                                         cerResPreSurveyDT$baHotWater)
  # code these last so they over-write previous
  cerResPreSurveyDT$baHotWater <- ifelse(cerResPreSurveyDT$az == 1,
                                         "Electricity_Instantaneous",
                                         cerResPreSurveyDT$baHotWater)
  cerResPreSurveyDT$baHotWater <- ifelse(cerResPreSurveyDT$ay == 1,
                                         "Electricity_Immersion",
                                         cerResPreSurveyDT$baHotWater)
  
  print("# create number of people variable")
  cerResPreSurveyDT$baNpeople[cerResPreSurveyDT$question410whatbestdescribes == 1] <- "1"
  cerResPreSurveyDT$baNpeople[cerResPreSurveyDT$question420howmanypeopleove == 1] <- "2"
  cerResPreSurveyDT$baNpeople[cerResPreSurveyDT$question420howmanypeopleove == 2] <- "3"
  cerResPreSurveyDT$baNpeople[cerResPreSurveyDT$question420howmanypeopleove == 3] <- "4"
  cerResPreSurveyDT$baNpeople[cerResPreSurveyDT$question420howmanypeopleove >= 4] <- "5+"
  
  print(
    with(cerResPreSurveyDT,
         table(baNpeople,baCompletedPreSurvey)
    )
  )
  
  print("# create employment variable")
  cerResPreSurveyDT$ba_empl[cerResPreSurveyDT$question310whatistheemploym == 1] <- "paid_work" # employee
  cerResPreSurveyDT$ba_empl[cerResPreSurveyDT$question310whatistheemploym == 2] <- "paid_work" # self-employed, with empls
  cerResPreSurveyDT$ba_empl[cerResPreSurveyDT$question310whatistheemploym == 3] <- "paid_work" # self-employed, no empls
  cerResPreSurveyDT$ba_empl[cerResPreSurveyDT$question310whatistheemploym == 4] <- "unemployed" # unemployed seeking work
  cerResPreSurveyDT$ba_empl[cerResPreSurveyDT$question310whatistheemploym == 5] <- "unemployed" # unemployed not seeking
  cerResPreSurveyDT$ba_empl[cerResPreSurveyDT$question310whatistheemploym == 6] <- "retired" # retired
  cerResPreSurveyDT$ba_empl[cerResPreSurveyDT$question310whatistheemploym == 7] <- "carer" # carer
  
  print(
    with(cerResPreSurveyDT,
         table(ba_empl,baCompletedPreSurvey)
    )
  )

  # globalise
  cerResPreSurveyDT <<- cerResPreSurveyDT
}

loadPostSurvey <- function() {
  cerResPostSurvey <- paste0(
    cerPath,dPath,postTrialSurveyFile
  )
  cerResPostSurveyDT <- as.data.table(
    read.dta(cerResPostSurvey)
  )
  setkey(cerResPostSurveyDT, ID)
  cerResPostSurveyDT$baCompletedPostSurvey <- 1
  cerResPostSurveyDT$baCompletedPostSurvey <- factor(cerResPostSurveyDT$baCompletedPostSurvey,
                                                     labels = c(
                                                       "Completed post-trial survey"
                                                     )
  )
  # check for NAs
  with(cerResPostSurveyDT,
       table(baCompletedPostSurvey,question9002groups, useNA = "always")
  )
  # globalise 
  cerResPostSurveyDT <<- cerResPostSurveyDT
}

loadArchrResults <- function() {
  cerArchrDT <<- as.data.table(
    read.dta(paste0(census2022Path,archrCensus2022))
  )
  setkey(cerArchrDT, ID)
}

#### Controller ----
loadConsumptionData()
loadPreSurvey()
loadPostSurvey()
loadArchrResults()

# create a small subset of the pre trial survey
cerResPreSurveyDTred <- cerResPreSurveyDT[,.(ID,
                                             baCompletedPreSurvey,
                                             baHeat,
                                             baHotWater,
                                             baNpeople,
                                             ba_empl)]


# create a small subset of the post trial survey
cerResPostSurveyDTred <- cerResPostSurveyDT[,.(ID,baCompletedPostSurvey)]

# load the sample allocations
cerSampleAlloc <- paste0(
  cerPath,samplePath,"SME and Residential allocations.csv"
)
cerSampleAllocDT <- fread(cerSampleAlloc)
# set key for ease of matching
setkey(cerSampleAllocDT, ID)
# change the code name to avoid clashes
cerSampleAllocDT$ba_allocation_code <- cerSampleAllocDT$Code
cerSampleAllocDT$ba_allocation_code <- factor(cerSampleAllocDT$ba_allocation_code,
                                              labels = c(
                                                "Residential",
                                                "SME",
                                                "Other"
                                              )
)

# match checks
# Do additional check using sample allocation file
with(cerOct09DT[cerSampleAllocDT],
     table(AllocCode, ba_allocation_code, useNA = "always")
)
# Looks OK

# Check for survey matches ----
# 2009
cerOct09DT <- cerOct09DT[cerResPreSurveyDTred]
with(cerOct09DT,
     table(AllocCode,baCompletedPreSurvey, useNA = "always")
)

cerOct09DT <- cerOct09DT[AllocCode == "Residential" & baCompletedPreSurvey == "Completed pre-trial survey"]
print(paste0("Oct 09 IDs who both answered pre trial survey and recorded data: ", uniqueN(cerOct09DT$ID)))

# Linkage and analysis ----

# Descriptives for Table 1 ----
# Pre-trial survey completions:
uniqueN(cerResPreSurveyDTred$ID)
# Number of households in residential data:
uniqueN(cerOct09DT$ID)
# Post-trial survey completions:
uniqueN(cerResPostSurveyDTred$ID)
# Number of households who completed both surveys
cerSurveysDT <- cerResPreSurveyDTred[cerResPostSurveyDTred]
table(cerSurveysDT$baCompletedPreSurvey,cerSurveysDT$baCompletedPostSurvey, useNA = "always")

# Descriptive statistics for mid-week (Table 2) ----
# half hour level - all
describe(cerOct09DT[mid_week == 1, kWh])
# baseload 02:00 - 05:00
describe(cerOct09DT[mid_week == 1 & r_hour >= 2 & r_hour <= 5, 
                       kWh
                       ]
         )
# evening peak 17:00 - 20:00
describe(cerOct09DT[mid_week == 1 & r_hour >= 16 & r_hour <= 20,
                       kWh
                       ]
         )

# daily summaries for use in tables
octSummarybyDateDT <- cerOct09DT[mid_week == 1,
                                 .(
                                   N = length(kWh), # n half hour records
                                   Sum = sum(kWh, na.rm = TRUE)  # remove NAs
                                 ),
                                 by = .(ID,r_date,baHeat, baHotWater)
                                 ][order(ID,r_date)]

summary(octSummarybyDateDT)
describe(octSummarybyDateDT$Sum)
octSummarybyDateDT[,
                   .(
                     Mean_daily_total = mean(Sum)
                   ),
                   by = baHeat
][order(baHeat)] # for use in discussion of affect of heat types

# remember skew!
#diff_heat <- kruskal.test(Sum~baHeat, data = octSummarybyDateDT, na.action = na.omit )
#summary(diff_heat)

# Descriptive statistics for mid-week (Table 3 - new) ----
# by number of people
cerOct09DT[mid_week == 1,
              .(
                N_hh = uniqueN(ID), # number of households in this joined table
                N = length(kWh), # n half hour records
                Sum = sum(kWh, na.rm = TRUE),  # remove NAs
                Mean = mean(kWh, na.rm = TRUE),
                sd = sd(kWh, na.rm = TRUE),
                median = median(kWh, na.rm = TRUE),
                min = min(kWh, na.rm = TRUE),
                max = max(kWh, na.rm = TRUE),
                skew = skew(kWh, na.rm = TRUE),
                kurtosi = kurtosi(kWh, na.rm = TRUE)
              ),
              by = baNpeople,
              ][order(baNpeople)]



# by heating type (not used in table)
cerOct09DT[mid_week == 1,
              .(
                N_hh = uniqueN(ID), # number of households in this joined table (survey + consumption data)
                N = length(kWh), # n half hour records
                Sum = sum(kWh, na.rm = TRUE),  # remove NAs
                Mean = mean(kWh, na.rm = TRUE),
                sd = sd(kWh, na.rm = TRUE),
                median = median(kWh, na.rm = TRUE),
                min = min(kWh, na.rm = TRUE),
                max = max(kWh, na.rm = TRUE),
                skew = skew(kWh, na.rm = TRUE),
                kurtosi = kurtosi(kWh, na.rm = TRUE)
              ),
              by = baHeat,
              ][order(baHeat)] # order results

# box plot to investigate differences by heat type
boxplot(octSummarybyDateDT$Sum~octSummarybyDateDT$baHeat)
# ggplot version
boxHeat <- ggplot(data = octSummarybyDateDT, 
       aes(baHeat, Sum)
       )
boxHeat + geom_boxplot()
ggsave(paste0(rPath,"boxHeat.png"), width = 10, height = 10)

# by hot water heating type (not used in table)
cerOct09DT[mid_week == 1,
           .(
             N_hh = uniqueN(ID), # number of households in this joined table (survey + consumption data)
             N = length(kWh), # n half hour records
             Sum = sum(kWh, na.rm = TRUE),  # remove NAs
             Mean = mean(kWh, na.rm = TRUE),
             sd = sd(kWh, na.rm = TRUE),
             median = median(kWh, na.rm = TRUE),
             min = min(kWh, na.rm = TRUE),
             max = max(kWh, na.rm = TRUE),
             skew = skew(kWh, na.rm = TRUE),
             kurtosi = kurtosi(kWh, na.rm = TRUE)
           ),
           by = baHotWater,
           ][order(baHotWater)] # order results

# ggplot version
boxHotWater <- ggplot(data = octSummarybyDateDT, 
                  aes(baHotWater, Sum)
)
boxHotWater + geom_boxplot()
ggsave(paste0(rPath,"boxHotWater.png"), width = 10, height = 10)

# kwh by time of day for Fig 1 ----
print("Paid work")
cerOct09DT[mid_week == 1 & ba_empl == "paid_work",
           .(
             Mean = mean(kWh, na.rm = TRUE),
             sd = sd(kWh, na.rm = TRUE)
           ),
           by = r_hour,
           ][order(r_hour)] # order results

print("Unemployed")
cerOct09DT[mid_week == 1 & ba_empl == "unemployed",
           .(
             Mean = mean(kWh, na.rm = TRUE),
             sd = sd(kWh, na.rm = TRUE)
           ),
           by = r_hour,
           ][order(r_hour)] # order results

print("Retired")
cerOct09DT[mid_week == 1 & ba_empl == "retired",
           .(
             Mean = mean(kWh, na.rm = TRUE),
             sd = sd(kWh, na.rm = TRUE)
           ),
           by = r_hour,
           ][order(r_hour)] # order results

print("Carer")
cerOct09DT[mid_week == 1 & ba_empl == "carer",
           .(
             Mean = mean(kWh, na.rm = TRUE),
             sd = sd(kWh, na.rm = TRUE)
           ),
           by = r_hour,
           ][order(r_hour)] # order results

# Analysis of autocorrelation coefficients - for Model 2.3 ----
# These were calculated using STATA and then aggregated, see
# https://github.com/dataknut/CER/blob/master/Census2022-CER-calculate-AR.do

cerArchrDT <- cerArchrDT[cerResPreSurveyDTred]
# summary of archr by lag up to lag 49 (for Figure 2)
cerArchrDT[lag_id == "mid-week" & lag < 49,
           .(
             Mean = mean(archr, na.rm = TRUE),
             sd = sd(archr, na.rm = TRUE),
             median = median(archr, na.rm = TRUE),
             min = min(archr, na.rm = TRUE),
             max = max(archr, na.rm = TRUE),
             skew = skew(archr, na.rm = TRUE),
             kurtosi = kurtosi(archr, na.rm = TRUE)
           ),
           by = lag,
           ][order(lag)] # order results

archByEmpl <- cerArchrDT[lag_id == "mid-week" & lag == 36,
           .(
             Mean = mean(archr, na.rm = TRUE),
             sd = sd(archr, na.rm = TRUE),
             median = median(archr, na.rm = TRUE),
             min = min(archr, na.rm = TRUE),
             max = max(archr, na.rm = TRUE),
             skew = skew(archr, na.rm = TRUE),
             kurtosi = kurtosi(archr, na.rm = TRUE)
           ),
           by = ba_empl,
           ][order(ba_empl)] # order results

print("# Autocorrelation coefficients (summary by employment for lag == 36)")
archByEmpl

boxArchByEmpl <- ggplot(data = cerArchrDT[lag_id == "mid-week" & lag == 36], 
                      aes(ba_empl, archr)
                      ) # for discussion of model 2.3

boxArchByEmpl + geom_boxplot()
ggsave(paste0(rPath,"boxArchByEmpl.png"), width = 10, height = 10)
