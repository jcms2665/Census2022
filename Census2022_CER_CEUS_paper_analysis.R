####################################################################################
# Data Analysis for ESRC Transformative project paper for Computers, Environment & Urban Systems
#
# - Using the Commission for Energy Regulation (CER)'s Irish Smart Meter Trial data
#   - http://www.ucd.ie/issda/data/commissionforenergyregulationcer/
#
# For the latest version of this code go to: https://github.com/dataknut/Census2022
#
# Uses CER data pre-processed using:
# https://github.com/dataknut/CER/blob/master/CER-data-processing-electricity.R
#
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
###########################################
# Housekeeping ----
# clear out all old objects etc to avoid confusion
#rm(list = ls()) 

library(data.table) # for super fast tables
library(foreign)  # to import the survey data - we've cheated and used STATA to read in and save out the .xlsx file as it 
    # converts q names and is a lot quicker!
library(psych) # useful for skew etc

###########################################
# Loading data
cerPath <- "~/Documents/Work/Data/CER Smart Metering Project/"
samplePath <- "data/original/CER_both/CER Electricity Revised March 2012/"
dPath <- "data/processed/"

# Load pre-trial survey ----
cerResPreSurvey <- paste0(
  cerPath,dPath,"Smart meters Residential pre-trial survey data.dta"
)
cerResPreSurveyDT <- as.data.table(
  read.dta(cerResPreSurvey)
)
setkey(cerResPreSurveyDT, ID)
cerResPreSurveyDT$baCompletedPreSurvey <- 1
cerResPreSurveyDT$baCompletedPreSurvey <- factor(cerResPreSurveyDT$baCompletedPreSurvey,
                                              labels = c(
                                                "Pre-trial survey"
                                              )
)

# check for NAs
with(cerResPreSurveyDT,
     table(baCompletedPreSurvey,question200pleaserecordsexf, useNA = "always")
     )

# create heating variable
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
# create number of people variable
cerResPreSurveyDT$baNpeople[cerResPreSurveyDT$question410whatbestdescribes == 1] <- "1"
cerResPreSurveyDT$baNpeople[cerResPreSurveyDT$question420howmanypeopleove == 1] <- "2"
cerResPreSurveyDT$baNpeople[cerResPreSurveyDT$question420howmanypeopleove == 2] <- "3"
cerResPreSurveyDT$baNpeople[cerResPreSurveyDT$question420howmanypeopleove == 3] <- "4"
cerResPreSurveyDT$baNpeople[cerResPreSurveyDT$question420howmanypeopleove >= 4] <- "5+"

with(cerResPreSurveyDT,
     table(baNpeople,baCompletedPreSurvey)
     )

# create a small subset of the pre trial survey
cerResPreSurveyDTred <- cerResPreSurveyDT[,.(ID,
                                             baCompletedPreSurvey,
                                             baHeat,
                                             baNpeople)]

# Load post-trial survey ----
cerResPostSurvey <- paste0(
  cerPath,dPath,"Smart meters Residential post-trial survey data.dta"
)
cerResPostSurveyDT <- as.data.table(
  read.dta(cerResPostSurvey)
)
setkey(cerResPostSurveyDT, ID)
cerResPostSurveyDT$baCompletedPostSurvey <- 1
cerResPostSurveyDT$baCompletedPostSurvey <- factor(cerResPostSurveyDT$baCompletedPostSurvey,
                                                 labels = c(
                                                   "Post-trial survey"
                                                 )
)
# check for NAs
with(cerResPostSurveyDT,
     table(baCompletedPostSurvey,question9002groups, useNA = "always")
)
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

# Load Oct 2009 consumption data ----
cerOct09 <- paste0(
  cerPath,dPath,"CER_October_2009_residential.csv"
)
cerOct09DT <- fread(cerOct09)
setkey(cerOct09DT, ID)

# Check we just have residential ----
# Use Code on consumption data
with(cerOct09DT,
     table(Code)
)

# Do additional check using sample allocation file
with(cerOct09DT[cerSampleAllocDT],
     table(Code, ba_allocation_code, useNA = "always")
)
# there are some 'Other', drop them using both allocation codes
cerOct09DTres <- cerOct09DT[cerSampleAllocDT[cerSampleAllocDT$ba_allocation_code == "Residential"]]
cerOct09DTres <- cerOct09DTres[cerOct09DTres$Code == "Residential"]
print(paste0(
  "Original Oct 09: ", length(cerOct09DT$ID), " records with ", uniqueN(cerOct09DT$ID), " unique IDs"
  )
)
print(paste0(
  "Clean Oct 09: ", length(cerOct09DTres$ID), " records with ", uniqueN(cerOct09DTres$ID), " unique IDs"
  )
)

# drop tables we don't need
cerOct09DT <- NULL

# Check for survey matches ----
# 2009
cerOct09DTres <- cerOct09DTres[cerResPreSurveyDTred]
with(cerOct09DTres,
     table(ba_allocation_code,baCompletedPreSurvey, useNA = "always")
     )

cerOct09DTres <- cerOct09DTres[ba_allocation_code == "Residential" & baCompletedPreSurvey == "Pre-trial survey"]
print(paste0("Oct 09 IDs who both answered pre trial survey and recorded data: ", uniqueN(cerOct09DTres$ID)))

# Create a useful date/time in the consumption data ----
summary(cerOct09DTres)
head(cerOct09DTres)
cerOct09DTres$r_datetime <- as.POSIXct(cerOct09DTres$datetime_start, 
                                                  tz="",
                                                  "%Y-%m-%d %H:%M:%S")
# check
head(cerOct09DTres)

# extract useful time elements
cerOct09DTres$r_date <- as.Date(cerOct09DTres$r_datetime)
cerOct09DTres$r_year <- as.POSIXlt(cerOct09DTres$r_datetime)$year # since 1900
cerOct09DTres$r_mday <- as.POSIXlt(cerOct09DTres$r_datetime)$mday
cerOct09DTres$r_wday <- as.POSIXlt(cerOct09DTres$r_datetime)$wday # Sunday = 0
cerOct09DTres$r_hour <- as.POSIXlt(cerOct09DTres$r_datetime)$hour

# create weekday, mid-week and weekend indicator
cerOct09DTres$weekend <- ifelse(cerOct09DTres$r_wday == 0 | cerOct09DTres$r_wday == 6, 
                                1, # if weekend
                                0 # if not
                                )
cerOct09DTres$weekday <- ifelse(cerOct09DTres$r_wday > 0 & cerOct09DTres$r_wday < 6, 
                                1, # if weekday
                                0 # if not
)
cerOct09DTres$mid_week <- ifelse(cerOct09DTres$r_wday > 1 & cerOct09DTres$r_wday < 5, 
                                1, # if Tues - Thurs
                                0 # if not
)
# check
table(cerOct09DTres$weekend, cerOct09DTres$r_wday, useNA = "always")
table(cerOct09DTres$weekday, cerOct09DTres$r_wday, useNA = "always")
table(cerOct09DTres$mid_week, cerOct09DTres$r_wday, useNA = "always")

# Descriptives for Table 1 (Table 1) ----
# Pre-trial survey completions:
uniqueN(cerResPreSurveyDTred$ID)
# Number of households in residential data:
uniqueN(cerOct09DTres$ID)
# Post-trial survey completions:
uniqueN(cerResPostSurveyDTred$ID)
# Number of households who completed both surveys
cerSurveysDT <- cerResPreSurveyDTred[cerResPostSurveyDTred]
table(cerSurveysDT$baCompletedPreSurvey,cerSurveysDT$baCompletedPostSurvey, useNA = "always")

# Descriptive statistics for mid-week (Table 2)
# half hour level - all
describe(cerOct09DTres[mid_week == 1, kWh])
# baseload 02:00 - 05:00
describe(cerOct09DTres[mid_week == 1 & r_hour >= 2 & r_hour <= 5, 
                       kWh
                       ]
         )
# evening peak 17:00 - 20:00
describe(cerOct09DTres[mid_week == 1 & r_hour >= 16 & r_hour <= 20,
                       kWh
                       ]
         )
# Descriptive statistics for mid-week (Table 3 - new)

# by number of people
cerOct09DTres[mid_week == 1,
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

# daily summaries
octSummarybyDateDT <- cerOct09DTres[mid_week == 1,
              .(
                N = length(kWh), # n half hour records
                Sum = sum(kWh, na.rm = TRUE)  # remove NAs
              ),
              by = .(ID,r_date)
              ][order(ID,r_date)]

describe(octSummarybyDateDT$Sum)
summary(octSummarybyDateDT)

# by heating type (not used in table)
cerOct09DTres[mid_week == 1,
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

# test heat differences
# remember skew!
#diff_heat <- kruskal.test(kWh ~baHeat, data = cerOct09DTres[mid_week == 1], na.action = na.omit )
#summary(diff_heat)
