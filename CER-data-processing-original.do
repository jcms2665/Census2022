/*  
**************************************************************
* Data preparation for ESRC Transformative project
* - Using the Commission for Energy Regulation (CER)'s Irish Smart Meter Trial data
*   - http://www.ucd.ie/issda/data/commissionforenergyregulationcer/

* This work was funded by RCUK through the ESRC's Transformative Social Science Programme via the
* "Census 2022: Transforming Small Area Socio-Economic Indicators through 'Big Data'" Project 
* - http://gtr.rcuk.ac.uk/project/2D2CD798-4F04-4399-B1AF-D810A233DD21
* - http://www.energy.soton.ac.uk/tag/census2022/
 
Copyright (C) 2014  University of Southampton

Author: Ben Anderson (b.anderson@soton.ac.uk, @dataknut, https://github.com/dataknut) 
	[Energy & Climate Change, Faculty of Engineering & Environment, University of Southampton]

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License 
(http://choosealicense.com/licenses/gpl-2.0/), or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

#YMMV - http://en.wiktionary.org/wiki/YMMV

*/

global where "/Users/ben/Documents/Work"
global proot "$where/Projects/ESRC-Transformative-Census2022"
global rfiles "$proot/results/CER-Irish-SM-Trial"
* original files
global odfiles "$where/Data/Social Science Datatsets/CER Smart Metering Project"
* processed files
global pdfiles "$proot/data/cer"

global version "v1"

set more off

clear all

capture log close

log using "$rfiles/CER-data-processing-$version.smcl", replace

timer clear

timer on 1

************************************
************************************
* start with the pre-trial survey
use "$odfiles/data/processed/Smart meters Residential pre-trial survey data.dta"

********
* test age, sex, employment status of chief income earner
lab def Question200PLEASERECORDSEXF 1 "Male" 2 "Female"
lab val Question200PLEASERECORDSEXF Question200PLEASERECORDSEXF
tab Question200PLEASERECORDSEXF 

lab def Question300MayIaskwhatage 1 "18-25" 2 "26-35" 3 "36-45" 4 "46-55" 5 "56-65" 6 "65+" 7 "Refused"
lab val Question300MayIaskwhatage Question300MayIaskwhatage
tab Question300MayIaskwhatage 

lab def Question310Whatistheemploym 1 "Employee" 2 "Self-employed (employees)" 3 "Self-employed (no employees)" 4 "Unemployed (seeking work)" 5 "Unemployed (not seeking work)" 6 "Retired" 7 "Carer: looking after relative or family"
lab val Question310Whatistheemploym Question310Whatistheemploym
tab Question310Whatistheemploym

lab def Question410Whatbestdescribes 1 "I live alone" 2 "All people are aged > 15" 3 "Adults & children < 18"
lab val Question410Whatbestdescribes Question410Whatbestdescribes
tab Question410Whatbestdescribes 

********
* income
* answers to 'gross hh income'
tab Question402Andconsideringinc, mi
* NB 6 = refused

* answers to banded income
tab Question4021Canyoustatewhic, mi
* NB 6 = refused

* asked of non-banded (numeric) responders: weekly/montly/yearly?
tab Question402Andconsideringinc Question403Isthatfigure, mi

* before/after tax?
* only asked of those who answered banded question
tab Question404CanIjustdoublec
* !

* start with those who gave a number 
* this has already been recoded to bands and we have to assune the weekly/monthly/yearly issue has already been dealt with
destring Question402Andconsideringinc Question4021Canyoustatewhic, force replace
* start with first income ques
gen ba_income = Question402Andconsideringinc if Question402Andconsideringinc < 6 // 6 = missing here
* have to ignore those who did not answer yearly as we cannot put them in the correct band
destring Question403Isthatfigure, force replace
replace ba_income = . if Question403Isthatfigure == 1 | Question403Isthatfigure == 2
* then add in from banded question
replace ba_income = Question4021Canyoustatewhic if Question4021Canyoustatewhic < 6 // 6 = missing here
* now remove those who said 'after tax' as everyone else is 'before tax'
replace ba_income = . if Question404CanIjustdoublec == "2"

tab ba_income Question404CanIjustdoublec, mi
tab ba_income Question403Isthatfigure, mi

*********
* floor area
su  Question6103Whatistheapprox, de
* max value = 999999999 = missing value!

* in sq feet or sq metres?
* use this to filter out missing
lab def Question61031Isthat 1 "sq m" 2 "sq feet"
destring Question61031Isthat , force generate(ds_Question61031Isthat)
lab val ds_Question61031Isthat Question61031Isthat
tab ds_Question61031Isthat
* lots  missing!

* most people answered in sq feet so keep that way
gen ba_floorarea = Question6103Whatistheapprox if ds_Question61031Isthat == 2
* 1 square metre = 10.7639104 sq feet
replace ba_floorarea = Question6103Whatistheapprox*10.76 if ds_Question61031Isthat == 1
* the large numbers tend to be sq metres?

tabstat Question6103Whatistheapprox  ba_floorarea, by(Question61031Isthat) s(mean n)

*********
* n adults
* NB this was asked if not living alone so 0 'others' = missing
destring Question420Howmanypeopleove, replace force
lab def Question420Howmanypeopleove 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7+"
lab val Question420Howmanypeopleove Question420Howmanypeopleove
tab Question420Howmanypeopleove

recode Question420Howmanypeopleove (1=1) (2=2) (3=3) (4=4) (5/max=5), gen(ba_nadults)
* in some cases this may have routed single persons into the 'how many people' question 
* in which case Question420Howmanypeopleove will be missing so need to correct
replace ba_nadults = 1 if Question410Whatbestdescribes == 1
lab def ba_nadults 1 "1" 2 "2" 3 "3" 4 "4" 5 "5+"
lab val ba_nadults ba_nadults
tab ba_nadults Question420Howmanypeopleove, mi

*********
* n kids
destring Question43111Howmanypeopleu, replace force
lab def Question43111Howmanypeopleu 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7+"
lab val Question43111Howmanypeopleu Question43111Howmanypeopleu
tab Question43111Howmanypeopleu

recode Question43111Howmanypeopleu (1=1) (2=2) (3/max=3), gen(ba_nchildren)
lab def ba_nchildren 0 "0" 1 "1" 2 "2" 3 "3+"
lab val ba_nchildren ba_nchildren
tab ba_nchildren Question410Whatbestdescribes, mi
replace ba_nchildren = 0 if Question410Whatbestdescribes == 1 | Question410Whatbestdescribes == 2 // alone or all adults (so 0 kids)
tab ba_nchildren Question410Whatbestdescribes, mi

recode Question310Whatistheemploym (1/3=1) (4/5=2) (6=3) (7=4), gen(ba_empl)
lab def ba_empl 1 "In work" 2 "Unemployed" 3 "Retired" 4 "Caring for relative or family"
lab val ba_empl ba_empl

save "$pdfiles/Smart meters Residential pre-trial survey data-$version.dta", replace

************************************
************************************
* load in the two cluster files, merge and save
insheet using "$pdfiles/October 2009 summaries/OctHH_wkend_clusterID.txt", tab clear
rename fitcluster wkend_fitcluster
lab var wkend_fitcluster "Weekend clusters" 
rename id ID
compress
save "$pdfiles/October 2009 summaries/OctHH_wkend_clusterID.dta", replace

insheet using "$pdfiles/October 2009 summaries/OctHH_midwk_clusterID.txt", tab clear
rename fitcluster midwk_fitcluster
lab var midwk_fitcluster "Mid-week clusters"
rename id ID
compress
save "$pdfiles/October 2009 summaries/OctHH_midwk_clusterID.dta", replace

merge 1:1 ID using "$pdfiles/October 2009 summaries/OctHH_wkend_clusterID.dta", nogen

* overlap between clusters?
tab wkend_fitcluster midwk_fitcluster, mi

save "$pdfiles/October 2009 summaries/OctHH_clusterIDs.dta", replace

merge 1:1 ID using "$pdfiles/Smart meters Residential pre-trial survey data-$version.dta"

* so 746 households don't match to the Oct 2009 sample leaving us with 3,486
gen oct_sample = 0
replace oct_sample = 1 if _merge == 3

save "$pdfiles/Oct-2009-summaries-survey-$version.dta", replace

******************************
* load in Sharon's daily summaries for weekdays (derived from the raw data)
* this one has spaces as delimiter
insheet using "$pdfiles/October 2009 summaries/CER_OctHH_midwk_long.txt", delim(" ") clear
destring ecf lf, replace force
gen midweek = 1
compress
save "$pdfiles/October 2009 summaries/CER_OctHH_midwk_long.dta", replace

* this one has tabs!
insheet using "$pdfiles/October 2009 summaries/CER_OctHH_wkend_long.txt", tab clear
destring ecf lf, replace force
gen midweek = 0
compress
save "$pdfiles/October 2009 summaries/CER_OctHH_wkend_long.dta", replace

* append mid-week
append using "$pdfiles/October 2009 summaries/CER_OctHH_midwk_long.dta"

rename id ID

* remove the dates that are NOT October 2009 (why are they in there anyway??)
drop if dateoct > 300

* add survey & cluster data
merge m:1 ID using "$pdfiles/Oct-2009-summaries-survey-$version.dta", gen(m_survey)

* some survey respondents not in the October data, some in October data not in survey
* keep what matches
keep if m_survey == 3
* save
save "$pdfiles/Oct-2009-daily-summaries-survey-$version.dta", replace

*********************
* Switch to 1/2 hour level data

* raw data
insheet using "$odfiles/data/original/HH2009_long.txt", delimiter(" ") names clear
rename HHID ID
rename kw kwh
tostring ds, force generate(ts_ds)
gen date = substr(ts_ds,1,3)
gen halfhour = substr(ts_ds,4,5)


* mid-week
insheet using "$pdfiles/CER_OctHH_data/CER_OctHH_mdwk_30min.txt", tab clear
li in 1/5
* the columns are munched
drop id
rename ds ID
lab var ID "ID"
rename kw timestamp
lab var timestamp "timestamp (original format)"
rename dateoct kwh
lab var kwh "kWh"
rename v5 date
lab var date "date (original format)"
li in 1/5
* need to weed out the October 2010 cases
keep if date < 365

tostring timestamp, gen(tmp_timestamp) force

gen halfhour = substr(tmp_timestamp,4,5)

tab date

* how many households do we have in this sample?
* should be same as from the clustering
preserve
	collapse (mean) kwh , by(ID)
	desc
restore

gen midweek = 1
lab def midweek 0 "Saturday/Sunday" 1 "Tuesday-Thursday"
lab val midweek midweek
drop tmp_timestamp

save "$pdfiles/CER_OctHH_data/CER_Oct2009HH_mdwk_30min.dta", replace

*******************************
* load in weekends
insheet using "$pdfiles/CER_OctHH_data/CER_OctHH_wkend_30min.txt", tab clear
li in 1/5
* the columns are munched again
drop id
rename ds ID
lab var ID "ID"
rename kw timestamp
lab var timestamp "timestamp (original format)"
rename dateoct kwh
lab var kwh "kWh"
rename v5 date
lab var date "date (original format)"
li in 1/5
* need to weed out the October 2010 cases
keep if date < 365

tostring timestamp, gen(tmp_timestamp) force

gen halfhour = substr(tmp_timestamp,4,5)
gen midweek = 0
lab val midweek midweek
drop tmp_timestamp

save "$pdfiles/CER_OctHH_data/CER_Oct2009HH_wkend_30min.dta", replace

*********************************
* append mid week to weekend
append using "$pdfiles/CER_OctHH_data/CER_Oct2009HH_mdwk_30min.dta"

* add the clustering results
merge m:1 ID using "$pdfiles/October 2009 summaries/OctHH_clusterIDs.dta", gen(m_cluster)

* fix dates properly
* we know date = 1 = Jan 1st 2009
gen double s_date = mdy(1, 1, 2009)
format s_date %td

* add the number of days but subtract 1 otherwise we will start on 2/1/2009!
replace s_date = s_date + (date - 1)

* create day of week (remember in stata 0 = Sunday)
gen s_dow = dow(s_date)

* we know halfhour 1 = 00:00
destring halfhour, force generate(ds_halfhour)

* but to get this to work we have to subtract 1 before the division
* otherwise half hour 1 starts at 00:30
gen hour = floor((ds_halfhour-1)/2)

gen mins = 0
replace mins = 30 if mod(ds_halfhour,2) == 1

gen sec = 0

* set a half hour variable (sets date to 1/1/1960!)
gen double s_halfhour = hms(hour, mins, sec)
format s_halfhour %tc

* create stata datetime
gen double s_datetime = dhms(s_date, hour, mins, sec)
format s_datetime %tc

* add the survey data (makes big file) but only keep what we need
merge m:1 ID using "$pdfiles/Smart meters Residential pre-trial survey data-$version.dta", gen(m_survey) ///
	keepusing(ba_*)

sort ID s_datetime

* check
li ID date halfhour s_* in 1/12, sep(2)

drop m_cluster timestamp date ds_halfhour halfhour hour mins sec

save "$pdfiles/CER_OctHH_data/CER_Oct2009HH_30min_survey.dta", replace


timer off 1
di "Time taken:"
timer list 1

log close