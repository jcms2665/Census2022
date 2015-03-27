/*  
**************************************************************
* Data Exploration for ESRC Transformative project
* - Using the Commission for Energy Regulation (CER)'s Irish Smart Meter Trial data
*   - http://www.ucd.ie/issda/data/commissionforenergyregulationcer/
* - data explorations

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

log using "$rfiles/CER-data-processing.smcl", replace

timer clear

timer on 1

* start with the pre-trial survey
use "$odfiles/data/processed/Smart meters Residential pre-trial survey data.dta"

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

destring Question420Howmanypeopleove, replace force
lab def Question420Howmanypeopleove 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7+"
lab val Question420Howmanypeopleove Question420Howmanypeopleove
tab Question420Howmanypeopleove

destring Question43111Howmanypeopleu, replace force
lab def Question43111Howmanypeopleu 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7+" 8 "0"
lab val Question43111Howmanypeopleu Question43111Howmanypeopleu
tab Question43111Howmanypeopleu

save "$pdfiles/Smart meters Residential pre-trial survey data-$version.dta", replace

* look at Sharon's daily summaries for weekdays
* this one has spaces as delimiter
insheet using "$pdfiles/October 2009 summaries/CER_OctHH_midwk_long.txt", delim(" ") clear
compress
save "$pdfiles/October 2009 summaries/CER_OctHH_midwk_long.dta", replace

* this one has tabs!
insheet using "$pdfiles/October 2009 summaries/CER_OctHH_wkend_long.txt", tab clear
compress
save "$pdfiles/October 2009 summaries/CER_OctHH_wkend_long.dta", replace

* load in the two cluster files, merge and save
insheet using "$pdfiles/October 2009 summaries/OctHH_wkend_clusterID.txt", tab clear
rename fitcluster wkend_fitcluster
rename id ID
compress
save "$pdfiles/October 2009 summaries/OctHH_wkend_clusterID.dta", replace

insheet using "$pdfiles/October 2009 summaries/OctHH_midwk_clusterID.txt", tab clear
rename fitcluster midwk_fitcluster
rename id ID
compress
save "$pdfiles/October 2009 summaries/OctHH_midwk_clusterID.dta", replace

merge 1:1 ID using "$pdfiles/October 2009 summaries/OctHH_wkend_clusterID.dta", nogen

* overlap between clusters?
tab wkend_fitcluster midwk_fitcluster 

save "$pdfiles/October 2009 summaries/OctHH_clusterIDs.dta", replace

merge 1:1 ID using "$pdfiles/Smart meters Residential pre-trial survey data-$version.dta"

* so 746 households don't match to the Oct 2009 sample leaving us with 3,486
gen oct_sample = 0
replace oct_sample = 1 if _merge == 3

save "$pdfiles/Smart meters Residential pre-trial survey data-$version.dta", replace

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
save "$pdfiles/CER_OctHH_data/CER_Oct2009HH_mdwk_30min.dta", replace

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
save "$pdfiles/CER_OctHH_data/CER_Oct2009HH_wkend_30min.dta", replace

append using "$pdfiles/CER_OctHH_data/CER_Oct2009HH_mdwk_30min.dta"

merge m:1 ID using "$pdfiles/October 2009 summaries/OctHH_clusterIDs.dta"

sort ID timestamp

* midweek profles for midweek clusters
table halfhour midwk_fitcluster if midweek == 1, c(mean kwh)

* weekend profles for weekend clusters
table halfhour midwk_fitcluster if midweek == 0, c(mean kwh)


timer off 1
di "Time taken:"
timer list 1

