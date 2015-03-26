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

local where "/Users/ben/Documents/Work"
local proot "`where'/Projects/ESRC-Transformative-Census2022"
local rfiles "`proot'/results/CER-Irish-SM-Trial"
* original files
local odfiles "`where'/Data/Social Science Datatsets/CER Smart Metering Project"
* processed files
local pdfiles "`proot'/data/cer"

set more off

clear all

capture log close

log using "`rfiles'/CER-data-processing.smcl", replace

timer clear

timer on 1

* start with the pre-trial survey
use "`odfiles'/data/Smart meters Residential pre-trial survey data.dta"

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

lab def Question43111Howmanypeopleu 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7+" 8 "0"
lab val Question43111Howmanypeopleu Question43111Howmanypeopleu
tab Question43111Howmanypeopleu

* look at Sharon's daily summaries for weekdays
* this one has spaces as delimiter
insheet using "`pdfiles'/October 2009 summaries/CER_OctHH_midwk_long.txt", delim(" ") clear
compress
save "`pdfiles'/October 2009 summaries/CER_OctHH_midwk_long.dta", replace

* this one has tabs!
insheet using "`pdfiles'/October 2009 summaries/CER_OctHH_wkend_long.txt", tab clear
compress
save "`pdfiles'/October 2009 summaries/CER_OctHH_wkend_long.dta", replace



timer off 1
di "Time taken:"
timer list 1

