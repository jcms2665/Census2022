/*  
**************************************************************
* Data Exploration for ESRC Transformative project
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

log using "$rfiles/CER-data-exploration.smcl", replace

timer clear

timer on 1

*****
* use the pre-created October 2010 dataset
* see https://github.com/dataknut/Census2022/blob/master/CER-data-processing.do

* start with the survey
use "$pdfiles/Smart meters Residential pre-trial survey data-$version.dta", clear

* simple table
tab Question420Howmanypeopleove, mi
tab Question43111Howmanypeopleu, mi
tab Question401SOCIALCLASSInterv, mi
tab Question300MayIaskwhatage, mi
tab Question310Whatistheemploym, mi
tab Question401SOCIALCLASSInterv, mi

tab ba_nchildren, mi
tab ba_empl, mi

* now use the half hour consumption data
use "$pdfiles/CER_OctHH_data/CER_Oct2009HH_30min_survey.dta", clear

* this may have fewer people as it is only October
preserve
	collapse (mean) kwh, by(ID Question200PLEASERECORDSEXF Question300MayIaskwhatage midwk_fitcluster wkend_fitcluster Question310Whatistheemploym Question410Whatbestdescribes Question420Howmanypeopleove Question43111Howmanypeopleu)
	tab Question420Howmanypeopleove, mi
	su Question420Howmanypeopleove Question43111Howmanypeopleu Question300MayIaskwhatage
	* actually it has more as there are more missing - presumably monitoring data without surveys	
	
	tab ba_empl
	* cluster overlap?
	lab var wkend_fitcluster "Weekend clusters"
	lab var midwk_fitcluster "Mid-week clusters"
	tab midwk_fitcluster wkend_fitcluster, mi
restore

tab ba_nchildren Question43111Howmanypeopleu, mi

* simple tables
* by children
bysort midweek: table halfhour ba_nchildren, c(mean kwh)
* by recoded employment status
bysort midweek: table halfhour ba_empl, c(mean kwh)

* midweek profles for midweek clusters
table halfhour midwk_fitcluster if midweek == 1, c(mean kwh)

* weekend profles for weekend clusters
table halfhour wkend_fitcluster if midweek == 0, c(mean kwh)

timer off 1
di "Time taken:"
timer list 1

