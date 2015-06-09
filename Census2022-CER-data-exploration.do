/*  
**************************************************************
* Data Exploration for ESRC Transformative project
* - Using the Commission for Energy Regulation (CER)'s Irish Smart Meter Trial data
*   - http://www.ucd.ie/issda/data/commissionforenergyregulationcer/

* This work was funded by RCUK through the ESRC's Transformative Social Science Programme via the
* "Census 2022: Transforming Small Area Socio-Economic Indicators through 'Big Data'" Project 
* - http://gtr.rcuk.ac.uk/project/2D2CD798-4F04-4399-B1AF-D810A233DD21
* - http://www.energy.soton.ac.uk/tag/census2022/

## For the latest version of this code go to: https://github.com/dataknut/Census2022

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

log using "$rfiles/CER-data-exploration-$version.smcl", replace

timer clear

timer on 1

*****
* use the pre-created October 2009 dataset
* see https://github.com/dataknut/Census2022/blob/master/Census2022-CER-data-processing.do

* start with the survey
use "$pdfiles/Smart meters Residential pre-trial survey data-$version.dta", clear

* simple table
tab Question420Howmanypeopleove, mi
tab Question43111Howmanypeopleu, mi
tab Question401SOCIALCLASSInterv, mi
tab Question430Andhowmanyofthe, mi // typically in - only asked in relation to the 'others' in the house!
tab Question300MayIaskwhatage, mi
tab Question310Whatistheemploym, mi
tab Question401SOCIALCLASSInterv, mi

tab ba_nchildren, mi
tab ba_nadults, mi
tab ba_empl, mi

************
* floor area
* remember lots missing and possibly mis-estimated data
su ba_floorarea, de

********
* income
* remember have to leave out people who didn't answer yearly or who gave values after tax
tab ba_income, mi

* switch to the daily summaries
* this includes Sharon's weekend & midweek cluster membership variables
use "$pdfiles/Oct-2009-daily-summaries-survey-$version.dta", clear

di "* check collinearity 'As a rule of thumb, a tolerance of 0.1 or less (equivalently VIF of 10 or greater)  is a cause for concern. '"
di "* http://www.ats.ucla.edu/stat/stata/webbooks/logistic/chapter3/statalog3.htm"

collin dailymax dailypktime dailybase dailymean dailysum dailyq975 ecf lf
di "* remove those that have tolerance < 0.1"
collin dailymax dailypktime dailybase dailymean dailyq975 ecf lf

local clusters "midwk wkend"
foreach cl of local clusters {
	di "* Testing `cl'"
	tab  `cl'_fitcluster, gen(`cl'_fitcluster_)
	* 6 clusters
	foreach n of numlist 1/6 {
		di "* Testing `cl' (`n')"
		* set iteration as risk of non-convergence
		qui: logit `cl'_fitcluster_`n' dailymax dailypktime dailybase dailymean dailyq975 ecf lf, ///
			cluster(ID) iterate(100)
		est store `cl'_fitcluster_`n'
	}
	estout `cl'_fitcluster* using "$rfiles/CER-`cl'_fitcluster-profile-indicators.txt", cells("b se p _star") stats(N r2_p chi2 p ll) replace 
}

* now use the half hour consumption data
* this was created using https://github.com/dataknut/Census2022/blob/master/CER-data-processing.do
use "$pdfiles/CER_OctHH_data/CER_Oct2009HH_30min_survey.dta", clear

* this may have fewer people as it is only October
preserve
	collapse (mean) kwh, by(ID ba_* Question* midwk_fitcluster wkend_fitcluster)
	tab Question420Howmanypeopleove, mi
	su Question420Howmanypeopleove Question43111Howmanypeopleu Question300MayIaskwhatage
	* actually it has more as there are more missing - presumably monitoring data without surveys	
	
	tab ba_empl midwk_fitcluster
	tab ba_nadults midwk_fitcluster
	tab ba_nchildren midwk_fitcluster
	
	tab ba_empl wkend_fitcluster
	tab ba_nadults wkend_fitcluster
	tab ba_nchildren wkend_fitcluster
	
	* cluster overlap?
	lab var wkend_fitcluster "Weekend clusters"
	lab var midwk_fitcluster "Mid-week clusters"
	tab midwk_fitcluster wkend_fitcluster, mi
	
	* can we predict membership of clusters from the survey data?
	* run as a series of binary logits
	* use baseoutcome = 1 as 4 turns out to be most interesting for peak demand
	tab wkend_fitcluster, gen(wkend_fitcluster_)
	tab midwk_fitcluster, gen(midwk_fitcluster_)
	* 6 clusters
	foreach c of numlist 1/6 {
		logit wkend_fitcluster_`c' i.Question300MayIaskwhatage i.ba_empl i.ba_nchildren i.ba_nadults
		est store ml_we_`c'
		logit midwk_fitcluster_`c' i.Question300MayIaskwhatage i.ba_empl i.ba_nchildren i.ba_nadults
		est store ml_mw_`c'
	}
	estout ml_we* using "$rfiles/CER-mlogit-clusters-weekend.txt", cells("b se p _star") stats(N r2_p chi2 p ll) replace
	estout ml_mw* using "$rfiles/CER-mlogit-clusters-midweek.txt", cells("b se p _star") stats(N r2_p chi2 p ll) replace
restore

tab ba_nchildren Question43111Howmanypeopleu, mi

* simple tables
local period "0 1"
local vars "ba_nchildren ba_empl"
foreach v of local vars {
	foreach p of local period {
		di "* Calculating a table for `v' for midweek == `p'"
		qui: tabout halfhour `v' using "$rfiles/CER-24hr-consumption-profile-by-`v'-weekend.txt" if midweek == `p', ///
			c(mean kwh) sum format(3) replace
	}
}

di "* midweek profles for midweek clusters"
qui: tabout halfhour midwk_fitcluster using "$rfiles/CER-24hr-consumption-profile-by-midwk_fitcluster-midweek.txt" if midweek == 1, ///
	c(mean kwh) sum format(3) replace
qui: tabout halfhour midwk_fitcluster using "$rfiles/CER-24hr-consumption-profile-by-midwk_fitcluster-midweek.txt" if midweek == 1, ///
	c(sum kwh) sum format(3) replace


di "* weekend profles for weekend clusters - mean"
qui: tabout halfhour wkend_fitcluster using "$rfiles/CER-24hr-consumption-profile-by-midwk_fitcluster-midweek.txt" if midweek == 0, ///
	c(mean kwh) sum format(3) replace
qui: tabout halfhour wkend_fitcluster using "$rfiles/CER-24hr-consumption-profile-by-midwk_fitcluster-midweek.txt" if midweek == 0, ///
	c(sum kwh) sum format(3) replace

timer off 1
di "Time taken:"
timer list 1

log close
