/*  
**************************************************************
* Data Exploration for ESRC Transformative project
* - Using One-Minute Resolution Domestic Electricity Use Data, 2008-2009 
*   - http://discover.ukdataservice.ac.uk/catalogue?sn=6583
* - Comparing energy demand profiles for different kinds of households
* Loops over March data producing regression models

## For the latest version of this code go to: https://github.com/dataknut/Census2022

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
local projroot "`where'/Projects/ESRC-Transformative-Census2022"
local rfiles "`projroot'/results/Lboro1min"
local dfiles "`where'/Data/Social Science Datatsets/6583 - One-Minute Resolution Domestic Electricity Use Data 2008-2009/processed/"

local pfile "`dfiles'/UKDA-6583_power_all_years_all_hhs_long_fillfull_wf_Feb_Jun_2008.dta"
local sfile "`dfiles'/UKDA-6583-survey-data-wf.dta"

set more off

clear all

capture log close

log using "`rfiles'/Lboro1min-consumption-by-hh-category-March-2008-models.log", replace

* households can be grouped by:
* ba_econ7 ba_timers ba_npeople ba_accom hh_id
local category "ba_npeople"

use "`sfile'", clear

foreach c of local category {
	di "* Table for `c'"
	tab `c'
}

timer clear

timer on 1

* it has to be hard coded due to the differing legend labels required etc
* although there must be a better way of doing it!

use "`pfile'", clear

merge m:1 hh_id using "`sfile'"

* keep March 2008
keep if tin(1mar2008 00:00, 31mar2008 23:59)

collapse (mean) power_m=import_kw ba_npeople (p50) power_p50=import_kw (sd) power_sd=import_kw, by(ba_30m_slot hh_id ba_dow)

merge m:1 hh_id using "`sfile'"

* create nice slot variable with text as labels
encode ba_30m_slot, gen(ba_30m_slotl)

* npeople
sort ba_npeople ba_30m_slotl

local days "1 2 3 4 5 6 7"
local days1 "Mondays in March 2008"
local days2 "Tuesdays in March 2008"
local days3 "Wednesdays in March 2008"
local days4 "Thursdays in March 2008"
local days5 "Fridays in March 2008"
local days6 "Saturdays in March 2008"
local days7 "Sundays in March 2008"

local tests "power_m power_p50 power_sd"
* overall model
foreach t of local tests {
	regress ba_npeople `t' if ba_dow > 2, vce(cluster hh_id)
	linktest
	est store est_`t'_wd
	predict ba_npeople_wdp_`t'
	graph box ba_npeople_wdp_`t', over(ba_npeople) name(box_wdp`t')
}	
pwcorr ba_npeople ba_npeople_wdp_* 

foreach t of local tests {
	preserve
		keep hh_id ba_dow `t' ba_30m_slotl
		reshape wide `t', i(hh_id ba_dow) j(ba_30m_slotl)
		di "Merge survey data"
		merge m:1 hh_id using "`sfile'"
		di "* Model: `t' (Weekdays ToD)"
		regress ba_npeople `t'* if ba_dow > 2, vce(cluster hh_id)
		linktest
		est store est_`t'_todwd
		predict ba_npeople_todwdp_`t'
		graph box ba_npeople_todwdp_`t', over(ba_npeople) name(box_todwdp`t')
		pwcorr ba_npeople ba_npeople_todwdp_`t' 
	restore
}

foreach t of local tests {
	* time of day model
	estout est_`t'_* using "`rfiles'/models-`t'-$S_DATE.txt", cells("b se _star") stats(r2 N) replace
}

timer off 1
di "Time taken:"
timer list 1

