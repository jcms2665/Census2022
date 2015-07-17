/*  
**************************************************************
* Data Exploration for ESRC Transformative project
* - Using One-Minute Resolution Domestic Electricity Use Data, 2008-2009 
*   - http://discover.ukdataservice.ac.uk/catalogue?sn=6583
* - Comparing energy demand profiles for different kinds of households
* Loops over March data producing summary graphs for each day of the week
* by whatever category you set up (or all of them)
* use medians through-out

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


local where "~/Documents/Work"
local projroot "`where'/Projects/ESRC-Transformative-Census2022"
local rfiles "`projroot'/results/Lboro1min"
local dfiles "`where'/Data/Social Science Datatsets/6583 - One-Minute Resolution Domestic Electricity Use Data 2008-2009/processed/"

local pfile "`dfiles'/UKDA-6583_power_all_years_all_hhs_long_fillfull_wf_Feb_Jun_2008.dta"
local sfile "`dfiles'/UKDA-6583-survey-data-wf.dta"


* Loops over March data producing summary graphs for each day of the week
* by whatever category you set up (or all of them)
* use medians through-out

set more off

clear all

capture log close

log using "`rfiles'/Lboro1min-consumption-by-hh-category-March-2008-descriptives.log", replace

* households can be grouped by:
* ba_econ7 ba_timers ba_npeople ba_accom hh_id
local category "ba_npeople c_npeople c_accom"

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

* keep March 2008
keep if tin(1mar2008 00:00, 31mar2008 23:59)

merge m:1 hh_id using "`sfile'"

tabstat import_kw, by(ba_npeople) s(mean sd semean n min p50 max)
tabstat import_kw, by(c_npeople) s(mean sd semean n min p50 max)
tabstat import_kw, by(c_accom) s(mean sd semean n min p50 max)

* use census definition here (for clarity in graphs)
collapse (mean) power_m=import_kw (p50) power_p50=import_kw, by(ba_30m_slot c_npeople ba_dow)

* create nice slot variable with text as labels
encode ba_30m_slot, gen(ba_30m_slotl)

* npeople
sort c_npeople ba_30m_slotl

local days "1 2 3 4 5 6 7"
local days1 "Mondays in March 2008"
local days2 "Tuesdays in March 2008"
local days3 "Wednesdays in March 2008"
local days4 "Thursdays in March 2008"
local days5 "Fridays in March 2008"
local days6 "Saturdays in March 2008"
local days7 "Sundays in March 2008"

graph drop _all
foreach day of local days {
	preserve
	di "* Drawing graphs by ba_npeople for `days`day''"
	keep if ba_dow == `day' 
		twoway line power_p50 ba_30m_slotl if c_npeople == 1, ///
			legend(label (1 "1 person")) note("Data: `days`day''") ytitle("Median Kw") ///
			xtitle("Time of day") name(l_`day'_n_people) ///
			|| line power_p50 ba_30m_slotl if c_npeople == 2, legend(label(2 "2 people")) ///
			|| line power_p50 ba_30m_slotl if c_npeople == 3, legend(label(3 "3 people")) ///
			|| line power_p50 ba_30m_slotl if c_npeople == 4, legend(label(4 "4 people")) /// 
			|| line power_p50 ba_30m_slotl if c_npeople == 5, legend(label(5 "5+ people"))
		graph export "`rfiles'/l_`days`day''_c_npeople.png", replace
		
	restore
}	

* do the same for timers 

use "`pfile'", clear

* keep March 2008
keep if tin(1mar2008 00:00, 31mar2008 23:59)

merge m:1 hh_id using "`sfile'"

collapse (mean) power_m=import_kw (p50) power_p50=import_kw, by(ba_30m_slot ba_timers ba_dow)

* create nice slot variable with text as labels
encode ba_30m_slot, gen(ba_30m_slotl)

sort ba_timers ba_30m_slotl

foreach day of local days {
	preserve
	di "* Drawing graphs by ba_npeople for `days`day''"
	keep if ba_dow == `day' 
		twoway line power_p50 ba_30m_slotl if ba_timers == 0, ///
			legend(label (1 "Does not use timers")) note("Data: `days`day''") ytitle("Median Kw") ///
			xtitle("Time of day") name(l_`day'_timers) ///
			|| line power_p50 ba_30m_slotl if ba_timers == 1, legend(label(2 "Uses timers")) 
						
		graph export "`rfiles'/l_`days`day''_ba_timers.png", replace
	restore
}	

* & accomodation
use "`pfile'", clear

* keep March 2008
keep if tin(1mar2008 00:00, 31mar2008 23:59)

merge m:1 hh_id using "`sfile'"

collapse (mean) power_m=import_kw (p50) power_p50=import_kw, by(ba_30m_slot c_accom ba_dow)

* create nice slot variable with text as labels
encode ba_30m_slot, gen(ba_30m_slotl)

sort c_accom ba_30m_slotl

foreach day of local days {
	preserve
	di "* Drawing graphs by ba_econ7 for `days`day''"
	keep if ba_dow == `day' 
		twoway line power_p50 ba_30m_slotl if c_accom == 1, ///
			legend(label (1 "Detached")) note("Data: `days`day''") ytitle("Median Kw") ///
			xtitle("Time of day") name(l_`day'_accom) ///
			|| line power_p50 ba_30m_slotl if c_accom == 2, legend(label(2 "Semi-Detached")) ///
			|| line power_p50 ba_30m_slotl if c_accom == 3, legend(label(3 "Terraced")) 
						
		graph export "`rfiles'/l_`days`day''_c_accom.png", replace
	restore
}	

timer off 1
di "Time taken:"
timer list 1

