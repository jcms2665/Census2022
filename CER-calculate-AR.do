/*  
**************************************************************
* Data Exploration for ESRC Transformative project
* - Using the Commission for Energy Regulation (CER)'s Irish Smart Meter Trial data
*   - http://www.ucd.ie/issda/data/commissionforenergyregulationcer/


* Calculate the AR coefficients for mid-weeks (Tues-Thurs) and weekends to 
* add to the regression modelling approach to estimating household characteristics

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

* original files
global odfiles "$where/Data/Social Science Datatsets/CER Smart Metering Project"

* processed files
global pdfiles "$proot/data/cer/CER_OctHH_data"

* results path
global rpath "$proot/results/CER-Irish-SM-Trial"


global version "v1"

set more off

clear all

capture log close

log using "$rfiles/CER-calculate-AR-$version.smcl", replace

timer clear

timer on 1

*****
* use the pre-created October 2009 dataset
* see https://github.com/dataknut/Census2022/blob/master/CER-data-processing.do

* load 1/2 hour data (don't need survey for this)
use "$pdfiles/CER_Oct2009HH_30min_survey.dta", clear

local midweek "0 1" // 0 = weekends
local midweekt1 "Midweek"
local midweekt0 "Weekends"

* we have 36 values 'per day' (not 48)
* so lag 35 = this time tomorrow
* for mid-week, this time next week = lag 105
* for weekend, this time next week = lag 70
* so set max lags = 150 to cover both (will also mean 2 weekends)
local max_lag = 150
local midweektl0 xline(35 70, lstyle(refline )) 
* the labels don't seem to show up?
local midweektll0 text(35 0.5 "This time tomorrow", place(w)) text(70 0.5 "This time next weekend", place(e)) // weekend

local midweektl1 xline(35 70 105, lstyle(refline )) 
* the labels don't seem to show up?
local midweektll1 text(35 0 "This time tomorrow", place(w)) text(70 0 "This time the day after tomorrow", place(w)) text(105 0 "This time next week", place(w)) // midweek

local econt1 "In work"
local econt2 "Unemployed"
local econt3 "Retired"
local econt4 "Caring for relative or family"

* filter for 
gen s_hour = hh(s_halfhour)

* this creates a big list of all IDs so we can loop over it - takes a while
* could just start from min & loop to max - but would then be testing for non-existent households
qui: levelsof ID, local(ids)

* for testing
* local ids "1002 1003 1004"

foreach m of local midweek {
	di "****************"
	di "* Calculating ARs for midweek = `m' (`midweekt`m'') " 
	foreach id of local ids {
		di "* -> ID = `id'"

		preserve
			* take out 00:00 - 06:00 as repeat cycle of sleep will swamp other cycles
			keep if ID == `id' & midweek == `m' & s_hour > 6
			* need to fool ac into thinking these observations are continuous
			qui: su ID
			qui: egen lag = fill(1(1)`r(N)')
			tsset lag
			* do this here so local is set to the unique valuye for this hh
			qui: levelsof ba_nchildren, local(nch)
			qui: levelsof ba_nadults, local(ba_nadults)
			qui: levelsof ba_empl, local(econ)
			qui: su s_hour
			di "Halfhours: `r(min)' - `r(max)' for ID: `id' (`midweekt`m'', N = `ba_nadults' adults, `nch' children, respondent: `econt`econ'')"
			* this draws graph - slow
			qui: ac kwh, gen(archr) lags(`max_lag') name(ac_hubid`id'_`midweekt`m'') ///
				`midweektl`m'' /// draw lines
				`midweektll`m'' /// draw line labels
				xlabel(0(35)140) ///
				note("Halfhours: `r(min)' - `r(max)' for ID: `id' (`midweekt`m'', `ba_nadults' adult(s), `nch' children, respondent: `econt`econ'')")
			graph export "$rpath/graphs/kwh_archr_hubid`id'_`midweekt`m''_$version.png", replace
			
			di "* save out arch results for household ID = `id'"
			qui: keep lag archr ID
			qui: save "$rpath/tmp/archr-`id'_`midweekt`m''_$version.dta", replace
			di "* Done"
		restore
	}
}

clear

* pool all the results
foreach m of local midweek {
	di "****************"
	di "* Loading `midweekt`m'' files"
	foreach id of local ids {
		append using "$rpath/tmp/archr-`id'_`midweekt`m''_$version.dta"
		erase "$rpath/tmp/archr-`id'_`midweekt`m''_$version.dta"
	}
	* distribution of average values for first 70?
	graph box archr if lag <= 105, over(lag)
	graph export "$rpath/graphs/box_mean_archr_all_hubids_`midweekt`m''_$version.png", replace
	save "$rpath/archr-all_hubids_`midweekt`m''_$version.dta", replace
}


timer off 1
di "Time taken:"
timer list 1

log close

