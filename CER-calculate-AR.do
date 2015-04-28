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
global rpath "$proot/results/CER-Irish-SM-Trial"
* original files
global odfiles "$where/Data/Social Science Datatsets/CER Smart Metering Project"
* processed files
global pdfiles "$proot/data/cer"
* results path

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
use "$pdfiles/CER_OctHH_data/CER_Oct2009HH_30min_survey.dta", clear

local midweek "0 1" // 0 = weekends
local midweekt1 "Mid-week"
local midweekt0 "Weekends"


local midweektl0 xline(36 72, lstyle(refline )) 
local midweektll0 text(36 0 "This time tomorrow") text(72 0 "This time next weekend") // weekend

local midweektl1 xline(36 72 108, lstyle(refline )) 
local midweektll1 text(36 0 "This time tomorrow") text(72 0 "This time the day after tomorrow") text(108 0 "This time next week") // midweek

* this creates a big list of all IDs so we can loop over it - takes a while
* could just start from min & loop to max - but would then be testing for non-existent households
qui: levelsof ID, local(ids)

local econt1 "In work"
local econt2 "Unemployed"
local econt3 "Retired"
local econt4 "Caring for relative or family"

foreach m of local midweek {
	di "****************"
	di "* Calculating ARs for midweek = `m'"
	foreach id of local ids {
		di "* -> ID = `id'"

		preserve
			keep if ID == `id' & midweek == `m' & ds_halfhour >= 12 & ds_halfhour <= 48 // 06:00 - 24:00 -> 36 hour day
			* qui: su ds_halfhour
			* check
			* di "Halfhours: `r(min)' - `r(max)'"
			* skip over if fails due to lack of hubid
			capture noisily {
				* use s_datetime so ac knows what is missing
				qui: tsset s_datetime
				* do this here so local is set to the unique valuye for this hh
				qui: levelsof ba_nchildren, local(nch)
				qui: levelsof ba_nadults, local(ba_nadults)
				qui: levelsof ba_empl, local(econ)
				di "ID: `id' (`midweekt`m'', N = `ba_nadults' adults, `nch' children, respondent: `econt`econ'')"
				* we have 36 values 'per day' (not 48)
				* so lag 36 = this time tomorrow
				* for mid-week, this time next week = lag 108
				* for weekend, this time next week = lag 72
				* so set max lags = 144 to cover both (will also mean 2 weekends)
				* this draws graph - slow
				qui: ac kwh, gen(archr) lags(150) name(ac_hubid`id'_`midweekt`m'') ///
					`midweektl`m''/// draw lines
					`midweektll`m'' /// draw line labels
					note("ID: `id' (`midweekt`m'', N = `ba_nadults' adults, `nch' children, respondent: `econt`econ'')")
				graph export "$rpath/graphs/kwh_archr_hubid`id'_`midweekt`m''_$version.png", replace
				
				di "* save out arch results for household ID = `id'"
				qui: su t
				qui: egen lag = fill(1(1)`r(max)')
				qui: keep lag archr ID
				qui: save "$rpath/tmp/archr-`id'_`midweekt`m''_$version.dta", replace
				di "* Done"
			}
		restore
	}
}

* reset the id loop values
qui: su ID
local id_min = `r(min)'
local id_max = `r(max)'
local id = `r(min)'

clear

* pool all the results
foreach m of local midweek {
	di "****************"
	di "* Loading `midweekt`m'' files"
	while `id' < `id_max' {
		capture noisily {
			append using "$rpath/tmp/archr-`id'_`midweekt`m''_$version.dta"
			erase "$rpath/tmp/archr-`id'_`midweekt`m''_$version.dta"
		}
		* distribution of average values for first 70?
		graph box archr if lag < 144, over(lag)
		graph export "`rpath'/graphs/box_mean_archr_all_hubids_`midweekt`m''_`version'.png", replace
		save "`rpath'/archr-all_hubids_`midweekt`m''_`version'.dta", replace
	}
}


timer off 1
di "Time taken:"
timer list 1

log close

