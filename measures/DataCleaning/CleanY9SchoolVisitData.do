/*
	_________________________________________________________________________
	
	Programmer 		   			: 	Gillian Santorelli		

	Purpose Of Program 			: 	To clean the AoW year 9 measurements data

	Date created				: 	13th July 2023
	
	Stata version				: 	17.0

	__________________________________________________________________________
*/

version 17

clear all

*------------------------------------------------------------------------------*
* Height and weight
*------------------------------------------------------------------------------*

use "U:\Born in Bradford - AOW Raw Data\redcap\measures\data\AoWYear9SchoolVisit.dta", clear

* Drop variables not required
keep date_time_collection hw_aow_id hw_height hw_weight

* Rename ID and make lower case so can merge with denominator data
gen aow_recruitment_id = lower(hw_aow_id)
drop hw_aow_id

* Merge with denominator to get sex and dob
merge m:1 aow_recruitment_id using "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\denom\data\denom_identifiable.dta", keep(3) keepusing(gender birth_date)  nogen

* Generate a date variable from date/time 
gen strdate = substr(date_time_collection, 1, 10)
gen date_measurement = date(strdate, "DMY")
format date_measurement %td
drop date_time_collection strdate

* Drop if no height and weight measurements
drop if hw_height==. & hw_weight==.

* Rename height and weight
rename hw_height height
rename hw_weight weight

* Generate BMI
gen bmi = weight/height^2 * 10000

* Check to see whether bioimpedance heights/weights are in this dataset
/* First rename master variables as I want to check them */
rename height cheight
rename weight cweight
rename bmi cbmi

merge m:1 aow_recruitment_id date_measurement using "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\measures\data\aow_bioimpedance.dta", keepusing(height weight bmi)

* For matched variables, replace any measurements missing from master
replace cheight = height if height!=. & cheight==.
replace cweight = weight if weight!=. & cweight==.
replace cbmi = bmi if bmi!=. & cbmi==.

* Check values 
sum cheight cweight cbmi, det

* Some implausible values here. Check them against bioimpedance data
sort cheight
replace cheight = height if aow_recruitment_id=="aow1059260" & date_measurement==d(12dec2022)

sort cweight
replace cweight = weight if aow_recruitment_id=="aow1077643" & date_measurement==d(25apr2023)
replace cbmi = bmi if aow_recruitment_id=="aow1077643" & date_measurement==d(25apr2023)

replace cweight = weight if aow_recruitment_id=="aow1056258" & date_measurement==d(28nov2022)
replace cbmi = bmi if aow_recruitment_id=="aow1056258" & date_measurement==d(28nov2022)

* Drop bioimpedance variables
drop height weight bmi _merge

* Rename variables again
rename cheight height
rename cweight weight
rename cbmi bmi

* Generate age variables
gen age_mths = (date_measurement - birth_date) / 30.4375
replace age_mths = floor(age_mths)
gen age_yrs = (date_measurement - birth_date) / 365.25
replace age_yrs = floor(age_yrs)
drop birth_date

* Label
lab var date_measurement "Date of measurement"
lab var age_mths "Age (months) at measurement"
lab var age_yrs "Age (years) at measurement"
lab var bmi "BMI (kg/m2)"

* Order variables
order aow_recruitment_id gender date_measurement age_mths age_yrs

* Generate z-scores (UK90)

* Check measurements	
tabstat height weight bmi, s(p50 min max) f(%9.2f)
sum height weight bmi, det

* BMI = 244 for one person
edit if bmi>240 & bmi<.
replace height=. if aow_recruitment_id=="aow1050913" & date_measurement==d(28nov2022)
replace bmi=. if aow_recruitment_id=="aow1050913" & date_measurement==d(28nov2022)

* Save
save "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\measures\data\aow_heightweight.dta", replace




*------------------------------------------------------------------------------*
* Blood pressure
*------------------------------------------------------------------------------*

use "U:\Born in Bradford - AOW Raw Data\redcap\measures\data\AoWYear9SchoolVisit.dta", clear

* Drop variables not required
keep date_time_collection bp_aow_id bp_sys_1 bp_dia_1 bp_sys_2 bp_dia_2

* Remove * from aow id
gen id=substr(bp_aow_id, 2, 10)

* Rename ID and make lower case so can merge with denominator data
gen aow_recruitment_id = lower(id)
drop bp_aow_id id

* Merge with denominator to get sex and dob
merge m:1 aow_recruitment_id using "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\denom\data\denom_identifiable.dta", keep(3) keepusing(gender birth_date) nogen

* Generate a date variable from date/time 
gen strdate = substr(date_time_collection, 1, 10)
gen date_measurement = date(strdate, "DMY")
format date_measurement %td
drop date_time_collection strdate

* Generate age variables
gen age_mths = (date_measurement - birth_date) / 30.4375
replace age_mths = floor(age_mths)
gen age_yrs = (date_measurement - birth_date) / 365.25
replace age_yrs = floor(age_yrs)
drop birth_date

* Drop if no bp measurements
drop if bp_sys_1==. & bp_dia_1==.

* Order variables
order aow_recruitment_id gender date_measurement age_mths age_yrs

* Label
rename gender sex
lab var date_measurement "Date of measurement"
lab var age_mths "Age (months) at measurement"
lab var age_yrs "Age (years) at measurement"

* Summary stats
sum bp*, det	/* all look plausible */

* Save
save "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\measures\data\aow_bp.dta", replace



*------------------------------------------------------------------------------*
* Skin folds
*------------------------------------------------------------------------------*

use "U:\Born in Bradford - AOW Raw Data\redcap\measures\data\AoWYear9SchoolVisit.dta", clear

* Drop variables not required
keep date_time_collection sk_aow_id sk_tricep sk_subscap

* Remove * from aow id
gen id=substr(sk_aow_id, 2, 10)

* Rename ID and make lower case so can merge with denominator data
gen aow_recruitment_id = lower(id)
drop sk_aow_id id

* Merge with denominator to get sex and dob
merge m:1 aow_recruitment_id using "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\denom\data\denom_identifiable.dta", keep(3) keepusing(gender birth_date) nogen

* Generate a date variable from date/time 
gen strdate = substr(date_time_collection, 1, 10)
gen date_measurement = date(strdate, "DMY")
format date_measurement %td
drop date_time_collection strdate

* Generate age variables
gen age_mths = (date_measurement - birth_date) / 30.4375
replace age_mths = floor(age_mths)
gen age_yrs = (date_measurement - birth_date) / 365.25
replace age_yrs = floor(age_yrs)
drop birth_date

* Drop if no skin fold measurements
drop if sk_tricep==. & sk_subscap==.

* Order variables
order aow_recruitment_id gender date_measurement age_mths age_yrs

* Label
rename gender sex
lab var date_measurement "Date of measurement"
lab var age_mths "Age (months) at measurement"
lab var age_yrs "Age (years) at measurement"

* Summary stats
sum sk*, det	/* all look plausible */

* Save
save "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\measures\data\aow_sk.dta", replace







