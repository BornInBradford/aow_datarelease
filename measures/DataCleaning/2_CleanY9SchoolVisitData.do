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

* Merge with denominator 
merge m:1 aow_recruitment_id using "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\denom\data\denom_identifiable.dta", keepusing(gender birth_date aow_person_id  BiBPersonID is_bib recruitment_era age_recruitment_y age_recruitment_m gender ethnicity_1 ethnicity_2 birth_year birth_month birth_month school_id year_group form_tutor_id)

* Save a dataset of not linked
preserve
keep if _merge==1
keep aow_recruitment_id aow_person_id BiBPersonID date_time_collection hw_height hw_weight
count	/* n=79 */
export delimited using "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\measures\data\aow_y9schoolvisit_notlinked.csv", replace
restore

keep if _merge==3
drop _merge

* Generate a date variable from date/time 
gen strdate = substr(date_time_collection, 1, 10)
gen date_measurement = date(strdate, "YMD")
format date_measurement %td
drop date_time_collection strdate

* Drop if no height and weight measurements
drop if hw_height==. & hw_weight==.

* Rename height and weight
rename hw_height height
rename hw_weight weight

* Generate and format BMI
gen bmi = weight/height^2 * 10000
replace bmi = round(bmi, 0.1)

* Check to see whether bioimpedance heights/weights are in this dataset
/* First rename master variables as I want to check them */
rename height cheight
rename weight cweight
rename bmi cbmi

merge m:1 aow_recruitment_id date_measurement using "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\measures\data\aow_bioimpedance.dta", keepusing(height weight bmi) nogen

* For matched variables, replace any measurements missing from Y9 measurements with those from bioimpedance
replace cheight = height if height!=. & cheight==.
replace cweight = weight if weight!=. & cweight==.
replace cbmi = bmi if bmi!=. & cbmi==.

* Check values 
sum cheight cweight cbmi, det

* Some implausible values here. Check them against bioimpedance data

* Identify height differences between the datasets
gen heightdiff = abs(cheight - height)
sum heightdiff
*edit aow_recruitment_id date_measurement cheight height cweight weight cbmi bmi heightdiff if heightdiff>1 & heightdiff<.
/* Some of the cheights are clearly wrong i.e. the weight value has been entered; I will update these. For others, it is obvious from the BMI which one is incorrect. For others still, either height is feasible so I will have to drop these 
replace cheight = height if aow_recruitment_id=="aow1059260" & date_measurement==d(12dec2022)
replace cbmi = bmi if aow_recruitment_id=="aow1059260" & date_measurement==d(12dec2022)
replace heightdiff=. if aow_recruitment_id=="aow1059260" & date_measurement==d(12dec2022)

replace cheight=. if aow_recruitment_id=="aow1159037" & date_measurement==d(28nov2022)
replace cbmi=. if aow_recruitment_id=="aow1159037" & date_measurement==d(28nov2022)
replace heightdiff=. if aow_recruitment_id=="aow1159037" & date_measurement==d(28nov2022)
*/
drop if heightdiff>1 & heightdiff<.

sort cweight
*edit aow_recruitment_id date_measurement cheight height cweight weight cbmi bmi if aow_recruitment_id=="aow1077643" | aow_recruitment_id=="aow1056258" 
/*
replace cweight = weight if aow_recruitment_id=="aow1077643" & date_measurement==d(25apr2023)
replace cbmi = bmi if aow_recruitment_id=="aow1077643" & date_measurement==d(25apr2023)

replace cweight = weight if aow_recruitment_id=="aow1056258" & date_measurement==d(28nov2022)
replace cbmi = bmi if aow_recruitment_id=="aow1056258" & date_measurement==d(28nov2022)
*/

* Drop bioimpedance variables
drop height weight bmi 

* Rename variables again
rename cheight height
rename cweight weight
rename cbmi bmi

* Generate age variables
gen age_m = (date_measurement - birth_date) / 30.4375
replace age_m = floor(age_m)
gen age_y = (date_measurement - birth_date) / 365.25
replace age_y = floor(age_y)
drop birth_date

* Label
lab var date_measurement "Date of measurement"
lab var age_m "Age (months) at measurement"
lab var age_y "Age (years) at measurement"
lab var bmi "BMI (kg/m2)"
lab var aow_recruitment_id "Age of Wonder recruitment ID"

* Order variables
order aow_person_id BiBPersonID is_bib aow_recruitment_id recruitment_era age_recruitment_y age_recruitment_m gender ethnicity_1 ethnicity_2 birth_year birth_month birth_month school_id year_group form_tutor_id date_measurement age_m age_y 

* Check measurements	
tabstat height weight bmi, s(p50 min max) f(%9.2f)
sum height weight bmi, det

/* Errors:
	aow1109172 height and weight both = 175.3; remove record
	aow1095991 height=157.5, weight=158; remove record
	aow1050913 height and weight both = 40.9; remove record	
	aow1203082 weight=851.21; set weight and bmi to missing
	aow1212273 height=16, weight=66; remove record
*/

*replace height=. if aow_recruitment_id=="aow1050913" & date_measurement==d(28nov2022)
*replace bmi=. if aow_recruitment_id=="aow1050913" & date_measurement==d(28nov2022)

* Set bmi to 1dp 
replace bmi = round(bmi, 0.1)

save "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\measures\data\aow_heightweight_20240918.dta", replace




*------------------------------------------------------------------------------*
* Blood pressure
*------------------------------------------------------------------------------*

use "U:\Born in Bradford - AOW Raw Data\redcap\measures\data\AoWYear9SchoolVisit.dta", clear

* Drop variables not required
keep date_time_collection hw_aow_id bp_sys_1 bp_dia_1 bp_sys_2 bp_dia_2

* Rename ID and make lower case so can merge with denominator data
gen aow_recruitment_id = lower(hw_aow_id)
drop hw_aow_id

* Merge with denominator to get sex and dob
merge m:1 aow_recruitment_id using "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\denom\data\denom_identifiable.dta", keepusing(gender birth_date aow_person_id BiBPersonID is_bib recruitment_era age_recruitment_y age_recruitment_m gender ethnicity_1 ethnicity_2 birth_year birth_month birth_month school_id year_group form_tutor_id) nogen keep(matched)

* Generate a date variable from date/time 
gen strdate = substr(date_time_collection, 1, 10)
gen date_measurement = date(strdate, "YMD")
format date_measurement %td
drop date_time_collection strdate

* Generate age variables
gen age_m = (date_measurement - birth_date) / 30.4375
replace age_m = floor(age_m)
gen age_y = (date_measurement - birth_date) / 365.25
replace age_y = floor(age_y)
drop birth_date

* Drop if no bp measurements
drop if bp_sys_1==. & bp_dia_1==.
count	/* n=5,226 */

* Order variables
order aow_person_id BiBPersonID is_bib aow_recruitment_id recruitment_era age_recruitment_y age_recruitment_m gender ethnicity_1 ethnicity_2 birth_year birth_month birth_month school_id year_group form_tutor_id date_measurement age_m age_y 

* Label
rename gender sex
lab var date_measurement "Date of measurement"
lab var age_m "Age (months) at measurement"
lab var age_y "Age (years) at measurement"
lab var aow_recruitment_id "Age of Wonder recruitment ID"


**** Identify implausible values and export data for checking
preserve
keep aow_recruitment_id date_measurement age_y - bp_dia_2

* Summary stats
sum bp*, det	

/* Some don't look plausible. 

SPB 1st and 99th percentile = 93 and 163 (lowest and highest of both readings)
DPB 1st and 99th percentile = 54 and 119 (lowest and highest of both readings)
*/

* Generate a flag if <1st or >99th percentile
gen sbp_lowhi_percentile_flag = .
replace sbp_lowhi_percentile=1 if (bp_sys_1<93 | bp_sys_2<93) | ((bp_sys_1>163 & bp_sys_1<.) | (bp_sys_2>163 & bp_sys_2<.))

gen dbp_lowhi_percentile_flag = .
replace dbp_lowhi_percentile=1 if (bp_dia_1<54 | bp_dia_2<54) | ((bp_dia_1>119  & bp_dia_1<.) | (bp_dia_2>119 & bp_dia_2<.))


* Generate differences between first and second readings
* SBP
*gen sbpdiff = (bp_sys_1 - bp_sys_2)
*sum sbpdiff
gen sbpdiff_abs = abs(bp_sys_1 - bp_sys_2)
sum sbpdiff_abs
/*
    Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
     sbpdiff |      5,156    8.732556    9.539512          0    138.861
*/


* DBP
*gen dbpdiff = (bp_dia_1 - bp_dia_2)
*sum dbpdiff
gen dbpdiff_abs = abs(bp_dia_1 - bp_dia_2)
sum dbpdiff_abs
/*
    Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
 dbpdiff_abs |      5,156    7.029674    9.810086          0        124
*/

order bp_sys_1 bp_sys_2 sbp_lowhi_percentile sbpdiff_abs bp_dia_1 bp_dia_2 dbp_lowhi_percentile dbpdiff_abs, after(age_y)

* Flag if difference between readings is >20
gen sbpdiff_flag=.
replace sbpdiff_flag=1 if sbpdiff_abs>20 & sbpdiff_abs<.

gen dbpdiff_flag=.
replace dbpdiff_flag=1 if dbpdiff_abs>20 &  dbpdiff_abs<.

* keep flagged 
keep if sbp_lowhi_percentile_flag==1 | dbp_lowhi_percentile_flag==1 | sbpdiff_flag==1 | dbpdiff_flag==1 
count	/* n=657. This is a lot! */

/* Drop high / low readings as long as the difference is <20 */
drop if (sbp_lowhi_percentile_flag==1 & sbpdiff_abs<20) & (dbp_lowhi_percentile_flag==1 & dbpdiff_abs<20)
count	/* n=655 */

/* Drop readings where the difference is <20 */
drop if sbpdiff_abs<20 & dbpdiff_abs<20
count	/* n=574 */

export delimited using "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\measures\data\bp_checks.csv", replace
restore



/* Save
save "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\measures\data\aow_bp.dta", replace
*/


*------------------------------------------------------------------------------*
* Skin folds
*------------------------------------------------------------------------------*

use "U:\Born in Bradford - AOW Raw Data\redcap\measures\data\AoWYear9SchoolVisit.dta", clear

* Drop variables not required
keep date_time_collection hw_aow_id sk_tricep sk_subscap

* Rename ID and make lower case so can merge with denominator data
gen aow_recruitment_id = lower(hw_aow_id)
drop hw_aow_id

* Merge with denominator to get sex and dob
merge m:1 aow_recruitment_id using "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\denom\data\denom_identifiable.dta", keepusing(gender birth_date aow_person_id BiBPersonID is_bib recruitment_era age_recruitment_y age_recruitment_m gender ethnicity_1 ethnicity_2 birth_year birth_month birth_month school_id year_group form_tutor_id) nogen keep(matched)

* Generate a date variable from date/time 
gen strdate = substr(date_time_collection, 1, 10)
gen date_measurement = date(strdate, "YMD")
format date_measurement %td
drop date_time_collection strdate

* Generate age variables
gen age_m = (date_measurement - birth_date) / 30.4375
replace age_m = floor(age_m)
gen age_y = (date_measurement - birth_date) / 365.25
replace age_y = floor(age_y)
drop birth_date

* Drop if no skin fold measurements
drop if sk_tricep==. & sk_subscap==.

* Order variables
order aow_person_id BiBPersonID is_bib aow_recruitment_id recruitment_era age_recruitment_y age_recruitment_m gender ethnicity_1 ethnicity_2 birth_year birth_month birth_month school_id year_group form_tutor_id date_measurement age_m age_y 

* Label
rename gender sex
lab var date_measurement "Date of measurement"
lab var age_m "Age (months) at measurement"
lab var age_y "Age (years) at measurement"
lab var aow_recruitment_id "Age of Wonder recruitment ID"

* Summary stats
sum sk*, det	/* all look plausible */

* Save
save "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\measures\data\aow_sk.dta", replace







