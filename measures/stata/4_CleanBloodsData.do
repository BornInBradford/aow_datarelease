/*
	_________________________________________________________________________
	
	Programmer 		   			: 	Gillian Santorelli		

	Purpose Of Program 			: 	To clean the AoW blood test data and link to administrative data

	Date created				: 	19h September 2025
	
	Stata version				: 	17.0

	__________________________________________________________________________
*/

version 17

clear all


*------------------------------------------------------------------------------*
* Import and inspect data
*------------------------------------------------------------------------------*

// Open dataset
use "U:\Born in Bradford - AOW Raw Data\sql\bloods\data\aow_y9_bloods.dta"

// Inspect data
codebook 	

// Drop variables with high levels of missing data 
drop fasting_plasma_glucose nucleated_rb_cs_percent e_gfr_result_epi adjusted_calcium_bcp calcium phosphate fbc_comment film_comment_2 nrbc serum_glucose iron calculated_gfr unsuitable_sample unsuitable_sample_chem reticulocyte_count reticulocyte_hb reticulocyte_percent globulin total_protein film_comment

// Check values
sum cholesterol - wbc, det

// Generate a numeric date variable from date/time 
gen date_test = date(test_date, "YMD")
format date_test %td
drop test_date
order aow_recruitment_id date_test
lab var date_test "Date of blood test"

// Check for duplicates
duplicates drop

// Merge with denominator 
merge 1:1 aow_recruitment_id using "U:\Born In Bradford - Confidential\Data\BiB\processing\AoW\denom\data\denom_identifiable.dta", keep(3) nogen keepusing(gender birth_date aow_person_id BiBPersonID is_bib recruitment_era age_recruitment_y age_recruitment_m gender ethnicity_1 ethnicity_2 birth_year birth_month birth_month school_id year_group form_tutor_id)

// Generate age variables
gen age_m = (date_test - birth_date) / 30.4375
replace age_m = floor(age_m)
gen age_y = (date_test - birth_date) / 365.25
replace age_y = floor(age_y)
drop birth_date
lab var age_m "Age in months at test"
lab var age_y "Age in years at test"

// Rename some vars
rename non_hdl_cholesterol cholesterol_nonhdl
rename plasma_glucose glucose_plasma
rename total_bilirubin bilirubin_total
rename total_vitamin_d vitamin_d_total

// There are some vars that might be relating to the same test, check these
codebook bilirubin bilirubin_total
replace bilirubin = bilirubin_total if bilirubin==. & bilirubin_total!=.
drop bilirubin_total

codebook albumin albumin_bcp
replace albumin = albumin_bcp if albumin==. & albumin_bcp!=.
replace albumin=round(albumin, 0.1)
format albumin %9.1f
drop albumin_bcp


// Order variables, starting with admin data, then test data alphabetically
order aow_recruitment_id aow_person_id is_bib BiBPersonID birth_year birth_month age_recruitment_m age_recruitment_y recruitment_era school_id year_group form_tutor_id gender ethnicity_1 ethnicity_2 date_test age_m age_y albumin alp alt basophils bilirubin cholesterol cholesterol_hdl_ratio cholesterol_nonhdl creatinine eosinophils glucose_plasma haemoglobin haematocrit hba1c hdl ldl lymphocytes mch mchc mcv monocytes neutrophils platelets potassium rbc rdw sodium triglycerides vitamin_d_total urea wbc

// Compres
compress
save "U:\Born In Bradford - Confidential - Data\BiB\processing\AoW\bloods\data\Bloods.dta", replace


