
/********************************************************************************
MIDCAB outcomes in patients with diabetes mellitus
********************************************************************************/
// at the beginning of every do file:
macro drop _all // remove macros from previous work, if any
capture log close // Close any open logs. Capture will ignore a command that gives 
//                   an error. So if there isn't an open log, instead of giving you 
//                   an error and stopping here, it'll just move onto the next line.
clear all // clean the belfries
drop _all // get rid of everything!

//log using output.log, replace text // change the name of this to whatever you'd like

// The purpose of this .do file is... [say why you are writing this do file]

version 16 // Every version of Stata is slightly different, but all are backwards 
//            compatible with previous ones. If you open up this do file with a way 
//            newer version, it'll run it in version 14 compatibility mode. Change 
//            this to the current version of Stata that you are using. This will 
//            also keep your code from running on older versions of stata that will 
//            break with new code that it isn't designed to handle. 

set more off, permanently // so you don't have to keep clicking through stata to 
//                           keep it running

set linesize 255 // this keeps longer lines from getting clipped. Helpful for making 
//                  tables.

// set dir 

// cd "E:\MIDCAB_DM\MIDCAB-DM"

// get data.

import delim "D:/MIDCAB_DM/midcab_dm2.csv", clear 

codebook, compact

// label some variables

label variable v106 "followup stroke"

rename v106 followup_stroke

// before going any further, we need to creat the variable for metabolic syndrome
/* met_s defined as 3/4 --- obesity by bmi, dm, htn, presence of hyperlipedemia 
in our study we have hyperlip == 0/1, so we can select that as 2 points, we can say that these 
patients were statin treated and hence assume that they get 2 points / then we need 1 more point 
from obesity, dm, htn. */ 

gen hpl_2 = 2*hyperlip

// gen obesity.

gen obese = .

replace obese = 1 if (bmi > 30 & bmi < . & sex == 1)
replace obese = 1 if (bmi > 25 & bmi < . & sex == 2)  

replace obese = 0 if (bmi <= 30 & sex == 1)
replace obese = 0 if (bmi <= 25 & sex == 2)

tab obese, missing

gen score = .

replace score = hpl_2 + diabetes + htn + obese

misstable summ score

summ score 

// now to get mets variable 

gen mets =  .

replace mets = 1 if (score > 2 & score < 6)

replace mets = 0 if (score < 3)

tab mets 

misstable summ mets

// from this 14 patients have missing data for mets_s and hence will be removed from analysis.

drop if mets == .

save "D:/MIDCAB_DM/data2", replace 

// now to go ahead with analysis.
// convert time to years and see outcome for mets / just preliminary analysis prior to further models.


// 02/11/2021 ---

use "D:/MIDCAB_DM/data2.dta", replace

gen fupyears = .

replace fupyears = (1 + survival_days)/365.24

rename lvfuncion lvfunction 

destring preop_egfr, gen(pre_gfr) force


// need to create table 1. all variables with -9 were already handled with R.

table1_mc, by(mets) vars(age contn \ sex cat \ bmi contn \ diabetes bin \ ///
ohga bin \ insulin bin \ preop_creat_mg contn \ ///
preop_aspirin bin \ preop_plavix bin \ preop_dialysis bin \ ///
smoke bin \ copd bin \ hyperlip bin \ pvd bin \ ///
lvef contn \ lvfunction cat \ priority cat \ ///
prior_pci bin) nospace percent onecol  missing saving("D:/MIDCAB_DM/table1.xlsx", replace)

// table of preop vars according to metabolic syndrome.

table1_mc, by(mets) vars( preop_stroke bin \ prior_ohs bin \ ///
preop_mi bin \ prior_pci bin \ ///
log_euroscore conts ) nospace percent saving("D:/MIDCAB_DM/table1_1.xlsx", replace)

// see if we have hybrid procedures 

tab total_hybrid  // ? remove hybrid procedures ??

table1_mc, by(mets) vars(convert_onpump bin \ convert_sternotomy bin \  ///
surg_time conts \  post_iabp bin \ post_ecmo bin \ post_mi bin \ ///
postop_cpr bin \ postop_af bin \ post_bypass_revise bin \  ///
bleeding bin \ stroke bin \ post_dialysis bin \ vent_failure bin \ trach bin \ ///
los conts ) nospace onecol percent_n  saving("D:/MIDCAB_DM/table1_2.xlsx", replace )

// stset data .

help stset

stset fupyears, fail(died_total)

sts graph, by(mets) risktable

// 


sts graph, by(diabetes) risktable 

