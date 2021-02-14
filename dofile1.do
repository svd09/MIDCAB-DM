
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

// we have already removed patients to keep only SVD patients. However
// we also need to remove those with hybrid revasc done.

tab total_hybrid

drop if total_hybrid == 1



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


table1_mc, by(mets) vars (convert_onpump bin \ convert_sternotomy bin \  ///
surg_time conts \  post_iabp bin \ post_ecmo bin \ post_mi bin \ ///
postop_cpr bin \ postop_af bin \ post_bypass_revise bin \  ///
bleeding bin \ stroke bin \ post_dialysis bin \ vent_failure bin \ trach bin \ ///
los conts \ died_inhouse bin) nospace onecol percent_n  saving("D:/MIDCAB_DM/table1_2.xlsx", replace )

// stset data .

help stset

stset fupyears, fail(died_total)

sts graph, by(mets) risktable

// not much difference in survival between groups.
// plan to use the rstmp2 model with hazard scale 

stpm2 mets copd sex smoke preop_dialysis pvd i.lvfunction prior_pci, scale(hazard) df(5) eform

// now to do the same for diabetes and then obesity.
// for obesity, we can see the impact of BMI on early outcome.

tab diabetes 

tab1 diabetes ohga insulin newer_antidm // make var for nodm, nonitdm, itdm

gen dm_group = .

replace dm_group = 0 if (diabetes == 0)
replace dm_group = 1 if (diabetes == 1 & insulin == 0)
replace dm_group = 2 if (diabetes == 1 & insulin == 1)

tab dm_group // 0 - no dm, 1 - non-itdm, 2 - itdm patients.

stset fupyears, fail(died_total)

sts graph, by(dm_group) risktable // different survival in all 3 groups.

// make table 1 for dm and no dm patients.

table1_mc, by(diabetes) vars(age conts \ sex cat \ ///
bmi conts \ diabetes bin \ ///
preop_creat_mg conts \ ///
preop_aspirin bin \ preop_plavix bin \ preop_dialysis bin \ ///
smoke bin \ copd bin \ hyperlip bin \ pvd bin \ ///
lvef conts \ lvfunction cat \ priority cat \ ///
preop_stroke bin \ prior_ohs bin \ ///
preop_mi bin \ prior_pci bin \ ///
log_euroscore conts \ ///
prior_pci bin) nospace percent onecol ///
missing saving("D:/MIDCAB_DM/table1_dm.xlsx", replace)

// to look at operative and early postoperative outcome for DM patients.


table1_mc, by(diabetes) vars (convert_onpump bin \ convert_sternotomy bin \  ///
surg_time conts \  post_iabp bin \ post_ecmo bin \ post_mi bin \ ///
postop_cpr bin \ postop_af bin \ post_bypass_revise bin \  ///
bleeding bin \ stroke bin \ post_dialysis bin \ vent_failure bin \ trach bin \ ///
los conts \ died_inhouse bin) nospace onecol percent_n  saving("D:/MIDCAB_DM/table2_dm.xlsx", replace )

// determine age for every ten years.

gen age_10 = age/10

stset fupyears, fail(died_total)

stpm2 age_10 diabetes copd sex smoke preop_dialysis pvd i.lvfunction prior_pci, scale(hazard) df(5) eform

// diabetes is done, now to focus on obesity as an independent variable.

summ bmi, detail

// really large value as highest, so replace at 99th percentile.

replace bmi = 51.2 if (bmi > 53 & bmi < .) 

summ bmi, detail // now bmi values are reasonable.

// create tables according to obesity.


table1_mc, by(obese) vars(age conts \ sex cat \ ///
bmi conts \ diabetes bin \ ///
preop_creat_mg conts \ ///
preop_aspirin bin \ preop_plavix bin \ preop_dialysis bin \ ///
smoke bin \ copd bin \ hyperlip bin \ pvd bin \ ///
lvef conts \ lvfunction cat \ priority cat \ ///
preop_stroke bin \ prior_ohs bin \ ///
preop_mi bin \ prior_pci bin \ ///
log_euroscore conts \ ///
prior_pci bin) nospace percent onecol ///
missing saving("D:/MIDCAB_DM/table1_ob.xlsx", replace)



table1_mc, by(obese) vars (convert_onpump bin \ convert_sternotomy bin \  ///
surg_time conts \  post_iabp bin \ post_ecmo bin \ post_mi bin \ ///
postop_cpr bin \ postop_af bin \ post_bypass_revise bin \  ///
bleeding bin \ stroke bin \ post_dialysis bin \ vent_failure bin \ trach bin \ ///
los conts \ died_inhouse bin) nospace onecol percent_n ///
saving("D:/MIDCAB_DM/table2_ob.xlsx", replace )

sts graph, by(obese)

stpm2 age_10 obese diabetes htn ///
hpl copd sex smoke preop_dialysis ///
pvd i.lvfunction prior_pci, scale(hazard) df(5) eform


// save this dataset.

save "D:/MIDCAB_DM/dataset2.dta", replace 

// graphs for the paper have been created with R.
// plan to maybe do a spline model with bmi.

// 02/14/2021 - Happy Valentines Day.

use "D:/MIDCAB_DM/dataset2.dta", replace 

// create spline term and then run the stpm2 model using spline term.


mkspline bmi_sp = bmi, cubic nknots(3)
mat bmi_knots = r(knots)

stset fupyears, fail(died_total)

stpm2 age_10 bmi_sp* diabetes htn ///
hpl copd sex smoke preop_dialysis ///
pvd i.lvfunction prior_pci, scale(hazard) df(5) eform


// after creating model with using bmi as spline term, graph it...

xbrcspline bmi_sp , values(17(0.5)50) ref(30) eform matknots(bmi_knots) gen(ctn hr lb ub) level(95)

