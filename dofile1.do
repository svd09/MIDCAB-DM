
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

cd "E:\MIDCAB_DM\MIDCAB-DM"

// get data.

import delim "E:\MIDCAB_DM\midcab_dm.csv", clear

codebook, compact

log close

