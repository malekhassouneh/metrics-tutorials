// Tutorial #1 ECO375 23/9/2022
*******************************

/* Stolen from Prepared Material

Part 1: What is the goal of the tutorials (5 minutes) 

Prepare for assignments
Learn the best coding practices
Apply the concepts seen in class
Learn how to estimate models and interpret the results
Improve coding abilities (very, very useful)
Review the class material in an empirical fashion
Introduce the best practices for data visualization

Part 2: What is Stata? Why Stata? (5-10 minutes) 

Show the students around (console, history of commands, do-file, drop-down menus, variables, etc.)
Stata is the most used statistical software in econometrics
Students will HAVE to use Stata for their assignements
Cutting-edge methods and models are usually implemented */

/* Notes:
1) Please Google!! No one knows everything (but Nick Cox)
2) Or data PDF manuals
3) Abbreviating functions/commands is common practice */

/* Prelims Misc:

1) Stata UI
2) Stata dark mode in Preferences
3) do-file editor
4) running commands (ctrl-D or "execute" button)
5) Stata is case sensitive (in general, file names?)

* comment on a seperate line (>1 *, **, ***)
// to comment after code (>2 ///, ////)
/* write code in between, create minimizable sections */  */

***************************
// Stata Basics and Prelims
***************************

** Log File ** (which you need to submit along with a do-file for PSs)
// Find where log file is using 'cd'

capture log close // close any log running previously 
capture noisily log using "eco375_code_hw1_winter2021.log", replace 
capture log close // end the log after code

// noisily vs quietly (show and suppress output), ,replace to overwrite existing file with same name

** Read in Stata Data ** (Usually not)

sysuse auto, clear // ,clear to clear data before loading dataset
// OR
clear 
clear all // clears variables and programs defined (see later)

** Setting Working Directory Using cd "change directory" and Reading in Data**
cd "C:\Users\malek\Desktop\TAship\ECO375H5F\My Tutorials\Tutorial 1" //Paste wd here, go to file and copy directory bar
use wage1 //Paste file name w/o extension, use only applies to .dta files STATA data
// OR just load file w/o wd (inconvenient for later saving files/graphs)
use "C:\Users\malek\Desktop\TAship\ECO375H5F\My Tutorials\Tutorial 1\WAGE1.dta"

// What if data not in .dta form? Make one: File -> Import
// Then save the data, e.g.:
save WAGE1, replace //,replace as explained above

** Help **
help
help //name of command

***********************************
// Useful Commands (not exhaustive)
***********************************

//Displaying and Editing Data 
browse // float/double vs integer/byte, levels of precision 
edit

//Describing the Data (ALL variables loaded)
describe  // labels in Data -> Variables manager
summarize// descriptive statistics

//Display Variables
list wage female in 1/10 // "in" defines a subset of data

//Describing Variables
describe wage
summarize wage // ,detail for more, skewness, percentiles etc.
table female // frequency table

// Conditional Statements
browse if wage > 10
su wage if wage > 10 & female == 0 // Abbreviate!!

// Generate, Define, Drop a (new) Variable (egen > gen)
egen mean_wage = mean(wage) // Check browse to see what is defined!, whole column
gen v=. // empty column useful for looping later
display mean_wage
drop mean_wage

// Other Summary Statistics and Commands
corr educ wage
tabulate female, summarize(wage) // categorize summary stats not frequency
di invnormal(0.975) // must include di for all operations
replace wage = 10000 if wage > 1 // Also useful, e.g. for missing values .

// Graphing (how to save/export, see below)
scatter wage educ
hist wage

// Loops
foreach i in 1 2 3 {
	gen wage_`i' = wage^`i' // can use index for labelling, mathematical operators or if conditions
}
// foreach loop can be used for indexing with strings/characters not necessarily numbers (not often the case, just defining variables)

forvalues i=1/10 {
	gen wage_`i' = wage^`i'
}
// more useful for our cases
// Stata will stop you if you are running code that defines a same variable as before

***********************************
// Monte Carlo Simulations (PS Question) (CLT Dem.)
***********************************

set seed 123 //for replicability, DON'T set within a loop, get degenerate results

// Write a program/function (alternative to loops) (for transparency rather than predefined commands, e.g with regressions)

program mc_mean1, rclass // start program , note: rclass is where the values are stored
drop _all // drop before so no conflict in variables
set obs 1000 // increase and see what happens to the histogram, note: _N is obs size, and _n is the index for obs
gen x = rnormal(5,5) // increase variance see what happens
qui summarize // qui is quitely abbreviated...
return scalar x_bar = r(mean) // stores result in r(), r(mean) is from summarize (temporary memory)
end

// Simulate Monte Carlo samples
simulate "mc_mean1" b=r(x_bar), reps(1000) // get multiple means
sum b // get mean OF means... CLT
hist b, frequency normal xscale(range(2 8)) // plot histogram AND superimposed normal density
graph export "mc_mean.jpg", replace // graph must be open to save

// Note: If interested, other way of running a simulation using a loop instead of defining a program is on Querces (Thursday's Tutorial)

/* Computer exercise (20 minutes) */ // Try it yourself, straightforward...
// Wooldridge, Exercise C1, p.15-16