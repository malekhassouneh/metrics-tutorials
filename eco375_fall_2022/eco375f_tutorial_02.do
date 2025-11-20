// Tutorial #2 ECO375 30/9/2022
*******************************

*************************
// Some Comments/Mistakes
*************************

cd "C:\Users\malek\Desktop\TAship\ECO375H5F\My Tutorials\Tutorial 1" 
use wage1

/*Prelims: 1) Questions from last tutorial??
           2) OH 1-2pm Library Study Room 19?
           3) Was too harsh on Stata...
           4) Save Save graphs and data files just by using FILE tab (in browse or graph)
           5) Open multiple do-files as tabs instead of getting new Stata UI each time
		   6) Open new do-file using doedit (name of file for existing do-file) 
		   7) Go through mean independence result quickly */ 
		   
** Extra Comments **		   
		   
//How to find Working Directory (can also see it in bottom left of interface)
cd // turns out if you close Stata, will revert back to default wd (Stata file)

//Continuation lines (dismissed it last time) (if you want to keep to the right margin)
sum wage female ///
	educ

//Table vs Tabulate
table educ // get frequency count only
tabulate educ // get frequency, percentage and empirical distribution, abbreviate tab

//Loops (cover briefly just for completeness) (revisit later Tutorial 5)
foreach i in 1 2 3 {
	gen wage_`i' = wage^`i' // can use index for labelling, mathematical operators or if conditions
}
* foreach loop can be used for indexing with strings/characters not necessarily numbers 
* (not often the case, just defining variables)

forvalues i=1/10 {
	gen wage_`i' = wage^`i'
}
* more useful for our cases
* NOTE: Stata will stop you if you are running code that defines a same variable as before

**********************************
// Monte Carlo Simulations (Again)
**********************************

cd "C:\Users\malek\Desktop\TAship\ECO375H5F\My Tutorials\Tutorial 2" 

set seed 123 //for replicability, DON'T set within a loop, get degenerate results

// Write a program/function (alternative to loops) (for transparency rather than predefined commands, e.g with regressions)

program mc_mean1, rclass // start program , note: rclass is where the values are stored
drop _all // drop before so no conflict in variables
set obs 10000 // increase and see what happens to the histogram, note: _N is obs size, and _n is the index for obs
gen x = rnormal(5,5) // increase variance see what happens // COULD use any distribution, runiform()
qui summarize x // qui is quitely abbreviated...
return scalar x_bar = r(mean) // stores result in r(), r(mean) is from summarize (temporary memory)
end

// Simulate Monte Carlo samples
simulate "mc_mean1" b=r(x_bar), reps(10000) // get multiple means
sum b // get mean OF means... CLT
hist b, frequency normal xscale(range(2 8)) // plot histogram AND superimposed normal density
graph export "mc_mean.jpg", replace // graph must be open to save

// TUTORIAL 1 END OF COVERAGE

********************
// Some New Commands
********************

cd "C:\Users\malek\Desktop\TAship\ECO375H5F\My Tutorials\Tutorial 2" 
use 401K

*** Data on firm pension plans, each observation is a pension. mrate is a dollar 
**  amount, expresses how much firm contributes to employees' pension

//Descriptive Statistics
corr mrate prate totemp // change totemp to age, another story
tabstat mrate, statistics(n mean sd min) // table of summary stats

** More on Describing Data **

//Describing Data
codebook prate mrate // half way between summarize and describe

** Go back to wage dataset, easier interpretation
tabulate educ female // calculate joint frequencies (i.e. distribution of pairs)
tabulate educ female, row // add row percentages
tabulate educ female, column // add column percentages

** Graphing Data **

//Histograms
histogram mrate
sum mrate // to find the minimum of mrate
hist mrate, normal start(0.01) width(0.1) // change bin size with width(), 0.01 is min, can't be larger than min

//Bar and Scatter Graphs
graph bar (count), over(age) //crude form of histogram (graph singletons (i.e. single values) NOT specified intervals)
scatter totelg totpart // works best with two continuous variables, recall wage data set, educ is NOT
twoway (scatter totelg totpart) (lowess totelg totpart) // layered graph, lowess NOT only linear

//More on Histogram Visuals
twoway (hist totelg, color(green)) (hist totpart), legend(order(1 "totelg" 2 "totpart" ))
twoway (hist totelg) (hist totpart, fcolor(none) lcolor(black)), legend(order(1 "totelg" 2 "totpart" )) // transparent second not first
twoway (hist totelg, color(red%30) lcolor(black)) (hist totpart, color(blue%30) lcolor(black)), legend(order(1 "totelg" 2 "totpart" ))
// tinted colors for transparent fill

** Data Management **

//Generating Variables (Tutorial 1)
summ prate, detail //to find the median
gen prate_high = 1 if prate > r(p50) // define as 1 if observation is greater than median
table prate_high // notice, NO 0 values defined
replace prate_high = 0 if prate_high == . // replace missing values from above generation
table prate_high

//Other Variable Renaming, Recoding, and Relabeling
rename mrate matchrate //self explanatory renaming

sum female //recode is for categorical, using wage data, educ or female
recode female (0=1) (1=0) // (old value = new value)

describe
label data "Data Set on Wages and Education" // add a label to your data set (not interesting?)
describe

label variable wage "average hourly earnings (thousands)"// or la var, underlined letters are required for abbreviation, see help
label define sex 0 "male" 1 "female" // define a label on VALUES of a variable, then applying it
label list sex // inspect defined value label
label dir // list of all defined value labels
label value female sex // apply value label

gen string = "female" if female==1 //generating a variable that contains strings, not memory friendly
replace string = "male" if female==0
encode string, generate(sex) //apply value label defined above to generate new binary variable, MUST have same names
** VERY USEFUL

** Accessing Results ** (IMPORTANT) (Will go in depth next tutorial, r() vs e())
sum prate
return list

reg prate mrate
return list // compare the list of numbers returned above and here
ereturn list

**************************
// Regression (Tutorial 3)
**************************

scatter prate mrate 
correlate prate mrate
correlate prate mrate totemp

regress prate mrate // run a simple regression (one regressor + constant)

predict prate_hat, xb // get fitted values Xb (from most RECENT regression on Stata)
predict resid, residuals // get residuals

generate new_var = prate_hat + resid // fitted + residuals gives you response variable / regressand y
drop new_var
