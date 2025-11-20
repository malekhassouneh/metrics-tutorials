// Tutorial #3 ECO375 7/10/2022
*******************************

**********************
// Additional Comments
**********************

/*Prelims: 1) "Sampling Error" in Monte Carlo Simulation
           2) Why include "capture log" not just "log", so that Stata doesn't
		      return an error if there is no log to close etc., could also run capture clear
		   3) Assignment 1 questions?
		   4) Reading week, no tutorial next Friday? OH? If so after 5pm 
		   5) Emphasize, many ways of doing things in Stata. If I write code that
		      produces the same output differently in different context, it is on purpose.
			  Please compare and contrast, choose what's best for you.*/
			
** Extra Comments **	

//Including "capture program drop" Before Program (USEFUL)

cap program drop testprogram
program testprogram, rclass
set obs 10 
gen u = rnormal(0,10)
end

/* Stata can't define a new program with a used name, capture program drop
   will tell Stata to delete any old programs and redefine a new program.
   Helpful when tinkering with programs, changing a few numbers */

//Translate
translate @Results newlog.txt // generate a log file if didn't remember

*******************************
// New Commands From Tutorial 2
*******************************

cd "C:\Users\malek\Desktop\TAship\ECO375H5F\My Tutorials\Tutorial 2" 
use 401K, clear

/* Data on firm pension plans, each observation is a pension. mrate is a dollar 
   amount, expresses how much firm contributes to employees' pension */

//Descriptive Statistics
corr mrate prate totemp // change totemp to age, another story (corr works with 2+ variables)
tabstat mrate prate totemp, statistics(n mean sd min p50) // table of summary stats (USEFUL)

** More on Describing Data **

//Describing Data
codebook prate mrate // half way between summarize and describe

** Go back to wage dataset just for here, easier interpretation
cd "C:\Users\malek\Desktop\TAship\ECO375H5F\My Tutorials\Tutorial 1" 
use wage1, clear

tabulate educ female // calculate joint frequencies (i.e. marginal distributions)
tabulate educ female, row // add row percentages
tabulate educ female, column // add column percentages

** Graphing Data **

cd "C:\Users\malek\Desktop\TAship\ECO375H5F\My Tutorials\Tutorial 2" 
use 401k, clear

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
summ prate, d //to find the median (,d or ,detail)
gen prate_high = 1 if prate >= r(p50) // define as 1 if observation is greater than median
table prate_high // notice, NO 0 values defined
replace prate_high = 0 if prate_high == . // replace missing values from above generation
table prate_high // so essentially, have split the sample into two groups

* Another Example (split population based on quartiles, many groups)
summ age, d //to find the median (,d or ,detail)
gen age_group = 1
replace age_group = 2 if age >= r(p25)
replace age_group = 3 if age >= r(p50)
replace age_group = 4 if age >= r(p75)
table age_group

/* Above is typically helpful when dealing with a regressor like "age", which we usually
   treat as a continuous variable. But it might be more interesting to define "age brackets"
   e.g. dividing our sample into 4 groups based on the distribution of age. Interesting HOW?
   Think about how it changes our regression interpretation. Above also applies to other 
   variables like education*/
   
//Some Extra Boolean Operators (for completeness)

sum prate if age>5 | age<2 // | = or

sum wage if inlist(female, "female") //inlist selects observations for which a variable takes on specific character values

sum prate if age<=10 & age>=2 //equivalent to below
sum prate if inrange(age,2,10) //inrange selects observations for which a variable takes on values in a range 2<=age<=10

//Other Variable Renaming, Recoding, and Relabeling
rename mrate matchrate //self explanatory renaming

** Go back to wage dataset
cd "C:\Users\malek\Desktop\TAship\ECO375H5F\My Tutorials\Tutorial 1" 
use wage1, clear

table female //recode is for categorical, using wage data, educ or female
recode female (0=1) (1=0) // (old value = new value)
table female //compare counts before and after

describe
label data "Data Set on Wages and Education" // add a label to your data set (not interesting?)
describe

label variable wage "average hourly earnings (thousands)" // or la var, underlined letters are required for abbreviation, see help
label define sex 0 "male" 1 "female" // define a label on VALUES of a variable, then applying it
label values female sex // apply value label

label list sex // inspect defined value label
label dir // list of all defined value labels (including sex that we defined above)

gen string = "female" if female==1 //generating a variable that contains strings, not memory friendly
replace string = "male" if female==0 //dark red color is for strings/characters vs blue for labels
//OR JUST:
decode female, generate(female_new) 

encode female_new, generate(female_new2) //apply value label defined above to generate new binary variable, MUST have same names
** VERY USEFUL (e.g. survey data multiple choice)

******************************************************
// Accessing Returned Results (IMPORTANT but OPTIONAL)
******************************************************

/* Types of Classes (Briefly) (command can be more than one):
1) r-class, general commands
2) e-class, estimation commands
3) Others you won't use: s-class, n-class, c-class (e.g. to get the time)

NOTE: For ALL classes, results will be stored up until a command from the same class is run*/

//Demonstration of the note above
sum prate
return list // get all values stored from LAST r-class command that was used

reg prate mrate
return list // compare the list of numbers returned above and here

ereturn list // get all values stored from LAST e-class result that was used

//How to access values from r and e class
matrix list e(b) // for matrices, matrix list
matrix b = e(b) // save the matrix
dis e(N) // for scalars, display
dis _b[mrate]
dis _se[mrate]

sum read if e(sample)==1 // eg used if have missing values in variables
gen flag = e(sample)

//Local vs Global Macros (shortcuts)

local ci = invnormal(0.975)
local uc = 2 + `ci'
di `uc'

global ci = invnormal(0.975)
global uc = 2 + $ci
di $uc

**************************
// Regression (Tutorial 3)
**************************

cd "C:\Users\malek\Desktop\TAship\ECO375H5F\My Tutorials\Tutorial 2" 
use 401k, clear

capture log close // close any log running previously 
capture noisily log using "eco375tutorial3.log", replace 

scatter prate mrate 
correlate prate mrate // change mrate to age, another story

regress prate mrate // or reg: run a simple regression (one regressor + constant), poor R2

predict prate_hat, xb // get fitted values Xb (from most RECENT regression on Stata)
predict resid, residuals // get residuals

generate new_var = prate_hat + resid // fitted + residuals gives you response variable i.e. regressand y

gen mrateresid = mrate*resid
sum resid 
dis r(sum) // sum/mean should be equal to 0, why?
sum mrateresid 	
dis r(sum) // sum/mean should be equal to 0, why?
* Above two equal to 0, result in textbook (2.30 and 2.31) (if not exactly 0 here, rounding error..)

** Standardized Regression - Statistical Significance of Correlation **

*NOTE: Works ONLY for simple regression WITH constant/intercept 

/* You have 3 options when standardizing: either run "sum" before each "gen" to change what Stata
   has saved in its local memory. OR define as a local macro the value of the scalars we need,
   in this case mean and sd. OR generate two new "variables" that store mean and sd for each
   variable. */

// OPTION 1
sum prate 
gen prate_standardized = (prate - r(mean))/r(sd) // standardize BOTH variables
sum mrate
gen mrate_standardized = (mrate - r(mean))/r(sd) 
drop prate_standardized mrate_standardized //dropping the variables so I can show you other ways of doing this

// OPTION 2
sum prate
local prate_mean = r(mean)
local prate_sd = r(sd)
sum mrate
local mrate_mean = r(mean)
local mrate_sd = r(sd)
gen prate_standardized = (prate - `prate_mean')/`prate_sd'
gen mrate_standardized = (mrate - `mrate_mean')/`mrate_sd'
drop prate_standardized mrate_standardized

// OPTION 3 (not memory friendly, storing too many variables)
sum prate
gen prate_mean = r(mean)
gen prate_sd = r(sd)
sum mrate
gen mrate_mean = r(mean)
gen mrate_sd = r(sd)
gen prate_standardized = (prate-prate_mean)/prate_sd
gen mrate_standardized = (mrate-mrate_mean)/mrate_sd

hist prate // plot histogram of prate (or wlog mrate) to see what standardization does to the distribution
hist prate_standardized

reg prate_standardized mrate_standardized
/* TRY AT HOME: show that you just need to divide by the variables' respective standard deviations,
   and not also subtract the mean, for us to get the sample correlation as the OLS estimate*/

capture log close // end the log after code

*********************************
// Another Monte Carlo Simulation
*********************************

clear all
set seed 123

program unbiasedols, rclass
drop _all
set obs 50
generate x = rnormal(0,1)
generate u = rnormal(0,1)
generate y = 1 + 3*x + u
qui regress y x
return scalar b1 = _b[x] // extract estimated parameter for variable "x"
end

simulate "unbiasedols" b1 = r(b1), reps(1000)

mean b1
histogram b1, normal xscale(range(2,4))