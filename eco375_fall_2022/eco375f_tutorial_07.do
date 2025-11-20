// Tutorial #7 ECO375 11/11/2022
*******************************

**********************
// Additional Comments
**********************

/*Prelims: 1) Any questions on Tutorial 6 video (Ch. 6.1-6.2)?
		   2) When standardizing all variables, intercept term is 0, why?
           3) PLEASE read Ch. 6.1-6.2 and Ch. 7.1-7.4 at home ...
		   4) Import .csv Q2, OK? "import delimited CAschools.csv"
		   5) Graphing, lfitci and qfitci (instead of lfit and qfit) */
		   
** Extra Comments **

// Some More egen (For Completeness)

clear all
set obs 100
gen x1= runiform(0,10) // generating some data

sum x1
gen x1_avg = r(mean)
** ALTERNATIVE using egen
egen x1_avg_e = mean(x1)

sort x1 // Stata restructures dataset in ascending order of x1
gen x1_max = x1[_N] // extract the last observation, which after sorting is the MAX
** ALTERNATIVE using egen
egen x1_max_e = max(x1)

gen female = runiformint(0,1) // randomly generate a categorical/dummy/indicator variable for gender

sort female // if don't sort first, get an error using by (below)
by female: egen x1_avg_gender = mean(x1) // assigns male average to males, and female average to females 
* by VAR : repeats the code to the right of : for each group/subset in VAR
** ALTERNATIVE using bysort
bysort female: egen x1_avg_gender_e = mean(x1) // bysort is the same as by, but it sorts as well (don't have to do seperately)

******************************
// Unbiasedness vs Consistency
******************************

// Biased BUT Consistent

clear all
program define alice_modified, rclass
	clear
	syntax [, obs(integer 100)]
	set obs `obs'
	gen y = rnormal(50000,5000)
	summarize y
	return scalar y_bar = r(mean) + 10000/`obs' // In Assignment 1, just had + 1000 (but + 10000/n better for dem.)
	// think of this as Alice measuring Y with error for a subset of people only (i.e. not all)
end

set seed 123
simulate y_bar=r(y_bar), reps(1000): alice_modified, obs(1000)
sum y_bar // bias is roughly 10,000/50 = 200
hist y_bar, xscale(range(48000 53000)) // change 50 above to n=1,000, see that the distribution collapses to the true mean, i.e. bias goes to 0

*NOTE: BIASED but bias goes to 0 as obs (n) goes to infinity, so asymptotically unbiased i.e. CONSISTENT

// Unbiased BUT Inconsistent

clear all
program define bob_modified, rclass
	clear
	syntax [, obs(integer 100)]
	set obs `obs'
	gen y = rnormal(50000,5000)
	summarize y if _n<50 // In Assignment 1, took one obs. only (but first 50 obs. better for dem.)
	return scalar y_bar = r(mean)
end

set seed 123
simulate y_bar=r(y_bar), reps(1000): bob_modified, obs(50)
hist y_bar, xscale(range(48000 53000)) // even with n=100,000, distribution DOESN'T collapse to the true mean

*NOTE: UNBIASED but INCONSISTENT, y_bar always uses the first 50 obs. ONLY

/* OPTIONAL CLT and LLN Simulation Demonstration(s)
***************************************
// CLT and LLN Simulation Demonstration
***************************************

** Law of Large Numbers **

// LLN Example

** What are we doing? Draw a sample size of n={100, 1,000 , 10,000} from a N(0,10^2) 1000 times and 
*  calculate the sample means for each repetition, for each n

clear all
program define llnsim, rclass // a program that generates the Normal draws and finds the sample mean
	clear
	syntax [, obs(integer 100) mu(real 10) sigma(real 100)] // define local macros
	quietly set obs `obs'
	quietly gen y = rnormal(`mu',`sigma')
	quietly summarize y
	return scalar y_bar = r(mean)
end

set seed 123
simulate y100=r(y_bar), reps(1000): llnsim, obs(100) mu(10) sigma(100)
save y100.dta, replace

set seed 123 
simulate y1000=r(y_bar), reps(1000): llnsim, obs(1000) mu(10) sigma(100) // Repeat with n=1,000 observations
save y1000.dta, replace

set seed 123 
simulate y10000=r(y_bar), reps(1000): llnsim, obs(10000) mu(10) sigma(100) // Repeat with n=10,000 observations

** Now plot histograms of the 1000 sample means for each n

merge 1:1 _n using y100, nogenerate
merge 1:1 _n using y1000, nogenerate
*NOTE: one-to-one merge of ALL _n observations COMBINES the current dataset in memory (master dataset) with another. 
*      The nogenerate option is so that Sata does not generate a new variable _merge, but rather keeps the name y100 (or y1000).
*      With datasets e.g. file records, people have IDS, so could replace _n with an identifier variable e.g. people's ID, so match by ID.
*      Here just match by _n, i.e. by corresponding observation.

graph twoway (hist y10000) (hist y1000, fcolor(green)) (hist y100, fcolor(red))

** Central Limit Theoreom

// CLT Example #1, Chi-Squared(1)

** What are we doing? Draw a sample size of n={100, 1,000 , 10,000} from a chi2(1) 1000 times and 
*  calculate the sample means for each repetition, for each n. THEN, generate a standardized variable: sqrt(N)(y_bar - mu)/sqrt(variance)

*Note: The mean and the variance of chi-squared dist. is: df and 2*df

clear all
program define llnsim_chi, rclass // a program that generates the Chi-Squared draws and finds the sample mean
	version 14.2
	syntax [, obs(integer 100) df(real 1)]
	clear 
	quietly set obs `obs'
	quietly gen y = rchi2(`df')
	quietly summarize y
	return scalar y_bar = r(mean)
end

set seed 123
simulate chi100=r(y_bar), reps(1000): llnsim_chi, obs(100) df(1)  
gen chi100_std = sqrt(100)*(chi100-1)/sqrt(2*1) //generating the standardized variable
save chi100.dta, replace

set seed 123
simulate chi1000=r(y_bar), reps(1000): llnsim_chi, obs(1000) df(1) // Repeat with n=1,000 observations
gen chi1000_std = sqrt(1000)*(chi1000-1)/sqrt(2*1)
save chi1000.dta, replace

set seed 123
simulate chi10000=r(y_bar), reps(1000): llnsim_chi, obs(10000) df(1) // Repeat with n=10,000 observations
gen chi10000_std = sqrt(10000)*(chi10000-1)/sqrt(2*1)

** Now plot histograms of the 1000 STANDARDIZED variables for each n, and compare to N(0,1) dist.

merge 1:1 _n using chi100, nogenerate
merge 1:1 _n using chi1000, nogenerate

graph twoway (hist chi10000_std) (hist chi1000_std, fcolor(green)) (hist chi100_std, fcolor(red)) (function normalden(x, sqrt(1)), range(-2 2)) 

// CLT Example #1, Uniform(0,1)

*Note: The mean and variance of uniform(a,b) is: 0.5*(a+b) and 1/12*(b-a)^2

clear all
program define llnsim_u, rclass // a program that generates the uniform draws and finds the sample mean
	version 14.2
	syntax [, obs(integer 100) a(real 0) b(real 1)]
	clear 
	quietly set obs `obs'
	quietly gen y = runiform(`a',`b')
	quietly summarize y
	return scalar y_bar = r(mean)
end

set seed 123
simulate u100=r(y_bar), reps(1000): llnsim_u, obs(100)
gen u100_std = sqrt(100)*(u100-(0.5))/sqrt(1/12) //generating the standardized variable
save u100.dta, replace

set seed 123
simulate u1000=r(y_bar), reps(1000): llnsim_u, obs(1000) // Repeat with n=1,000 observations
gen u1000_std = sqrt(1000)*(u1000-(0.5))/sqrt(1/12)
save u1000.dta, replace

set seed 123
simulate u10000=r(y_bar), reps(1000): llnsim_u, obs(10000) // Repeat with n=10,000 observations
gen u10000_std = sqrt(10000)*(u10000-(0.5))/sqrt(1/12)
save u10000.dta, replace

** Now plot histograms of the 1000 STANDARDIZED variables for each n, and compare to N(0,1) dist.

merge 1:1 _n using u100, nogenerate
merge 1:1 _n using u1000, nogenerate
graph twoway (hist u10000_std) (hist u1000_std, fcolor(green)) (hist u100_std, fcolor(red))  (function normalden(x, sqrt(1)), range(-2 2)) 
*/

**********************************************
// Single Dummy Independent Variable (Ch. 7.2) 
**********************************************

* MOTIVATION: Use dummy/binary/indicator variables to store QUALITATIVE information (e.g. race, gender)

// Example; Wage Gender

cd "C:\Users\malek\Desktop\TAship\ECO375H5F\My Tutorials\Tutorial 7"
use WAGE1, clear

regress wage female educ // female is a dummy (as always =1 if female, =0 if male)

twoway (function wage = _b[_cons] + _b[educ]*x) (function wage = _b[_cons] + _b[female] + _b[educ]*x) 
// graph MALE and FEMALE estimated equations, notice the roughly -2.2 (_b[female]) vertical shift between equations

* IMPORTANT: As we always include a constant, DON'T include both female and male dummies, otherwise get PERFECT COLINEARITY (female+male=1)

* INTERPRETATION: Due to the above, BASE group is males (intercept estimate) and delta_0 is the DIFFERENCE between males and females  

// Log-Dependent Variable

use hprice1, clear

regress lprice llotsize lsqrft bdrms colonial  
 
*INTERPRETATION: As always, in % terms, so COLONIAL coefficient is the % DIFFERENCE in prices between colonial-style and noncolonial houses

****************************************************
// Dummy Variables For Multiple Categories (Ch. 7.3)
****************************************************

* MOTIVATION: Use SEVERAL dummies for different characteristics

// Example; Wage Gender Marriage-Status

use WAGE1, clear

generate male = 1 - female // generate dummies for the 4 (minus 1) categories
generate marrmale = married*male
generate marrfem = married*female
generate single = 1 - married
generate singfem = single*female

regress lwage marrmale marrfem singfem educ exper expersq tenure tenursq // base group: single men (dummy not included)
// notice, "marriage premium" here NOT the same for male and female...

* RULE OF THUMB: With n binary categories, always include 2^n -1 dummies

// Ordinal Information Using Dummies

* MOTIVATION: Ordinal (FACTOR) variable = qualitative ranking that has multiple levels (e.g. LEVEL of educ). Has ordinal meaning
*             so CAN'T treat as continuous variable. Define seperate dummy for each level/value, leaving out a base group

// Example

use LAWSCH85, clear // Find the effect of the rank of lawyers' law school on their starting salaries

generate r61_100 = 1 if rank >= 61 & rank <= 100 // generate new dummy to CHANGE possible base group (see below)
replace r61_100 = 0 if rank <61 | rank > 100

regress lsalary top10 r11_25 r26_40 r41_60 r61_100 LSAT GPA llibvol lcost // base group: schools with rank < 100

* NOTE: DOESN'T make sense to include "rank" variable in model ... difference between e.g. 20th and 21st ranked NOT the same as 30th and 31st

// Stata Generated Ordinal Dummies

cd "C:\Users\malek\Desktop\TAship\ECO375H5F\My Tutorials\Tutorial 5"
use HTV, clear

* NOTE: educ, the # of education years, can be treated as continuous OR categorical/discrete. Two regressions:

reg wage educ // Continuous, or c.educ (default for variables is continuous in Stata)

* INTERPRETATION: ON AVERAGE, one more years of education increases hourly wage by $1.4. BUT ASSUME that the marginal effect
* is the SAME anywhere in the domain of educ (i.e. go from 6 to 7 and 18 to 19, same effect)

reg wage i.educ // Discrete, using i.VARNAME to treat educ as a factor/ordinal variable

* INTERPRETATION: MUST interpret every coefficient in reference to the omitted base group/level (educ=6 here, the constant term).
* ONLY significant difference in wage is individuals with 18 and 19 years of educ statistically earning more than individuals 
* with 6 years of educ. Rest are insignificant. Why? could be effect only after graduate from university at 18 years? 

* Again, why omit baseline category? If we include the constant (always should) then we have PERFECT colinarity... problematic.
* But can choose to omit any category (change reference category):

reg wage ib20.educ // ib<base>.var, change reference category/base level to educ=20, so compare all groups to people with 20 years of educ

* INTERPRETATION: Notice, signs of most coefficients change to negative, as now comparing to the largest education level

***************************************************
// Interactions Involving Dummy Variables (Ch. 7.4)
***************************************************

// Interaction Between Two Dummies

use WAGE1, clear // gender and marital status example REVISITED

regress lwage female married c.female#c.married educ exper expersq tenure tenursq
regress lwage c.female##c.married educ exper expersq tenure tenursq // SAME REGRESSION, ## to specify full FACTORIAL between the two (or more) variables

* RULE OF THUMB: When include interaction, always include single variables alone also, otherwise interpretation is weird

// Interaction Between One Dummy and One Non-dummy (Allow for Different Slope)

regress lwage female educ c.female#c.educ // test whether RETURN to educ (slope) is same for male and female + allow for a constant wage differential

// Testing for Differences in Regression Functions Across Groups

* MOTIVATION: Want to test whether two populations/groups follow the same regression function/specification.

use GPA3, clear // e.g. test whether the same regression model describes college GPA for male (1) and female (2) college athletes

keep if spring == 1 // drop all observations NOT in spring semester (formality)

regress cumgpa sat hsperc tothrs // POOLED regression (on all groups)
global ssr_p = e(rss)
global n = e(N)
regress cumgpa sat hsperc tothrs if female == 1 // SUBSET 1 regression (female=1) Regress on a subset of the data (female = 1)
global ssr_1 = e(rss)
regress cumgpa sat hsperc tothrs if female == 0 // SUBSET 2 regression (female=0)
global ssr_2 = e(rss)

di (($ssr_p -($ssr_1 + $ssr_2 ))/(4))/(($ssr_1 + $ssr_2 )/($n -2*(3+1))) // Calculation of Chow Statistic (special case of F statistic)
di invFtail(4,$n -2*(3+1),0.05) // find critical value, CONCLUSION: reject H0 that male and female follow same regression function