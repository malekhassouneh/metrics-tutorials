// Tutorial #5 ECO375 28/10/2022
*******************************

**********************
// Additional Comments
**********************

/*Prelims: 1) Assignment 1 grades are out, my margin of error +-1 for Q1
           2) Assignment 2 out today, a bit longer (Computer-based section)
		   3) Emphasize population NOT same as sample (Assignment 1 Q1 Q3)
		   4) Theoretical exercise solutions posted on Quercus (THURSDAY TA's) (Good Q for practice)*/
			
** Extra Comments **	

// Looking through my notes, some things that I might have missed? (not sure)

keep // keeping and dropping VARIABLES
drop
keep if // keeping and dropping OBSERVATIONS
drop if 

// Clarification local vs global macros:

cd "C:\Users\malek\Desktop\TAship\ECO375H5F\My Tutorials\Tutorial 5"
use HTV, clear

local var motheduc fatheduc 
regress educ `var' // HAVE to run both lines at same time
* Local macro deleted after every chunk of code you run, UNLIKE global macro (until exit Stata), hence prefer global macros

********************************
// More on Omitted Variable Bias
********************************

** Constructed/Simulated Example **

/* Data Generating Process (DGP): wage = 1 + 2*educ + 3*ability + epsilon, n=2000
                                  educ ~ uniform(0,20) + 0.1*ability // years of education
                                  ability ~ normal(100,6^2) // measure of ability e.g. IQ score
                                  epsilon ~ normal(0,5^2) */
   
clear // generating the data described by the DGP
set obs 2000
gen ability = rnormal(100, 6)
gen educ = runiform(0, 20) + 0.1*ability
gen epsilon = rnormal(0, 5)   
gen wage = 1 + 2*educ + 3*ability + epsilon

reg wage educ ability // First, estimate the "true"/"complete" model
* BUT in real life can't observe/measure "ability" adequately, so instead we run:
reg wage educ // auxiliary model WITHOUT ability as a control, ability is in the error term, so OVB

* Finding the sign of the bias, TWO steps:
corr educ ability // positive correlation AND b_2 > 0 from above so bias is +ve (compare b_1's above)

* Calculating the empirical and theoretical bias:
gen b_1_tilde = _b[educ]
di b_1_tilde - 2 // empirical bias, estimate minus actual b_1

reg ability educ // theoretical bias using b_2 * delta_1_tilde formula
gen delta_1_tilde = _b[educ]
di 3*delta_1_tilde // theoretical bias, b_2 = 3 from DGP
* notice, small difference due to sampling error... but as n=sample size increases, values converge

** OVB in Data Example #1 (Exercise C3.6) **

use WAGE2, clear

regress IQ educ // affect of education on IQ scores
gen delta_1_tilde2 = _b[educ]
regress lwage educ // affect of education on log wage, OVB IQ (ability)
gen b_1_tilde2 = _b[educ] // BIASED estimate
regress lwage educ IQ // affect of education AND IQ on log wage
di  _b[educ] + _b[IQ]*delta_1_tilde2 // calculate UNBIASED estimate + bias, get b_1_tilde2 BIASED above

** OVB in Data Example #2 **

use JTRAIN98, clear

regress earn98 train // simple regression, negative effect of job training on earnings? OVB can seriously skew conclusions...
regress earn98 train earn96 educ age married // multiple regression with "relevant" controls

**************************************************************
// Inference (Hypothesis Testing, NO Confidence Intervals Yet)
**************************************************************

** Example of Hypothesis Testing (Ch4 QC11)**

use HTV, clear

regress educ motheduc fatheduc // affect of mother and father educ levels on child educ level
display _b[motheduc]

** NOTE: ANYWHERE I use GLOBAL below, can simply use gen instead, then drop all the $ signs everywhere

// Critical Value Approach (TWO TAILED TEST)

global tstat = _b[motheduc]/_se[motheduc] // calculate t-statistic
global df = e(N)-2-1 // df = n - k -1 NOTE: some people define this as n-k (k includes constant)
di invttail($df ,0.05/2) // find critical value
** CONCLUSION: t statistic > critical value so reject H0 with 95% confidence (1-prob Type I error) i.e. at 5% significance level

// P-value Approach (TWO TAILED TEST) (My preference)

global pvalue = 2*ttail($df, $tstat ) // calculate pvalue, due to symmetry of t distribution can double
di $pvalue // 
** CONCLUSION: p-value less than 0.05 so reject H0 at 5% level (and 1%, 0.1%), pvalue very small...

test motheduc // alternative way of implementing test above
** NOTE:  test gives the F-statistic, which is t-statistic^2 FOR SINGLE PARAMETER TEST (can verify algebraically)
**        Also, test applies to the LAST regression that was conducted, otherwise interpretation is off

// Conducting ONE TAILED TESTS (to verify sign of parameter)

// H0 : beta_1_hat = 0     H1 : beta_1_hat > 0 (not =/=)

di ttail($df, $tstat ) // half the pvalue calculated above, why? rejection region here is one sided, but probablity is the same
** CONCLUSION: Same as above
** NOTE: Lower significance level (alpha), more conservative test as SMALLER REJECTION region. Parameter chosen by researcher

di invttail($df ,0.05) // OR with critical value approach take critical value at alpha NOT alpha/2
** CONCLUSION: Same as above

// Testing Non-Zero Nulls

test motheduc = 4 // easy way of implementing (can also be done for multiple testing, see BELOW)
cap drop new_var1 // alternative, mechanical way of implementing
gen new_var1 = educ - 4*motheduc
regress new_var1 motheduc fatheduc abil // check that the t-statistic squared is the F above, and that the pvalues are the same

// Multiple Restriction Testing, Joint Testing (F test) (TWO TAILED TEST)

reg educ motheduc fatheduc abil // estimate model with added variable for ability (of child)
di e(F) // F statistic for GLOBAL F TEST
** CONCLUSION: Very large F statistic, pvalue virtually 0, so reject global hypothesis (that all parameters are 0).
**             SO, motheduc fatheduc and abil are JOINTLY SIGNIFICANT

test motheduc fatheduc // LOCAL F TEST, test if motheduc and fatheduc influence child education
** CONCLUSION: Reject H0... at what signifiance level? ALMOST ANY

di invFtail(2,$df ,0.05) // Or can compare above F value with critical value (not necessary, pvalue is calculated for you)

reg educ motheduc fatheduc abil // ALTERNATIVE way of calculating F statistic for Critical Value Approach (above line)
global rss_u = e(rss)
reg educ abil
global rss_r = e(rss) 
global F = (($rss_r -$rss_u )/2)/(($rss_u )/$df )

** Note: F is always positive, why? unrestricted RSS < restricted RSS ALWAYS. Unrestricted is with more explanatory variables
*        so it must be AT LEAST as good in prediction as the restricted model

test motheduc = fatheduc // LOCAL F TEST, test if motheduc and fatheduc have SAME effect
** CONCLUSION: pvalue is 0.0669 so can reject H0 at the 7% signifiance level (not conventional alpha choice)

***********************
// Loops (Introduction)
***********************

** Some Applications of Loops **

// Standardizing Variables

** CONTINUATION OF ABOVE -> see if significance results (from above) with standardized variables changes
global list_of_variables = "educ motheduc fatheduc abil" // define a string
global var_list educ motheduc fatheduc // OR

foreach x of varlist $list_of_variables { // if don't define global macro above, simply write "educ ... abil" in its place (but too long)
	
	quietly sum `x'
	gen `x'_std = (`x'-r(mean))/r(sd) // or just use std() function with egen (SHORTCUT)
	
} 

reg $list_of_variables // same regression as above
reg *_std // regression on standardized variables

** NOTE: * here is called the wildcard, takes ALL variables that end with _std, for us generated from loop
** CONCLUSION: t-statistics and pvalues NOT CHANGED, interpretation is in terms of standard deviations
**             i.e. on average an increase of the mother's education of one standard deviation is associated with 
**             an increase of her child's education of 0.18 standard deviations. It is significant at the 0.1% level.

* Above interpretation/conclusion IMPORTANT for Assignment 2

// Changing units of observations

global months "jan_avg_f feb_avg_f mar_avg_f"

foreach x of global months { 
	
set obs 100	
gen `x' = runiform(32,100) // just generating data from a uniform dist. for this demonstration
		
}

foreach x of varlist $months {
	
	gen `x'_c = (`x'-32)*5/9 // generating new Celsius variables
		
}

// Generating Dummies (VERY HELPFUL) (See application next week)

drop _all // removing all variables generated above
set obs 100
gen year = runiformint(2010,2020) // generating a variable that includes year (from a uniform distrubtion)

forvalues i = 2010/2020 { // alternative notation 2010(1)2020, (1) is the increment
	
	gen dum_`i' = 1 if year == `i'
	replace dum_`i' = 0 if dum_`i'==.
}