// Tutorial #8 ECO375 18(25)/11/2022
************************************

**********************
// Additional Comments
**********************

/*Prelims: 1) Sorry for the delays and last-minutisms...
		   2) Assignment 2 grades out soon ... Saturday morning?
           3) Tip: start Assignment 3 early, quite long (hard deadline, Dec. 9th)
		   4) MUST include regression tables as pics in solutions (will be penalized Assignment 3)
		   5) When saying something is a good fit or not, use *overpredict* and *underpredict* terminology, 
		      not just *middle of cluster so good fit* logic. + don't focus on few points for analysis */
			  
** Extra Comments **
		   
***************************************************
// Interactions Involving Dummy Variables (Ch. 7.4)
***************************************************

// Interaction Between Two Dummies

cd "C:\Users\malek\Desktop\TAship\ECO375H5F\My Tutorials\Tutorial 7"
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

********************************************
// Heteroskedasticity Robust Standard Errors
********************************************

/* Motivation: Robust standard errors used to get better inference results. Otherwise, NOT related to unbiasedness */ 

cd "C:\Users\malek\Desktop\TAship\ECO375H5F\My Tutorials\Tutorial 8"
use bwght.dta, clear

reg bwght cigs faminc
rvpplot cigs, yline(0) // Residual plot (rvp = residuals vs predicted). Sign of heteroskedasticity? YES

reg bwght cigs faminc, robust // , robust is an OPTION (everything after a comma is an "option" in Stata)

* What changes? Coefficient estimates (no), R2 (no), standard errors (yes, they decrease), 
* t and F statistics (yes), signifiance conclusions (no, ONLY in this case)

*************************
// Feasible GLS (Ch.8.4b)
*************************

/* Motivation: We have seen that WLS can generate an efficient estimator in the case of heteroskedasticity.
			   We now want to find a sequence of numbers {h_i} which can act as our weights in our regression */

use SMOKE, clear

** STEP 1 - Generate Log-(Residuals^2) From Model Specification

regress cigs lincome lcigpric educ age agesq restaurn
predict u_hat, residuals // extract residuals original model

generate log_u2_hat = log(u_hat^2) // apply transformation to residuals

** STEP 2 - Generate Exponential(Fitted Values) (i.e. weights) From Auxiliary Regression
 
regress log_u2_hat lincome lcigpric educ age agesq restaurn
predict g_hat, xb // extract fitted values

generate h_hat = exp(g_hat) // apply transformation to fitted values

** IMPORTANT: This specification, taking the exponent to invert the log is an ASSUMPTION.
**            It could be that this assumption is wrong, hence we could be misspecifying our model (TRADEOFF)

** STEP 3 - Applying Weights to Original Model Specification (STEP 1)

regress cigs lincome lcigpric educ age agesq restaurn [aweight = 1/h_hat] // notice, DIFFERENT estimates to OLS (STEP 1)

*********************************
// Testing for Heteroskedasticity
*********************************

// Breusch-Pagan Test (Ch.8.3)

use hprice1, clear

** STEP 1 - Get Squared Residuals

regress price lotsize sqrft bdrms
predict u_hat, residuals
generate u_hat2 = u_hat^2

** STEP 2 - Regress Squared Residuals on All Explanatory Variables

regress u_hat2 lotsize sqrft bdrms

** STEP 3 - Test For Joint Significance Using GLOBAL F-Test

test lotsize sqrft bdrms // REJECT H0 of homoskedasticty at 5% (and 1%)

// Repeating With Log-Dependent Variable

** Why? Recall from Tutorial 6, log-transformation (i.e. Box Cox) can reduce heteroskedasticity 
** (+ skewness (not our concern)), SO we could get a reversal of our rejection results

drop u_hat u_hat2

regress lprice lotsize sqrft bdrms
predict u_hat, residuals
generate u_hat2 = u_hat^2

regress u_hat2 lotsize sqrft bdrms

test lotsize sqrft bdrms // Now FAIL TO REJECT H0 of homoskedasticity at 5% (10% etc.)

// "Special" White Test (Ch.8.3a) (WHAT YOU SHOULD USE)

drop u_hat u_hat2

** STEP 1 - Get Squared Residuals and Squared Fitted Values

regress lprice lotsize sqrft bdrms

predict u_hat, residuals
generate u_hat2 = u_hat^2

predict y_hat, xb
generate y_hat2 = y_hat^2

** STEP 2 - Regress Squared Residuals on Squared Fitted Values AND Fitted Values

regress u_hat2 y_hat y_hat2

** STEP 3 - Test For Joint Significance Using GLOBAL F-Test

test y_hat y_hat2 // FAIL TO REJECT H0 of homoskedasticity at 5% (10% etc.)

scatter u_hat y_hat // graphically we see no clear heteroskedasticity, confirms result above

** STEP (EXTRA) - Generate and Apply Weights (WLS) (Beyond Testing Procedure)

predict h_hat, xb // obtain fitted values from latest regression (which are our weights)
summarize h_hat // check that the weights are ALL positive

regress lprice lotsize sqrft bdrms [aweight = 1/h_hat] // applying generated weights
regress lprice lotsize sqrft bdrms [aweight = 1/h_hat], robust // check what happens with ,robust option

** Notice, coefficient estimates + statistical significance are similar to OLS (accounting for rounding)
** EXCEPT for bdrms (originally was insignificant anyway). SHOWS OLS is unbiased. And, when apply ,robust
** significance results are similar to above. SO EITHER SOLUTION IS PLAUSIBLE/FEASIBLE, BUT prefer HETERO-ROBUST SE

** IMPORTANT: FOR ALL testing procedures, we ideally would be able to test for heteroskedasticity using our error terms u.
**            BUT these are unobservables, hence we have to do with our residuals u_hat that CONSISTENTLY estimate u