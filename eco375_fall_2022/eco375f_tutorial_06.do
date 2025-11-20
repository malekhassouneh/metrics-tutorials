// Tutorial #6 ECO375 4/11/2022
*******************************

**********************
// Additional Comments
**********************

/*Prelims: 1) Ch.5 (next week), asymptotics critical for Q2bd in Assignment 2 
           2) Covering Ch. 6.1-6.2 and Ch. 7.1-7.4 (Outside Lectures) (more practical chapters)
		   3) Busy week, I'll upload slides and .do files tut. 5 + 6 today */
		   
** Extra Comments **

// Regression with a Conditional (not made clear previously?)

cd "C:\Users\malek\Desktop\TAship\ECO375H5F\My Tutorials\Tutorial 5"
use HTV, clear

reg educ motheduc fatheduc if urban==1 // regression ONLY on urban people (restrict sample size)

/* // Some Selected Testing Facts 

	1) When we say significant/not, referring to the VARIABLE itself not the estimate
	2) p-value formal def: smallest significance level at which we reject the H0
	3) 1 restriction F test is a t-test (equivalent)
	4) R2 form of: F = ((RSS_r - RSS_u)/q)/(RSS_u/(n-k-1)) = ((R2_u - R2_r)/q)/(1-R2_u/(n-k-1))
	5) Exact test vs asymptotically exact test? (didn't cover asymptotics yet) */

// Testing Linear Combinations (lincom)

regress educ motheduc fatheduc

test motheduc = fatheduc // from tutorial 5, see if the effect of motheduc is the same as fatheduc
lincom motheduc - fatheduc // lincom gives more info than test (SE and CI)

** How does Stata do this? Runs an implicit regression by manipulating (+- some variable) on both sides of our regression
* Which regression? regress educ on motheduc AND (fatheduc - motheduc)

gen newvar = fatheduc + motheduc
regress educ motheduc newvar // t-statistic IS the F statistic from above (19.20), hence equivalent test
		   
**************************************
// Covering Tutorial 5 (see other .do)
**************************************

***************************************
// Effects of Data Rescaling (Ch. 6.1)
***************************************

** Motivation: Mostly rescale variables to change units of measurments of variables. Will this change
*              coefficients (yes), std errors (and SSR) (yes), CIs (yes), t statistics (no), F statistics (and R2) (no)

** Observe: We will preserve ALL measured effects and testing outcomes

** Rescaling Explanatory (IV) and Response (DV) Variables **

// Rescaling the Dependent Variable

cd "C:\Users\malek\Desktop\TAship\ECO375H5F\My Tutorials\Tutorial 6"
use BWGHT, clear

regress bwght cigs faminc // units = (ounces, count of individual cigs/day)

regress bwghtlbs cigs faminc // bwghtlbs = bwght/16 (ounces to POUNDS)

** CONCLUSION: All estimates change by the SAME factor 1/16, but NO effect of rescaling the dependent variable 
*              on statistical significance results, as standard errors ALSO rescaled by 1/16 factor (so t-statistic ratio is invariant)

* What about SSR? NOT the same, changes by a factor of (1/16)^2. BUT ESS ALSO by a factor of (1/16)^2 so R2 (and F statistic) DON'T change

// Rescaling an Independent Variable

regress bwght packs faminc // packs = cigs/20 (count cigs vs PACKS)

** CONCLUSION: The estimate for packs, the rescaled explanatory variable, is the ONLY ESTIMATE that changes (by a factor of 20)
*              (along with its std error and CI). All else, including R2 and F are preserved as before 

// Stanardizing Variables (a specific type of rescaling)

* Recall: Discussion in Tutorial 3, our estimates will now be correlations

egen bwght_std = std(bwght)
egen cigs_std = std(cigs)
egen faminc_std = std(faminc)

reg bwght_std cigs_std faminc_std // notice, what value is the constant? 0. Why? You can show it algebraically (equation 6.3 in the textbook)

reg bwght_std cigs_std faminc_std // another illustration, ONLY standardize the IVs. Compare to original regression (regress bwght cigs faminc)

*************************************
// Specific Functional Forms (Ch 6.2)
*************************************

** Logarithmics **

* Note: Log-log model, previously seen, used approximation to find % change of the dependent variable (not good approx. when DV is large). 
*       Can actually use the exact formula (estimate approximation hence is BIASED, but turns out to be CONSISTENT)

/* Why bother with logs? 

	1) log variable SLOPE coefficients invariant to rescaling (log(ax)=log(a)+log(x), log(a) absorbed by constant (NO free lunch))
	2) log(y) instead of y as the DV satisfies CLM assumptions "better"
	3) Strictly positive y (e.g. income) have heteroskedastic or skewed conditional distribution (e.g. income), and log
	   can alleviate these concerns (general concept: Box-Cox transformations)

	BUT: 1) Log transformations create extreme values, especially for small original y
		 2) Not well-defined for non-positive values (0 or -ive)
		 3) R2 comparison between model with log(y) vs y not plausible (models aren't "nested")
		 
	Rule of thumb take logs when: 1) y is a $ value, especially if large value (e.g. GDP), so
									can lessen sensitivty of estimates to outliers
								  2) NOT applied to y that is a count variable (e.g. years, age), want to preserve this integer structure
								  3) % variables (e.g. interest rate), taken usually at level form*/						  
								  
// Quick Demonstration
								  
gen log_bwght = log(bwght) 
gen log_bwghtlbs = log(bwghtlbs)

reg log_bwght cigs faminc // Show that the slope estimates (NOT the constant) are invariant to rescaling the DV (log_bwght to log_bwghtlbs)
reg log_bwghtlbs cigs faminc  
								  
** Quadratic **

* Captures increasing or decreasing marginal effects. Previous regressions, assumed implicitly constant marginal effects, 
* irrespective of the actual LEVEL of the variable (BOLD assumption)

* Notice: when we model and run regressions, could be making very bold implied assumptions need to be aware of

/* Two cases: 1) beta1 < 0 , beta2 > 0 (beta1 for x, beta2 for x^2), decreasing to a minimum (convex function)
			  2) beta1 > 0 , beta2 < 0 (as below) increasing up to a maximum (concave function)
			  ** If BOTH same sign, no turning point (not our interest here)
			  
        Why could this (case 2) arise? 1) Actually a correct phenomenon
					 or case 1	       2) Few observations to the right of maximum, that part is not identified
							           3) Some of the estimates are biased (e.g. educ is not used below), OR functional relationship 
									      is incorrect (misspecified model)*/

// Example (Mincer equation from Tutorial 4)

use WAGE1, clear

regress wage exper expersq
di -1*_b[exper]/(2*_b[expersq])  // Finding the level of experience at which we maximize wage, BEFORE getting a negative effect (take FOC)

predict fitted, xb
twoway (scatter fitted exper) (qfit fitted exper) // plot estimated quadratic relation, qfit or lowess (lfit obviously for lines)
twoway (scatter wage exper) (qfit wage exper) (lfit wage exper) // plot estimated quadratic equation against data AND nonquadratic model

** Interactions ** (USEFUL)

* Used when the partial effect of an explanatory variable on the DV depends on the magnitude of ANOTHER exp. variable (interaction effect)
* Look at Assignment 2 Q1, price of house depends on number of rooms AND number rooms GIVEN the number of floors

// Example (another housing price e.g.)

use hprice1, clear

regress price sqrft bdrms c.sqrft#c.bdrms // # signifies the interaction term between sqrft and bdrms
predict fitted, xb

twoway (function price = _b[_cons]+_b[sqrft]*100+(_b[bdrms]+_b[c.sqrft#c.bdrms])*x) /// \\ graphing the interation effect
(function price = _b[_cons]+_b[sqrft]*300+(_b[bdrms]+_b[c.sqrft#c.bdrms]*300)*x)

* And if interested in AVERAGE partial effect of sqrft, plug in average bdrms value in estimated effects equation (the derivative)
* OR if interested in AVERAGE partial effect of bdrms, plug in average sqrft value in estimated effects equation (the derivative)

** NOTE: This will be important NEXT tutorial; in Stata have c.VAR (used here) and i.VAR. We'll see i.VAR later, but c.VAR tells
*        Stata to treat the variable VAR as a continuous variable. Especially important if the variable is programmed
*        as a FACTOR/ORDINAL (different terminology) variable (e.g. level (not years) of education).
*        So: c.VAR = treat variable as continuous, i.VAR = treat variable as a factor
