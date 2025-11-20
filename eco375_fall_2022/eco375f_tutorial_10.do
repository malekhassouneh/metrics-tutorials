// Tutorial #10 ECO375 2/12/2022
*******************************

**********************
// Additional Comments
**********************

/*Prelims: 1) Video is up for TUTORIAL 9, Tutorial 6's video can't load for some reason 
		   2) Last tutorial, not alot of content today, review at the end (open for questions) */
		   
** Extra Comments **

/* Some Other IMPORTANT Commands You Should Know: 

	insheet, outsheet (working with CSV files)
	merge, reshape, collapse (data management)
	inlist, inrange (Boolean operators) 
	bysort: sum
	matrix[i,j] (matrices in Stata)
	(areg, absorb) 
	lasso 
	** Might find these useful later*/

**************************************
// General Instrumental Variables (IV)
**************************************

cd "C:\Users\malek\Desktop\TAship\ECO375H5F\My Tutorials\Tutorial 10"
use MROZ, clear 

// Multiple Instruments Case

regress educ exper expersq motheduc fatheduc if hours>0 // first stage regression
test (motheduc = 0) (fatheduc = 0) // testing RELEVANCE (F-test), one of the identification conditions 
predict educ_hat, xb
regress lwage educ_hat exper expersq // second stage regression

ivregress 2sls lwage (educ = motheduc fatheduc) exper expersq // Compare estimates and standard errors to the second stage

// Another Example (Exercise C15.9)

use WAGE2, clear

ivregress 2sls lwage (educ = sibs) exper tenure black // IV estimation

regress educ sibs exper tenure black // 2SLS estimation (manual IV), first stage regression
predict educ_hat, xb
regress lwage educ_hat exper tenure black // second stage regression

regress lwage educ exper tenure black // compare second stage to OLS (educ and not educ_hat)

/* Note: Think of the reason behind why we replace educ with educ_hat as the following: 
         educ_hat is the closest thing to educ without it being endogeneous like educ
         so educ_hat is an "approximate" exogenous replacement for educ */

// Multiple Instruments and Multiple Endogenous Variables Case (NO EXAMPLE)

/* In General: CAN ALSO BE EXTENDED TO INCLUDE THIS CASE e.g. ivregress 2sls y1 (y2 y3 = z1 z2) x1 x2
               where y2,y3 are endogeneous, z1 z2 are instruments, x1 x2 other exogeneous controls */
** Note: MUST have at least # exogeneous INSTRUMENTS >= # endogeneous variables

*******************
// Endogeneity Tests
*******************

/* Motivation: Want to test for the endogeneity of the variable that we suspect is endogeneous.
               If there is evidence for endogeneity, we estimate our regression using IV. Otherwise, when 
			   explanatory variables are ALL exogeneous, 2SLS/IV is less efficient than OLS, so we use OLS.*/
			   
/* How procedure works: Exogenous variables are by definition uncorrelated with the error term u,
                        hence the endogeneous variable is uncorrelated with u IFF v, the error from the
						second regression is uncorrelated with u. Use v_hat instead of v, as v_hat is a 
						consistent estimate of v. Think of v as the potentially "endogenous" part of 
						the endogenous variable, and we want to see if it is inducing endogeneity or not
						so that Cov(u,v)!=0 --> Cov(x,v)!=0 */
// Control Function Approach

use MROZ, clear // Back to the first data set

** STEP 1 - Obtain Residuals from FIRST STAGE Regression (i.e. REDUCED FORM EQUATION)

regress educ exper expersq motheduc fatheduc if hours>0 // first stage i.e. estimate reduced form equation
predict v_hat, residuals

** STEP 2 - SECOND STAGE Regression (i.e. STRUCTURAL EQUATION) Including Residuals

regress lwage educ exper expersq v_hat if hours>0
 
** Result: Moderate evidence of endogeneity, as we REJECT H0 at alpha=0.1, so v_hat explains lwage, which
**         depends on u.

/* Are the estimates from the control function regression the same as the 2SLS estimates? YES, so you
   can either (1) account for endogeneity explicitly using v_hat (Control Function), or (2) *purge* endogeneity by using the 
   exogenous part of x, x_hat (2SLS). Either are equivalent */

** In General: CAN ALSO BE EXTENDED FOR MULTIPLE ENDOGENEOUS VARIABLES: would run two seperate first stages, one for
*  each endogeneous variable, and extract two sets of residuals that would be included in the structural equation.
*  Then for a test of endogeneity, would have to conduct an F-test, joint significance --> AT LEAST ONE endogeneous.
*  BUT result is ambiguous, as we don't know which of the two variables is endogeneous, we only know ONE of them is.

******************************************
// Testing Overidentification Restrictions
******************************************

** Will see this next class, but will cover now as this is the last tutorial

** VALID UNDER HOMOSKEDASTICITY ONLY

/* Motivation: If we have two instruments, we can use one for IV estimation, and the other to test exogeneity.
               This is the Hausman test/approach (that is problematic). If we estimate IV using either instrument, 
			   and IV estimates are statistically close to each other, then BOTH instruments are exogenous. Otherwise 
			   AT LEAST one is not exogeneous, BUT don't know which. BUT problem with this test is that it could be 
			   that both instruments are NOT exogeneous, AND we coincidentally have a small difference between the
			   estimates as BOTH estimates are inconsistent. We hence need a different more general approach: */

// Overidentification Tests

** By construction H0: all of IVs are exogeneous (good) H1: some of IVs are NOT exogeneous (problematic)

** STEP 1 - Obtain Residuals from 2SLS Estimation of the Structural Equation

ivregress 2sls lwage (educ = motheduc fatheduc) exper expersq if hours>0
predict u_hat, residuals // these residuals should be consistent estimates of the unobservable u (if instruments are VALID)

** STEP 2 - Obtain N*R2 Statistic from Regression of Residuals on ALL Exogenous Variables

regress u_hat motheduc fatheduc exper expersq if hours>0 // regression of residuals on all exogenous variables
global r2 = e(r2) // obtain R2
global statistic = $r2*e(N) // caluclate N*R2
di chi2tail(1, $statistic) // p-value = 0.538 > alpha. Theory can show that N*R2 ~ Chisq(#instruments - #endogenous)

** FAIL TO REJECT H0, so conclude that IVs are ALL exogeneous, so we can safely proceed with IV

******************************
// 2SLS and Heteroskedasticity
******************************

** With some caveats, ALL that applies for OLS, e.g. testing for heteroskedasticity,
** or applying robust SE (reg blah blah, robust) works for 2SLS... (Ch.15.6 for more details)

**********************
// Quick IV Simulation
**********************

clear all
set seed 123

program ivsim, rclass
	qui drop _all
	qui set obs 100
	qui generate z = rnormal(0,1)
	qui generate v = rnormal(0,1)
	qui generate x = 1*z + v // Change the 1 to a number <<1 (e.g. 0.05), what happens to the means of OLS and IV? ** 
	qui generate w = rnormal(0,1)
	qui generate u = 0.5*v + w // Observing u and x, Cov(x,u)=0.5*Cov(v,v)=0.5 which is not 0, hence endogeneity
	qui generate y = 0*x + u
	qui regress y x, noconstant
	qui return scalar b1ols = _b[x]
	qui ivregress 2sls y (x = z), noconstant
	qui return scalar b1iv = _b[x]
end

simulate "ivsim" beta1ols = r(b1ols) beta1iv = r(b1iv), reps(1000)

summarize beta1ols
summarize beta1iv

twoway (histogram beta1ols, color(red%30)) (histogram beta1iv, color(blue%30) lcolor(black)), graphregion(color(white))

** Notice that even IV is inconsistent now. When we make the instrument weak, it can cause ASYMPTOTIC BIAS.
*  So only use IV if you are confident that your instrument is not weak (using F>10 rule of thumb)

** END OF CONTENT **