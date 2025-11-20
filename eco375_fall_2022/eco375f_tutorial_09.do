// Tutorial #9 ECO375 25/11/2022
*******************************

**********************
// Additional Comments
**********************

/*Prelims: 1) Video is for TUTORIAL 9, TUTORIAL 8 = heteroskedasticity covered in class (so no video for that) */
		   
** Extra Comments **

/*Heteroskedasticity Comments: 

	1) ALWAYS use hetero-robust SE, BUT with small sample size sampling distribtion is not t-dist., BAD INFERENCE, be careful
	2) Why H0 is homosk. when testing for heterosk.? By construction, as homosk. is what we "ideally" want
	3) FGLS NOT unbiased (as we use h_hat not h) and NOT BLUE, BUT it is consistent and asymptotically efficient
	4) Know that the GLS specification is correct if when use ,robust, standard errors not change that much. Why? Hetero-robust
	   standard errors account for arbitrary form of heteroskedasticity, while GLS only for a specific form (that we assume).
	   SO if GLS standard errors WHEN account for arbitrary heteroskedasticity experience no change, then GLS assumed 
	   form of heteroskedasticity IS the form of heteroskedasticity that we see in data */

******************************
// Instrumental Variables (IV)
******************************

cd "C:\Users\malek\Desktop\TAship\ECO375H5F\My Tutorials\Tutorial 9"
use MROZ, clear // Data on married working women

regress lwage educ // OLS estimation

** CLEARLY have endogeneity, Cov(educ,u)!=0, unobservables such as family income, parent educ etc.
** OR could have bias due to reverse causality. Expect the direction of bias to be POSITIVE

/* Now, propose fatheduc as an INSTRUMENT for educ. Check EXOGENEITY/VALIDITY and RELEVEANCE. CAN'T test
   exogeneity, hence must assume/argue it, why? CAN'T get consistent residuals from OLS as did when
   testing for heteroskedasticity and use them in place of u, as now OLS residuals are INCONSISTENT. */

regress educ fatheduc if hours>0 // verify the relevance of fatheduc (significant effect, explains 17% of variation in educ)

ivregress 2sls lwage (educ = fatheduc) // estimation by IV (2SLS), syntax: (endogenous VAR = instrument)
ivregress 2sls lwage (educ = fatheduc), first // ,first to display FIRST STAGE of 2SLS

** CHECK that IV estimate is indeed ratio of sample covariances Cov(lwage,fatheduc)/Cov(educ,fatheduc)
** NOTE: NEED to add if hours>0, as only want to focus on working women. Above, Stata drops these observations

corr fatheduc educ if hours>0, covariance // check that IV estimate is indeed Cov(y,z)/Cov(x,z)
global cov1 = r(cov_12) 
corr fatheduc lwage if hours>0, covariance
global cov2 = r(cov_12) 
di $cov2/$cov1 // EXACTLY the 2SLS estimate from Stata above

** Notice, IV point estimate is HALF of OLS (confirms our prior), but significance NOT HOLD anymore? Why?
** Standard errors INFLATED (IV standard errors > OLS standard errors). NOT correcting potential bias? 

** NOTE: DO NOT use "ivreg" (old Stata command, outdated auxiliary tests and commands)

*********************************
// Two Stage Least Squares (2SLS)
*********************************

// Implement IV Regression Manually (2SLS)

** NOTE: Makes more sense in a case with 2+ instruments, but could also implement here

** STEP 1 - FIRST STAGE Regression of Endogenous Variable on Instrument

reg educ fatheduc if hours>0
predict educ_hat, xb // extract fitted values

** STEP 2 - SECOND STAGE Regression of Explanatory Variable on Fitted Values

reg lwage educ_hat

** NOTE: Coefficients are identical as ivregress 2SLS, BUT standard errors in second stage MUST be adjusted,
** i.e. standard errors NOT the same, and are WRONG for our purposes. So use IV standard errors, NOT these

// Example (Question C15.2) (With CONTROLS)

use FERTIL2, clear

regress children educ age agesq // OLS Estimation

regress educ age agesq frsthalf // Use frsthalf = dummy if born in first 6 months, as an INSTRUMENT

** frsthalf is clearly a good instrument for educ (ASSUME exogeneity, and SHOWED relevance)
** IMPORTANT: testing for relevancy (i.e. first stage), MUST include ALL other controls

ivregress 2sls children (educ = frsthalf) age agesq // IV Estimation, coefficient of educ MUCH larger

regress children educ age agesq electric tv bicycle // Adding, (electric, tv, bicycle) assume exogeneous
ivregress 2sls children (educ = frsthalf) age agesq electric tv bicycle // adding these controls REDUCES effect of educ slightly in OLS AND IV

*****************************
// Detecting Weak Instruments
*****************************

** Why care about weak instruments? Can lead to 1) LARGER SE 2) LARGER asymptotic bias than OLS

** Rules of Thumb: t-stat > 3.2 (sqrt 10) for HOMOSKEDASTIC errors + t-stat > 4.47 (sqrt 20) HETEROSKEDASTIC (first OR second stage)
** BUT rules of thumb aren't knife-edge cutoffs, e.g. F=9.98<10, could still be a non-weak instrument

** Is fatheduc a weak instrument? 
reg educ fatheduc if hours>0 // F statistic 88.84, or t statistic 9.43, SO REJECT H0 of "weakness"

** Is frsthalf a weak instrument? 
regress educ age agesq frsthalf // NOT using global F statistic as now have other controls, but t statistic |-7.55|

** NOTE: Will see later, if have TWO instruments, need to use F-test (test instrument1 instrument2)