// Tutorial #4 ECO375 21/10/2022
*******************************

**********************
// Additional Comments
**********************

/*Prelims: 1) When are grades out?
           2) Friday solution for upcoming weeks? Poll?
		   3) Any feedback before getting into content?*/
			
** Extra Comments **	

/* What we skipped? 1) Standardized regression, you can run at home, 3 different ways of standardizing
				*/

 /*OPTIONAL - Random Walks (useful in Financial modelling) - to motivate programing (not needed for us, hopefully)
*************************************************
// Program and Simulation Example (Multi-Layered)
*************************************************

/* NOTE: The examples discussed below are NOT strictly related to class content.
         It is more for you to understand how programs and simulations work using
		 interesting examples */

** Simple Correlation Simulation **

/* What are we doing? We generate 3 random variables x1, x2, x3 from different uniform
   distributions. They are generated/sampled INDEPENDENTLY, because x1 or x2 being
   a certain value doesn't affect the value of x3 generated etc. We then find the
   pairwise correlation between the 3 variables, and store the values. We repeat this
   "experiment" 1000 times to find the AVERAGE correlation between the 3 variables.
   We then plot the histogram of the correlations of the 3 variables. 
   
   What do we expect to get? An average correlation of 0 for all variables, and narrow histograms 
   centered around 0. Why? Again, the 3 variables are independently sampled, but in one sample,
   it might just be coincidentally that one variable is slightly correlated with another,
   but on average we expect this "slight correlation" to be 0. The histograms being narrow
   is because if any correlation arises in one of our samples it will always be "small",
   and the histograms being centered at 0 is because the average correlation should be 0.*/

program corr123, rclass
	clear
	drop _all
	set obs 1000
	
	gen x1= runiform(0,10)
	gen x2= runiform(0,5)
	gen x3= runiform(0,2)

	corr x1	x2
	return scalar cx1x2= r(rho)
	corr x1 x3
	return scalar cx1x3 = r(rho)
	corr x2 x3
	return scalar cx2x3 = r(rho)
end

simulate "corr123" corrx1x2=r(cx1x2) corrx1x3=r(cx1x3) corrx2x3=r(cx2x3) , reps(10000)
summarize // to fine the average pairwise correlations
histogram corrx1x2, normal // repeat this with corrx1x3 and corrx2x3

** Random Walk Return on Index Funds Simulation **

/* What are we doing? We generate 2 index funds using a random walk model. We start at 100 for one fund 
   at period t=1, and at t=2 the value of our fund can either go up or down by some N(0,1) shock/noise
   relative to t=1, and so on for all periods up to t=1000. Then we record the correlation between the
   two funds, the standard deviation of each fund, and the percentage return of each fund (calculated
   as the value at t=1000 minus 100 (the value at t=1) divided by 100). So we get a total of 5 values.
   
   What do we expect to get? We repeat the experiment 1000 times. We would expect that for each repetition 
   of this experiment, whenever the standard deviation of one of the index funds declines, then given that
   the returns on the funds are roughly of the same order of magniture, then the correlation between the two
   funds should decline. Why? Because one is varying less, while the other is varying just as much, so there is
   a weeker simultaneous relationship. If they both vary alot then the correlation will be high.
 .*/

program define rwalk, rclass
	clear
	drop _all
	set obs 1000

	// Generate time variable
	gen t= _n
	label var t "Time (Trading Days)"

	// Generate indices
	gen s1 = 100 if t==1
	label var s1 "Index 1 Value(in %,100 at t=1)"

	gen s2 = 100 if t==1
	label var s2 "Index 2 Value(in %,100 at t=1)"
	
	// Random walk function
	replace s1 = s1[_n-1] + rnormal() if _n > 1
	replace s2 = s2[_n-1] + rnormal() if _n > 1
	
	// Graph the two functions
	**qui twoway (line s1 t) (line s2 t)
	
	// Save correlations and standard deviations
	corr s1 s2
	return scalar corrs1s2 = r(rho)
	sum s1 
	return scalar s1sd = r(sd)
	sum s2
	return scalar s2sd = r(sd)
	
	// Save returns for each "path" and each iteration
	return scalar s1return = (s1[1000]-100)/100 // return rate on index s1, value at t=1000 vs t=1
	return scalar s2return = (s2[1000]-100)/100 // return rate on index s1, value at t=1000 vs t=1
end

simulate "rwalk" corr_s1s2=r(corrs1s2) s1_sd=r(s1sd) s2_sd=r(s2sd) s1_return=r(s1return) s2_return=r(s2return), reps(1000)

sum s1_return,detail
sum s2_return,detail
summ corr_s1s2,detail
summ s1_sd, detail
summ s2_sd, detail

scatter s1_return s2_return
graph box corr_s1s2 */

***********************************************
// New Commands From Tutorial 2 (FINAL ATTEMPT)
***********************************************

cd "C:\Users\malek\Desktop\TAship\ECO375H5F\My Tutorials\Tutorial 2" 
use 401K, clear

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

sum prate if age<=10 & age>=2 //equivalent to below
sum prate if inrange(age,2,10) //inrange selects observations for which a variable takes on values in a range 2<=age<=10

** Go back to wage dataset
cd "C:\Users\malek\Desktop\TAship\ECO375H5F\My Tutorials\Tutorial 1" 
use wage1, clear

sum wage if inlist(female, "female") //inlist selects observations for which a variable takes on specific character values
* This line of code doesn't work as we don't have a string variable female yet

//Other Variable Renaming, Recoding, and Relabeling
rename wage wage_earnings //self explanatory renaming

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

*****************************************
// Accessing Returned Results (IMPORTANT)
*****************************************

/* Types of Classes (Briefly) (command can be more than one):
1) r-class, general commands
2) e-class, estimation commands
3) Others you won't use: s-class, n-class, c-class (e.g. to get the time)

NOTE: For ALL classes, results will be stored up until a command from the same class is run*/

cd "C:\Users\malek\Desktop\TAship\ECO375H5F\My Tutorials\Tutorial 2" 
use 401k, clear

//Demonstration of the note above
sum prate
return list // get all values stored from LAST r-class command that was used

reg prate mrate age
return list // compare the list of numbers returned above and here

ereturn list // get all values stored from LAST e-class result that was used

//How to access values from r and e class
matrix list e(b) // for matrices, matrix list ; here extracting the parameter estimates
matrix b = e(b) // save the matrix
matrix b2 = get(_b) // OR alternative way to extract estimates
matrix list e(V) // extracting variance-covariance matrix
matrix cov2 = get(VCE) // alternative way to extract variance-covariance matrix
dis e(N) // for scalars, display
dis _b[mrate] // get single value scalars
dis _se[mrate]

sum if e(sample)==1 // summarize variables given only their data points used in the regression
gen flag = e(sample) // eg used if have missing values in variables that you didn't remove
sum flag // mean is 1 so we used all of the data points

//Local vs Global Macros (shortcuts)

local ci = invnormal(0.975)
local uc = 2 + `ci'
di `uc'

global ci = invnormal(0.975)
global uc = 2 + $ci
di $uc
** ALMOST ALWAYS use global macros, local macros will be discarded by your memory frequently

*e.g. for predicting values using a regression model
global b0 = _b[_cons] // saving the estimated parameters
global b1 = _b[mrate]
global b2 = _b[age]

dis $b0 + $b1*2 + $b2*5 // equation of a line/plane, find prate when mrate=2 and age=5

*****************************
// Multiple Regression Topics
*****************************

** Some Miscs from Simple Linear Regression **

cd "C:\Users\malek\Desktop\TAship\ECO375H5F\My Tutorials\Tutorial 2" 
use 401k, clear

regress prate mrate age

predict fitted, xb // get fitted values Xb (from most RECENT regression on Stata)
predict resid, residuals // get residuals

generate new_var = fitted + resid // fitted + residuals gives you response variable i.e. regressand y by construction

gen mrate_resid = mrate*resid // can repeat with age instead of mrate
sum resid 
dis r(sum) // sum/mean should be equal to 0, why?
sum mrate_resid 	
dis r(sum) // sum/mean should be equal to 0, why?
* Above two equal to 0, result in textbook (2.30 and 2.31) (if not exactly 0 here, rounding error..)
* SO ALSO hold for MLR

** Perfect Collinearity **

cd "C:\Users\malek\Desktop\TAship\ECO375H5F\My Tutorials\Tutorial 4" 
use wage1, clear

gen single = 1 if married !=1 // generate an indicator variable = 1 if not married
replace single = 0 if single !=1 // replace missing values with 0

regress lwage educ exper married // running a regression including married ONLY
regress lwage educ exper married single // Stata is smart enough to catch PERFECT colinearity

** Aside
ssc install outreg2 // installing a package for fancy Stata regression tables
outreg2 using coll, tex ctitle(No Perfect Collinearity) replace // Get fancy paper-style Stata regression table
**

gen single2 = single + rnormal(0,0.01) // trying to trick Stata, want a dummy variable not exactly 1, add small random shock
replace single2 = 0 if single2 == .

gen test = single2 + married // now single2+married is not exactly 1 for all observations, so no perfect collinearity

regress lwage educ exper married single2 // Stata is successfully tricked.... standard errors explode (see why next class)

** Partialling Out ** (Exercise C3.5)

regress lwage educ exper tenure // Ordinary regression

regress educ exper tenure // Partialling out method, regress regressor of interest on rest of regressors
predict resid, residuals // saving residuals

regress lwage resid // Partialing out auxiliary regression

** Omitted Variable Bias (OVB) **

regress lwage educ // have OVB, positive bias
regress lwage educ exper // still have OVB, even more positive bias
regress lwage educ exper expersq // Mincer equation (labor economics), beta1 decreases as expected
