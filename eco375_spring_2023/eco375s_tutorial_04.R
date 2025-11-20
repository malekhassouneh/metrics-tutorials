## Tutorial 4 ECO375 10/2/2023
################################################################################

# Housekeeping: 1) N/A

#################
# Extra Comments:
#################

################################################################################

##########################################
# Ordinary Least Squares (OLS) Regression:
##########################################

## Example 2 - Salary and Return on Equity (ROE):

?ceosal1
View(ceosal1)

with(ceosal1, cor(salary, roe)) # Initial correlation analysis, compare sign and
                                # magnitude to the estimated slope

lm_sal <- lm(salary ~ roe, data = ceosal1)
summary(lm_sal)

with(ceosal1, my_ols(salary, roe)) # Verify estimates with our function (Tutorial 3)

# NOTICE: VERY poor R2 ... because of an outlier? Check below

with(ceosal1, plot(salary ~ roe,
                   xlab = "Return on Equity", ylab = "Salary",
                   main = "Effect of ROE (Managerial Performance) on Salary",
                   xlim = c(0, max(roe)), ylim = c(0, max(salary)), 
                   pch = 1 , col = rgb(0,0,0) ))
abline(lm_sal, 
       col = rgb(1,0,0), lwd = 2) # Add the ESTIMATED/SAMPLE regression line to 
                                  # the scatter plot

# NOTE: TRY AT HOME, verify the properties of OLS as done in Example 1

### Standardized Regression:

# NOTE: Works ONLY for SLR WITH constant/intercept, to find the statistical
#       significance of the CORRELATION between two variables

wage1$wage_std <- scale(wage1$wage) # scale(), standardizes a variable i.e.
                                    # variable now has mean = 0, var = 1
wage1$educ_std <- scale(wage1$educ)

with(wage1, hist(wage))
with(wage1, hist(wage_std))

# NOTICE: Comparing the empirical distributions for non-standardized and 
#         standardized variable, SIMILAR shape BUT recentered AND lower
#         degree of variation

lm_std <- lm(wage_std ~ educ_std, data = wage1)
summary(lm_std)

# INTERPRETATION: An increase in educ by 1 STANDARD DEVIATION, increases wage
#                 by 0.4059 STANDARD DEVIATIONS

# NOTICE: Significance results did NOT change, i.e. if a variable is significant
#         then it is significant when it is standardized

with(wage1, cor(wage, educ)) # Coincidence? NO, OLS slope IS correlation coefficient

coef(lm_std)[2]*sd(wage1$wage)/sd(wage1$educ) # Convert coefficient to
                                              # non-standardized estimate
coef(lm_educ)

### Log Models & Elasticities:

## Log-Regressand (Log-Level) Model:

lm_logy <- lm(log(wage) ~ educ, data = wage1)
summary(lm_logy)

# INTERPRETATION: An increase in educ by 1 increases wage by 8.2% APPROXIMATELY

## Log-Regressor (Level-Log) Model:

lm_logx <- lm(wage ~ log(educ), data = wage1) # Why do we get an error?
table(wage1$educ) # CAN'T apply log(0) = - infinity, NOT well defined

lm_logx <- lm(wage ~ log(educ), data = subset(wage1, educ > 0))
summary(lm_logx)

# INTERPRETATION: An increase in educ by 1% increases wage by 0.053 APPROXIMATELY

## Log-Log Model:

lm_loglog <- lm(log(wage) ~ log(educ), data = subset(wage1, educ > 0))
summary(lm_loglog)

# INTERPRETATION: An increase in educ by 1% increases wage by 0.83% APPROXIMATELY. 
#                 This is a CONSTANT ELASTICITY, less than a one-to-one increase

# NOTE: TRY AT HOME, Exercise C2.5 using ?rdchem

### Dummy Variable Regression:

lm_female <- lm(wage ~ female, data = wage1)
summary(lm_female)

w_bar_f <- mean(subset(wage1, female == 1)$wage)
w_bar_m <- mean(subset(wage1, female == 0)$wage)

w_bar_f - w_bar_m # Gender wage differential, with female == 0 as base group

################################################################################

###################
# OLS Unbiasedness:
###################

### Unbiasedness Monte Carlo Simulation:

# MOTIVATION: Unbiasedness, efficiency etc. are properties of OLS estimators
#             that only make sense in a world where we have MANY data sets
#             of the same variables, so we can estimate a SEQUENCE of OLS
#             estimates. Unbiasedness would then be connected to the MEAN of this
#             sequence of estimates, and efficiency the VARIANCE of this sequence.
#             In reality we DON'T have many data sets, but to mimic this fictional
#             case in which we do, we GENERATE/SIMULATE our own data.

# Data Generating Process (DGP): y = 1 + 2*x + u, x ~ N(0,1), u ~ N(0,1) 
#                                independently.

# NOW, simulate this data with n = 10, N = 100 times/replications. Plot the
# histograms for beta1_hat AND beta0_hat. 

n <- 10 # Change to 25, 50, 100, 1000 to see what happens to the histograms

beta0_hat <- c() # Empty vectors to store the results from the simulation loop
beta1_hat <- c()

set.seed(123)

for (i in 1:100) { # Number of REPETITIONS improves the accuracy of our results
  
  x <- rnorm(n)
  u <- rnorm(n)
  y <- 1 + 2*x + u
  
  ols <- lm(y ~ x)
  beta0_hat[i] <- coef(ols)[1]
  beta1_hat[i] <- coef(ols)[2]
}

mean(beta0_hat)
mean(beta1_hat) # BOTH means are very close to the TRUE parameter values

par(mfrow=c(2,1)) # Graphical PARAMETERS, change how many windows in Plots tab

hist(beta0_hat, breaks = seq(-1, 3, len = 15), prob = TRUE)
curve(dnorm(x, 1, 1/sqrt(n)), 
      col = rgb(1,0,0), lwd = 2, add = TRUE)

hist(beta1_hat, breaks = seq(0, 4, len = 15), prob = TRUE)
curve(dnorm(x, 2, 1/sqrt(n)), 
      col = rgb(1,0,0), lwd = 2, add = TRUE)

dev.off() # Resetting Plots margins

# NOTE: Unbiasedness DOESN'T depend on the homoskedasticity assumption, in this
#       case from assuming u ~ N(0,1). Can replace u <- rnorm(n) above with:
#       u <- c()
#       for(j in 1:n){
#         u[j] <- rnorm(1, 0, x[j])
#       }

################################################################################

#################
# Some Exercises:
#################

### Computer Exercises (Textbook):

## Exercise C2.6:

?meap93
View(meap93)

# Q1: Do you think that each additional $ spent has the same effect on the pass 
#     rate?

# NO, we would expect diminishing returns to the additional dollar spent, i.e., 
# schools with low expenditure (per student) are expected to experience higher
# returns from the additional dollar invested, compared to schools that are already 
# highly-invested in, with high-quality educational capital and staff.

# Q2: In math10 = beta0 + beta1*log(expend) + u, argue that beta1/10 is the ppt 
#     (percentage point) change in math10 given a 10% increase in expend:

# Generic: y '=. beta0 + beta1*log(x), where '=. indicates "approximately equal"
#          y' '=. beta0 + beta1*log(x')

# y' - y '=. beta1*[log(x')-log(x)]
#         =  beta1*log(x'/x)          
#         =  beta1*log(1 + x'/x - 1)
#         =  beta1*log(1 + (x'-x)/x)
#         =  beta1*log(1 + dx/x)       # dx = x'-x is typically written delta(x)
#        '=. beta1*(dx/x)              # log(1 + a) '=. a when a is small
#         =  beta1/100 * (dx/x*100)
#         =  beta1/100 * %dx

# If %dx = 10, then dy = y'-y '=. beta1/100*10 = beta1/10.
# As y is a (pass) rate (between 0 and 100), a unit increase in y means a ppt
# increase in y. SO, given a 10% increase in `expend`, `math10` changes by 
# beta1/10 units (ppt).

# NOTE: EMPHASIZE, percentage point NOT the same thing as percentage

# Q3: Estimate the model in Q2. Report the estimated equation:

lm_math <- lm(math10 ~ log(expend), data = meap93)
summary(lm_math)

# Q4: How big is the estimated spending effect?

# A 10% increase in expenditure per student is associated with 1.12 PERCENTAGE
# POINT increase in the math pass rate ON AVERAGE.

# Q5: One might worry that regression analysis can produce fitted values for 
#     math10 that are greater than 100. Why is that not a worry in this data set?

with(meap93, plot(math10 ~ log(expend)))
abline(lm_math, 
       col = rgb(1,0,0), lwd = 2)

max(meap93$math10) # Max pass rate observed
max(lm_math$fitted) # Max fitted value (predicted pass rate) given observed log(expend)

# BOTH, largest observed pass rate (math10) AND largest predicted pass rate
# (fitted value) are far away from 100. SO, unless we extrapolate (i.e. use model
# to predict values beyond the observed range of the regressor, i.e. extend the
# fitted line ... NOT a good idea usually), NOT worry about producing fitted
# values greater than 100.

# NOTE: TRY AT HOME, exercise C2.8, with random number generation (if have time
#       will do next class)

################################################################################

#############################
# Multiple Linear Regression:
#############################

### Multiple Linear Regression (MLR):

# NOTE: NOT the same thing as Multivariate Regression, where we have more
#       than one IV (regressor) AND more than one DV (response) (beyond scope
#       of this course)

## Example 1 - Wage, Education, Experience:

cor(wage1[c("wage", "educ", "exper")]) # Take note of signs of correlations

lm_multi <- lm(wage ~ educ + exper, data = wage1)
sum_multi <- summary(lm_multi)
sum_multi

# NOTICE: Comparing estimate of educ's coefficient here compared to SLR, estimate
#         of effect is LARGER. Why? NEGATIVE correlation between educ and exper
#         (makes sense? see more next class, OVB). NOW interpretation is caveated 
#         with ceteris paribus i.e. "all other things (VARIABLES) being equal"

# NOTE: ALL that was discussed in SLR, properties, unbiasedness etc. HOLDS in 
#       MLR case. Try repeating and showing at home.

### Calculating Estimates with Matrix Algebra:

# NOTE: Why bother with matrix algebra? Get a CONCISE representation of the
#       coefficient estimates, instead of a system of equations that is 
#       COMPLICATED to solve for each coefficient estimate individually (as
#       was done in SLR)

# install.packages("matlib")
library(matlib) # Package needed to take inverse of a matrix

constant <- rep(1, nrow(wage1))
regressors <- as.matrix(wage1[c("educ", "exper")])

X <- cbind(constant, regressors) # n x k = 526 x 3 matrix

Y <- as.matrix(wage1$wage) # n x 1 = 526 x 1 matrix (i.e. column vector)

beta <- inv(t(X)%*%X)%*%t(X)%*%Y # Using beta_hat = (X'X)^(-1)*X'Y

beta - coef(lm_multi) # Compare manual estimates to lm() estimates

### Goodness of Fit (R2) in MLR:

set.seed(123)

wage1$random_noise <- rnorm(nrow(wage1))

cor(wage1[c("wage", "educ", "exper", "random_noise")])

lm_withnoise <- lm(wage ~ educ + exper + random_noise, data = wage1)
sum_withnoise <- summary(lm_withnoise)
sum_withnoise 

r2_nonoise <- sum_multi$r.squared
r2_withnoise <- sum_withnoise$r.squared

c(r2_nonoise, r2_withnoise)

# NOTE: Can experiment with the specification of the noise, to get more
#       drastic noise, and STILL show that R2 DOESN'T decrease when add a
#       regressor, EVEN if extremely irrelevant

# NOTE: Try at home, go back to the unbiasedness simulation, ADD another
#       regressor, say z <- rnorm(n), so that y <- 1 + 2*x + 3*z + u, 
#       and check that ALL of beta0_hat, beta1_hat, beta2_hat are unbiased

### Prediction:

?predict 

# NOTE: predict is a VERY helpful function, will use later when constructing
#       confidence intervals etc.

predict(lm_educ, newdata = data.frame(educ=c(0, 12, 16))) # Over/under predict?

# NOTE: Works exactly the same with SLR, in either case need to specify a 
#       DATAFRAME to input new values, and dataframe must have SAME column
#       names as the variables in the model
