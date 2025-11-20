## Tutorial 3 ECO375 3/2/2023
################################################################################

# Housekeeping: 1) Updated tutorial 2 R script on Quercus
#               2) MindTap, Ch. 3-4 February 19th. Will cover content needed
#                  for later questions over the next 2 tutorials

#################
# Extra Comments:
#################

na.omit(bwght) # Remove ANY row/observation with an NA in ANY column

nrow(bwght)
summary(bwght)

nrow(na.omit(bwght))

### Some Concepts:

# 1) Sample vs Population Quantities (Prob vs. 1/n)
# 2) Correlation vs Causation
# 3) Statistical vs Economic Significance

################################################################################

####################
# For & While Loops:
####################

### Some Practical Applications of Loops:

# NOTE: Loops will be most useful when conducting SIMULATIONS (see later today)

## Standardizing Variables:

library(wooldridge)

?wage1
View(wage1)

vars <- c("wage", "exper", "educ") # Simple example, only 3 variables, more
                                   # useful if have MANY variables

for (i in vars){ # Loop over STRINGS, NOT numbers
  
  name <- paste(i, "_std", sep = "") # Create a NAME object for variables
  assign(name, (wage1[,i] - mean(wage1[,i]))/sd(wage1[,i]))
}

# NOTE: BUT, in this method, the generated variables are not saved in a 
#       dataframe format, not ideal. SO, see methods applied below, creating
#       an empty data frame BEFORE hand and BINDING it with existing data

# NOTE: Could just use scale() to standardize variables ... will use below
#       instead of a manual loop

## Changing Units of Observations:

set.seed(123)

# NOTE: A seed is a number that is chosen (it is trivial) so that any pseudo-random
#       procedure that R runs can be tractable, i.e. results are reproducible
#       if needs be. 

jan_avg <- runif(100, 32, 100) # Just randomly generating some temperature data
                               # from a uniform dist. for this demonstration
feb_avg <- runif(100, 32, 100)
mar_avg <- runif(100, 32, 100)

temps_f <- data.frame(jan_avg, feb_avg, mar_avg)

temps_c <- data.frame(matrix(NA, nrow = 100, ncol = 3))

for (i in 1:ncol(temps_f)){ # Loop over NUMBERS, NOT strings
  
  temps_c[,i] <- (temps_f[,i]- 32)*5/9 # Convert Fahrenheit to Celsius
  colnames(temps_c)[i] <- paste(colnames(temps_f)[i], "_c", sep = "")
}

View(temps_c)

## Generating Dummies:

# NOTE: This is VERY USEFUL; will see an application later today (more Ch.6)

months <- sample(1:12,100,TRUE) # Uniformly sample WITH REPLACEMENT 100 values 
                                # from 1-12, think of this as months Jan-Dec

dummies <- data.frame(matrix(NA, nrow = 100, ncol = 12))

for (i in seq(from = 1, to = 12, by = 1)){ # seq SAME as 1:12, but changes increment
  
  name <- paste("month_", i, sep = "")
  dummies[i] <- ifelse(months == i, 1, 0)
  colnames(dummies)[i] <- name
}

View(cbind(months, dummies))

# NOTE: Will actually skip WHILE loops. NOT as important for us

################################################################################

##########################################
# Ordinary Least Squares (OLS) Regression:
##########################################

### Simple Linear Regression (SLR):

## Example 1 - Wage and Education:

with(wage1, cor(educ, wage)) # Take note of magnitude and sign of correlation

lm_educ <- lm(wage ~ educ, data = wage1) # lm() for Linear Model
sum_educ <- summary(lm_educ)
sum_educ

typeof(lm_educ)
str(lm_educ) # Check the contents of the LIST using str()

typeof(sum_educ)
str(sum_educ)

# NOTICE: The model itself is a DIFFERENT list to the summary. Get different
#         information from both when apply $ operator.

with(wage1, plot(wage ~ educ,
                 xlab = "Years of Education", ylab = "Hourly Wage",
                 main = "Effect of Education on Wage",
                 xlim = c(0, max(educ)), ylim = c(0, max(wage)), 
                 pch = 1 , col = rgb(0,0,0) ))
abline(lm_educ, 
       col = rgb(1,0,0), lwd = 2)

# Programming Our Own OLS Function:

# MOTIVATION: Want to verify that R estimates the coefficients using the same
#             formulas that we derived from theory. We create a new OBJECT
#             called a FUNCTION that does OLS for us manually

my_ols <- function(y, x) { # Write a function with generic arguments x and y
  
  dx = x - mean(x)
  dy = y - mean(y)
  beta1_hat = sum(dx*dy) / sum(dx^2)
  beta0_hat = mean(y) - beta1_hat * mean(x)
  return( c(beta0_hat, beta1_hat) )
} 

with(wage1, my_ols(wage, educ))
coefficients(lm_educ) # OR, coef()

# NOTE: Argument order MATTERS if DON'T specify argument name. R will take
#       default order for each function

# NOTICE: The objects that are IMPLICITLY calculated in my_ols are NOT saved
#         in the environment (e.g. dx and dy), ONLY in LOCAL ENVIRONMENT 
#         (temporary space) then discarded. SCOPE of function is different

# Verifying Some OLS Properties:

wage_hat <- lm_educ$fitted.values
u_hat <- lm_educ$residuals

wage1$wage - (wage_hat + u_hat) # Check that y = y_hat + u_hat, BY CONSTRUCTION

sum(u_hat) # Sample equivalent of E(u|X) = 0 => E(u) = 0 (2.30 in text?)
sum(wage1$educ*u_hat) # Sample equivalent of E(u*x) = 0 (2.31 in text?)

# NOTE: Numbers above not EXACTLY 0, why? Rounding error, otherwise numbers are
#       virtually 0.

# NOTE: Will see later that these properties ALSO hold for MLR, with more than
#       one regressor, i.e. they aren't unique to SLR

# Goodness of Fit:

wage_bar <- mean(wage1$wage)

SSR <- sum((wage1$wage - wage_hat)^2)
SSE <- sum((wage_hat - wage_bar)^2)
SST <- sum((wage1$wage - wage_bar)^2)

SSE + SSR == SST

# NOTE: This last Boolean check might NOT always work, as R will have some
#       rounding error (see example below, where we have to round).

R2 <- SSE/SST
sum_educ$r.squared == R2

# INTERPRETATION: educ explains 16.48% of the TOTAL variation in wage

beta <- lm_educ$coefficients # OR, coef(lm_educ)
educ_bar <- mean(wage1$educ)

beta[1] + educ_bar*beta[2] == wage_bar # Check if estimated regression line
                                       # goes through point (x_bar, y_bar)

round(beta[1] + educ_bar*beta[2], 2) == round(wage_bar, 2)
# ROUND both numbers up to 2 digits to avoid issue of rounding error

## Example 2 - Salary and Return on Equity (ROE):

?ceosal1
View(ceosal1)

with(ceosal1, cor(salary, roe)) # Initial correlation analysis, compare sign and
                                # magnitude to the estimated slope

lm_sal <- lm(salary ~ roe, data = ceosal1)
summary(lm_sal)

with(ceosal1, my_ols(salary, roe)) # Verify estimates with our function

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
#       significance of the correlation between two variables

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

with(wage1, cor(wage, educ)) # Coincidence? NO, OLS slope IS correlation coefficient

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
#                 This is a CONSTANT ELASTICITY, less than one-to-one increase

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

# Data Generating Process (DGP): y = 1 + 2*x + u, x ~ N(0,1), u ~ N(0,1) independently.

# NOW, simulate this data with n = 10, N = 1000 times/replications. Plot the
# histograms for beta1_hat AND beta0_hat. 

n <- 10 # Change to 25, 50, 100, 1000 to see what happens to the histograms

beta0_hat <- c() # Empty vectors to store the results from the simulation loop
beta1_hat <- c()

set.seed(123)

for (i in 1:1000) {
  
  u <- rnorm(n)
  x <- rnorm(n)
  y <- 1 + 2*x + u
  
  ols <- lm(y ~ x)
  beta0_hat[i] <- coef(ols)[1]
  beta1_hat[i] <- coef(ols)[2]
}

mean(beta0_hat)
mean(beta1_hat) # BOTH means are very close to the TRUE parameter values

par(mfrow=c(1,2)) # Graphical PARAMETERS, change how many windows in Plots window

hist(beta0_hat, breaks = seq(-1, 3, len = 15), prob = TRUE)
curve(dnorm(x, 1, 1/sqrt(10)), 
      col = rgb(1,0,0), lwd = 2, add = TRUE)
hist(beta1_hat, breaks = seq(0, 4, len = 15), prob = TRUE)
curve(dnorm(x, 2, 1/sqrt(10)), 
      col = rgb(1,0,0), lwd = 2, add = TRUE)

dev.off() # Resetting Plots margins

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

# BOTH, largest observed pass rate (math10) AND largest predicted pass rate (fitted 
# value) are far away from 100. SO, unless we extrapolate (i.e. use model to
# predict values beyond the observed range of the regressor, i.e. extend the
# fitted line ... NOT a good idea usually), NOT worry about producing fitted
# values greater than 100.

# NOTE: TRY AT HOME, exercise C2.8, with random number generation (if have time
#       will do next class)