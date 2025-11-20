## Tutorial 8 ECO375 17/3/2023
################################################################################

# Housekeeping: 1) Midterm grades released, 6% curve, pick up copy TODAY
#               2) ALL code required for MindTap is readily available, from today
#                  or previous R scripts (go back and browse them)

#################
# Extra Comments:
#################

################################################################################

########################################
# CLT and LLN Simulation Demonstrations:
########################################

### Law of Large Numbers (LLN):

# NOTE: LLN applies for the means of ALL distributions, e.g. go above and try 
#       simulating data from: 
#       1) U(0,1), mean = 1/2 (replace rnorm() with runif(as.numeric(n), 0, 1))
#       2) Chisq(2), mean = 2 (replace rnorm() with rchisq(as.numeric(n), 2))

# NOTE: LLN essentially says that the sample mean is a CONSISTENT estimator of
#       the population mean. Also will see that LLN is just a CONSEQUENCE of CLT,
#       although CLT provides additional information about the distribution that
#       LLN is silent/agnostic on.

### Central Limit Theorem (CLT):

# MOTIVATION: Draw a sample size of n={100, 1,000, 10,000} from, say, a Chisq(1)
#             distribution, 1000 times for EACH n, and each repetition, calculate 
#             the sample mean. SO, SAME as above, BUT now, generate a standardized
#             variable: sqrt(n)(x_bar - mu)/sqrt(sigma^2), and find its dist.

## Example 1 - Chi-Square(1):

# NOTE: The mean and the variance of chisq(df) are: df and 2*df

means <- data.frame(matrix(NA, nrow = 1000, ncol = 3)) # Two empty dataframes
colnames(means) <- c("100", "1000", "10000")

standardized <- data.frame(matrix(NA, nrow = 1000, ncol = 3)) 
colnames(standardized) <- c("100", "1000", "10000")

mean <- 1
var <- 2*1

set.seed(123)

for (n in c("100","1000","10000")){
  for (i in 1:1000){
    
    sample <- rchisq(as.numeric(n), 1)
    means[i,n] <- mean(sample)
  }
  
  standardized[,n] <- sqrt(as.numeric(n))*(means[,n] - mean)/sqrt(var)
  # Generating the standardized variable for the ENTIRE column (shorter code)
}

# Now, plot the histograms of the 1000 standardized variables for each n, and
# compare to the N(0,1) dist.:

hist(standardized[["100"]], col = rgb(0,0,1,1/4), prob = TRUE)
hist(standardized[["1000"]], col = rgb(1,0,0,1/4), prob = TRUE, add = TRUE)
hist(standardized[["10000"]], col = rgb(0,1,0,1/4), prob = TRUE, add=TRUE)
curve(dnorm(x, 0, 1), add = TRUE)

# NOTE: Significant overlap in the histograms, as n increases, resemble
#       less and less a chi-square distribution (skewed to the right)

## Example 2 - Uniform(0,1):

# NOTE: The mean and variance of uniform(a,b) is: 0.5*(a+b) and 1/12*(b-a)^2

# NOTE: GO into above code and replace:
#       1) rchisq(as.numeric(n), 1) with runif(as.numeric(n), 0, 1)
#       2) mean <- 1 with mean <- 0.5*(0+1)
#       3) var <- 2*1 with var <- 1/12*(0-1)^2

# NOTICE: Ultimately, CLT applies to ALL distributions, but distributions that
#         are LESS Normal, e.g. uniform (or other skewed distribution) will
#         require LARGER n to have a better Normal APPROXIMATION.

# NOTE: Good exercise at home, revisit "OLS Unbiasedness" code in Tutorial 4
#       R script, and modify the value of n in the code. This is an example of
#       the CONSISTENCY of OLS. The histograms also show the NORMALITY of OLS
#       by comparing the empirical histogram to the THEORETICAL N(0,1) density
#       (not exactly, as by construction, x and u are normal, SO, change their
#       distributions to anything else, and run the code again). Run the code, 
#       removing the "breaks" arguments inside of the hist() code.

##########################################
# Unbiasedness vs Consistency Simulations:
##########################################

### Biased BUT Consistent:

# EXAMPLE: Suppose you survey people for their annual incomes to estimate the
#          average income of the population. BUT some misreport their income by 
#          some error factor, say 10,000. So if someone's income is 90,000
#          they will wrongly report that they earn 100,000. Overall, 
#          the mean is exaggerated by 10,000/n (think about this as ONLY a 
#          subset, 1/n*100%, of people misreporting their income, NOT all)

n <- 25 # Change to 1000, see what happens to bias and histogram
set.seed(123)

mean_inc1 <- c() # Empty vector to store results

for (i in 1:1000){
  
  income <- rnorm(n,50000,5000)
  mean_inc1[i] <- mean(income) + 1000/n
}

mean(mean_inc1) # Bias is roughly 10,000/25 = 400

hist(mean_inc1, breaks = seq(40000, 60000, 100))

# NOTE: As change n from 25 to 1000, see that the distribution SHIFTS to the
#       true mean, 50000, i.e. bias goes to 0, AND the distribution collapses, 
#       i.e. narrows down as variance goes to 0. SO ASYMPTOTICALLY UNBIASED,
#       i.e. CONSISTENT (as n --> inf) even though BIASED in finite samples

### Unbiased BUT Inconsistent:

n <- 25 # Change to 1000, see what happens to histogram
set.seed(123)

mean_inc2 <- c()

for (i in 1:1000){
  
  income <- rnorm(n,50000,5000)
  mean_inc2[i] <- mean(income[1:25]) # ONLY look at first 25 people, REGARDLESS
                                     # of whether more people were surveyed
}

mean(mean_inc2) # Relatively UNBIASED, roughly 50,000, some sampling error as n
                # is not as large

hist(mean_inc2, breaks = seq(46000, 55000, 100)) 

# NOTE: EVEN with n=100,000 , distribution DOESN'T collapse to the true mean, 
#       as the variance DOESN'T go to 0. This is because the extra information
#       we get with more observations ISN'T being used when calculating the mean.
#       SO, UNBIASED but INCONSISTENT, as sample mean always uses the first 25
#       observations ONLY

# NOTE: Good exercise at home, revisit "OLS Unbiasedness" code in Tutorial 4
#       R script. Add beta1_tilde[i] <- coef(ols)[2] + 1/n INSIDE at the END of 
#       the for loop. THEN, find the mean, and plot the histogram for the
#       vector of estimates, as was done for beta1_hat. This will give you
#       an example of a BIASED BUT CONSISTENT estimator. An example of an
#       UNBIASED BUT INCONSISTENT estimator would be one that is based on
#       a regression with a FIXED sample size that DOESN'T change with n.
#       This can be generated by adding ols2 <- lm(y[1:10] ~ x[1:10]) AND
#       beta1_tilde[i] <- coef(ols2)[2] INSIDE the loop. This OLS estimator
#       focuses on the first 10 observations ONLY.

################################################################################

####################
# Interaction Terms:
####################

# MOTIVATION: Used when the PARTIAL/MARGINAL EFFECT of an explanatory variable 
#             on the response variable depends on the magnitude of ANOTHER 
#             explanatory variable (called the interaction effect). Think of this
#             as the generalization of the quadratic term (x^2) regression.

## Example - Housing Prices:

?hprice1
View(hprice1)

lm_price <- lm(price ~ sqrft + bdrms + sqrft:bdrms, data = hprice1)
summary(lm_price) # : is syntax for INTERACTION

lm_price2 <- lm(price ~ sqrft*bdrms, data = hprice1)
summary(lm_price2) # SAME regression as above, * is the FACTORIAL interaction,
                   # i.e. tells R to include the interaction term AND any lower 
                   # order terms (i.e. variables by themselves).

# NOTE: Does the interaction make sense? YES, larger houses (greater sqrft) will
#       have LARGER bedrooms that will have a GREATER effect on price compared
#       to the additional bedroom in a smaller house (lower sqrft)

# NOTE: If had 3 variables, * would include 3-way interaction AND all possible
#       2-way combinations (total of 3) AND single variables.

# NOTE: If you choose to include an interaction, ALWAYS include lower order
#       terms, otherwise interpretation WON'T make sense

### Graphing the Interaction Effect:

# NOTE: Want to graph:
#       price = beta0 + beta1*sqrft + beta2*bdrms + beta3*sqrft*bdrms
#             = beta0 + beta1*sqrft + (beta2 + beta3*sqrft)*bdrms
#       for a FIXED sqrft level (sqrft = low (100), high (800))

beta <- lm_price$coefficients # Extract coefficients from the model

curve( beta[1] + beta[2]*100 + (beta[3] + beta[4]*100)*x, 
       from = 0, to = 10, col = rgb(1, 0, 0), lwd = 2, 
       ylab = "price", xlab = "bdrms", ylim = c(-200, 300)) # At sqrft = 100

curve( beta[1] + beta[2]*800 + (beta[3] + beta[4]*800)*x, 
       col = rgb(0, 0, 1), lwd = 2, add = TRUE) # At sqrft = 800

# NOTICE: Graph displays both different intercepts AND different slopes due to
#         the interaction term. SO, larger houses have a larger price at EVERY
#         bdrms level (as sqrft is higher), AND experience LOWER decreases in 
#         price per extra room relative to smaller houses (flatter slope).

### Average Partial Effects:

# MOTIVATION: Want to find what is the partial effect at the AVERAGE house
#             in terms of a certain variable.

# NOTE: 1) Derivative of regression function with respect to bdrms is:
#       beta2 + beta3*sqrft
#       2) Derivative of regression function with respect to sqrft is:
#       beta1 + beta3*bdrms

beta[3] + beta[4]*mean(hprice1$sqrft) # Average partial effect of sqrft 
                                      # (evaluate at MEAN bdrms)

beta[2] + beta[4]*mean(hprice1$bdrms) # Average partial effect of bdrms 
                                      # (evaluate at MEAN sqrft)

##################
# Dummy Variables:
##################

### Single Dummy Variable:

?wage1 
View(wage1)

lm_female <- lm(wage ~ female + educ, data = wage1)
summary(lm_female) # BASE group is males (intercept estimate)

# IMPORTANT: Recall tutorial 3/4, DUMMY VARIABLE TRAP, DON'T include both female 
#            and male dummies, otherwise PERFECT COLINEARITY (female + male = 1)

# NOTE: SAME log-level logic could be extended here if we used log(wage)

## Graphing the Dummy Effect:

# NOTE: Want to graph:
#       wage = beta0 + delta0*female + beta1*educ , for a FIXED educ level (say 1)
#       and show the INTERCEPT vertical shift between females and males at
#       EVERY level of educ

beta <- lm_female$coefficients

with(wage1[wage1$female == 1,], plot(wage ~ educ, col = rgb(0,0,1)))
with(wage1[wage1$female == 0,], points(educ, wage, col = rgb(1,0,0)))

abline(a = beta[1] + beta[2], b = beta[3], 
       col = rgb(0,0,1), lwd=2) # Estimated regression function for FEMALES
abline(a = beta[1] , b = beta[3], 
       col = rgb(1,0,0), lwd=2 ) # Estimated regression function for MALES

### Multiple Categories (Multiple Dummies):

?wage1
View(wage1)

lm_multi <- lm(wage ~ female + married + educ + exper, data = wage1)
summary(lm_multi) # BASE group is SINGLE males (intercept estimate)

# NOTICE: Marriage premium is the SAME for females and males ... just the 
#         coefficient for married

## Alternative Model Specification:

# MOTIVATION: Want to estimate the wage differential SEPARATELY for EACH group

wage1$male <- 1 - wage1$female # Create male and single dummies
wage1$single <- 1 - wage1$married

# Generate dummies for the  4 - 1 = 3 categories of male/female and married/single
# LEAVE OUT single males as a BASE GROUP
wage1$mar_male <- wage1$married*wage1$male 
wage1$mar_female <- wage1$married*wage1$female
wage1$sing_female <- wage1$single*wage1$female

lm_multi2 <- lm(wage ~ sing_female + mar_female + mar_male + educ + exper, 
                data = wage1)
summary(lm_multi2)

# NOTICE: Marriage premium is NOT the same for females and males, but close enough

# RULE OF THUMB: With n binary categories, ALWAYS include 2^n - 1 dummies, i.e.
#                always omit ONE base group

### Interaction Between Two Dummies:

# NOTE: Instead of defining groups separately manually, can simply do this
#       by INTERACTING two dummies

lm_multi3 <- lm(wage ~ female*married + educ + exper, data = wage1)
summary(lm_multi3) # Compare to estimates in lm_multi2 ... SAME, but different
                   # representation ... e.g. for MARRIED FEMALE, add coefficients
                   # for female, married AND interaction to get TOTAL effect

# NOTICE: AGAIN, as above, marriage premium is NOT the same for females and males,
#         test for significance of DIFFERENCE in wage premium by looking at the
#         significance of the INTERACTION term.

### Interaction Between One Dummy and One Non-Dummy:

# MOTIVATION: Above, when included single dummy, got a difference in INTERCEPT.
#             Now, want to capture difference in intercept AND difference in slope

lm_slope <- lm(wage ~ female + educ + female:educ, data = wage1)
summary(lm_slope)

lm_slope2 <- lm(wage ~ female*educ, data = wage1)
summary(lm_slope2) # SAME regression ... again, just use factorial interaction

# NOTE: Want to test whether RETURN to educ (i.e. slope) is the SAME for male
#       and female, i.e. test if the INTERACTION is significant, controlling
#       for a constant wage differential (i.e. difference in intercept as before)

# NOTE: Want to graph:
#       wage = beta0 + delta0*female + beta1*educ + delta1*educ*female
#            = beta0 + delta0*female + (beta1 + delta1*female)*educ
#       for a FIXED educ level (say 1), and show the INTERCEPT SHIFT for every
#       level of educ, AND the slope difference between females and males

beta <- lm_slope2$coefficients

with(wage1[wage1$female == 1,], plot(wage ~ educ, col = rgb(0,0,1)))
with(wage1[wage1$female == 0,], points(educ, wage, col = rgb(1,0,0)))

abline(a = beta[1] + beta[2], b = beta[3] + beta[4], 
       col = rgb(0,0,1), lwd=2) # Estimated regression function for FEMALES
abline(a = beta[1] , b = beta[3], 
       col = rgb(1,0,0), lwd=2 ) # Estimated regression function for MALES

# NOTE: Graphically, DON'T see a sizable difference in slope ... reflects the
#       fact that the estimate of the the interaction coefficient is SMALL AND 
#       STATSISTICALLY INSIGNIFICANT. FAIL to reject H0: delta1 = 0

linearHypothesis(lm_slope, c("female = 0", "female:educ = 0"))

# NOTE: Test for gender discrimination in EITHER form, REJECT H0 that there is
#       no difference between females and males