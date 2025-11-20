## Tutorial 9 ECO375 24/3/2023
################################################################################

# Housekeeping: 1) Pick up midterm if haven't already
#               2) A lot today, will be a bit fast paced

#################
# Extra Comments:
#################

some_numbers <- c("1", "2", "3")
str(some_numbers) # NOT numerical, CAN'T run a proper regression with this

some_numbers <- as.numeric(some_numbers)
str(some_numbers)

################################################################################

####################
# Interaction Terms:
####################

### Average Partial Effects:

# MOTIVATION: Want to find what is the partial effect at the AVERAGE house
#             in terms of a certain variable.

# NOTE: 1) Derivative of regression function with respect to bdrms is:
#       beta2 + beta3*sqrft
#       2) Derivative of regression function with respect to sqrft is:
#       beta1 + beta3*bdrms

beta[3] + beta[4]*mean(hprice1$sqrft) # Average partial effect of bdrms 
                                      # (evaluate at MEAN sqrft)

beta[2] + beta[4]*mean(hprice1$bdrms) # Average partial effect of sqrft 
                                      # (evaluate at MEAN bdrms)

################################################################################

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
       col = rgb(0,0,1), lwd = 2) # Estimated regression function for FEMALES
abline(a = beta[1] , b = beta[3], 
       col = rgb(1,0,0), lwd = 2) # Estimated regression function for MALES

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
       col = rgb(0,0,1), lwd = 2) # Estimated regression function for FEMALES
abline(a = beta[1] , b = beta[3], 
       col = rgb(1,0,0), lwd = 2) # Estimated regression function for MALES

# NOTE: Graphically, DON'T see a sizable difference in slope ... reflects the
#       fact that the estimate of the the interaction coefficient is SMALL AND 
#       STATSISTICALLY INSIGNIFICANT. FAIL to reject H0: delta1 = 0

linearHypothesis(lm_slope, c("female = 0", "female:educ = 0"))

# NOTE: Test for gender discrimination in EITHER form, REJECT H0 that there is
#       no difference between females and males

################################################################################

############################################
# Heteroskedasticity Robust Standard Errors:
############################################

# MOTIVATION: Robust standard errors are used to get CORRECT inference results. 
#             Otherwise, NOT related to unbiasedness/consistency etc.

# TERMINOLOGY: Also called Eicker-White OR White standard errors

# install.packages("sandwich")
library(sandwich) # Package for vcovHC, for estimating robust std errors
?vcovHC

# install.packages("lmtest")
library(lmtest) # Package for coeftest, for test of significance using robust se
?coeftest

?bwght
View(bwght)

lm_bwght <- lm(bwght ~ cigs, data = bwght)
summary(lm_bwght)

bwght$resids <- lm_bwght$residuals

with(bwght, plot(resids ~ cigs)) # Residual plot, sign of heteroskedasticity? Maybe,
                                # variation in observations HIGHER for low cigs

standard_vcov <- vcov(lm_bwght) # Normal covariance matrix (see tutorial 5 video)
robust_vcov <- vcovHC(lm_bwght, type = "HC0") # Robust covariance matrix for beta_hat

sqrt(diag(robust_vcov)) # HETERO-ROBUST standard errors
sqrt(diag(standard_vcov)) # NON-ROBUST standard errors

# NOTICE: Difference in standard errors NOT that large, indicates that there
#         might NOT be any heteroskedasticity as we initially suspected.

# NOTE: "HC0" refers to heteroskedasticity-consistent (what we are looking for).
#        There are other types of robustness "HC1,2,3", that don't concern us

coeftest(lm_bwght, df = Inf, vcov = robust_vcov) # Significance test using 
                                                 # robust std errors

# NOTE: df = Inf indicates that we are in the ASYMPTOTIC NORMALITY context, as
#       we have n = 1388 observations, ASYMPTOTICALLY justified

# NOTICE: Compare to summary above, t-stats/p-values DON'T change much. ALSO, 
#         estimates, beta_hat remain UNCHANGED when use heteroskedasticity robust
#         standard errors ... NO effect on estimation/bias/consistency etc.

### Calculate Hetero-Robust Standard Errors Manually:

# NOTE: Can ONLY use formula in the slides in SLR case. For MLR, formula is 
#       presented in matrix "sandwich" notation (more complicated, won't cover)

mean_cigs <- mean(bwght$cigs)
resids2 <- bwght$resids^2

num <- sum((bwght$cigs - mean_cigs)^2*resids2) 
denom <- sum((bwght$cigs - mean_cigs)^2)^2

sqrt(num/denom)

sqrt(diag(robust_vcov)) # Compare to the above value, SAME

# CONCLUSION: ALWAYS use hetero-robust standard errors. BUT, beware, in small
#             sample sizes, sampling distribution of our t-statistics are NOT 
#             t-distribution, SO, if hetero-robust SE are used when not needed, 
#             get BAD inference. 

# CONCLUSION: What changes with hetero-robust standard errors? 
#             NO: coefficient estimates, R2
#             YES: standard errors (they decrease here), t and F statistics, 
#                  significance conclusions i.e. p-values

###########################################
# Feasible Generalized Least Squares (GLS):
###########################################

# MOTIVATION: Want to TRANSFORM our regression by multiplying by some WEIGHTS.
#             Now, will minimize the sum of WEIGHTED squared residuals. Ultimately, 
#             will give LESS weight to observations with HIGHER variation, and
#             will get an EFFICIENT estimator (more than OLS). Now, need to find
#             the weights:

?smoke
View(smoke)

# STEP 1 - Generate Log-(Residuals^2) From Main Regression:

lm_cigs <- lm(cigs ~ lincome + lcigpric + educ + age + agesq + restaurn, 
              data = smoke)

log_uhat2 <- log(lm_cigs$residuals^2) # Extract RESIDUALS from original model
                                      # and apply transformations (^2 and log)

# STEP 2 - Generate Exponential Fitted Values From Auxiliary Regression:

lm_aux <- lm(log_uhat2 ~ lincome + lcigpric + educ + age + agesq + restaurn, 
             data = smoke)

h_hat <- exp(lm_aux$fitted.values) # Extract FITTED VALUES from aux. model and 
                                   # apply exponential transformation

# IMPORTANT: These h_hat's are our WEIGHTS. BUT, taking the exponent to invert 
#            the initial log that was applied to the residuals is an ASSUMPTION. 
#            Assumption on the FORM of Var(u|x) = h could be WRONG, hence we 
#            could be mis-specifying our model. SO, a trade off between:
#            1) Making structural assumptions on things we DON'T know
#            2) The probability that we are WRONG

# NOTE: To account for that, can implement BOTH FGLS AND ROBUST STANDARD ERRORS

# STEP 3 - Applying Weights to Original Model Specification (In STEP 1):

lm_weight <- lm(cigs ~ lincome + lcigpric + educ + age + agesq + restaurn, 
                data = smoke, weights = sqrt(1/h_hat))

summary(lm_weight)
summary(lm_cigs) # Compare to STEP 1, standard OLS

# NOTICE: Estimates are DIFFERENT, AND standard errors. Standard errors in FGLS
#         from the weighted regression are ALL LOWER than the OLS standard errors, 
#         hence, MORE EFFICIENT.

#	NOTE: FGLS is NOT unbiased (as we use h_hat not h, which is unknown), SO, it
#       is NOT BLUE, BUT it is consistent and asymptotically efficient, so 
#       BLUE ASYMPTOTICALLY, which is GOOD ENOUGH when we have large sample size