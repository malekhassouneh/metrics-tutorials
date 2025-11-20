## Tutorial 10 ECO375 31/3/2023
################################################################################

# Housekeeping: 1) FINAL tutorial today ... WON'T have a review session, BUT will
#                  hold extended OH (2 hours), to be announced on Quercus
#               2) For empirical project, look at tutorial 6 for creating nice 
#                  regression tables, using R-markdown (explain for 5 min)

#################
# Extra Comments:
#################

### Final Exam Short Answer Questions:

################################################################################

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

################################################################################

##############################
# Instrumental Variables (IV):
##############################

### Simple Instrumental Variables:

# NOTE: "This is where econometrics actually starts"

# install.packages("AER")
library(AER) # "Applied Econometrics with R", package for ivreg function

?mroz # Data on married working women
View(mroz)

mroz <- subset(mroz, !is.na(mroz$lwage)) # Remove NA observations, of women that
                                    # DON'T work (so don't have lwage), otherwise
                                    # estimates in 2SLS (below) are OFF

lm_ols <- lm(lwage ~ educ, data = mroz)
summary(lm_ols) # Standard OLS estimation

# NOTE: CLEALRY, have endogeneity, as Cov(educ,u)!=0, omitted variables such as
#       family income, ability etc. Expect the direction of bias to be POSITIVE.
#       Why? family income, ability POSITIVE effect and POSITIVE correlation with
#       educ

# PLAN: Propose fatheduc as an INSTRUMENT for educ. Need to check BOTH:
#       1) Exogeneity, CAN'T test when only have one valid instrument, SO
#          have to argue using economic theory, or some structural element
#          that guarantees exogeneity. Why can't test? CAN'T get consistent
#          residuals from OLS, as OLS is inconsistent
#       2) Relevance, CAN test, using t-statistic (one instrument) OR F-statistic
#          (more than one instrument; see later)
#      These are IDENTIFICATION conditions, without which CAN'T estimate with IV

lm_first <- lm(educ ~ fatheduc, data = mroz)
summary(lm_first) # Relevance: fatheduc, find a significant effect, AND explains 
                  # 17% of variation in educ (not bad)

lm_iv <- ivreg(lwage ~ educ | fatheduc, data = mroz)
summary(lm_iv) # IV estimation; instrument = fatheduc

# NOTICE: IV point estimate for educ is roughly HALF of OLS, confirms our prior
#         of POSITIVE bias. What about significance? NOT hold at 5%. Why?
#         Remember, IV standard errors ALWAYS larger than OLS standard errors 
#         (making coefficient smaller and standard error larger, SO t statistic 
#         will be attenuated).

# NOTE: On syntax, ivreg(y ~ x | z) , where x = endogenous, z = instrument

# NOTE: How display first stage? Other than running the regression manually?

# NOTE: Although we "assume" exogeneity, we have to be SURE of this, otherwise
#       IV estimate's asymptotic bias could be LARGER than OLS, which we were
#       trying to correct in the first place. Even if Cov(z,u) sightly greater 
#       than 0, e.g. 0.05, could result in LARGE asymptotic bias, which would be 
#       the equivalent of Cov(x,u) being VERY LARGE (see slides and Exercise 15.5)

### Method of Moments Estimator:

# MOTIVATION: Want to check that the IV estimate above is indeed the ratio of
#             the SAMPLE covariances Cov(lwage,fatheduc)/Cov(educ,fatheduc)
#             Recall: beta1_IV = Cov(y,z)/Cov(x,z)

beta1hat_iv <- with(mroz, cov(lwage, fatheduc) / cov(educ, fatheduc))
beta0hat_iv <- with(mroz, mean(lwage) - beta1hat_iv*mean(educ))

beta_manual <- c(beta0hat_iv, beta1hat_iv)
beta_ivreg <- lm_iv$coefficients

beta_ivreg
beta_manual # EXACTLY the IV estimate from R above using ivreg

# NOTE: Recall discussion on BIASED but CONSISTENT estimators, IV is an example
#       of this ... endure small sample bias for IV, as we KNOW it is consistent
#       when have LARGE sample size

### General Instrumental Variables:

# MOTIVATION: Want to extend above IV framework to MLR setting, with many controls
#             so a SATURATED model

lm_ols2 <- lm(lwage ~ educ + exper + I(exper^2), data = mroz)
summary(lm_ols2)

lm_iv2 <- ivreg(lwage ~ educ + exper + I(exper^2) | 
                exper + I(exper^2) + fatheduc, data = mroz)
summary(lm_iv2)

# NOTE: MUST include ALL exogenous variables after the | in ivreg, not only the 
#       instrument fatheduc.

###############################################################################

#################################
# Two Stage Least Squares (2SLS):
#################################

# MOTIVATION: Found more than one valid instrument, want to find a way to COMBINE
#             all of those instruments in estimation, instead of discarding some.
#             This will provide LOWER standard errors (R2_x,z higher) due to
#             MORE explanatory power with more instruments.

# NOTE: Can also follow procedure below when we have only ONE instrument. Here, 
#       will have 2 instruments, fatheduc and motheduc. In general, can have
#       # endogenous variables <= # instruments, BUT NOT the other way around.

### One Endogenous Variable Multiple Instruments:

# STEP 1 - FIRST STAGE Regression of Endogenous on ALL Exogenous Variables:

lm_first <- lm(educ ~ exper + I(exper^2) + fatheduc + motheduc, data = mroz)
summary(lm_first) # Include exogenous controls AND instruments

linearHypothesis(lm_first, c("motheduc = 0", "fatheduc = 0"))
# Testing joint RELEVANCE of instruments, one of the IDENTIFICATION conditions

mroz$educ_hat <- lm_first$fitted # Extract fitted values

# STEP 2 - SECOND STAGE Regression of STRUCTURAL EQUATION with Fitted Values:

lm_second <- lm(lwage ~ educ_hat + exper + I(exper^2), data = mroz)
summary(lm_second) # Estimate the STRUCTURAL equation

# NOTE: Think of the first stage as PURGING the endogenous variable of any
#       endogeneity (remove residuals, only keep fitted values), making the
#       endogenous variable "exogenous". In other words, educ_hat is the CLOSEST 
#       thing to educ without it being endogenous like educ, SO educ_hat is an 
#       "approximate" exogenous replacement for educ

lm_iv2 <- ivreg(lwage ~ educ + exper + I(exper^2) | 
                  exper + I(exper^2) + fatheduc + motheduc, data = mroz)
summary(lm_iv2) # Compare IV estimates to 2SLS estimates

# NOTICE: Coefficient estimates are identical as ivreg, BUT WRONG standard errors
#         in the second stage when conducting 2SLS. Need to ADJUST standard errors.
#         OR, just use the standard errors from ivreg (by default are adjusted)

### Multiple Endogenous Variables and Instruments:

# NOTE: 2SLS procedure above is GENERALIZABLE to as many endogenous variables
#       as we suspect, SO LONG as # endogenous variables <= # instruments.
#       Would then run SEPARATE OLS first stages for EACH endogenous variable
#       on ALL exogenous variables (controls and instruments). So, when use ivreg, 
#       ivreg(y1 ~ y2 + y3 + x1 + x2 | z1 + z2 + x1 + x2), where y2 y3 are
#       endogenous, z1 z2 are instruments, x1 x2 other exogenous controls

###################
# Weak Instruments:
###################

# MOTIVATION: If endogenous variable and instrument are WEAKLY correlated, then
#             we have a WEAK INSTRUMENT, and IV estimates have HIGH standard
#             errors (R2_x,z close to 0), AND maybe LARGER asymptotic bias than 
#             OLS. SO, inference FAILS. When checking relevance, need the corr. 
#             to be SUFFICIENTLY large.

# RULE OF THUMB: 1) Homoskedastic errors, t-stat > 3.2 (10 for F-statistic)
#                2) Heteroskedadtic errors in first OR second stage, 
#                   t-stat > 4.47 (20 for F-statistic)

# ASIDE: How do we apply heteroskedasticity with IV/2SLS? With some caveats
#        ALL that applies for OLS works here (use vcovHC)

# NOTE: Rules of thumb AREN'T knife-edge cutoffs, e.g. F = 9.98 < 10, could 
#       still be a sufficiently non-weak instrument, even though technically
#       we "fail to reject" H0. 

lm_first <- lm(educ ~ exper + I(exper^2) + fatheduc + motheduc, data = mroz)
summary(lm_first)

linearHypothesis(lm_first, c("fatheduc = 0", "motheduc = 0"))
# F test, as have 2 instruments, so t test NOT sufficient; REJECT joint "weakness"

summary(lm_iv2, diagnostics = TRUE) # OR, use R output

###########################################
# Hausman Test of Exogeneity of Regressors:
###########################################

# MOTIVATION: Want to test for the endogeneity of the variable that we suspect 
#             is endogenous. If there is evidence for endogeneity, we estimate 
#             our regression using IV. Otherwise, when explanatory variables are 
#             ALL exogenous, 2SLS/IV is LESS EFFICIENT than OLS (HIGHER VARIANCE),
#             so we use OLS (is BLUE under Gauss Markov Assumptions). BUT, test
#             is agnostic if have more than one endogenous variable, don't know
#             WHICH is endogenous ... only know at least ONE is.

# NOTE: H0: ALL variables are exogenous (use OLS)
#       H1: Some variables are endogenous (use IV), DON'T know which

# STEP 1 - Obtain OLS and IV Coefficient Estimates and Variances:

lm_iv <- ivreg(lwage ~ educ + exper + I(exper^2) | motheduc + fatheduc +
               exper + I(exper^2), data = mroz)
sum_iv <- summary(lm_iv)

lm_ols <- lm(lwage ~ educ + exper + I(exper^2), data = mroz)
sum_ols <- summary(lm_ols)

beta_iv <- sum_iv$coefficients[2,1]
beta_ols <- sum_ols$coefficients[2,1]

var_iv <- (sum_iv$coefficients[2,2])^2
var_ols <- (sum_ols$coefficients[2,2])^2

# STEP 2 - Construct Hausman Statistic:

H <- (beta_iv - beta_ols)^2/(var_iv - var_ols) # Some rounding error here...

# NOTE: Can show theoretically, under the H0, i.e. assuming the variables are are
#       exogenous, that H ~ Chisq(k), where k = # of potential endogenous variables

p <- pchisq(H, df = 1, lower.tail = FALSE)
p < c(0.05, 0.1)

# CONCLUSION: FAIL to reject H0 at 0.05, variables (educ) are EXOGENOUS, BUT
#             REJECT at 0.1 level

summary(lm_iv, diagnostics = TRUE, df = Inf) # OR, let R conduct the test

##################################################
# Sargan-Hansen Test of Exogeneity of Instruments:
##################################################

### Testing Over-identifying Restrictions:

# MOTIVATION: Usually argue the exogeneity of instruments, i.e. exclusion
#             restrictions using economic intuition, as error terms are unobserved,
#             and CAN'T test that the covariance is 0. BUT, with more than one
#             instrument, can ASSUME the validity of one, and use it in IV 
#             estimation, and TEST the exogeneity of the other, vice versa. If
#             we estimate IV using either instrument, and IV estimates are 
#             statistically close to each other, then BOTH instruments are 
#             exogenous. Otherwise, AT LEAST one is not exogenous, BUT, don't know 
#             which. A clear problem with this test: could be that both instruments
#             are NOT exogenous, AND we coincidentally have a small difference 
#             between the estimates as BOTH estimates are inconsistent. A more 
#             general approach:

# NOTE: This test is valid under HOMOSKEDASTICITY ONLY,
#       H0: ALL IVs are exogenous (good)
#       H1: Some IVs are NOT exogenous (bad), DON'T know which

# STEP 1 - Obtain Residuals from 2SLS Estimation of the Structural Equation:

lm_one <- ivreg(lwage ~ educ + exper + I(exper^2) | motheduc + fatheduc + 
                exper + I(exper^2), data = mroz)
u_hat <- lm_one$residuals # These residuals should be CONSISTENT estimates of 
                          # the unobservable u (if instruments are VALID)

# STEP 2 - Obtain n*R2 Statistic from Regression of Residuals on ALL Exogenous 
#          Variables:

lm_two <- lm(u_hat ~ motheduc + fatheduc + exper + I(exper^2), data = mroz)
sum_two <- summary(lm_two)

R2 <- sum_two$r.squared # Obtain R2, want it to be SMALL. Why? EXOGENEITY
stat <- R2*nobs(lm_two) # Calculate n*R2 

# NOTE: Can show theoretically, under the H0, i.e. assuming the instruments are
#       valid, that n*R2 ~ Chisq(q), where q = # instruments - # endogenous

p <- pchisq(stat, df = 2 - 1, lower.tail = FALSE)
p < 0.05 

# CONCLUSION: FAIL to reject H0, BOTH instruments are valid (or BOTH invalid)

summary(lm_iv, diagnostics = TRUE, df = Inf) # OR, let R conduct the test

###############################################################################

################
# IV Simulation:
################

# SET UP: Let x = 1*z + v, u = 0.5*v + w, and y = 0*x + u where z, v, w ~ N(0,1)
#         Assume Cov(z,v) = Cov(z,w) = Cov(w,v) = 0. Want to estimate 
#         y = beta*x + u, using OLS and IV, and compare.

# NOTE: MUST check identifying assumptions:
#       1) Endogeneity, Cov(x,u) = Cov(z + v,0.5*v + w) = 0.5*Var(v) != 0
#       2) Relevance, Cov(z,x) = Cov(z,z + v) = Var(z) != 0
#       3) Exogeneity, Cov(z,u) = Cov(z,0.5*v + w) = 0

set.seed(123)

n <- 100

beta_ols <- c()
beta_iv <- c()

for (i in 1:1000) {
  
  z <- rnorm(n,0,1) 
  v <- rnorm(n,0,1)
  x <- 1*z + v # Change the 1 to < 1, e.g. 0.5, what happens to OLS, IV?
  w <- rnorm(n,0,1)
  u <- 0.5*v + w 
  y <- 0*x + u
  
  lm_ols <- lm(y ~ 0 + x) # 0 to remove CONSTANT (no constant in model above)
  lm_iv <- ivreg(y ~ 0 + x | z)
  
  beta_ols[i] <- lm_ols$coef
  beta_iv[i] <- lm_iv$coef
}

mean(beta_ols)
sd(beta_ols)

mean(beta_iv)
sd(beta_iv) 

# NOTICE: IV is CONSISTENT, OLS INCONSISTENT. BUT, IV LARGER standard errors

hist(beta_ols, xlim = c(-1, 1), col = rgb(1, 0, 0, 0.5))
hist(beta_iv, add = TRUE, col = rgb(0, 0, 1, 0.5))
abline(v = 0, col = rgb(0,0,0), lwd = 3) # Add vertical line at TRUE beta = 0

# NOTICE: When change 1 to, say, 0.5, IV WILDLY INEFFICIENT. When we make 
#         the instrument weak, it can cause ASYMPTOTIC BIAS if Cov(z,u) != 0. 
#         So only use IV if confident that instrument is not weak (using 
#         F > 10 rule of thumb)

#############
# Conclusion:
#############

# FINAL REMARKS: Making ASSUMPTIONS is critical in econometrics. Trade off between
#                making assumptions to get stronger results but higher chance of
#                being wrong, and not not making assumptions so lower chance of
#                of being wrong but get weaker results. ALWAYS question your
#                empirical results ... we are not certain about anything.