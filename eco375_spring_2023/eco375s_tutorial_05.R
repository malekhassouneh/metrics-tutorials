## Tutorial 5 ECO375 17/2/2023
################################################################################

# Housekeeping: 1) Midterm NEXT CLASS Feb. 27th. Extra OH to be scheduled during
#                  reading week, announced soon on Quercus

#################
# Extra Comments:
#################

# NOTE: recall, $coefficients OR $coef OR coef()
#               $residuals OR $resid OR resid()
#               $fitted.values OR $fitted OR fitted()

### Random Assignment and Causality:

### Regression with Subset of Data:

?wage1
View(wage1)

lm_male <- lm(wage ~ educ + exper, data = wage1, subset = (female == 0))
lm_male <- lm(wage ~ educ + exper, data = subset(wage1, female ==0)) # OR
summary(lm_male)

table(wage1$female)

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
#         (makes sense? see more below, OVB). NOW interpretation is caveated 
#         with ceteris paribus i.e. "all other things (VARIABLES) being equal"

# NOTE: ALL that was discussed in SLR, properties, log-interpretations, unbiasedness 
#       etc. HOLDS in MLR case. Try repeating and showing at home.

### Calculating Estimates with Matrix Algebra:

# NOTE: Why bother with matrix algebra? Get a CONCISE representation of the
#       coefficient estimates, instead of a system of equations that is 
#       COMPLICATED to solve for each coefficient estimate individually (as
#       was done in SLR)

# install.packages("matlib")
library(matlib) # Package needed to take inverse of a matrix

constant <- rep(1, nrow(wage1))
regressors <- as.matrix(wage1[c("educ", "exper")])

X <- cbind(constant, regressors) # n x (k+1) = 526 x 3 matrix

Y <- as.matrix(wage1$wage) # n x 1 = 526 x 1 matrix (i.e. column vector)

beta <- inv(t(X)%*%X)%*%t(X)%*%Y # Using beta_hat = (X'X)^(-1)*X'Y

beta - coef(lm_multi) # Compare manual estimates to lm() estimates

# NOTE: Will go through matrix math to prove matrix representation actually
#       works (for k=1 SLR case) if have time (doubt it), else in OH if interested

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

# INTUITION: You are giving the "model" more "information" when you add
#            a variable, so either incoroporate this information OR discard
#            it (i.e. coefficient is zeroed out). So more info can't hurt. How
#            adjust R2 to account for this behavior? ADJUSTED R2, namely
#            R2_a = 1 - (1 - R2)(n-1)/n-k-1, apply a degree of freedom adjustment

# NOTE: Try at home, go back to the unbiasedness simulation, ADD another
#       regressor, say z <- rnorm(n), so that y <- 1 + 2*x + 3*z + u, 
#       and check that ALL of beta0_hat, beta1_hat, beta2_hat are unbiased

### Prediction:

?predict 

# NOTE: predict is a VERY helpful function, will use later when constructing
#       confidence intervals etc.

predict(lm_multi, newdata = data.frame(educ = c(0, 12, 16),
                                       exper = c(0, 2, 10))) # Over/under predict?

# NOTE: Works exactly the same with SLR, in either case need to specify a 
#       DATAFRAME to input new values, and dataframe must have SAME column
#       names as the variables in the model, else WON'T work

### Variance Inflation Factor (VIF):

# install.packages("car")
library(car) # Package needed for vif()

lm_multi2 <- lm(wage ~ educ + exper + tenure , data = wage1)

vif(lm_multi2)

# INTERPRETATION: ALL VIFs quite small, GOOD sign that there is NO strong
#                 multicollinearity that can INFLATE our standard errors. Why
#                 is that bad? See below in inference, will make t statistic
#                 SMALL, so won't be able to reject H0, and our CI will be WIDE.
#                 High VIF signals informational OVERLAP, so high uncertainty
#                 in ALLOCATING that information

# VIF Manually:

# NOTE: By definition, vif_j = 1/(1 - R2_j), from formula for variance of
#       beta_hat_j:
#                  Var(beta_hat_j) = sigma2 /(SST_j * (1 - R2_j))

lm_aux <- lm(educ ~ exper + tenure, data = wage1)
sum_aux <- summary(lm_aux)
R2_educ <- sum_aux$r.squared
vif_educ <- 1/(1 - R2_educ)
vif_educ

vif_exper <- 1/(1-summary(lm(exper ~ educ + tenure, data = wage1))$r.squared)
vif_tenure <- 1/(1-summary(lm(tenure ~ educ + exper, data = wage1))$r.squared)
vif_exper
vif_tenure

# NOTE: Above is a good POST-regression tool to check for multicollinearity, 
#       BUT ideally should have sifted that out in initial variable examination

plot(wage1[,c("educ","exper","tenure")]) # OR, pairs() instead of plot()

# NOTICE: Some but WEAK/NOISY correlation between variables, confirms results
#         from VIF analysis above

################################################################################

#####################
# Multi-Collinearity:
#####################

### Perfect Collinearity:

# Example - Dummy Variable Trap:

# NOTE: As seen in lectures, perfect collinearity NOT only in dummy variable case

?wage1
View(wage1)

wage1$single <- 1 - wage1$married # New complementary indicator variable, = 1 if 
                                  # NOT married

lm_married <- lm(lwage ~ educ + exper + married, data = wage1)
summary(lm_married) # Regression with married ONLY

lm_both <- lm(lwage ~ educ + exper + married + single, data = wage1)
summary(lm_both) 

# NOTICE: R is smart enough to catch PERFECT collinearity, drops "single" 
#         from the model, others model NOT well-defined.

# NOTE: R will ALWAYS drop the SECOND dummy in your specification

lm_both2 <- lm(lwage ~ 0 + educ + exper + married + single, data = wage1)
summary(lm_both2) # CORRECT way of including both dummies, REMOVE constant, 0 +

# NOTE: Interpretation of coefficients here is DIFFERENT, each coefficient is 
#       the estimated MEAN of that group ... NO longer interpret in terms of 
#       difference (e.g. wage differential between married/non-married)

### Near-Perfect Collinearity (Multicollinearity):

# MOTIVATION: From example above, R was able to sift out PERFECT collinearity.
#             what about high (non-perfect) multicollinearity? Trying to 
#             TRICK R, want a variable that is NOT exactly 0, 1, SO a pseudo-dummy
#             variable, adding small random shock to definition from above

wage1$single2 <- 1 - wage1$married + rnorm(nrow(wage1), 0, 0.01) 

wage1$sum <- wage1$single2 + wage1$married # Now, sum is NOT exactly 1 for all 
                                           # obs., so NO perfect collinearity

View(wage1)

lm_both2 <- lm(lwage ~ educ + exper + married + single2, data = wage1)
summary(lm_both2) # R is SUCCESSFULLY deceived, DOESN'T remove "single2"

# NOTICE: Standard errors explode, why? VIF argument from above

lm_aux <- lm(married ~ single2, data = wage1)
summary(lm_aux) # What is the R2_married?

########################
# Omitted Variable Bias:
########################

# Example 1 - Wage, Education & Experience:

?wage1
View(wage1)

lm1 <- lm(lwage ~ educ, data=wage1)
summary(lm1) # Omit, exper, suspect NEGATIVE bias

with(wage1, cor(educ, exper)) * with(wage1, cor(wage, exper)) < 0

lm2 <- lm(lwage ~ educ + exper, data=wage1)
summary(lm2) # Estimate for educ went UP. Omit, female, suspect POSITIVE bias

with(wage1, cor(educ, female)) * with(wage1, cor(wage, female)) > 0

lm3 <- lm(lwage ~ educ + exper + female, data=wage1)
summary(lm3) # Estimate for educ went DOWN. Omit, ability, suspect POSITIVE bias

# NOTE: Economic intuition is CRUCIAL when trying to find the OVB of variables
#       that CAN'T be readily measured, e.g. ability (although could use proxy
#       for it like IQ)

# Example 2 - Sample Selection in Job Training Program:

?jtrain98
View(jtrain98)

lm_train <- lm(earn98 ~ train, data = jtrain98)
summary(lm_train) # SLR, find a NEGATIVE effect of job training on earnings.
                  # Intuitive? Rational to have program then? OVB can SERIOUSLY
                  # skew conclusions, on SIGN (not just magnitude bias)

lm_train2 <- lm(earn98 ~ train + earn96 + educ + age + married, 
                data = jtrain98)
summary(lm_train2) # MLR with "relevant" controls/confounding variables. Does
                   # the sign on train make sense? YES

################################################################################

#################################
# Inference - Hypothesis Testing:
#################################

# NOTE: Need MLR1-6, especially normality of u (emphasis on MLR6) for our tests
#       and CI's to be VALID (exactly/perfectly), BUT with LARGE n, will see 
#       later that tests and CI are good approximations/valid (under ASYMPTOTICS)
#       even WITHOUT MLR6 assumption

### Aside - Variance-Covariance Matrix:

# MOTIVATION: Want to EXTRACT standard errors of estimated coefficients, AND
#             any shared COVARIANCE between coefficients. Useful for inference
#             procedures, ESPECIALLY tests of linear combination of parameters, 
#             BUT R can test this with a built-in function (see later)

?vcov

lm_demo <- lm(wage ~ educ + exper, data = wage1)
sum_demo <- summary(lm_demo)
sum_demo

v_demo <- vcov(lm_demo) # Notice, matrix is SYMMETRIC (mirror across diagonal)
v_demo
sqrt(diag(v_demo)) # Compare standard errors to lm() output, are the SAME

# Manual Calculation of Covariance Matrix:

sigma2_hat <- (sum_demo$sigma)^2
sigma2_hat <- sum(sum_demo$resid^2)/sum_demo$df[2] # OR, = SSR/(n-k-1)

constant <- rep(1, nrow(wage1)) # Same as in previous section
regressors <- as.matrix(wage1[c("educ", "exper")])
X <- cbind(constant, regressors)

v_manual <- sigma2_hat * inv(t(X)%*%X) # Var(beta_hat)_hat = sigma2_hat*(X'X)^(-1)
v_manual

### Exercise C4.11 - THROUGHOUT:

?htv
View(htv) 

### Conducting Two-Tailed Tests:

# NOTE: Tests of the form H0 : beta_1 = 0 ; H1 : beta_1 != 0 (i.e. =/= 0)

lm_educ <- lm(educ ~ motheduc + fatheduc, data = htv)  
summary(lm_educ) # Effect of mother and father educ levels on OFFSPRING educ level

library(car) # Package from above for vif(), here for linearHypothesis()

linearHypothesis(lm_educ, "motheduc = 0") # Implement two-tailed t-test, compare
                                          # to R lm() output from above

# NOTE: linearHypothesis() gives the F-statistic, which is (t-statistic)^2 ONLY 
#       for tests of SINGLE restrictions (can verify algebraically if interested)

# Manually - CRITICAL VALUE APPROACH:

beta_motheduc <- lm_educ$coefficients[2]
se_motheduc <- sqrt(diag(vcov(lm_educ)))[2]

t_stat <- beta_motheduc/se_motheduc # Calculate t-statistic
df <- nobs(lm_educ) - 2 - 1 # Using df = n - k -1 
df <- lm_educ$df.residual # OR, alternative to sum_xxx$df used in Aside above

# NOTE: Some people define the df as n-k (k includes constant)

t_critical <- qt(0.05/2, df, lower.tail = FALSE) # Find 0.975 critical value, 
                                                 # i.e. (1 - alpha/2), alpha = 0.05

t_stat > t_critical

# CONCLUSION: Reject H0 with 95% confidence (1-probability of Type I error) 
#             i.e. at 5% significance level

# NOTE: When n is big enough (as is the case here, n = 1230), t-dist. CONVERGES 
#       to N(0,1), hence can use normal critical values (e.g. 1.96 at alpha = 0.05)

# Manually - P-VALUE APPROACH:

p <- 2*pt(t_stat, df, lower.tail = FALSE) # Calculate p-value, and due to symmetry
                                          # of t-dist, can DOUBLE tail probability
p < 0.05

# CONCLUSION: Reject H0 at 5% level (SAME as above) (and 1%, 0.1% etc.) , p-value
#             is VERY small...

### Conducting One-tailed Tests:

# NOTE: Tests of the form H0 : beta_1 = 0 ; H1 : beta_1 >(<) 0 (not !=), used
#       to verify the SIGN of the parameter

t_stat # UNCHANGED from above

t_critical <- qt(0.05, df, lower.tail = FALSE) # Find 0.95 critical value, 
                                               # i.e. (1 - alpha), alpha = 0.05

t_stat > t_critical

# CONCLUSION: SAME as above

p <- pt(t_stat, df, lower.tail = FALSE) # HALF the p-value calculated above. WHY?
                                        # Rejection region here ONE sided, BUT 
                                        # probability is the same on each side
p < 0.05

# CONCLUSION: SAME as above

# NOTE: A lower significance level (alpha) implies a more CONSERVATIVE test as have 
#       SMALLER REJECTION region, so reject less often. Alpha is a CHOICE PARAMETER,
#       i.e. the researcher (you) chooses it before hand (0.05, 0.01 most used)

### Testing Non-Zero Nulls:

# NOTE: Tests of the form H0 : beta_1 = c ; H1 : beta_1 != (>, <) c

linearHypothesis(lm_educ, "motheduc = 2") # Easy way of implementing

t_stat <- (beta_motheduc - 2)/se_motheduc # Modify t-statistic, otherwise SAME
                                          # manual procedure follows from above

# Mechanical Implementation:

lm_test1 <- lm(educ - I(2*motheduc) ~ motheduc + fatheduc, data=htv)
summary(lm_test1) # I() allows you to enter MODIFIED variables, tells R to
                  # treat "as is", instead of having to define new variable first

# NOTICE: Verify that the t-statistic^2 for motheduc IS the F statistic above

### Testing Linear Combinations of Parameters:

# NOTE: Tests of the form H0 : beta_1 + a*beta_2 = c ; 
#                         H1 : beta_1 + a*beta_2 != (>, <) c

linearHypothesis(lm_educ, "motheduc = fatheduc")
linearHypothesis(lm_educ, "motheduc - fatheduc = 0") # OR, doesn't need to be 0

# CONCLUSION: REJECT H0 at 5% significance level

# Mechanical Implementation:

lm_test2 <- lm(educ ~ motheduc + I(fatheduc + motheduc), data = htv)
summary(lm_test2) # Notice, t-statistic^2 for motheduc IS F-statistic from above

# NOTE: How know what variables to include, i.e. what manipulation to do in
#       implied regression? if test beta_1 = beta_2, let beta_1 - beta_2 = theta, 
#       then plug beta_1 = beta_2 + theta into model, and COMBINE parameters, 
#       test for theta.

# Manual Implementation:

beta <- lm_educ$coefficients
var <- vcov(lm_educ)

se <- sqrt(var[2,2] + var[3,3] - 2*var[2,3]) # OR [3,2], remember SYMMETRY

# NOTE: Recall, Var(X - Y) = Var(X) + Var(Y) - 2*Cov(X, Y), for us, 
#       X = beta_1_hat, Y = beta_2_hat

t_stat <- (beta[2] - beta[3])/se # Calculate t-statistic, notice, is sqrt of
                                 # F-statistic from above, p-value will be SAME

p <- 2*pt(t_stat, df, lower.tail = FALSE)

p < 0.05 # SAME conclusion as before

# NOTE: Repeat this entire section at home with wage1, educ and exper (for practice)

###################################
# Inference - Confidence Intervals:
###################################

### Confidence Intervals for Parameters:

lm_multi <- lm(wage ~ educ + exper, data = wage1)
sum_multi <- summary(lm_multi)
sum_multi

confint(lm_multi, level = 0.95) # 95% is DEFAULT two-sided CI
confint.default(lm_multi, level = 0.95)

# NOTE: CI's NOT exactly the same, confint() calculates the EXACT 95% CI for
#       all coefficients, ASSUMING normality of errors, i.e. WITHOUT asymptotics.
#       BUT, confint.default(), calculates LARGE-SAMPLE, i.e. ASYMPTOTIC CI. SO
#       confint() uses t dist. critical values, confint.default() uses NORMAL
#       critical values. Here, CI' similar, could verify assumption that errors 
#       are normally distributed

# Construct Manually:

df <- lm_multi$df.residual

beta1_lower <- sum_multi$coef[2,1] - qt(0.975, df)*sum_multi$coef[2,2]
beta1_upper <- sum_multi$coef[2,1] + qt(0.975, df)*sum_multi$coef[2,2]

c(beta1_lower, beta1_upper)
ci_exact <- c(beta1_lower, beta1_upper)

beta1_lower <- sum_multi$coef[2,1] - qnorm(0.975)*sum_multi$coef[2,2]
beta1_upper <- sum_multi$coef[2,1] + qnorm(0.975)*sum_multi$coef[2,2]

c(beta1_lower, beta1_upper)
ci_asymptotic <- c(beta1_lower, beta1_upper)

### Confidence & Prediction Intervals (CI & PI):

# MOTIVATION: Want to find some upper and lower bounds for our POPULATION
#             regression function, OR, bounds for one of our OBSERVATION POINTS.
#             I.e., want an interval for E(Y|X), a CI, OR, an interval for Y, a PI.

predict(lm_multi, newdata = data.frame(educ = c(0, 12, 16),
                                       exper = c(0, 2, 10))) # E(Y|X=X0), as before
        
predict(lm_multi, interval = "confidence", 
        newdata = data.frame(educ = c(0, 12, 16), exper = c(0, 2, 10)))

predict(lm_multi, interval = "prediction", 
        newdata = data.frame(educ = c(0, 12, 16), exper = c(0, 2, 10)))

# NOTE: See how to calculate manually and GRAPH (important for visualizing
#       uncertainty of your estimates) next time (useful for project)

# NOTE: Prediction Interval ALWAYS wider than CI, as the variance of the ACTUAL 
#       observation point, Y, incorporates the variance of u (error term),
#       SO variance is HIGHER than variance of E(Y|X) (expected value). BUT, as
#       n grows LARGE, CI and PI should be roughly equal, as u becomes negligible
