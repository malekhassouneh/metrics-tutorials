## Tutorial 7 ECO375 10/3/2023
################################################################################

# Housekeeping: 1) MindTap extended to 23rd of March (NOT 16th)
#               2) Empirical project posted, length: 3-5 pages 

#################
# Extra Comments:
#################

################################################################################

#################################
# Inference - Hypothesis Testing:
#################################

### Multiple/Joint (Exclusion) Restriction Testing:

# NOTE: Tests of the form H0 : beta_1 = c1 & beta_2 = c2 ; 
#                         H1 : At least one beta_i !=c_i (at least one is false)

# NOTE: We will only examine two-tailed tests for F-tests ... one-tailed are MORE
#       complicated (beyond scope of this class)

# NOTE: In general, we could have Multiple LINEAR Restrictions, i.e. testing 
#       MULTIPLE linear combination of parameters at the SAME time (see slides).
#       In EITHER case, MUST use an F-test, t-test is NOT enough, i.e. CAN'T
#       individually test significance and conclude joint significance for example.
#       Again, could be shown that when testing ONE restriction (a special case)
#       then F-test and t-test ARE equivalent, as F-stat = (t-stat)^2

## Global/Full F Test:

# MOTIVATION: Test the significance of the ENTIRE regression (i.e. all regressors)

?htv
View(htv)

lm_educ <- lm(educ ~ motheduc + fatheduc, data = htv) # From tutorial 5
summary(lm_educ) # Effect of mother and father educ levels on OFFSPRING educ level

lm_educ2 <- lm(educ ~ motheduc + fatheduc + abil, data = htv)
sum_educ2 <- summary(lm_educ2) # Same as lm_educ, but with abil (of individual)
sum_educ2

sum_educ2$fstatistic # F-statistic GLOBAL F TEST, H0: beta_i = 0 for ALL regressors
                     # Numerator df = q = k (ONLY here), number of restrictions
                     # Denominator df = n-k-1, unrestricted df

f <- sum_educ2$fstatistic[1] # Extract the 3 values from above
df_num <- sum_educ2$fstatistic[2]
df_denom <- sum_educ2$fstatistic[3]

pf(f, df1 = df_num, df2 = df_denom, lower.tail = FALSE)
# Associated p-value with the GLOBAL F-statistic above, same as lm() p-value

# CONCLUSION: VERY large F statistic, p-value virtually 0, so REJECT global H0, 
#             i.e. reject that all parameters are 0. SO motheduc, fatheduc AND
#             abil are JOINTLY significant (makes sense, all are individually
#             significant)

## Local/Partial F Test:

# MOTIVATION: Test the significance of a SUBSET of regressors

linearHypothesis(lm_educ2, c("motheduc = 0", "fatheduc = 0"))
# LOCAL F test, testing if motheduc and fatheduc JOINTLY influence child
# education, ONLY (not including abil)

# CONCLUSION: Reject H0 ... at what significance level? ALMOST ANY alpha

# Manual Implementation - SSR Method:

# STEP 1 - Estimate UNRESTRICTED Model:

lm_unrestricted <- lm(educ ~ motheduc + fatheduc + abil, data = htv) # i.e. lm_educ2
sum_unrestricted <- summary(lm_unrestricted)

ssr_u <- sum_unrestricted$df[2]*(sum_unrestricted$sigma)^2
ssr_u <- sum(lm_unrestricted$resid^2) # OR

# STEP 2 - Estimate RESTRICTED Model:

# NOTE: Restricted model is the original model WITH H0 IMPOSED (like a proof
#       by contradiction, impose to see if true)

lm_restricted <- lm(educ ~ abil, data = htv)
sum_restricted <- summary(lm_restricted)

ssr_r <- sum_restricted$df[2]*(sum_restricted$sigma)^2
ssr_r <- sum(lm_restricted$resid^2) # OR

# STEP 3 - Calculate F Statistic:

F1 <- ((ssr_r - ssr_u)/2)/(ssr_u/sum_unrestricted$df[2]) # q = 2
F1 <- ((ssr_r - ssr_u)/2)/(ssr_u/lm_unrestricted$df.residual) # OR
F1

f_critical <- qf(0.05, df1 = 2, df2 = sum_unrestricted$df[2], lower.tail = FALSE)

F1 > f_critical # SO, REJECT H0 

p <- pf(F1, df1 = 2, df2 = sum_unrestricted$df[2], lower.tail = FALSE) # OR

p < 0.05 # ALSO REJECT H0

# Manual Implementation - R2 Method:

R2_u <- sum_unrestricted$r.squared
R2_r <- sum_restricted$r.squared

F2 <- ((R2_u - R2_r)/2)/((1 - R2_u)/sum_unrestricted$df[2])
F2 

# NOTE: Using R2 NOT adjusted R2 (makes a difference)

F2 - F1 # NOW, repeat critical value AND p-value approach, get SAME conclusions

# NOTE: F is ALWAYS positive, why? Unrestricted SSR (R2) < (>) Restricted SSR (R2)
#       ALWAYS, as seen in Tutorial 3/4, SSR (R2) NEVER increases (decreases) with
#       more regressors

# NOTE: Can show that ((SSR_r - SSR_u)/q)/(SSR_u/(n-k-1)) = 
#                     ((R2_u - R2_r)/q)/(1 - R2_u/(n-k-1)) mathematically

# NOTE: Can extend above procedures for GLOBAL F test discussed above. ALSO,
#       restricted model just includes a constant, SO R2_r = 0, use above formula

### Testing General Restrictions:

# NOTE: SAME idea as above ... just IMPOSE H0 condition (however complicated) in 
#       regression, run implied regression, and compare SSR. Best to avoid R2 
#       approach here as SST might change, so second formula might not hold.

# Example: H0: motheduc = fatheduc & abil = 1 ; H1: At least one !=

lm_unrestricted <- lm(educ ~ motheduc + fatheduc + abil, data = htv) # i.e. lm_educ2
sum_unrestricted <- summary(lm_unrestricted)

ssr_u <- sum(lm_unrestricted$resid^2)

lm_restricted <- lm(I(educ - abil) ~ I(motheduc + fatheduc), data = htv)
sum_restricted <- summary(lm_restricted)

ssr_r <- sum(lm_restricted$resid^2)

Fstat <- ((ssr_r - ssr_u)/2)/(ssr_u/sum_unrestricted$df[2]) # q = 2
Fstat

p <- pf(Fstat, df1 = 2, df2 = sum_unrestricted$df[2], lower.tail = FALSE)

p < 0.05 # REJECT H0

### Some Selected Testing Facts:

#	1) When we say significant/not, referring to the VARIABLE itself, or the 
#   variable's effect, NOT the estimate
#	2) p-value formal def: smallest significance level at which we reject the H0
#	3) 1 restriction F test is a t-test (equivalent)
#	4) Exact test vs asymptotically exact test? (didn't cover asymptotics yet)
# 5) F statistic ONLY valid under homoskedasticity (MLR 5)

################################################################################

########################################
# CLT and LLN Simulation Demonstrations:
########################################

### Law of Large Numbers (LLN):

# MOTIVATION: Draw a sample size of n={100, 1,000, 10,000} from a N(0,1)
#             distribution, 1000 times for EACH n, and each repetition calculate
#             the sample mean for each repetition

means <- data.frame(matrix(NA, nrow = 1000, ncol = 3)) # Create empty dataframe
colnames(means) <- c("100", "1000", "10000")

set.seed(123)

for (n in c("100","1000","10000")){
  for (i in 1:1000){
    
    sample <- rnorm(as.numeric(n), mean = 0, sd = 1)
    means[i, n] <- mean(sample)
  }
}

# Now, plot the histograms of the 1000 sample means for each n:

hist(means[["100"]], col = rgb(0,0,1,1/4))
hist(means[["1000"]], col = rgb(1,0,0,1/4), add = TRUE)
hist(means[["10000"]], col = rgb(0,1,0,1/4), add = TRUE)

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