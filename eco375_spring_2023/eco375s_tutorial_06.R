## Tutorial 6 ECO375 3/3/2023
################################################################################

# Housekeeping: 1) Tutorial 5 follow up video on Quercus, please watch
#               2) Midterms will be graded by?
#               3) New Mindtap assignment by March 16

#################
# Extra Comments:
#################

################################################################################

####################
# Midterm Solutions:
####################

####################
# Tutorial 5 Excess:
####################

### OVB Example 2 - Sample Selection in Job Training Program:

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

## Graphing CI's and PI's:

educ_seq <- seq(min(wage1$educ), max(wage1$educ), by = 0.1)

ci <- predict(lm_multi, interval = "confidence", 
              newdata = data.frame(educ = educ_seq, exper = 10))

pi <- predict(lm_multi, interval = "prediction", 
              newdata = data.frame(educ = educ_seq, exper = 10))

# NOTE: Above, we get CI's and PI's for wage HOLDING experience fixed at some
#       level ( = 10). Change the value and see what happens to the graph

with(wage1, plot(wage ~ educ))
lines(educ_seq, ci[,1], lty = 1) # Alternative to, abline(lm_multi)
lines(educ_seq, ci[,2], lty = 2, col = rgb(1, 0, 0))
lines(educ_seq, ci[,3], lty = 2, col = rgb(1, 0, 0))
lines(educ_seq, pi[,2], lty = 2, col = rgb(0, 0, 1))
lines(educ_seq, pi[,3], lty = 2, col = rgb(0, 0, 1))
abline(v = mean(wage1$educ))

# NOTE: IMPORTANT for visualizing uncertainty of your estimates. 

# NOTICE: CI reaches a minimum/trough at the MEAN value of educ. AND, PI is 
#         very wide, capturing the uncertainty in u (recall the R2 being low).

# NOTE: Prediction Interval ALWAYS wider than CI, as the variance of the ACTUAL 
#       observation point, Y, incorporates the variance of u (error term),
#       SO variance is HIGHER than variance of E(Y|X) (expected value). BUT, as
#       n grows LARGE, CI and PI should be roughly equal, as u becomes negligible

################################################################################

#############
# R Markdown:
#############

### Compiling Regression Tables:

lm_demo1 <- lm(wage ~ educ, data = wage1)
lm_demo2 <- lm(wage ~ educ + exper + female, data = wage1)
lm_demo3 <- lm(wage ~ educ + exper + tenure + female + married, data = wage1)
summary(lm_demo3)

# install.packages("stargazer")
library(stargazer)

stargazer(lm_demo1, lm_demo2, lm_demo3,
          title = "Regression Results", 
          dep.var.labels = "Hourly Wage", 
          covariate.labels = c("Education", "Experience", "Tenure", "Female",
                               "Married"),
          omit.stat=c("LL","ser"), no.space = TRUE, header = FALSE)

# NOTE: Add {r, results = "asis"} in chunk BEFORE compiling

# install.packages("texreg")
# library(texreg) # Alternative package ... NOT my go-to

# NOTE: Will revisit R Markdown later if have time, OTHERWISE, can simply
#       learn LaTex code on your own (Google...) OR use cheat sheets in Help