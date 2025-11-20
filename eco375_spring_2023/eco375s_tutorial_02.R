## Tutorial 2 ECO375 27/1/2023
################################################################################

# Housekeeping: 1) People joining from Angelo Melino's section, let me know
#                  (for attendance keeping reasons)
#               2) Any Questions? Assignment?
#               3) Will start regression stuff next class, and continue with
#                  basics (other TA tutorial started regression)

#################
# Extra Comments:
#################

x <- "blah blah blah"
remove(x) # Removing a SINGLE object from the global environment

(10-5)/5 # Percentage change
log(10)-log(5) # APPROXIMATED % change, with logs (recall Taylor expansion)

(10-8)/8
log(10)-log(8) # SO, the smaller the difference between the numbers, the BETTER
               # the approximation

################################################################################

####################################
# Some Useful Data Management Tools:
####################################

### Displaying and Summarizing Data:

# NOTE: Good idea to explore your data set first prior to any analysis. Check 
#       for missing values, errors like duplicate observations, top coding etc.

?wage1 # From library(wooldridge)
View(wage1) 
str(wage1) # To understand the STRUCTURE of the dataframe, i.e. variables in the
           # data set and their precision level, e.g. integer/byte, string, 
           # float/double etc.
summary(wage1) 

### Subsetting Data:

head(wage1[c("wage","educ","female")], 10) # [] brackets to subset, here by 
                                           # variable name 
# head, displays the first 10 entries of selected variables in the data set

head(wage1[c(1,2,6)], 10) # OR, subset by column number

# NOTE: Read complicated code INSIDE OUT, as functions WRAP on the outside

# NOTE: Common theme that will keep popping up, there are MANY ways of doing the
#       same thing in R. Some ways are more efficient, i.e. less code to write, 
#       OR less computing time. I will mix and match between the ways depending
#       on convenience, but you can choose what you are most comfortable with.

summary(wage1[c(1,2,6)]) # Summary statistics of selected variables

table(wage1["educ"]) # Gives a table of COUNTS for each level of educ
table(wage1$educ) # OR, $ tells R to obtain that variable from data set, ONLY
                  # works for one variable, i.e. can't have $educ$female 

# NOTE: There is an OBJECT in R called a LIST, think of it like a vector that
#       can contain ANY object (even another list). $ sign tells R to ACCESS
#       that list and obtain whatever item you ask it to obtain. Notice,
#       dataframe IS a list, and will see later, regression table IS a list

table(wage1$educ, wage1$female) # JOINT FREQUENCIES table (i.e. marginal dists., 
                                # distribution of pairs)
table(wage1[c("educ","female")]) # OR, here [] more convenient than $

# NOTICE: Some summary statistics are missing, e.g. variance, std dev, cov/cor 
#         (for more than two variables)

var(wage1$educ) # Similarly, mean(), min(), max(), range() etc.
sd(wage1$educ)

cov(wage1$wage, wage1$educ)
cor(wage1$wage, wage1$educ) # See later CORRELATION/COVARIANCE MATRIX, with more
                            # than two variables

# install.packages("psych")
library(psych) # OR, use a package that gives MORE summary statistics

describe(wage1[c("wage","educ", "female")])

### Boolean Statements/Operators and Subsetting:

x <- c(1,2,3) # Simple Demonstration:

x > 1
x == 2 # x != 2
x <= 3
x > 0 & x > 2 # & indicates intersection, i.e. and
x == 2 | x == 3 # # | indicates union, i.e. or

subset(wage1, wage > 10) # To subset using a boolean/logical vector. Gives ONLY
                         # the observations with wage > 10
  
nrow(wage1)
sum(wage1$wage>10 & wage1$female==0) # Number of people with wage > 10 AND are
                                     # male (female == 0)
new_data <- subset(wage1, wage > 10 & female == 0)
summary(new_data$wage) # Summarize wage for this group

### Generating and Modifying Variables:

wage1$daily_wage <- wage1$wage*8 # Create a 9-5pm wage variable
wage1$log_wage <- log(wage1$wage) # By default, log is ln, i.e. natural log
View(wage1)
wage1 <- wage1[,-(25:26)] # Drop the newly created variable

wage1$wage <- ifelse(wage1$wage < 0 , 0 , wage1$wage) 
# Replace certain values, e.g. here if someone records a negative wage (doesn't
# make sense), then replace it with 0, else keep the value the SAME

# NOTE: ifelse VERY useful command/function. 
#       Syntax: ifelse(condition, what to do if TRUE, what to do if FALSE)

#####################################################
# Graphing and Exporting Graphs (Data Visualization):
#####################################################

# NOTE: For the purpose of this course, will only look at graphing commands
#       that come with base R. ggplot2, ANOTHER package very popular to create
#       nice looking graphs, and a variety of different graphs (e.g. violin,
#       heat map etc.) (I use ggplot2). Won't concern us, but quite easy to
#       learn if you are interested. Part of tidyverse.

### Graphing:

# Data Set 1 - Labor Data on Wages and Characteristics (CROSS SECTIONAL):

?wage1
View(wage1)

plot(wage1$educ, wage1$wage) # SCATTER PLOT

# NOTE: Always try to make your graphs "easy on the eye", e.g. adding labels, 
#       titles, using colors. See ?plot for these options. IMPORTANT for research
#       project (end of class)

with(wage1, plot(educ, wage)) # OR, with() allows you to set the "default" 
                              # data set for the graph, don't need wage1$. with()
                              # not only for graphing ... quite useful
with(wage1, plot(wage ~ educ)) # OR, syntax: y ~ x

# NOTE: Scatter plot works best with two continuous variables. Here, educ is
#       discrete/integers, so vertical lines structure

with(wage1, hist(wage)) # HISTOGRAM, see how to customize later

counts<- table(wage1$wage) # Find the FREQUENCIES/EMPIRICAL DIST. of obs.
with(wage1, barplot(counts)) # BAR GRAPH, a crude form of a histogram, as we
                             # graph singletons (single values), NOT intervals

# Data Set 2; Puerto Rico Minimum Wage (TIME SERIES):

# NOTE: This class will focus on cross-sectional data, time series estimation
#       is MUCH more complicated (ECO475?). But graphing still straightforward

?prminwge
View(prminwge)

with(prminwge, plot(year, avgmin, type="l")) # LINE GRAPH, avg minwage over time

### Exporting Graphs:

pdf(file = "test.pdf") # Export as a pdf (better resolution). MUST run chunk of
                       # code TOGETHER with the graph and dev.off()
with(wage1, barplot(counts))
dev.off()

jpeg(file = "test.jpg") # Export as a jpeg image (lower resolution?)
with(wage1, barplot(counts))
dev.off()

# NOTE: Can also export manually by clicking on the export button in the Plots tab.

################################################################################

############################
# Some More Data Management:
############################

### Generate a Binary Variable:

library(wooldridge)

x <- median(wage1$wage)
x

wage1$wage_higher <- ifelse(wage1$wage > x, 1, 0)
table(wage1$wage_higher) # Almost a 50:50 split, as x is median by construction

### Generate a Categorical Variable:

q <- quantile(wage1$exper) # Finds the minimum + quartiles of a variable
q

wage1$exper_group <- ifelse(wage1$exper > q[2] , 2, 1)
wage1$exper_group <- ifelse(wage1$exper > q[3] , 3, wage1$exper_group)
wage1$exper_group <- ifelse(wage1$exper > q[4] , 4, wage1$exper_group)

table(wage1$exper_group) # Again, almost a 25:25:25:25 split, but some ties
                         # with people having the same level of exper

# NOTE: Above is typically helpful when dealing with a regressor like "age",
#       which we usually treat as a continuous variable. But it might be more 
#       interesting to define "age brackets" e.g. dividing our sample into 4 groups 
#       based on the distribution of age. Interesting HOW? Think about how it 
#       changes our regression interpretation. Above also applies to other 
#       variables like education (will see more of this in Ch.6 later)

### Variable Recoding, Renaming and Relabeling:

table(wage1$female)
wage1$female <- ifelse(wage1$female == 1, 0, 1)
table(wage1$female) # NOW, after recoding female, female == 1 indicates male,
                    # NOT intuitive, so can rename variable:

# install.packages("dplyr")
library(dplyr) # FYI, this package is a part of tidyverse

wage1 <- rename(wage1, male=female) # Rename variable, female to male
View(wage1)

# install.packages("expss")
library(expss) # Package for apply_labels, applies character labels to variables

wage1 <- apply_labels(wage1,
                      wage = "This is a measure of hourly wage",
                      exper = "This is a measure of experience") 
View(wage1)

data(wage1) # Refresh data set
wage1$sex <- ifelse(wage1$female == 1, "female", "male") 
# Generate string variable, NOT memory friendly for large data sets
View(wage1)

#################
# Some Exercises:
#################

### Computer Exercises (Textbook):

## Exercise C1.1:

?wage1
View(wage1)

# Q1: Find the average, minimum, maximum educ level in the sample:

mean(wage1$educ)
min(wage1$educ)
max(wage1$educ)

summary(wage1$educ) # OR

# Q2: Find the average hourly wage in the sample. Does it seem high or low?

mean(wage1$wage) # $5.90, VERY low

# Q3: The wage data are reported in 1976 $s. Find the CPI for 1976, 2013, 2020:

# Source: FRED (Federal Reserve Economic Data) (GREAT source for data, macro)
cpi1976 <- 56.7
cpi2013 <- 232.45
cpi2020 <- 257.22

# Q4: Using the CPI values, find the average hourly wage in 2013 and 2020 dollars. 
#     Are the values reasonable now?

wage1$wage2013 <- wage1$wage / cpi1976 * cpi2013 # R divides first, then multiply
wage1$wage2020 <- wage1$wage / cpi1976 * cpi2020
mean(wage1$wage2013)
mean(wage1$wage2020)

# Q5: How many women/men are in the sample?

nfemale <- sum(wage1$female) # female is a DUMMY/INDICATOR/BINARY variable
nmale <- nrow(wage1) - nfemale # Residual of obs. are men (subtract women count)
nmale

wage1$male <- 1 - wage1$female # OR, now counting the number of men
sum(wage1$male)

table(wage1$female) # OR

## Exercise C1.2:

?bwght
View(bwght)

# Q1: How many women in the sample? How many report smoking during pregnancy?

str(bwght)
nrow(bwght) # OR
dim(bwght) # OR

sum(bwght$cigs > 0) # Number of SMOKING women
nrow(bwght) - sum(bwght$cigs > 0) # Number of NON SMOKING women
sum(bwght$cigs == 0) # OR

# Q2: What is the average number of cigs smoked per day? Is the average a good
#     measure of the "typical" woman in this case?

mean(bwght$cigs) # Makes more sense to say a typical woman did not smoke during
                 # pregnancy. Use the MEDIAN.
median(bwght$cigs)

# Q3: Among women who smoked during pregnancy, what is the average number of cigs 
#     smoked per day? Compare with Q2:

bwghtsmoke <- subset(bwght, cigs > 0) # Create a subset of the data, for cigs > 0
mean(bwghtsmoke$cigs) # MUCH higher than Q2, there the average is pulled down by 
                      # the large fraction of nonsmokers (cigs=0).
hist(bwght$cigs)
mean(bwght$cigs[bwght$cigs > 0]) # OR, subset the data directly

# Q4: What is the average of father education level in the sample? How many obs. 
#     are used to compute this average?

mean(bwght$fatheduc) # Why do we get an NA?

sum(is.na(bwght$fatheduc))
summary(bwght$fatheduc) # OR

# NOTE: NA will ALWAYS dominate ANY other number, character etc,, so beware of
#       NAs in data, account for them OR remove them

mean(bwght$fatheduc, na.rm=T) # NAs removed, T stands for TRUE, na.rm is an 
                              # option/argument
nrow(bwght) - summary(bwght$fatheduc)["NA's"] # Count of non-NA observations

# Q5: What is the average family income and its st. dev. in DOLLARS?

?bwght # faminc presented in terms of $1000s

mean(bwght$faminc) * 1000
sd(bwght$faminc) * 1000

################################################################################

####################
# For & While Loops:
####################

### Some Practical Applications of Loops:

## Standardizing Variables:

?wage1
View(wage1)

vars <- c("wage", "exper", "educ") # Simple example, only 3 variables, more useful
                                   # if have MANY variables

for (i in vars){ 
  
  name <- paste(i, "_std", sep = "")
  assign(name, (wage1[,i] - mean(wage1[,i]))/sd(wage1[,i]))
}

## Changing Units of Observations:

set.seed(123)

jan_avg <- runif(100, 32, 100) # Just randomly generating some temperature data
                               # from a uniform dist. for this demonstration
feb_avg <- runif(100, 32, 100)
mar_avg <- runif(100, 32, 100)

temps_f <- data.frame(jan_avg, feb_avg, mar_avg)

temps_c <- data.frame(matrix(NA, nrow = 100, ncol = 3))

for (i in 1:ncol(temps_f)){ 
  
  temps_c[,i] <- (temps_f[,i]- 32)*5/9 # Convert Fahrenheit to Celsius
  colnames(temps_c)[i] <- paste(colnames(temps_f)[i], "_c", sep="")
}

## Generating Dummies:

# NOTE: This is VERY USEFUL; will see an application later (Ch.6)

months <- sample(1:12,100,TRUE) # Uniformly sample WITH REPLACEMENT 100 values 
                                # from 1-12, think of this as months Jan-Dec

dummies <- data.frame(matrix(NA, nrow = 100, ncol = 12))

for (i in 1:12){
  
  name <- paste("month_", i, sep = "")
  dummies[i] <- ifelse(months == i, 1, 0)
  colnames(dummies)[i] <- name
}

dummies

# NOTE: Could use seq(from=1, to=12, by=1) instead of 1:12 in loop, more flexible
#       as the increment can be changed (by=2,3,4 etc.)

# NOTE: Will actually skip WHILE loops. NOT as important for us