## Tutorial 1 ECO375 20/1/2023
################################################################################

# Housekeeping: 1) OH, Fridays 2-3 (?) TA Room Econ Dept. (Innovation Complex) 
#               2) Signing the attendance sheet
#               3) Assignment on MindTap Jan. 29th + MindTap videos
#               4) Stata gaffe
#               5) How many familiar with R? Other programs? 
#               6) The value of Googling
#               7) R UI dark mode (in Global Options)
#               8) Rmarkdown and LaTex (Especially for the Research Project)

################################################################################

###################
# Initial Comments:
################### 

### Goal of the Tutorials:

# Learn the best coding practices
# Apply the concepts seen in class
# Learn how to estimate models and interpret the results
# Improve coding abilities
# Review the class material in an empirical fashion*
# Introduce the best practices for data visualization
# Personal Aim: Provide you with a repository of code for doing multiple things
#               that you might have to do working with real data
# * Some textbook questions (from Problem Sets) will occasionally be solved 
# (time permitting)

### General Approach When Coding - What Are We Doing?:

# 0) OBTAIN data (PRE-R)
# 1) IMPORT the raw data, i.e. read the data file
# 2) CLEAN and MANIPULATE the data (e.g. NA observations)
# 3) EXPLORE and VISUALIZE the data; summary statistics (mean, median etc.)
# 4) ANALYZE; estimating, testing, prediction, forecasting etc.
# 5) OUTPUT results, displayable tables and figures
# 6) POLISH your written report (POST-R)

### Downloading R and RStudio:

# https://cran.rstudio.com/
# https://posit.co/download/rstudio-desktop/

# Why R? FREE (unlike Stata) and OPEN SOURCE (constantly updating) language

################################################################################

############################################
# Getting Familiar with RStudio's Interface:
############################################

# 4 Panes: Source/Script/Editor, Console, Environment/History, Files etc.

# 1) Script: where you write your code/commands (and save them for later 
#            reproducibility). It WON'T store/show your outputs. You can also
#            leave comments using #. # tells R that your comments AREN'T commands

# 2) Console: where R runs the commands that you tell it to run and displays
#             the results of the command. Run commands using "Run" OR keyboard
#             shortcut "Ctrl+Enter", after highlighting lines you want to run

# NOTE: ALWAYS work in SCRIPTS (or R markdowns) instead of the console directly
#       so that you can replicate your results, and debug your code easily

# 3) Environment: where R saves any OBJECTS that you construct/load (explain 
#                 more later), e.g. data sets or variables or functions

# 4) History: a log of any commands that were run by R

# 5) Files: a list of all the folders on your device. Helpful for setting working
#           directories, i.e. the default folder R refers to for loading data
#           and saving graphs etc.

setwd("C:/Users/malek/Desktop/TAship/ECO375H5S/My Tutorials/Tutorial 1") # OR

# NOTE: " " indicates to R that you are referring to a character/string, NOT a 
#       command or number

getwd() # To check what your current working directory is

# 6) Plots: where R displays any graphs that were plotted

# 7) Packages: list of all "bundles" of code that are currently available on
#              your device (in your Library). Think of these as downloadable 
#              extensions (see more later)

# install.packages("wooldridge") # Install package for the textbook's data sets
library(wooldridge) # Load package

View(wage1)

# detach("package:wooldridge", unload = TRUE) 

# Above detach for if you want to unload a package. Why? e.g. if two different
# commands from two different packages have the same name, R will get confused.
# Won't usually be the case for us

# NOTE: IMPORTANT; you MUST tell R to use the package that you installed, i.e.
#       it's NOT enough to just download, you need to load it also. You only
#       need to install the package once, but every time you want to use a
#       command from the package, you need to load it from your library (once in
#       the beginning is sufficient)

# NOTE: There are MANY different ways of doing things in R. Biggest paradigm
#       divide is Base R vs. tidyverse, tidyverse is a PACKAGE of a combination
#       of helpful commands. Won't be using tidyverse (although it is very
#       helpful) for simplicity (even though it is quite easy). If interested, 
#       the mindtap walkthrough videos use tidyverse commands.

# 8) Help: access the description of any command/online data set, along with its
#          arguments/list of variables, and some examples (see more later)

?mean # ?name of command
help("mean") # OR

?wage1 # Works for data sets also

# NOTE: Help -> Cheat Sheets for helpful pdfs on many useful commands/functions

################################################################################

################################
# Understanding Data Structures:
################################

### Some Technical Classifications:

# R works with OBJECTS, which belong to MODES and CLASSES:

# 1) Mode, e.g. numeric, character, logical (1/0 or TRUE/FALSE).
#    Each object can only have ONE mode (mutually exclusive).

# 2) Class, labels put on objects to tell R how to apply certain commands to 
#    these objects. An object CAN have more than one class.

# Certain commands ONLY work on certain classes/modes of objects, e.g. adding 
# characters/strings DOESN'T make sense to R.

### The Basics of R:

x <- 375 # <- is the assignment operator, assigning a numerical value
mode(x)
X = 375 # = also works, but <- is conventional
x <- "economics" # Assigning a character/string
mode(x)

x  # OR print(x), extra print() is redundant here (useful with loops, see later)
X

# NOTE: R is CASE SENSITIVE for EVERYTHING

# NOTE: R will OVERWRITE any object that is already defined with the same name

even <- c(2,4,5) # c = CONCATENATE, i.e. collect, assigning a numeric vector
cities <- c("Toronto","Montreal","Vancouver") # Assigning a string vector

cities[c(1,3)] # Returns a SUBSET of the vector above (1st and 3nd elements)
even[3] <- 6
even[3]

A <- matrix(1:9, nrow=3, ncol=3) # Generates a matrix with entries 1-9, : notation 
                                 # for sequence of integers
A <- matrix(1:9, 3, 3) # OR

# NOTE: SO for R's FUNCTIONS/COMMANDS, can omit the ARGUMENTS, BUT the order of 
#       the arguments has to be preserved, as shown in ?matrix for example. So 
#       be careful to not be doing something you don't want to be doing

args(matrix) # Find the possible arguments for a specific function

A
mode(A)
class(A)
attributes(A) # Now, this object has a "dimension", dim(A)

A[2,3] # Returns entry in 2nd row, 3rd column. Which #, 6 or 8?

# NOTICE: R fills in a matrix COLUMN-wise NOT ROW-wise as we are used to. If
#         want by row, need to add argument , byrow = TRUE

A[,2] # Leaving the row index empty tells R to return ALL rows, of 2nd column
      # Analogously A[2,] returns the 2nd row

A*A # Element-wise multiplication, NOT matrix multiplication as used to
A%*%A # Matrix multiplication, as used to
t(A) # Transpose of a matrix, etc. (many other matrix operations, add, inverse
     # etc.)

odd <- c(1,3,5)
odd2 <- c(1,3,5,7)

even + odd # Other operations: multiply (*), divide (\), subtract (-) etc.
           # All operations are done ELEMENT-WISE, i.e. each corresponding 
           # entry in objects
even + odd2 # R STILL runs the operation even though lengths of vectors don't
            # match, odd2 is longer. BUT R gives you a warning
odd + x # Even though x is a scalar, can still pair-wise add (etc.) to a vector
odd + rep(375,3) # OR, rep() creates a vector of 375's with length 3 (redundant)

odd3 <- c(1,3,5,7,9,11)

even + odd3 # No warning even though even and odd3 have different lengths. Why?
# R RECYCLES the vector, i.e. 1 + 2, 3 + 4, 5 + 6
#                             7 + 2, 9 + 4, 11 + 6 . SO NO warning if vectors
# are multiples of each other. Watch out for this

### Example - Applying the Basics of R:

# A simple game of chance; fair die, earn points = side of the die MINUS 3.5

# Q1: What are the expected number of points earned in ONE round?

points <- c(1,2,3,4,5,6) - 3.5 # ALL possible point totals that you can earn
points

((-2.5) + (-1.5) + (-0.5) + 0.5 + 1.5 + 2.5) / 6 # Find the mean/expected value
sum(points) / 6 # OR
avgpts <- mean(points) # OR
avgpts

# Q2: Suppose you play the game for 4 rounds, and in EACH round you roll the die 
#     3 times. The outcomes of the rounds are:
#     Round 1: 3 5 1
#     Round 2: 4 1 2
#     Round 3: 1 2 3
#     Round 4: 5 6 4
#     What is the average points total earned in EACH round?

round <- c(1,2,3,4) # observation/row index for rounds
r1 <- c(3,5,1) - 3.5 # outcomes in round 1
r2 <- c(4,1,2) - 3.5 # outcomes in round 2 etc.
r3 <- c(1,2,3) - 3.5
r4 <- c(5,6,4) - 3.5

outcomes <- rbind(r1,r2,r3,r4) # FIRST, BIND the 3 vectors into a 3x4 matrix,
                               # each col. is ONE round, rbind = MATCH ROWS
data <- cbind(round,outcomes) # SECOND, append the index/round column to matrix,
                              # cbind = MATCH COLUMNS
colnames(data) <- c("round","roll1","roll2","roll3") # Renaming the columns
                                                     # Analogous, rownames()...
data

data <- data.frame(data) # Converting the matrix into a DATAFRAME (df)

# NOTE: What is the difference between a matrix and a dataframe? Matrix entries 
#       have to be of the SAME class, e.g. all NUMERICS or all CHARACTERS. BUT 
#       df can have MANY classes, e.g. character and numeric in different columns.
#       Either way, BOTH are rectangular data objects (how R stores data).

summary(data) # Provides some SUMMARY STATISTICS for variables/columns. WRONG, 
              # as this finds the COLUMN/ROLL means, not the ROW/ROUND average
rowMeans(data[,-1]) # -1 indicates to R to obtain all columns EXCEPT for row 1
                    # Analogous, colMeans(), same job as summary (only for mean)

################################################################################

####################################
# Importing and Exporting Data Sets:
####################################

### Importing:

getwd()

# install.packages("haven")
library(haven) # Package to import .dta files, Stata files
somedata1 <- read_dta("WAGE1.DTA")
View(somedata1) # Display the data set

# NOTE: Can also import manually by clicking on a data set in the Files tab.
#       This can be helpful if the data set is messy (e.g. missing column names)

# install.packages("readxl")
library(readxl) # Package to import Excel files
somedata2 <- read_excel("Gasoline.xls") # Works for .xlsx also
View(somedata2)

# NOTE: Can have MULTIPLE data sets open. Nice feature of R (unlike Stata)

# install.packages("readr")
library(readr) # Package to import .csv files
somedata3 <- read.csv("CAschools.csv")
somedata4 <- read_csv("CAschools.csv") # OR
View(somedata3)

class(somedata3)
class(somedata4)

# NOTE: Difference between read.csv and read_csv? When using a . , R creates
#       a dataframe, BUT _ gives an object called a tibble. VERY similar to
#       dataframe, BUT has more attributes. A tibble IS a dataframe, but dataframe
#       NOT a tibble. Won't concern us here, will use both of these objects 
#       interchangeably (but mainly dataframes).

### Exporting:

write_dta(somedata1, path = "test.dta")

write_csv(somedata1, file = "test.csv")

# install.packages("writexl")
library("writexl") # Package to export as an .xlsx file
write_xlsx(somedata1, path = "test.xlsx")

# NOTE: R will OVERWRITE any existing file with the same name, so be careful

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

# NOTE: Common theme that will keep popping up, there are MANY ways of doing the
#       same thing in R. Some ways are more efficient, i.e. less code to write, 
#       OR less computing time. I will mix and match between the ways depending
#       on convenience, but you can choose what you are most comfortable with.

summary(wage1[c(1,2,6)]) # Summary statistics of selected variables

table(wage1["educ"]) # Gives a table of COUNTS for each level of educ
table(wage1$educ) # OR, $ tells R to obtain that variable from data set, ONLY
                  # works for one variable, i.e. can't have $educ$female 

table(wage1$educ, wage1$female) # JOINT FREQUENCIES table (i.e. marginal dists., 
                                # distribution of pairs)
table(wage1[c("educ","female")]) # OR, here [] more convenient than $

# NOTICE: Some summary statistics are missing, e.g. variance, std dev, cov/cor 
#         (for more than two variables)

var(wage1$educ) # Similarly, mean(), min(), max()
sd(wage1$educ)

cov(wage1$wage, wage1$educ)
cor(wage1$wage, wage1$educ) # See later CORRELATION/COVARIANCE MATRIX, more
                            # than two variables

### Boolean Statements/Operators and Subsetting:

x <- c(1,2,3) # Simple Demonstration:

x > 1
x == 2
x <= 3
x > 0 & x > 2 # & indicates intersection, i.e. and
x == 2 | x == 3 # # | indicates union, i.e. or

subset(wage1, wage > 10) # To subset using a boolean/logical vector. Gives ONLY
                         # the observations with wage > 10

new_data <- subset(wage1, wage > 10 & female == 0)
summary(new_data$wage) # Summarize wage for those with wage > 10 AND are male 
                       # (female == 0)

### Generating and Modifying Variables:

wage1$daily <- wage1$wage*8 # Create a 9-5pm wage variable
View(wage1)
wage1 <- wage1[,-25] # Drop the newly created variable

wage1$wage <- ifelse( wage1$wage < 0 , 0 , wage1$wage) 
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
#       learn if you are interested. Part of tidyverse

### Graphing:

# Data Set 1 - Labor Data on Wages and Characteristics (CROSS SECTIONAL):

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

counts<- table(tablewage$educ) # Find the FREQUENCIES/EMPIRICAL DIST. of obs.
with(wage1, barplot(counts)) # BAR GRAPH, a crude form of a histogram, as we
                             # graph singletons (single values), NOT intervals

# Data Set 2; Puerto Rico Minimum Wage (TIME SERIES):

# NOTE: This class will focus on cross-sectional data, time series estimation
#       is MUCH more complicated (ECO475?). But graphing still straightforward

View(prminwge)
with(prminwge, plot(year, avgmin, type="l")) # LINE GRAPH, avg minwage over time

### Exporting Graphs:

pdf(file = "test.pdf") # Export as a pdf (better resolution). MUST run chunk of
                       # code TOGETHER with the graph and dev.off()
with(tablewage, barplot(counts))
dev.off()

jpeg(file = "test.jpg") # Export as a jpeg image (lower resolution?)
with(tablewage, barplot(counts))
dev.off()

# NOTE: Can also export manually by clicking on the export button in the Plots tab.