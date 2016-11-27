###########################################
## self code : R for Marketing Analytic
## dt: 11/25/2016
## auth: satya pati
## chap 2
###########################################

# create a named object and a vector of numbers
# read "x gets vector (2,4,6,8)
x <- c(2, 4, 6, 8)
# install required add-on packages
install.packages(c('lavaan', 'semPlot', 'corrplot', 'multcomp'))

# load the dataset from books website and examine
satData <- read.csv('http://goo.gl/UDv12g')
satData$Segment <- factor(satData$Segment)
head(satData)
summary(satData)
# get number of rows in the dataset
nrow(satData) 

# load library for correlation plot
install.packages('corrplot')
library('corrplot')

# check the colnames for the dataset
colnames(satData)
# exclude "Segment" from the corrplot
corrplot.mixed(cor(satData[, -3]))
# another way to exclude "Segment" ==>
## corrplot.mixed(cor(satData[colnames(satData) != "Segment"]))

# computer mean satisfaction by segment
aggregate(iProdSAT ~ Segment, satData, mean)

# anova test to confirm the difference in satisfaction 
#+ is statistically significant
sat.anova <- aov(iProdSAT ~ -1 + Segment, satData)
summary(sat.anova)

# plot confidence interval of mean satisfaction by segment
library(multcomp)
par(mar=c(4,8,4,2))
plot(glht(sat.anova))

# latent variable model
satModel <- "SAT =~ iProdSAT + iSalesSAT
             REC =~ iProdREC + iSalesREC
             REC ~ SAT"

# fit latent variable model(structural model) to data using lavaan
library(lavaan)
sat.fit <- cfa(satModel, data=satData)
summary(sat.fit, fit.m=T)

# visualize structural model using semPlot
library(semPlot)
semPaths(sat.fit, what="est", residuals = F, intercepts = F, nCharNodes = 9)


# vectors : one dimensional data points of similar kind
x <- c(2, 4, 6, 8)
xNum <- c(1, 3.14159, 5 , 7)
xLog <- c(T, F, T, T)
xChar <- c('foo', 'bar', 'boo', 'far')
xMix <- c(1, T, 3, "Hello, World!")

# appending 2 vectors
x2 <- c(x, x)

# summary of vectors
summary(x2)
summary(xMix)

# indexing
xMix[4]

# multipying number to a vector
#+ each member of the vector
x2*pi

# vector addition - unequal length
#+ larger vector ideally should be
#+ of ideall of multiple length of smaller vector
#+ smaller vector is stretched for addition
x3 <- c(1,2,3)
x + x2 + x3

# converting string to numeric
xMix[1] + 1 # !!!Err
as.numeric(xMix[1]) + 1 # works as expected

# get the information about an object using str()
str(xMix)
str(xNum)
str(xLog)
str(xChar)

# getting help
## ?numeric # gives detail about the function and related topic
## ??anova # gives list of related help section for the concept

# see vignette files (help files) for each of the package installed
browseVignettes()

###########################
# MORE VECTORS AND INDEXES
###########################
xSeq <- 1:10
1:5*2
# vs
1:(5*2)
# : operator has higher precedence over math operators

# using sequences for indexing
xNum[2:4]
myStart <- 2
xNum[myStart: sqrt(7+myStart)]

# using seq() & rep()
seq(from=5, to=100, by =3)
rep(c(1,2,3), each=3)
rep(seq(from=-3, to=13, by=4), c(1,2,3,2,1))

# exclude items using negative indexes
# excludes elements from 5 to 7 including both
xSeq[-5:-7]

# [1] in the output row indicates vector
#+ position index. For a long output it 
#+ the index gets value is displayed after 
#+ the width of screen is used up, for example
1:300
1001:1300

# indexing using vector of logical values
i_vec = c(T,F,F,T,F)
# use logical vector to select only 1st & 4th element
xNum[i_vec]
# using logical expression
xSeq[xSeq > 7] 

# NA treatment
my.test.scores <- c(98, 97, 99, NA, NA)
# operation with NA values returns NA
mean(my.test.scores)
# if we want to ignore NA 
mean(my.test.scores, na.rm = T)

# ignore NA when assigning values to a new array
not.na.score <- na.omit(my.test.scores)
sum(not.na.score)

# yet another way to filter NAs 
my.test.scores[!is.na(my.test.scores)]

# replace program specific constants as NA
my.test.scores <- c(97, 49, -999, -999)
my.test.scores [my.test.scores == -999] <- NA


############################################
# LISTS <- Collection of objects of any type
############################################
xList <- list(xNum, xChar)
str(xList)

# lists are indexed using [[]]
#+ for example, to access 1st element
xList[[1]]
summary(xList[[1]])

# lapply() : list apply. used to run a command 
#+ against each element of list at once
lapply(xList, summary)

# assigning names to objects within a list
xList <- list(xNum, xChar)
names(xList) <- c("itemnum", "itemchar")

# or alternatively
xList <- list(itemnum=xNum, itemchar=xChar)
names(xList)

# indexing a list using name
xList$itemnum
# or
xList[["itemnum"]]

##############################################
# DATA FRAMES
#+ multiple columns with same data type or NA
#+ multiple rows of data
##############################################
#+ when xChar is added it gets added as
#+ nominal factors. it's values are added
#+ as nominal factors.
x.df <- data.frame(xNum, xLog, xChar)
x.df
# names of the columns are inherited from 
#+ the individual vectors
names(x.df) 
# indexing using [ROW, COLUMN] notation
# get me the 1st colum of 2nd row
x.df[2,1]
# get me the 1st column of 2nd to 4th row
x.df[2:4,1] 
# categorical field gives the # of levels as well
x.df[1,3] 

# access the levels of categorical variable
levels(x.df[,3])

# prevent automatic conversion to factors
x.df2 <- data.frame(xNum, xLog, xChar, stringsAsFactors = F)
str(x.df2)
x.df2[,3]

# selecting all columns of a row
x.df[2, ]
# select all rows for a column
x.df[ , 2]

# some indexing techniques in data frame
## select row 2-3 and all columns
x.df[2:3, ]

## select column 1-2 and all rows
x.df[, 1:2]

## select all rows except 3rd 
x.df[-3, ]

## select all columns except 2nd
x.df[, -2]

## clean up the workspace
rm(list=ls())
store.num <- factor(c(3, 14, 21, 32, 54))
store.rev <- c(543, 654, 345, 678, 234)
store.visits <- c(45, 78, 32, 56, 34)
store.manager <- c("Annie", "Bert", "Carla", "Dave", "Ella")
(store.df <- data.frame(store.num, store.rev, store.visits, store.manager, stringsAsFactors = F))

# access store managers
store.df$store.manager

# calculate the mean revenue
mean(store.df$store.rev)

# calculate correlation coefficient for store.rev & store.visits
cor(store.df$store.rev, store.df$store.visits)

summary(store.df)
str(store.df$store.num)
levels(store.df$store.num)

# serialize and de-serialize objects in R
save(store.df, file="store-df-backup.RData")
rm(store.df)

# load / de-serialize store.df object
load("store-df-backup.RData")
str(store.df)

# serializing vector of objects
save(list=c("store.df", "store.visits"), file="store-df-backup.rdata")
rm(list=c("store.df", "store.visits"))
store.df
store.visits
load("store-df-backup.rdata")
store.df
store.visits

# upon reloading same variables are created in memory
#+ if there is already a variable with same name in the
#+ environment, that gets overwritten by the load operation
store.df <- 5
load("store-df-backup.rdata")
# store.df is now overwritten by the data present in the file
store.df

# writing data to csv
# without filename the output emitted to console
write.csv(store.df, row.names = F)
# with filename the output is saved in file
write.csv(store.df, file = "store-df.csv", row.names = F)
store.df2 <- read.csv(file = "store-df.csv", stringsAsFactors = F)
store.df2$store.num <- factor(store.df2$store.num)

# compare if the dataframes are exactly same
store.df == store.df2
# or approximately same, considering the 
#+ round off due to finite approximation
all.equal(store.df, store.df2)

##############################################
# FUNCTIONS
##############################################
# define a function to calculate standard error
se <- function(x) {sd(x) / sqrt(length(x))}
# use "se" to calculate the std. error for store.visits
se(store.visits)

# "se" written in proper coding standard
se <- function(x) {
  # compute standard error of the mean
  tmp.sd <- sd(x)
  tmp.N <- length(x)
  tmp.se <- tmp.sd / sqrt(tmp.N)
  return (tmp.se)
}

# inspect function defintion by just typing it's "name"
se

###################################
## TEMPLATE of control flows
## if (TEST) EXPR [else EXPR.b]
## while (TEST) EXPR
## for (name in vector) EXPR
## switch (INDEX, LIST) 
## repeat EXPR # repeats the statement forever 'infinite loop'
###################################
# vector form of ifelse()
x <- -2:2
log(x)
ifelse(x > 0, x, NA)
log(ifelse(x > 0, x, NA))

# anonymous functions
my.data <- matrix(runif(100), ncol = 5) #100 random number from uniform distribution in 5 cols
apply(my.data, 2, median) /2

# alternatively
halfmedian <- function(x) {median(x) / 2}
apply(my.data, 2, halfmedian)

# alternatively with anonymous function
apply(my.data, 2, function(x) {median(x)/2})

# cleaning up after work
ls(pattern = "store")
rm(list = ls(pattern = "store"))


list.files()
file.remove(c("mySession_1126.RData","mySession_1126_2.RData"))
# saving a working session in .RData file
save.image("mySession_1126.RData")
save.image("mySession_1126_2.RData")
# to restore the saved session
#+ this will add the saved variables
#+ to the current session and overwrite
#+ variable with same name. NOTE: load()
#+ doesn't rollback the session to a previous
#+ state. it adds saved variables to existing session

load("mySession_1126.RData")