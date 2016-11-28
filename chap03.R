###########################################
## self code : R for Marketing Analytic
## dt: 11/7/2016
## auth: satya pati
## chap 3
###########################################
# code to get the data set for chapter
store.df <- read.csv("http://goo.gl/QPDdMl")
head(store.df)
# but, I'm not gonna use this data set
#+ lets create the dataset on my own
rm(store.df)

# simulate dataset
k.stores <- 20 # 20 stores, using "k" for constant
k.weeks <- 104 # 2 years data, 52*2=104 weeks

# create empty data frame which will hold the values
store.df <- data.frame(matrix(NA, ncol=10, nrow = k.stores*k.weeks))
names(store.df) <- c("storeNum", "Year", "Week", "p1sales", "p2sales",
                     "p1price", "p2price", "p1prom", "p2prom", "country")
dim(store.df)

# create store numbers
store.num <- 101:(100 + k.stores)
(store.city <- c(rep("US",3), rep("DE", 5), rep("GB", 3), rep("BR", 2), rep("JP", 4)
                 , rep("AU", 1) , rep("CN",2)))
length(store.city)

# fill in store num in the data frame
store.df$storeNum <- rep(store.num, each=k.weeks)
# fill in country in the data frame
store.df$country <- rep(store.city, each=k.weeks)
# clean up unwanted objects
rm(store.num, store.city)
# fill in week and year
(store.df$Week <- rep(1:52, times=k.stores*2))
(store.df$Year <- rep(rep(1:2, each=k.weeks/2), times=k.stores))

# check overall data structure
str(store.df)

# since storeNum and country contain discrete value
#+ it's converted to factors before analysis
store.df$storeNum <- factor(store.df$storeNum) # 20 distinct levels
store.df$country <- factor(store.df$country) # 7 distinct levels

str(store.df)

# inspect data from begining and end
head(store.df)
head(store.df, 120)
tail(store.df, 120)

names(store.df)

# set seed to ensure same set of random
#+ numbers are generated everytime. this
#+ helps in meaningful comparison of the results
set.seed(98250) # seed has be set to favorite us postal code

# assign p1prom & p2prom values based on binomial distribution
store.df$p1prom <- rbinom(n=nrow(store.df), size=1, p=0.1)
store.df$p2prom <- rbinom(n=nrow(store.df), size=1, p=0.15)
head(store.df)

# set p1price & p2price
# read "sample from x as many times
#+ as there are rows in the store.df
#+ each time sampled with replacement
store.df$p1price <- sample(x=c(2.19, 2.29, 2.49, 2.79, 2.99)
                           , size=nrow(store.df), replace = T)
store.df$p2price <- sample(x=c(2.29, 2.49, 2.59, 2.99, 3.19)
                           , size=nrow(store.df), replace = T)
head(store.df)

# sales data is filled using poisson (counts)
#+ distribution. rpois() first, default sales
#+ in the absense of promotion data
tmp.sales1 <- rpois(nrow(store.df), lambda = 120) # lambda is slightly higher than prod2
tmp.sales2 <- rpois(nrow(store.df), lambda = 100)

# scale the sales according to price
#+ price effect follows logarithmic 
#+ function as per book.
tmp.sales1 <- tmp.sales1 * (log(store.df$p2price)/log(store.df$p1price)) # higher the price lower the sales
tmp.sales2 <- tmp.sales2 * (log(store.df$p1price)/log(store.df$p2price))

# final sales gets 30% or 40% lift when promoted
store.df$p1sales <- floor(tmp.sales2 * (1 + store.df$p1prom * 0.3))
store.df$p2sales <- floor(tmp.sales2 * (1 + store.df$p2prom * 0.4))

head(store.df)
# checking random sample rows to 
#+ ensure data is mocked up correctly
install.packages("car")
library("car")
some(store.df, 10)

##############################
# DATA SUMMARIZATION
##############################
# Frequency Count of discrete variable
p1.table <- table(store.df$p1price)
p2.table <- table(store.df$p2price)

plot(p1.table)
plot(p2.table)

# two-way crosstable for p1
p1.table2 <- table(store.df$p1price, store.df$p1prom)
# get the exact fraction of time p1 was promoted
#+ and append the new field to the end
(p1.table2 <- cbind(p1.table2, round(p1.table2[,2]/(p1.table2[,1] + p1.table2[,2]),3)))

# dealing with continuous variables
min(store.df$p1sales)
max(store.df$p1sales)
mean(store.df$p1sales)
min(store.df$p2sales)
max(store.df$p2sales)
mean(store.df$p2sales)
var(store.df$p1sales)
var(store.df$p2sales)
mad(store.df$p1sales)
quantile(store.df$p1sales, probs = c(0.25, 0.5, 0.75))
quantile(store.df$p1sales, probs = c(0.2, 0.4, 0.6, 0.8))
median(store.df$p1sales)
IQR(store.df$p1sales)

# check what values the central 90% data live in
quantile(store.df$p1sales, probs = c(0.05, 0.95))
quantile(store.df$p1sales, probs = 0:10/10)

