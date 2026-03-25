"
DISCLAIMER: This is simply my attempt of the midterms. I sadly also don't have the
official answer. So I would say I don't 100% guarantee that everything here is correct. 
Therefore, if you see any errors, please do let me know!! And just like all T&C, to 
protect myself, if you plan to use any of this in your exam, use it at your own risk!!

That said, I don't want the fear of making mistakes to stop me from sharing resources 
and knowledge with you all!
"


# Before midterms, you can already create a file and setwd
setwd("~/My Drive (krlee7001@gmail.com)/NUS (Acads Related)/AY2526 Sem 2/DSA1101 TA")

#### MIDTERMS PAPER AY25/26 SEM 1 ####

#### Q1 ####

readLines("data/ford.csv", n = 3)

data = read.csv("data/ford.csv")

head(data)
dim(data) # rows = 17965, column = 9
str(data)


### 1.1 ###

### Q: Change all listings with fuelType equals to "Other" into "Hybrid"

table(data$fuelType) # only 1 "Other"

data$fuelType[data$fuelType == "Other"] = "Hybrid"

table(data$fuelType) # no more "Other" now


### 1.2 ###

### Q: Remove all listings with invalid year. Report no rows after cleaning.

# the question wants year from 1996 to 2020

data = data[data$year >= 1996 & data$year <= 2020,]

nrow(data) # 17964 rows now


### 1.3 ###

### Q: Contingency table (named tab1) for transmission and fuelType.

tab1 = table(data$transmission, data$fuelType)
tab1

tab1["Automatic", "Petrol"]

# Number of cars that use petrol and automatic transmission = 773


### 1.4 ###

### Q: Create box plots of price by groups of year for years 2016 to 2020.

data_filtered = data[data$year >= 2016 & data$year <= 2020,]

boxplot(price ~ year, data = data_filtered)

"
Comments: 
Median, first and third quartile of price shows increasing 
trend as year increases.

All years have high amounts of upper-tail outliers. 

Interquartile range (IQR) and range of price seems to be 
increasing over the years (more and more spread out).

There is a rare lower outlier in 2019.
"


### 1.5 ###

### Q: priceHL, which equals to high if the 
###    price of the car is above $12500, and equals to low otherwise.
###    Contingency table for priceHL and transmission 
###    (only for Automatic and Manual cars.

data$priceHL = ifelse(data$price > 12500, "high", "low")

data_filtered_2 = data[data$transmission != "Semi-Auto",]

# or data_filtered_2 = data[data$transmission %in% c("Automatic", "Manual"),]

tab2 = table(data_filtered_2$priceHL, data_filtered_2$transmission)

tab2

"
       Automatic Manual
  high       929   5619
  low        431   9898
"


### 1.6.1 ###

### Q: probability of high price car in the group of cars with automatic transmission


tab2[1,1] / sum(tab2[,1]) # 0.683

### 1.6.2 ###

### Q: probability of high price car in the group of cars with manual transmission

tab2[1,2] / sum(tab2[,2]) # 0.362

# Alternative: prop.table(tab2, margin = 2)

"
Probability of high price car amongst automatic: 0.683
Probability of high price car amongst manual: 0.362
Difference: 0.683-0.362 = 0.321

Intepretation: Automatic cars are 32.1% more likely to be 
high-priced than manual cars
"


### 1.7 ###

### Q: correlation between price and mileage
###    compare with the correlation between 1/price and mileage.

cor(data$price, data$mileage) # -0.531

cor(1/data$price, data$mileage) # 0.579

"
Mileage and price have a moderate negative linear relationship,
which make sense as more worn out cars have lower value.

There is a stronger linear relationship between 1/price and mileage,
than price and mileage (0.579 > 0.531). Mileage and 1/price has 
a moderate positive linear relationship (though more linear 
than price vs mileage)
"


### 1.8 ###

### Q: linear model, called model1, which uses fuelType, trans-
###    mission, mileage and mpg to predict a car's price. show coef.

model1 = lm(price ~ fuelType + transmission + mileage + mpg, 
            data = data)

# Note: lm() automatically converts all chr column to factors
model1$coefficients


"
price_hat = 30852.783 + 428.485 * I(fuelType = Electric) 
            + 11334.702 * I(fuelType = Hybrid) 
            - 4823.380 * I(fuelType = Petrol)
            - 730.548 * I(transmission = Manual)
            - 397.073 * I(transmission = Semi-Auto)
            - 0.146 * mileage - 194.098 * mpg
"


### 1.9 ###

### Q: predict the price of a car which has a manual transmission, runs
###    on petrol at 60 miles per gallon, and has a mileage of 30000.

new_data = data.frame(transmission = "Manual", fuelType = "Petrol",
                      mileage = 30000, mpg = 60)
  
predict(model1, new_data)

# Price of new data: $9256.379


### 1.10 ###

### Q: create a new data frame, called data.KNN which its first column
###    is the response and other columns are mileage, tax, mpg, 
###    and engineSize after standardization for all observations

scaled.KNN.X = scale(data[, c("mileage", "tax", "mpg", "engineSize")])
head(scaled.KNN.X)

data.KNN = data.frame(priceHL = data$priceHL, scaled.KNN.X)
head(data.KNN) 


### 1.11 ###

set.seed(210)
n = nrow(data.KNN)

train.indices = sample(1:n, n/2)

train.set = data.KNN[train.indices,]
test.set = data.KNN[-train.indices,]

dim(train.set)
dim(test.set) # two must be same 


### 1.12 ###

library(class)

K = seq(3, 25, 2)
K

fnr = numeric(length(K))
accuracy = numeric(length(K))

# important note: don't do for (i in 1:K)!
for (i in 1:length(K)) {
  pred = knn(train.set[,-1], test.set[,-1], train.set[,1], k = K[i])
  
  conf_mat = table(pred, test.set[,1])
  
  accuracy[i] = sum(diag(conf_mat)) / sum(conf_mat)
  
  # FNR = FN / (TP + FN)
  FN = conf_mat["low","high"] # predict low but actually is high
  TP = conf_mat["high", "high"]
  fnr[i] = FN / (FN + TP)
}


# Inspect accuracy and fnr 
fnr
accuracy 


### 1.13 ###

low_fnr_k = K[fnr < 0.1]
low_fnr_k
# k = 7 9 11 13 15 17 19 21 23 25

good.fnr = cbind(k = low_fnr_k, fnr = fnr[fnr < 0.1], accuracy = accuracy[fnr < 0.1])
is.matrix(good.fnr) # TRUE
good.fnr

best.K = good.fnr[good.fnr[,"accuracy"] == max(good.fnr[,"accuracy"]), "k"]
best.K # k = 11

good.fnr[good.fnr[,"k"] == best.K, ]

"
         k        fnr   accuracy 
11.0000000  0.0968550  0.9071476 

best k = 11
fnr = 0.0969, accuracy= 0.907
"


### 1.14 ###

# train now is test.set!
pred.best.K = knn(train = test.set[,-1], 
                  test = train.set[,-1], 
                  cl = test.set[,1], 
                  k = best.K)


### 1.15 ###

conf_mat_new = table(pred.best.K, train.set[,1])
conf_mat_new

"
pred.best.K high  low
       high 3174  463
       low   394 4951
"

fnr_new = conf_mat_new[2,1] / sum(conf_mat_new[,1]) # 0.111
acc_new = sum(diag(conf_mat_new)) / sum(conf_mat_new) # 0.904

# FNR: 0.111, Accuracy = 0.904


### 1.16 ###

new_point = data.frame(mileage = 30000, tax = 110, mpg = 60, engineSize = 1)

new_point_scaled = scale(new_point, 
                         center = attr(scaled.KNN.X, "scaled:center"),
                         scale = attr(scaled.KNN.X, "scaled:scale"))

pred_new = knn(train = test.set[,-1], 
               test = new_point_scaled, 
               cl = test.set[,1], 
               k = best.K,
               prob = TRUE)
pred_new # low, with probability 0.909 
# (prob in this question is extra, just for your ref)


### 2.1 ###

balance = 700000
month = 0

while (balance > 0) {
  
  # interest added at start of month
  balance = balance * (1 + 0.026/12)
  
  balance = balance - 2500
  
  month = month + 1
}

month/12 # need 36 years, their plan is not possible



### 2.2 ###

F <- function(payment) {
  balance = 0
  total_months = 25*12
  
  for (i in 1:total_months) {
    balance = (balance + payment) / (1 + 0.026/12)
  }
  return(balance)
}

F(2500) # $551061.9


### 2.3 ###

payment = 1
while (F(payment) < 700000) {
  payment = payment + 1
}
payment # $3176