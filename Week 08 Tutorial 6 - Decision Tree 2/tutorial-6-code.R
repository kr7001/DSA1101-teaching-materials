setwd("~/My Drive (krlee7001@gmail.com)/NUS (Acads Related)/AY2526 Sem 2/DSA1101 TA")

########## ONSITE QUESTION PREP ##############

# Import data
readLines("Data/churn.csv", n = 3)
churn = read.csv("Data/churn.csv")

# Inspect data
head(churn)
=
# Remove ID column 
churn = churn[,-1]
head(churn)

# Switch categorical variables to factor
churn$Married = as.factor(churn$Married)
churn$Churned = as.factor(churn$Churned)

str(churn)

########## ONSITE QUESTION 1A ##############

library(rpart)
library(rpart.plot)

# Here we choose minsplit and information gain 
fit <- rpart(Churned ~ Age + Married + Cust_years + Churned_contacts,
      data = churn,
      method = "class",
      control = rpart.control(minsplit = 1),
      parms = list(split = 'information'))

summary(fit)

# Visualizing Decision Tree 
rpart.plot(x = fit, type = 4, extra = 2,
           clip.right.labs = FALSE, varlen = 0, faclen = 0)


########## ONSITE QUESTION 1B ##############

age = c(26,23,56,36,45,28,22,22,60,32)
married = c(1,1,1,1,0,0,1,0,1,0)
cust =c(2,3,5,5,2,2,3,3,2,3)
contact = c(2,3,2,2,1,2,0,2,1,1)

married = as.factor(married)

# Create a data frame for the 10 test points. 
# This data frame MUST have the header the same as the name of the factors in the tree.
names(churn)

new = data.frame(Age = age, 
                 Married = married, 
                 Cust_years = cust, 
                 Churned_contacts = contact)
new

# Predict the 10 new data 
pred = predict(fit, newdata = new, type = 'prob') # to view probability
pred

pred = predict(fit, newdata = new, type = 'class')
pred


########## OFFSITE QUESTION 1 ##############

iris = read.csv("Data/iris.csv")
head(iris)

table(iris$class) # 3 categories, each class has 50 observations

"
So for 5-fold equal split between categories: 
Each category will have 10 in test and 40 in train in each fold. 

Steps: 
1) Randomly split 50 observations in each category into 2 parts: 
train (40) and test (10)

2) Combine 40 of setosa, versicolor, virginica to get full train set of 120.
Combine 10 of setosa, versicolor, virginica to get full test set of 30. 

3) Create decision tree model using train set (120). Apply tree to test set 
to get accuracy. 
"

n_folds = 5 

set.seed(888)

folds_1 <- sample(rep(1:n_folds, length.out = 50)) # for type 1 = setosa
folds_2 <- sample(rep(1:n_folds, length.out = 50)) # for type 2 = versicolor
folds_3 <- sample(rep(1:n_folds, length.out = 50)) # for type 3 = virginica

table(folds_1) # still equal split

# Coincidentally, in actual dataset, observations are sorted by "class"
data1 = iris[1:50,] # first 50 is setosa
data2 = iris[51:100,] # next is versicolor
data3 = iris[101:150,] # last 50 is virginica

# Alternative method 
data1 = iris[iris$class == "Iris-setosa",]
data2 = iris[iris$class == "Iris-versicolor",]
data3 = iris[iris$class == "Iris-virginica",]


# to store the value of accuracy for each fold
acc = numeric(n_folds) 

for (j in 1:n_folds) {
  
  # get the 10 rows with indexes = j to put in the test set
  test1 <- which(folds_1 == j) 
  test2 <- which(folds_2 == j)
  test3 <- which(folds_3 == j) 
  
  # take the rest 40 to be train set
  train.1 = data1[-test1, ] 
  train.2 = data2[-test2, ] 
  train.3 = data3[-test3, ] 
  
  # Stacking them to make train set of 120 rows
  train = rbind(train.1, train.2, train.3)
  
  # Test set of 30 rows
  test = rbind(data1[test1,], data2[test2,], data3[test3,] )
  
  # Fit decision tree using the train set
  fit.iris <- rpart(class ~ ., # . means all features
                    method = "class", 
                    data = train, 
                    control = rpart.control(minsplit = 1),
                    parms = list(split ='information'))
  
  # predicted response for the test set
  pred = predict(fit.iris, newdata = test[,1:4], type = 'class')
  
  # form the confusion matrix of real response and predicted response
  confusion.matrix = table(pred, test[,5])
  
  # get the accuracy
  # there are 5 folds, hence there are 5 values of accuracy
  acc[j] = sum(diag(confusion.matrix))/sum(confusion.matrix)
}

acc # 0.9333333 1.0000000 0.9000000 0.9333333 0.8666667

# average accuracy 
mean(acc) # 0.9266667



########## OFFSITE QUESTION 2 PREP ##############

"
Context: 
Consider the data set ‘bank-sample.csv’ we discussed in the lectures. 
For this exercise, we will fit a decision tree with subscribed as outcome; 
and job, marital, education, default, housing, loan, contact and poutcome
as 8 feature variables. We want to find the best cp value in terms of 
mis-classification error rate.
"

readLines("Data/bank-sample.csv", n = 3)

banktrain <- read.csv("Data/bank-sample.csv")

dim(banktrain)
head(banktrain)


# drop columns that we won't use (not part of the 8)
drops <- c("age", "balance", "day", "campaign",
         "pdays", "previous", "month", "duration")

banktrain <- banktrain [,!(names(banktrain) %in% drops)]

head(banktrain) # last column is the response

# ALTERNATIVE just choose the 8 that you want + 1 response 
keeps <- c("job", "marital", "education", "default",
           "housing", "loan", "contact", "poutcome", "subscribed")

banktrain <- banktrain [,(names(banktrain) %in% keeps)]

head(banktrain)


########## OFFSITE QUESTION 2 ##############

n_folds = 10
n = nrow(banktrain)
set.seed(666)

folds = sample(rep(1:n_folds, length.out = n))

# 11 values of cp
cp = 10^(-5:5)

# vector misc_cp to record the rate of average misclassification for each cp
misc_cp = numeric(length(cp)) 

# for each index value of cp
for(i in 1:length(cp)) {
  
  # vector to store value of misclassification for each fold
  misc_fold = numeric(length(n_folds))
  
  # for each fold 
  for (j in 1:n_folds) { 

    test_idx = which(folds == j) 
    train = banktrain[-test_idx,]
    test = banktrain[test_idx, ]
    
    # decision tree
    fit = rpart(subscribed ~ job + marital + education + default + housing + loan + contact + poutcome,
                method = "class",
                data = train,
                control = rpart.control(cp = cp[i]), # for each i in cp
                parms = list(split='information'))
    
    # predict the response for test set based on fitted tree 
    pred = predict(fit, test[,1:8], type='class')
    
    # add misclassification rate to misc_fold vector
    misc_fold[j] = mean(pred != test[,9])
  }
  
  # for each cp value, get mean misclassification rate
  misc_cp[i] = mean(misc_fold)
  
}

plot(cp, misc_cp, type='b') # weird plot
plot(log(cp, base = 10), misc_cp, type='b') # log it

"
From the plot, we could observe that cp = 0.01 (10^-2) is a reasonable 
choice with low mis-classification rate.

High cp → only large improvements allowed → small/simple tree
Low cp → even small improvements allowed → larger/deeper tree
"

best.cp = 0.01

fit = rpart(subscribed ~ job + marital + education + default + housing + loan + contact + poutcome,
             method = "class",
             data = banktrain,
             control = rpart.control(cp = best.cp),
             parms = list(split='information'))

rpart.plot(fit, type=4, extra=2, clip.right.labs=FALSE, varlen=0)
