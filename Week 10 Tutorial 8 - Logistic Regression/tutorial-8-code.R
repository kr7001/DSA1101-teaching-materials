setwd("~/My Drive (krlee7001@gmail.com)/NUS (Acads Related)/AY2526 Sem 2/DSA1101 TA")

########## ONSITE QUESTION PREP ##############

readLines("Data/Smarket.csv", n = 3)

market = read.csv("Data/Smarket.csv")

head(market)
str(market)

# Logistic Regression: convert response to 0 and 1
market$y = ifelse(market$Direction == "Up", 1, 0)

ifelse(market$Direction == "Down", 0, 1)

########## ONSITE QUESTION 1 ##############

prop.table(table(market$Direction)) 
# 51.84% of the days that direction is up


########## ONSITE QUESTION 2 ##############

M3 <- glm(y ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
          data = market, 
          family = binomial)

summary(M3)


########## ONSITE QUESTION 3 ##############

# Step 1: Probability of y = 1 (Direction = Up) for each day
pred.M3 = predict(M3, newdata = market[,3:8], type = "response")
pred.M3[1:3] # Preview the probabilities

# Step 2: Probability >= 0.5184 then that day is predicted as ‘Up’, else 'Down'
pred.direction = ifelse(pred.M3 >= 0.5184, "Up", "Down")

table(pred.direction) # 619 Up, 631 down

# Alternatively:
pred.direction = ifelse(pred.M3 >= 0.5184, 1, 0)

# Step 3: Calculate accuracy
accuracy = mean(pred.direction == market$Direction)
accuracy # 0.5352

"Note: If you used 0, 1 in step 2, 
then you should compare with market$y instead
"

# Alternatively: using confusion matrix to get accuracy
conf.mat = table(market$Direction, pred.direction)

accuracy = sum(diag(conf.mat))/sum(conf.mat)
accuracy # 0.5352


########## ONSITE QUESTION 4 ##############

library(ROCR)

roc.obj = prediction(pred.M3, market$y) # rmb pred.M3 is alr prob of y=1!

roc = performance(roc.obj, measure = 'tpr', x.measure = 'fpr')

plot(roc, col = 'pink')

auc = performance(roc.obj, "auc")@y.values[[1]]
auc # 0.5387 (not much more ideal than random...)


########## OFFSITE QUESTION PREP ##############

titanic = read.csv("Data/Titanic.csv")

head(titanic)


########## OFFSITE QUESTION 1 ##############

### Q: Perform logistic regression using all the 
###    feature variables to predict the survival status, 
###    called model M2

# Convert response variable to 0 and 1
titanic$y = ifelse(titanic$Survived == "Yes", 1, 0)

M2 = glm(y ~ Class + Sex + Age,
         data = titanic, 
         family = binomial)


########## OFFSITE QUESTION 2 ##############

### Q: Write down the fitted equation of model M2.

summary(M2)

"
log_odds <- 2.0438 +
            (-1.0181) * I(Class == 2nd) +
            (-1.7778) * I(Class == 3rd) +
            (-0.8577) * I(Class == Crew) +
            (-2.4201) * I(Sex == Male) +
            (1.0615)  * I(Age == Child)
"


########## OFFSITE QUESTION 3 ##############

### Q: Interpret the coefficient of the variable 
###    ‘Sex’ in M2

"
SexMale: -2.4201

Fixing other variables, being male DECREASES 
the LOG ODDS OF SURVIVING by 2.4201 compared to 
being female.

Fixing other variables, being male multiplies 
the ODDS of surviving by e^(-2.4201) = 0.0889 
compared to being female

Alternatively you can say:
Fixing other variables, being FEMALE multiplies 
the ODDS of surviving by e^(2.4201) = 11.25 compared 
to being MALE
"


########## OFFSITE QUESTION 4 ##############

### Q: Interpret the coefficient of the variable 
###    ‘Age’ in M2

"
AgeChild: 1.0615

Fixing other variables, being a child INCREASES 
the LOG ODDS OF SURVIVING by 1.0615 compared to being 
an adult.

Fixing other variables, being a child multiplies 
the ODDS of surviving by e^(1.0615) = 2.89 compared 
to being an adult
"


########## OFFSITE QUESTION 5 ##############

### Q: Obtain and compare the ROC curves and 
#      AUC for the two classifiers: naive Bayes
#      and logistic regression.

### ROC for Logistic Regression
pred_log = predict(M2, titanic[,1:3], type = "response") 

roc_log_obj = prediction(pred_log, titanic$Survived)

roc_log = performance(roc_log_obj, measure="tpr", x.measure="fpr")

plot(roc_log, col = "red")

### ROC for Naive Bayes
library('e1071')

M1 = naiveBayes(y ~ Class + Sex + Age, data = titanic)

pred.M1 = predict(M1, titanic[,1:3], type = 'raw')

pred.M1 = pred.M1[, 2] # rmb for NB need to extract positive prob column

roc_nb_obj = prediction(pred.M1, titanic$y)

roc_nb = performance(roc_nb_obj, measure="tpr", x.measure="fpr")

plot(roc_log, col = "red")
plot(roc_nb, add = TRUE, col = "blue")

legend("bottomright", 
       c("logistic regression" , "naive Bayes"),
       col=c("red", "blue"), 
       lty=1)

### AUC
auc_log = performance(roc_log_obj , measure ="auc")@y.values[[1]]
auc_log # 0.760

auc_nb <- performance(roc_nb_obj, measure = "auc")@y.values[[1]]
auc_nb # 0.716

"
From the ROC curves and AUS values, it suggests that 
logistic regression is slightly better than Naive Bayes
in predicting the survival status for this data set.
"
