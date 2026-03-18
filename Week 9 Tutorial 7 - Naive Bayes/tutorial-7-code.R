setwd("~/My Drive (krlee7001@gmail.com)/NUS (Acads Related)/AY2526 Sem 2/DSA1101 TA")

########## ONSITE QUESTION PREP ##############

readLines("Data/Titanic.csv", n=3)

titanic = read.csv("Data/Titanic.csv")

head(titanic)
str(titanic)



########## ONSITE QUESTION 1 ##############

surv.prop = prop.table(table(titanic$Survived))
surv.prop

"
P(Y=1) = 0.323. P(Y=0) = 0.677
"



########## ONSITE QUESTION 2 ##############

class.prop = prop.table(table(titanic[,c("Survived", "Class")]), margin = 1)
class.prop

"
P(Class = 1st  | Survived = No)  = 0.08187919
P(Class = 2nd  | Survived = No)  = 0.11208054
P(Class = 3rd  | Survived = No)  = 0.35436242
P(Class = Crew | Survived = No)  = 0.45167785

P(Class = 1st  | Survived = Yes) = 0.28551336
P(Class = 2nd  | Survived = Yes) = 0.16596343
P(Class = 3rd  | Survived = Yes) = 0.25035162
P(Class = Crew | Survived = Yes) = 0.29817159
"

sex.prop = prop.table(table(titanic[,c("Survived", "Sex")]), margin = 1)
sex.prop

"
P(Sex = Female | Survived = No)  = 0.08456376
P(Sex = Male   | Survived = No)  = 0.91543624

P(Sex = Female | Survived = Yes) = 0.48382560
P(Sex = Male   | Survived = Yes) = 0.51617440
"


age.prop = prop.table(table(titanic[,c("Survived", "Age")]), margin = 1)
age.prop

"
P(Age = Adult | Survived = No)  = 0.96510067
P(Age = Child | Survived = No)  = 0.03489933

P(Age = Adult | Survived = Yes) = 0.91983122
P(Age = Child | Survived = Yes) = 0.08016878
"



########## ONSITE QUESTION 3 ##############

prob_surv <- surv.prop["Yes"] * (class.prop["Yes", "2nd"] * 
                                 sex.prop["Yes", "Female"] * 
                                 age.prop["Yes", "Adult"]) # 0.02385937


prob_no_surv <- surv.prop["No"] * (class.prop["No", "2nd"] * 
                                   sex.prop["No", "Female"] * 
                                   age.prop["No", "Adult"]) # 0.006192319 

prob_surv / prob_no_surv # Ratio: 3.853059 



########## ONSITE QUESTION 4 ##############

# install.packages("e1071")
library(e1071)

M_NB <- naiveBayes(Survived ~ Class + Sex + Age, data = titanic)

test <- data.frame(Class = "2nd", Sex = "Female", Age = "Adult")

pred_class <- predict(M_NB, test)
pred_class # Yes 

pred_prob <- predict(M_NB, test, "raw")
pred_prob 

"
This is same ratio as what we manually calculated above 
(0.02385937, 0.006192319)! It's different because the package 
normalized the values.

       No       Yes
0.2060556 0.7939444

Normalized process (just for your reference):
0.02385937 / (0.02385937 + 0.006192319)  = 0.7939444  
0.006192319 / (0.02385937 + 0.006192319) = 0.2060556
"


# Regardless of the normalization. Both should give the same ratio:

# From e1071 package
pred_prob[1, "Yes"] / pred_prob[1, "No"] # 3.853059

# From our manual calculation
prob_surv / prob_no_surv # 3.853059



########## OFFSITE QUESTION 1 ##############

library(rpart)
library(rpart.plot)

M2 <- rpart(Survived ~ .,
            data = titanic,
            method = "class",
            parms = list(split = "information"),
            control = rpart.control(minsplit = 1))


########## OFFSITE QUESTION 2 ##############

rpart.plot(M2, type = 4, extra = 2)


########## OFFSITE QUESTION 3 ##############

# install.packages("ROCR")

library(ROCR)

##### ROC Curve For Naive Bayes Classifier #####

pred.NB = predict(M_NB, titanic[,1:3], type='raw')
pred.NB[1:3,] # View the probability of prediction

score = pred.NB[, 2] # only take probability of "Yes" in second column

pred_nb = prediction(score, titanic$Survived) # rmb prediction diff from predict!

roc_nb = performance(pred_nb, measure="tpr", x.measure="fpr")

plot(roc_nb, col = "red")


##### ROC Curve For Decision Tree #####

pred.M2 = predict(M2, titanic[,1:3], type = 'prob')

score2 = pred.M2[,2] # only take probability of "Yes" in second column

pred_dt = prediction(score2, titanic$Survived) # rmb prediction diff from predict!

roc_dt = performance(pred_dt, measure="tpr", x.measure="fpr")

plot(roc_nb, col = "red")
plot(roc_dt, add = TRUE, col = "blue")

legend("bottomright", 
       c("Naive Bayes","Decision Trees"), 
       col=c("red","blue"), 
       lty=1)


##### AUC Values #####

# NAIVE BAYES:
auc1 <- performance(pred_nb, measure="auc")@y.values[[1]]
auc1 # 0.7164944

# DECISION TREE:
auc2 = performance(pred_dt, measure="auc")@y.values[[1]]
auc2 # 0.7262628. Very close!

