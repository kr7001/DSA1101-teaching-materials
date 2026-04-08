setwd("~/My Drive (krlee7001@gmail.com)/NUS (Acads Related)/AY2526 Sem 2/DSA1101 TA")

########## IMPORTS ##############

# Can copy this into your script before exam!

library(rpart)
library(rpart.plot)
library(class)
library(ROCR)
library(e1071)

########## DATA PREP AND INSPECT ##############

readLines("Data/diabetes-dataset-1k.csv", n = 3)

data = read.csv("Data/diabetes-dataset-1k.csv")

head(data)
str(data)
dim(data) # 1000 rows 7 columns

# Convert categorical data to factor
data$hypertension = as.factor(data$hypertension)
data$heart_disease = as.factor(data$heart_disease)
data$diabetes = as.factor(data$diabetes)

########## QUESTION 1 ##############

set.seed(1101)

index = sample(1:1000) 
train.set = data[index[1:800],] 
test.set = data[-index[1:800],]

"
# Alternatively:
train_index = sample(1:1000, 800)
train.set = data[train_index,]
test.set = data[train_index,]
"


########## QUESTION 2 ##############

m = 1:50

acc_dt = numeric(length(m))
precision_dt = numeric(length(m))

for (i in m) {
  
  # 1. Call Decision Tree Model
  model_dt = rpart(diabetes ~ ., 
                   data = train.set,
                   method = "class",
                   parms = list(split='information'),
                   control = rpart.control(minsplit = i))
  
  # 2. Predict test set using model
  "Note: here the column 'diabetes' is also included 
  in test.set. But rpart will only use columns that were 
  used as predictors during training, so 'diabetes' is 
  safely ignored here. Be careful with other models 
  (e.g. knn) as this is not always the case!"
  
  pred_DT = predict(model_dt, test.set, type = "class")
  
  # 3. Create confusion matrix
  conf_DT = table(pred_DT, test.set$diabetes)
  
  # 4. Obtain precision and accuracy from Confusion Matrix
  precision_dt[i] = conf_DT[2, 2] / (conf_DT[2, 2] + conf_DT[2, 1])
  acc_dt[i] = sum(diag(conf_DT))/sum(conf_DT)
}

precision_dt
acc_dt

# Plot precision & accuracy against m
plot(m, acc_dt, col = "steelblue", type = "b",
     ylim = c(0.6, 1), ylab = "Value", xlab = "minsplit",
     main = "Accuracy and Precision vs minsplit")

lines(m, precision_dt, col = "orange", type = "b")

legend("bottomright", legend = c("Accuracy", "Precision"),
       col = c("steelblue", "orange"), lty = 1, pch = 1)


"
Q: can you do this? 
plot(m, acc_dt, col='steelblue')
plot(m, precision_dt, add = TRUE, col = 'orange')




### ANS: no, this is only for ROCR objects 
(ROCR own plot() function)
"


# Q: Which minsplit value would you pick?
cbind(m, precision_dt, acc_dt)
# Any value between 13 and 46 is pretty good


########## QUESTION 3 ##############

DT = rpart(diabetes ~ ., 
            data = train.set,
            method = "class",
            parms = list(split='information'),
            control = rpart.control(minsplit = 50))

rpart.plot(DT, type=4, extra=2)

# Predicted probabilities for class 1 (diabetes = 1)
# rmb to extract positive probability!
pred_dt = predict(DT, test.set, type = "prob")[,2]

# Plot ROC curve for DT (in black)
pred_obj_dt = prediction(pred_dt, test.set$diabetes)

perf_dt = performance(pred_obj_dt, 
                      measure = "tpr", 
                      x.measure = "fpr")

plot(perf_dt, col = "black", main = "ROC Curve of DT")

# Derive and report AUC value
auc_dt = performance(pred_obj_dt, measure = "auc")@y.values[[1]]
auc_dt # 0.9753188


########## QUESTION 4 ##############

NB = naiveBayes(diabetes ~ ., data = train.set)

"
For continuous features, Naive Bayes assumes a 
Gaussian (normal) distribution. It estimates the 
mean and sd of each feature for each class 
(diabetes = 0 and 1) from the training data, and uses 
these to compute probabilities during prediction.

But honestly idt you need to worry about this too much
"

########## QUESTION 5 ##############

# rmb for NB it's type = "raw"
pred_nb = predict(NB, test.set, type = "raw")[,2]

pred_nb_obj = prediction(pred_nb, test.set$diabetes)

perf_nb = performance(pred_nb_obj, 
                      measure="tpr", 
                      x.measure="fpr")

plot(perf_nb, col = "blue", main = "ROC curve for NB")

# Derive and report AUC value
auc_nb = performance(pred_nb_obj, measure = "auc")@y.values[[1]]
auc_nb # 0.9638009


########## QUESTION 6 ##############

NB_tpr = performance(pred_nb_obj, measure = "tpr")
NB_fpr = performance(pred_nb_obj, measure = "fpr")

# Both share the same cutoff vector
thresholds = NB_tpr@x.values[[1]] # x.values will be the threshold!
tpr_vals = NB_tpr@y.values[[1]]
fpr_vals = NB_fpr@y.values[[1]]

"
fun fact: the number of threshold is the number of 
unique predicted probabilities + 1 in your test set
"

plot(thresholds, tpr_vals,
     type = "l", col = "blue", lwd = 2,
     xlab = "threshold", ylab = "Rate",
     main = "TPR and FPR vs Threshold")

lines(thresholds, fpr_vals, col = "red", lwd = 2)

legend("topright",
       legend = c("TPR", "FPR"),
       col = c("blue", "red"),
       lwd = 2)

# In the official answer, another alternative way 
# to do this is shown. Both are fine!


########## QUESTION 7 ##############

"
TPR = TP / (TP + FP)
FPR = FP / (FP + TN)

So ideally, we want to find a threshold that maximizes TPR 
and minimizes FPR. 

From the plot above, and the table from cbind(delta, tpr, fpr), 
one might choose a threshold such as 0.106 that gives high 
TPR = 0.923 yet quite low FPR = 0.123.
"

cbind(thresholds, tpr_vals, fpr_vals)[34:45,]


########## QUESTION 8 ##############

delta_chosen = 0.106

pred_nb = predict(NB, test.set, type = "raw")[,2]

pred_nb_threshold = ifelse(pred_nb > delta_chosen, "1", "0")

acc_nb = mean(pred_nb_threshold == test.set$diabetes)
acc_nb # 0.875


########## QUESTION 9 ##############

# diabetes column is already "0" and "1"!
LR = glm(diabetes ~ ., data = train.set, family = binomial)


########## QUESTION 10 ##############

summary(LR)

"
Given that both people have the same values for the 
other features, having hypertension (hypertension = 1) 
multiples the odds of having diabetes by e^(0.885) = 2.423 
compared to a person without hypertension (hypertension = 0)

odds_w_hypertension = odds_wo_hypertension * 2.423

so the odd ratio is simply:
odds_w_hypertension / odds_wo_hypertension = 2.423
"


########## QUESTION 11 ##############

pred_LR = predict(LR, newdata = test.set, type = "response")

pred_obj_LR = prediction(pred_LR, test.set$diabetes)

perf_LR = performance(pred_obj_LR, 
                      measure = "tpr", 
                      x.measure = "fpr")

plot(perf_LR, col = "red", main = "ROC curve of LR")

auc_LR = performance(pred_obj_LR, "auc")@y.values[[1]]
auc_LR # 0.9744961


########## QUESTION 12 ##############

head(train.set) # view train set 

# for this course, you can forget abt the using train set's
# mean and std to standardize test 
train.set[,c(1,4,5,6)] = scale(train.set[,c(1,4,5,6)])
test.set[,c(1,4,5,6)] = scale(test.set[,c(1,4,5,6)])

# confirm changes
head(train.set)
head(test.set)

# this question is a lil ambigious abt this but it wants 
# you to fit the knn model with only quantitative inputs:

# build KNN model 
pred_knn = knn(train.set[,c(1,4,5,6)], 
               test.set[,c(1,4,5,6)], 
               train.set$diabetes, 
               k = 3, 
               prob=TRUE) # we want the winning probability


########## QUESTION 13 ##############

# If the predicted class is 1, winning_prob is P(diabetes=1).
# If the predicted class is 0, P(diabetes=1) = 1 - winning_prob.

# Extract winning probability using attr
winning_prob = attr(pred_knn, "prob")

prob_diabetes_knn = ifelse(pred_knn == 1, 
                       winning_prob, 
                       1 - winning_prob)

prob_diabetes_knn



########## QUESTION 14 ##############

# typo in this question, should be 3NN instead of 5NN

pred_knn_2 = ifelse(prob_diabetes_knn > delta_chosen, 1, 0)

mean(pred_knn_2 == test.set$diabetes) # 0.89
