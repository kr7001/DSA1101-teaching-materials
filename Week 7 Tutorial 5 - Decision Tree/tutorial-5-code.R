setwd("~/My Drive (krlee7001@gmail.com)/NUS (Acads Related)/AY2526 Sem 2/DSA1101 TA")


########## OFFSITE QUESTION 1a ##############

### Q: Read and explore the data from the file German_credit.csv

readLines("data/German_credit.csv", n = 3)

credit = read.csv("data/German_credit.csv")


# 1. Inspect Data

head(credit) # Note: Creditability here is our response variable

str(credit) # Inspect which column may be categorical

# 2. Further inspect if some columns are categorical 
table(credit$Creditability)

# Extra: Do you think this is categorical too? Is it categorical or nominal?
table(credit$Instalment) 

# 3. Transform categorical columns into factors 
credit$Creditability = as.factor(credit$Creditability)


########## OFFSITE QUESTION 1b ##############

### Q: Standardize the input features

credit[,2:5] = scale(credit[,2:5])

head(credit)


########## OFFSITE QUESTION 1c ##############

### Q: Randomly select 800 customer records to form the training data, 
###    and the remaining 200 records will be the test data.

set.seed(1)

n = nrow(credit)

train = sample(1:n, 800) # random sample 800 indexes in 1:1000

# rmb: df[row, col]! 
# rmb: -train means everything excluding indexes in train
train.X = credit[train, 2:5]
train.Y = credit[train, 1]
test.X = credit[-train, 2:5]
test.Y = credit[-train, 1]

########## OFFSITE QUESTION 1d ##############

### Q: Use 1NN classifier for the training data to predict 
###    if a loan applicant is credible for the 200 test points. 
###    Compute the accuracy of the classifier.

library(class)

knn_pred = knn(train.X, test.X, train.Y, k = 1)

conf_mat = table(knn_pred, test.Y)

conf_mat # View confusion matrix

# Method 1: accuracy
acc = (conf_mat[1,1] + conf_mat[2,2]) / sum(conf_mat)

# Method 2: accuracy 
acc = sum(diag(conf_mat)) / sum(conf_mat)

acc # 0.58 for me (if set different seed, then diff)


########## OFFSITE QUESTION 1e ##############

### Q: Use N-folds CV with N = 5 to find the average accuracy for the 
###    1-nearest neighbor classifier.

n_folds = 5
n = nrow(credit)

set.seed(1)
folds = sample(rep(1:n_folds, length.out = n))

table(folds)

acc_fold = numeric(n_folds) # store accuracies for each fold

for (i in 1:n_folds) {
  pred = knn(credit[folds!=i,2:5], # train X
             credit[folds==i,2:5], # test X
             credit[folds!=i,1], # train Y
             k = 1)
  
  conf_mat = table(pred, credit[folds==i, 1])
  
  acc_fold[i] = sum(diag(conf_mat)) / sum(conf_mat)
    
  # or acc_fold[i] = mean(credit[folds==i, 1] == pred)
  
}

acc_fold # 0.660 0.590 0.630 0.660 0.645

mean(acc_fold) # 0.637


########## OFFSITE QUESTION 1f ##############

K = 100 

acc_K = numeric(K)
acc_fold = numeric(n_folds)

for (i in 1:K) {
  for (j in 1:n_folds) {
    
    pred = knn(credit[folds!=j,2:5], # train X
               credit[folds==j,2:5], # test X
               credit[folds!=j,1], # train Y
               k = i)
    
    acc_fold[j] = mean(credit[folds==j, 1] == pred)
  }
  acc_K[i] = mean(acc_fold)
}

acc_K # accuracy for all K


########## OFFSITE QUESTION 1g ##############

# can plot to view how accuracy changes as we K increases
plot(1:K, acc_K) 

max(acc_K) # 0.714 is the highest accuracy 

best_index = which(acc_K == max(acc_K)) 
# k = 22 is the k for highest accuracy

abline(v = best_index, col = "red")


########## OFFSITE QUESTION 2 PREP ##############

readLines("data/iris.csv", n = 3)

iris = read.csv("data/iris.csv")

head(iris)


########## OFFSITE QUESTION 2a ##############

### Q: Use decision tree to predict species based on features

#install.packages("rpart")
library(rpart)

iris_fit = rpart(
  class ~ sepal.length + sepal.width + petal.length + petal.width, # or class ~ . (all)
  method = "class", # categorical
  data = iris,
  control = rpart.control(minsplit = 1),
  parms = list(split = 'information')
)


########## OFFSITE QUESTION 2b ##############

### Q: Visualize the decision tree

#install.packages("rpart.plot")
library(rpart.plot)

rpart.plot(x = iris_fit)

rpart.plot(
  x = iris_fit,
  type = 4,
  extra = 2, 
  clip.right.labs = FALSE,
  varlen = 0,
  faclen = 0
)


########## OFFSITE QUESTION 2c ##############

### Q: What are the more important features in the fitted tree above?

"
sepal length and sepal width is not as important while
petal length and petal width is more important.
most important feature is petal length

RMB: in this course  we don't use iris_fit$variable.importance to determine 
important features. We simply choose the feature closest to root node
"