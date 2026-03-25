setwd("~/My Drive (krlee7001@gmail.com)/NUS (Acads Related)/AY2526 Sem 2/DSA1101 TA")

########## QUICK REVISION ####################

"
# Set seed to make result reproducible
set.seed(1505)

# Eg. Split a total of N data points into: M points for train, K points for test

# randomly pick K indices to be the test set
test = sample(1:N, K)

train.X = X[-test,] # ALL rows EXCEPT test indices → training features
test.X = X[test,]  # ONLY test indices → test features
train.Y = Y[-test] # ALL labels EXCEPT test indices → training labels
test.Y = Y[test] # ONLY test indices → test labels
"

########## ONSITE QUESTION CODE (OPTIONAL) ##############

### STEP 1: Set up Data Frame
X1 <- c(0, 2, 0, 0, -1, 1)
X2 <- c(3, 0, 1, 1, 0, 1)
X3 <- c(0, 0, 3, 2, 1, 1)
Y <- c("Red", "Red", "Red", "Green", "Green", "Red")

df <- data.frame(X1, X2, X3, Y)
df

### STEP 2: Get Euclidean Distance
df$distance <- sqrt(
  df$X1^2 + df$X2^2 + df$X3^2
)

### STEP 3: Order Data Frame based on Euclidean Distance
df <- df[order(df$distance),]
df

### STEP 4: Class Label for K = 1
df[1,"Y"] # Green

### STEP 5: Class Label for K = 3
max(df[1:3, "Y"]) # Red

########## OFFSITE QUESTION PREP AND LOAD ##############

readLines("Data/crab.csv", n = 3)

crab <- read.csv("Data/crab.csv")

head(crab)
str(crab)

crab$spine = as.factor(crab$spine)
crab$color = as.factor(crab$color)

attach(crab)

########## OFFSITE QUESTION 1a ##############

### Q: Scatter plot of weight against width for different condition of spine.

# Method 1 (only works cause spine happens to be 1,2,3)
cols <- c("black", "lightblue", "pink")
plot(width, weight, col = cols[spine], pch = 20)
legend("topleft", legend = c("Spine = 1", "Spine = 2", "Spine = 3"),
       col = cols, pch = 20)

# Method 2
plot(width, weight, type = "n")
points(width[spine==1], weight[spine==1], pch = 20, col = "black")
points(width[spine==2], weight[spine==2], pch = 20, col = "lightblue")
points(width[spine==3], weight[spine==3], pch = 20, col= "pink")
legend("topleft", legend = c("Spine = 1", "Spine = 2", "Spine = 3"),
       col = c("black", "lightblue", "pink"), pch = c(20, 20, 20))


########## OFFSITE QUESTION 1b ##############

### Q: Linear regression model for weight which has two explanatories, 
###    width and spine

M1 <- lm(weight ~ width + spine, data = crab)


########## OFFSITE QUESTION 1c ##############

### Q: Is the fitted model significant?

summary(M1)

"
Yes model M1 is significant, it has a extremely small p-value < 2.2e-16
"

########## OFFSITE QUESTION 1d ##############

### Q: Derive R^2 and adjusted R^2 of the fitted model.

names(summary(M1))

summary(M1)$r.squared # 0.7917598
summary(M1)$adj.r.squared # 0.7880632


########## OFFSITE QUESTION 1e ##############

### Q: Write down the fitted model.

summary(M1)$coefficients

"
weight_hat = -3.93 + (0.244 x width) + (0.0554 x I(spine = 2)) 
             − (0.0697 x I(spine = 3))
"


########## OFFSITE QUESTION 1f ##############

### Q: Two female crabs of the same width, find the difference of their weight 
###    if one has good spine condition (spine=1) and another one with 
###    broken spines (spine=3)

"
CRAB WITH GOOD SPINE (SPINE = 1):
weight_hat = -3.93 + (0.244 x width)

CRAB WITH BROKEN SPINE (SPINE = 3):
weight_hat = -3.93 + (0.244 x width) - 0.0697

When they have the same width, then on average the one that has spines 
of good condition (spine = 1) is heavier than the one with broken spines 
(spine = 3) by 0.0697kg.
"

########## OFFSITE QUESTION 1g ##############

### Q: Predict the weight of a female crab that has width of 
###    27 cm and has both spines worn or broken.

new = data.frame(width = 27, spine = "3")
predict(M1, new) # 2.582352


########## OFFSITE QUESTION 2 PREP ##############

data <- data.frame(
  Yi = c(1, 1, 0, 1, 1, 0, 0, 1, 0, 0),
  Yi_hat = c(0.9, 0.5, 0.7, 0.4, 0.5, 0.2, 0.7, 0.9, 0.1, 0.1)
)
data 


########## OFFSITE QUESTION 2A ##############

### Function to get TPR and FPR given a threshold

get_rates <- function(data, threshold) {
  
  # 1. If larger than threshold then 1, else 0
  pred <- ifelse(data$Yi_hat > threshold, 1, 0)
  print(pred)
  
  # 2. Create Confusion Matrix for predictions made using the threshold
  conf_mat <- table(actual = data$Yi, pred = pred)
  print(conf_mat)
  
  # 3. Calculate TPR and FPR using confusion matrix
  TPR <- conf_mat[2, 2] / sum(conf_mat[2, ])
  FPR <- conf_mat[1, 2] / sum(conf_mat[1, ])
  print(c(TPR, FPR))
  
  return(c(TPR = TPR, FPR = FPR))
}

# Get rates for each threshold
rates_0.3 <- get_rates(data, 0.3)
rates_0.6 <- get_rates(data, 0.6)
rates_0.8 <- get_rates(data, 0.8)

#### Plot ####

# 1. Start with blank plot
plot(NULL, type = "n", 
     xlim = c(0, 1), ylim = c(0, 1), 
     xlab = "FPR", ylab = "TPR")

# 2. Add FPR and TPR points for different threshold
points(rates_0.3["FPR"], rates_0.3["TPR"], pch = 16, col = "blue")
points(rates_0.6["FPR"], rates_0.6["TPR"], pch = 17, col = "red")
points(rates_0.8["FPR"], rates_0.8["TPR"], pch = 18, col = "black")

# 3. Add legend
legend("bottomright",
       legend = c("Threshold = 0.3", "Threshold = 0.6", "Threshold = 0.8"),
       col = c("blue", "red", "black"), pch = c(16, 17, 18))


########## OFFSITE QUESTION 2B ##############

### Q: Can we add the two points (0, 0) and (1, 1) to the
###    ROC plot in part (a). Explain why or why not.

"
TPR = TP / (TP + FN) 
FPR = FP / (FP + TN) 

(0,0) means TPR = FPR = 0 (you never predict positive for anything).
Hence no true positives or no false positives.

(1,1) means TPR = FPR = 1 (you predict positive for everything).
Hence you catch all true positives, but flag all negatives to positives.

For this question:
If threshold > 0.9, then all points predicted as negative (TPR = FPR = 0)
If threshold < 0.1, then all points predicted as positive (TPR = FPR = 1)

Since there exist σ within the range from 0 to 1 for the two points to happen, 
these two points can be added to the plot.
"


########## OFFSITE QUESTION 3a ##############

### Q:  The company tries to sell insurance to a random selection of customers,
###     what is the success rate?

readLines("Data/Caravan.csv", n = 3)
caravan <- read.csv("Data/Caravan.csv")
head(caravan)

table(caravan$Purchase)[2]/sum(table(caravan$Purchase)) # 0.05977327

# alternatively
table(caravan$Purchase)[2]/length(caravan$Purchase) # 0.05977327


"
data set shows around 6% of people purchase insurance
"


########## OFFSITE QUESTION 3b ##############

### Q: Standardize the input features

# remove the first column X since it provides no information
caravan = caravan[,-1]

# scaling all the data set, except the last column
standardized.X = scale(caravan[,-86]) 

head(standardized.X)


########## OFFSITE QUESTION 3c ##############

### Q: Split 2000 data points to test set, remaining is train set

n = dim(caravan)[1] # sample size = 5822

set.seed(5)

test = sample(1:n, 2000) # sample a random set of 2000 indexes, from 1:n

train.X = standardized.X[-test ,] # training set
test.X = standardized.X[test ,] # test set
train.Y = caravan$Purchase[-test] # response for training set
test.Y = caravan$Purchase[test] # response for test set


########## OFFSITE QUESTION 3c (split then scale) ##############

# Split first
set.seed(5)
n = dim(caravan)[1]
test = sample(1:n, 2000)
train.X = caravan[-test, -c(1, 87)]  # Remove first and last column
test.X = caravan[test, -c(1, 87)]
train.Y = caravan$Purchase[-test]
test.Y = caravan$Purchase[test]

# Scale using training set parameters only
train.X.scaled = scale(train.X)

# Apply training set's mean and SD to test set
test.X.scaled = scale(test.X, 
                      center = attr(train.X.scaled, "scaled:center"),
                      scale = attr(train.X.scaled, "scaled:scale"))


########## OFFSITE QUESTION 3d ##############

### Q: Use 1-NN to predict whether a customer will purchase insurance

library(class)

knn.pred = knn(train.X, test.X, train.Y, k=1) 

confusion.matrix = table(test.Y, knn.pred)
confusion.matrix

precision = confusion.matrix[2,2] / sum(confusion.matrix[,2])
precision


########## OFFSITE QUESTION 3e ##############

### Q: Use 3-NN and 5-NN to do same thing as 3d. Which k has better precision?

### k = 3
knn.pred = knn(train.X, test.X, train.Y, k=3) 

confusion.matrix = table(test.Y, knn.pred)
confusion.matrix

precision = confusion.matrix[2,2] / sum(confusion.matrix[,2])
precision

### k = 5

knn.pred = knn(train.X, test.X, train.Y, k=5) 

confusion.matrix = table(test.Y, knn.pred)
confusion.matrix

precision = confusion.matrix[2,2] / sum(confusion.matrix[,2])
precision

# K = 3 gives the best precision