setwd("~/My Drive (krlee7001@gmail.com)/NUS (Acads Related)/AY2526 Sem 2/DSA1101 TA")

################### QUICK REVISION #############################

fev_data <- read.csv("Data/fev.csv")

######## wrong example ############
model <- lm(fev_data$FEV ~ fev_data$height)
new_data <- data.frame(height = 1.7)
predict(model, newdata = new_data)

######## correct example ############
model <- lm(FEV ~ height, data = fev_data)
new_data <- data.frame(height = 1.7)
predict(model, newdata = new_data)

summary(model)

summary(model)$coef

######## plotting fev and height ############

female <- fev_data[(fev_data$Sex==0),]
male <- fev_data[(fev_data$Sex==1),]

plot(fev_data$height, fev_data$FEV, type = "n") 
points(female$height, female$FEV, col = "red", pch = 20)  
points(male$height, male$FEV, col = "darkblue", pch = 20)

legend(x = 1.3, 
       y= 5.5, 
       legend = c("Female", "Male"),
       col = c("red","darkblue"), 
       pch=c(20,20))

title(main = "Scatterplot of FEV against height")

abline(model, lwd = 3)

######## predicting multiple new inputs ############

model <- lm(FEV ~ height, data = fev_data)
new_data <- data.frame(height = c(1.6, 1.56, 1.7))
predict(model, newdata = new_data)


####### MLR with categorical regressor ############

fev_data$Sex = as.factor(fev_data$Sex)

model_cat <- lm(FEV ~ height + Sex, data = fev_data)

summary(model_cat)


################### ONSITE QUESTION LOAD AND PREPARE #############################

# Import data into R

readLines("Data/house_selling_prices_FL.csv", n=3)

house = read.csv("Data/house_selling_prices_FL.csv")

# Examine data 

names(house) # columns
dim(house) # 100 9
head(house)
str(house) # Data types

# Declare data type of categorical variables BEFORE attach()!

house$NW <- as.factor(house$NW)
house$Quadrant <- as.factor(house$Quadrant)

str(house)

# attach data frame

attach(house)


################### ONSITE QUESTION 1 #############################

### Q: Derive the correlation between x (size of house) and y (selling price).

cor(price, size) # 0.76126, quite strong positive correlation 


################### ONSITE QUESTION 2 #############################

### Q: Derive a scatter plot of y against x. 

plot(size, price, pch = 20, col = "steelblue")


### Q: Give your comments on the association of y and x

"
There seems to be a strong positive linear association of x and y.

The variability of y (price) is also quite stable when x (size) changes. (Homoscedasticity)
"

################### ONSITE QUESTION 3 #############################

### Q: Derive R2 of Model 1

# Fit a simple linear regression for y on x (Model 1)

M1 = lm(price ~ size, data = house) 

summary(M1)$r.squared # 0.57952

# Extra: ?summary.lm

### Q: Verify that √R2 = |cor(y, x)|

root_r_square = sqrt(summary(M1)$r.squared) # 0.7612621

abs_cor = abs(cor(price, size)) # 0.7612621

all.equal(root_r_square, abs_cor) # TRUE


### Q: In which situation we can have √R2 = cor(y, x)

"
When cor(y, x) >= 0 then in a simple model y ∼ x, we always have √R2 = cor(y, x).
"


################### ONSITE QUESTION 4 #############################

### Q: Form a model (called Model 2) which has two regressors (x and NW). 

M2 <- lm(price ~ size + NW, data = house)

### Q: Report the coefficient of variable NW in Model 2. 

summary(M2)$coefficients 

# Coef for NW: 30569.1
# y = −15257.5 + 77.99x + 30569.1 × I(NW = 1)

### Q: Interpret it.  

"
`For two houses of the same size (fixed x), the house in the more desirable part 
(NW = 1) is $30569.1 more than the one in the less desirable part (NW = 0).`
"

### Q: (EXTRA) Can I change my reference to be NW=1 instead?

house$NW <- relevel(factor(house$NW), ref = "1")

M3 <- lm(price ~ size + NW, data = house)

summary(M3)$coefficients 

################### ONSITE QUESTION 5 #############################

### Q: Estimate and report the price of a house where its size is 4000 square feet and 
###    is located at the more desirable part of the town.

new = data.frame(size=4000, NW = "1")

predict(M2, newdata = new) # 327252.1

# The mean price of a house with size x = 4000 and NW = 1 is $327252.1

# Before we end, let's detach our data 
detach(house)


################### OFFSITE QUESTION 1a #############################

### Q: Read the data from the file Colleges.txt

readLines("Data/Colleges.txt", n = 3)

?read.table # from here, we noticed that read.delim() suits what we want

colleges <- read.delim("Data/Colleges.txt")

# alternatively
colleges <- read.table("Data/Colleges.txt", header = TRUE, sep = "\t")

# Inspect data

head(colleges)
str(colleges)
dim(colleges)

### Q: Write your own function in R, called simple, to derive the 
### intercept β0 and the slope β1 of Model M1

simple <- function (x,y) {
    
    beta_1 <- (sum(x*y)- sum(y)*mean(x)) / (sum(x**2)- sum(x)*mean(x))
    
    beta_0 <- mean(y) - (beta_1*mean(x))
    
    return(c(beta_0 , beta_1)) 
}


### Derive the intercept β0 and the slope β1 of Model M1 (Acceptance on SAT)

M1_est_coef <- simple(x = colleges$SAT, y = colleges$Acceptance) 

M1_est_int <- M1_est_coef[1] # 202.2677
M1_est_slope <- M1_est_coef[2] # -0.1300894


################### OFFSITE QUESTION 1b #############################

### Q: Use function lm() in R to derive the coefficients of Model M1

M1 <- lm(Acceptance ~ SAT , data = colleges)

M1_int <- M1$coef[[1]] # 202.2677
M1_slope <- M1$coef[[2]] # -0.1300894


### Q: Compare with your answer in part (a)

all.equal(M1_est_int, M1_int) # TRUE
all.equal(M1_est_slope, M1_slope) # TRUE


################### OFFSITE QUESTION 2a #############################

# Because we imported this before, we know header and sep matches default
hdb = read.csv("Data/hdbresale_reg.csv")

### Q: Use function simple you formed in the question above to find the 
###    coefficients of Model M2 (resale price on floor area). 

M2_est_coef <- simple(x = hdb$floor_area_sqm, y = hdb$resale_price) 

M2_est_int <- M2_est_coef[1] # 115145.7
M2_est_slope <- M2_est_coef[2] # 3117.212


################### OFFSITE QUESTION 2b #############################

### Q: Use function lm() in R to derive the coefficients of Model 2

M2 <- lm(resale_price ~ floor_area_sqm, data = hdb)

M2_int <- M2$coef[[1]] # 115145.7
M2_slope <- M2$coef[[2]] # 3117.212

all.equal(M2_est_int, M2_int) # TRUE
all.equal(M2_est_slope, M2_slope) # TRUE


################### OFFSITE QUESTION 3a #############################

### Q: Consider the resale price, plot a histogram of it and give your comments

hist(hdb$resale_price, col = "pink")

"
The histogram has a range of 200,000 to 1 million. 

It is unimodal and right-skewed. Also suspected some upper-tail outliers. 
"

### Q: Is it suitable to fit a linear model for this 
### response variable? Explain.

"
We need to check if column resale_price satisfies: 
(1) Quantitative
(2) Symmetric
(3) Variability is stable when other quantitative regressor(s) change.

Since it is right-skewed as noted by the histogram, resale price is NOT suitable to be the 
response of a linear model. 

For a right skewed variable, it is suggested to try with a transformation by taking log_e.
"


################### OFFSITE QUESTION 3b #############################

### Q: Consider the resale price, plot a histogram of log_e of it and give your comments. 

hist(log(hdb$resale_price), col = "pink")


### Q: Is it more suitable to fit a linear model for this response variable than the original 
###    resale price?

"
The histogram of the log of the resale price is more symmetric -> more suitable than the original
resale price as a response of a linear model. 

We may check to see if the variability of log(resale price) is stable as x (floor area) 
changes by the scatter plot (which leads us to our next question!)
"


################### OFFSITE QUESTION 3c #############################

### Q: Derive a scatter plot of the log_e of the resale price against the 
###    floor area in square meters.

hdb$log_resale_price = log(hdb$resale_price) # create a new column for the log(price)

plot(hdb$log_resale_price ~ hdb$floor_area_sqm, col = "steelblue")


### Q: Give your comments on the scatter plot. 

"
The scatter plot shows a strong, positive and quite linear association between 
log(price) and the floor area. 

The variability of the log(price) seems QUITE stable when the floor area changes.
Note: the variability is not strongly stable (as we can see near the right end of floor area),
but it's can be considered as somewhat stable.

From (b) and (c), it’s quite suitable to fit a linear model for log(price).
"


################### OFFSITE QUESTION 3d #############################

### Q: Fit a linear model where the log of the resale price be the response,
###    and floor area and flat type as regressor. 

M_hdb = lm(log_resale_price ~ floor_area_sqm + flat_type, data = hdb)
summary(M_hdb) 

### Q: Write down the fitted equation of the linear model.

"
log(price_hat) = 12.35 + 0.003712 ∗ X + 
                 0.119 ∗ I(flat type = 3 ROOM) +
                 0.2093 ∗ I(flat type = 4 ROOM ) +
                 0.2762 ∗ I(flat type = 5 ROOM ) +
                 0.4302 ∗ I(flat type = Executive)
                 
where X is the floor area in square meters.
"


################### OFFSITE QUESTION 3e #############################

### Q: Report the coefficient of the floor area in square meters and interpret it.

"
The coefficient of it is 0.003712. 

(1) For two flats of the same type, the flat that is 1 sqm larger is expected to have a 
log(price) that is 0.003712 larger than that of the smaller flat.

(2) For two flats of the same type, the flat that is 1 sqm larger multiplies the expected 
price of the smaller flat by 1.003719.
"


################### OFFSITE QUESTION 3f #############################

### Q: Predict the resale price of a 4-room HDB flat that is of 100 square meters.

new_hdb = data.frame(floor_area_sqm = 100, flat_type = "4 ROOM")

pred_log_price = predict(M_hdb, new_hdb) # log(price) = 12.93

exp(pred_log_price) # price = 412807.6

# The mean resale price for 4 Rooms Flat Type and floor area 100 square meter is about $412,807.6.



################### OFFSITE QUESTION 3g #############################

### Report R^2 of the model and interpret it.

summary(M_hdb)$r.squared # 0.7116378

"
R-squared is 0.712. That means model M_hdb can explain 71.2% the variability of the 
response in the sample.
"

################### EXTRA KNOWLEDGE #############################

# Polynomial model with both height and height^2
polynomial_model <- lm(FEV ~ height + I(height^2), data = fev_data)
summary(polynomial_model)$coef

# Plot
female <- fev_data[fev_data$Sex == 0,]
male <- fev_data[fev_data$Sex == 1,]

plot(fev_data$height, fev_data$FEV, type = "n") 

points(female$height, female$FEV, col = "red", pch = 20)  
points(male$height, male$FEV, col = "darkblue", pch = 20)

legend("topleft", 
       legend = c("Female", "Male"),
       col = c("red", "darkblue"), 
       pch = c(20, 20))
title(main = "Scatterplot of FEV against height (polynomial fit)")

# Add polynomial curve using curve()
curve(predict(polynomial_model, newdata = data.frame(height = x)), 
      add = TRUE, lwd = 3)

