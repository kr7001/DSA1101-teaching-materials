# setwd("~/My Drive (krlee7001@gmail.com)/NUS (Acads Related)/AY2526 Sem 2/DSA1101 TA")


################### ONSITE QUESTION 1 #############################

##### Import Dataset into R

# From here, we could examine the default values of read.csv()
# To know if we need to overwrite default parameters such as header, sep etc
?read.csv

# To know what separator a dataset is, you can do:
readLines("Data/hdbresale_reg.csv", n=3)

# Import CSV file into data frame
hdb <- read.csv("Data/hdbresale_reg.csv")

# Inspect the data:
names(hdb) # Columns of the data frame
head(hdb, n=3) # First 3 rows
str(hdb) # Data types, first few values of each column
dim(hdb) # Number of rows and columns of data


################### ONSITE QUESTION 2 #############################

##### How many flats in the sample given? (Official solution)

dim(hdb) # 6055  11

# Verify assumption (good practice):

length(unique(hdb$X))  # 6055
length(unique(hdb$X)) == dim(hdb)[1]  # TRUE - confirms one row per flat


################### ONSITE QUESTION 3 #############################

# attach(hdb) so we can directly call it's column, rather than hdb$column
# again, be careful when you call attach()!
attach(hdb)

# Get a list of all currently attached packages and R objects in the R search path
search()


##### PART A: Explore resale price by creating a histogram

hist(resale_price)

"
The sample of the resale price is not normally distributed, 
it is in fact right (positively) skewed. 

It is also unimodal, with one peak around 400,000-450,000

Range is from 200,000 to 1,000,000.

It also has possible upper-tail outliers. 
"

##### PART A (EXTRA): Make the plot prettier with colour, titles, remove scientific notation

# extra: turn off scientific notation
options(scipen = 999)

hist(resale_price, 
     col = "pink", # colour
     main = "Histogram of HDB Resale Price", # main title
     xlab = "Resale Price", # x-axis title
     ylab = "Number of flats") # y-axis title

# to change back to default
options(scipen = 0)



##### PART B: Explore resale price by creating a boxplot

boxplot(resale_price)

# Investigate Outliers 
outliers = boxplot(resale_price)$out
length(outliers) # 284 outliers (A lot!)

"
Many outliers on the high-end, not low-end -> Right-skewed & Upper-tail outliers
Outliers are around 650,000 and above

Lower Quartile is slightly below 400,000. Median is slightly above 400,000. 
Upper Quartile is slightly below 500,000. 
"


##### PART B (EXTRA): Make the plot prettier with colour and titles

# extra: turn off scientific notation
options(scipen = 999)

boxplot(resale_price,
        col = "pink",
        main = "Boxplot of HDB Resale Price",
        yaxt = "n" # suppresses default y-axis
)

axis(side = 2, at = seq(200000, 1000000, by = 100000), las = 1) # create customizable y-axis

## side = 2: left
## at: points at which tick-marks are to be drawn
## las = 1: Make labels horizontol

# to change back to default
options(scipen = 0)

# remember to detach()
detach()



################### OFFSITE QUESTION 1 #############################

readLines("Data/FEV.csv", n=3)

df <- read.csv("Data/FEV.csv")

# Inspect Data

head(df) # preview first 6 rows of data
str(df) # examine data type of columns in the data
dim(df) # 654   7

# attach df so we can call column directly
attach(df)

################### OFFSITE QUESTION 1a #############################

# FEV is the response variable in this study


################### OFFSITE QUESTION 1b #############################

# Create a histogram of FEV and comment on it

hist(FEV, col = "pink", ylab = "Number of children")


# EXTRA: Compare with normal distribution
hist(FEV, ylab = "Probability Density", col = "pink", freq = FALSE)

x <- seq(min(FEV), max(FEV), length.out = 100)

y <- dnorm(x, mean(FEV), sd(FEV))

lines(x, y, col = "red") # plot normal distribution

"
The range of the sample FEV is 0.5 to 6. 

It is also unimodal, with it's peak around 2-2.5. 

Compared to a normal distribution, it is also slightly right skewed. 
"

################### OFFSITE QUESTION 1c #############################

# Step 1: Plot boxplot

fev_boxplot <- boxplot(FEV, 
                       col = "pink", 
                       ylab = "FEV", 
                       main = "Boxplot of FEV")

# Step 2: Get outliers

fev_outliers <- fev_boxplot$out
length(fev_outliers) # 9 outliers

# Step 3: Examine outliers

index <- which(FEV %in% fev_outliers)
index # 321 452 464 517 609 624 632 648 649

df[index,]

## Alternative Method that does not use which()
df[(FEV %in% fev_outliers),]

"
From this, we can see that all outliers are male, 8/9 of them don't smoke, and they are rather tall
"

# EXTRA: Histogram of height
hist(height)


################### OFFSITE QUESTION 1d #############################

# Q: Generally, is the sample of FEV normally distributed?
# We kinda already answered this in question 1b. But let's try to qqplot way here. 

qqnorm(FEV, pch = 20)

qqline(FEV, col = "red")

"
Left tail sample quantiles are larger than expected, 
hence left tail shorter than normal. 

Right tail sample quantiles larger than expected, 
hence right tail longer than normal.

Combined with histogram in 1b, it's clear that the sample of 
FEV is not normal and quite right skewed
"


################### OFFSITE QUESTION 1e #############################

# Q: Create separate histograms for male and female FEV

# Step 1: Get Female and Male's FEV Value
female <- FEV[Sex==0] # or FEV[which(Sex==0)]
male <- FEV[Sex==1] # or FEV[which(Sex==1)]

# Alternative: Filter data to only female and only male
# This method retains other columns in case you need extra info
female <- df[(Sex==0),]
male <- df[(Sex==1),]

# Step 2: Arrange a figure of 1 row and 2 columns
opar <- par(mfrow=c(1,2))

# Step 3: Plot the two histogram
hist(female$FEV, 
     col = "pink", 
     freq = FALSE, 
     xlim = c(0,6), # align x-axis
     ylim = c(0,0.52), # align y-axis
     main = "Histogram of Female FEV")

hist(male$FEV, 
     col = "lightblue", 
     freq = FALSE, 
     xlim = c(0,6), # align x-axis
     ylim = c(0,0.52), # align y-axis
     main = "Histogram of Male FEV")

# Step 4: Rmb to reset to a figure of 1 row and 1 column
opar <- par(mfrow=c(1,1))

# Q: Obtain separate numerical summaries for male and female

summary(female$FEV)
summary(male$FEV)

IQR(female$FEV) # 1.04
IQR(male$FEV) # 1.5275

var(female$FEV) # 0.417
var(male$FEV) # 1.007

"
Both histograms are unimodal. 

The female histogram is almost symmetrical (when compared 
amongst females only), while the male histogram is quite 
right-skewed. 

The range of both samples' FEV differs substantially. 

Though both female and male have similar minimum values of 
0.79 and 0.8 respectively,their maximum values are 3.84 
and 5.79 respectively, showing males extend much further 
into higher values. 

Additionally, males show greater variability, with IQR 
being 1.54 vs 1.05 for females.
Male variance is also considerably larger at 1.007 vs 
0.417 for females.

From the numerical summary, it's clear that male FEV 
values are generally higher than female, with the 1st 
quartile, median, mean, and 3rd quartile all being larger 
for males. 

This suggests that, on average, males have higher 
FEV than females in this sample.
"


################### OFFSITE QUESTION 1f #############################

# Q: Create a scatterplot with height on the x-axis and FEV on the y-axis

plot(height, FEV)

# Let's beautify it, and also colour each point based on their sex:
# Step 1: start with empty plot
plot(height, FEV, type = "n") 

# Step 2: use points() to plot female and male points 
points(female$FEV ~ female$height, col = "red", pch = 20) # ?points for pch info!
points(male$FEV ~ male$height, col = "darkblue", pch = 20)

# or alternatively
points(female$height, female$FEV, col = "red", pch = 20)  
points(male$height, male$FEV, col = "darkblue", pch = 20)

# Step 3: Add legend
legend(x = 1.3, 
       y= 5.5, 
       legend = c("Female", "Male"),
       col = c("red","darkblue"), 
       pch=c(20,20))

# Step 4 (EXTRA): Add title
title(main = "Scatterplot of FEV against height")


################### OFFSITE QUESTION 1g #############################

cor(FEV, height) # 0.8675619

"
The correlation coefficient is ~0.87, indicating a strong positive 
linear association between FEV and height. 

The range of FEV for males appears larger than for females, 
as does the range of heights. 

The variability of FEV increases with height, showing greater spread at taller heights 
compared to shorter heights (heteroscedasticity).
"

detach()


################### EXTRA QUESTION #############################

"
**Important note: Correlation does not imply causation. 
While there's a strong association, we cannot conclude that height 
directly causes higher FEV.
""