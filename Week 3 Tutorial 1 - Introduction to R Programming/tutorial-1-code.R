################### ONSITE QUESTION 1A #############################

### 1. Define a vector to store x[1], x[2]..., x[30]

x <- numeric(30) # create a vector with 30 elements, all zeros.

### 2. Assign x[1] = 0 and x[2] = 1

x[1] <- 0
x[2] <- 1

x # check x to see if changes applied

### 3. Loop through x[n] <- 2*x[n-1] - x[n-2] + 5 where n is from 3 to 30

for (n in 3:30) {
  x[n] <- 2*x[n-1] - x[n-2] + 5
}

x # check x to see if changes applied

### 4. Obtain x[30]

x[30] # 2059

########### ONSITE Q1A Extra ###############

x1 <- 0
x2 <- 1

for (n in 3:30) {
  x3 <- 2*x2 - x1 + 5
  x1 <- x2
  x2 <- x3
}
x3


################### ONSITE QUESTION 1B #############################

x # view the x that we have

x>=1000

which(x>=1000)

min(which(x >= 1000))


################### ONSITE QUESTION 2 (Method 1) #############################

y <- numeric(50) # Try 50 first

# y[0] <- 10000 (unlike python, there's no 0 index in R)
y[1] <- 2800 + (1.02 * 10000)

for (n in 2:50) {
  y[n] <- 2800 + (1.02 * y[n-1])
}

y[50] # 263738.2 (does not exceed 300000 yet)

##################################

y <- numeric(70) # Try 70 this time

y[1] <- 2800 + (1.02 * 10000)

for (n in 2:70) {
  y[n] <- 2800 + (1.02 * y[n-1])
}

y[70] # 459933.7, exceeded 

min(which(y>=300000)) # 55


################### ONSITE QUESTION 2 (Method 2) #############################

z <- 10000
i <- 0 # i helps us keep track of the indices

while(z < 300000) {
  z <- 2800 + (1.02 * z)
  i <- i + 1
}

i # 55
z # 305759.6

### BTW: Below is another method in the official solution that Prof Daisy posted that uses while loop.

y <- numeric()
y <- append(y, 2800 + 1.02*10000)

while (max(y) < 300000) {
  y <- c(y, 2800 + 1.02*max(y))
}

length(y) # 55
y[55] # 305759.6

################### OFFSITE QUESTION 1ai #############################

# Step 1: Define all variables based on question 
# Question says to match name!

price <- 1200000
saved <- 10000
salary <- 7000
return <- 0.02 

downpayment <- 0.25*price
savedsalary <- 0.4*salary

# Step 2: While loop to find months

month <- 0 # start at month 0

while (saved < downpayment) {
  
  saved <- saved + ((return * saved) + (savedsalary))
  
  month <- month + 1 # month increases by 1 each time we loop
  
}

month # 55 months

################### OFFSITE QUESTION 1aii (repeat for salary = 10000) #############################

# Step 1: Define all variables based on question 
# Question says to match name!

price <- 1200000
saved <- 10000
salary <- 10000 # this changed only
return <- 0.02 

downpayment <- 0.25*price
savedsalary <- 0.4*salary

# Step 2: While loop to find months

month <- 0 # start at month 0

while (saved < downpayment) {
  
  saved <- saved + ((return * saved) + (savedsalary))
  
  month <- month + 1 # month increases by 1 each time we loop
  
}

month # 44 months


################### OFFSITE QUESTION 1bi #############################

# Step 1: Define all variables based on question 

price <- 1200000
saved <- 10000
salary <- 7000
return <- 0.02 
rate <- 0.02

downpayment <- 0.25*price
savedsalary <- 0.4*salary

# Step 2: While loop to find months

month <- 0 # start at month 0

while (saved < downpayment) {
  
  saved <- saved + ((return * saved) + (savedsalary))
  
  month <- month + 1 # month increases by 1 each time we loop
  
  # increase salary every 4 months
  if (month %% 4 == 0) { 
    salary <- salary + (salary * rate) 
    savedsalary <- 0.4*salary # update savedsalary
  }
}

month # 52 months


################### OFFSITE QUESTION 1bii #############################

# Step 1: Define all variables based on question 

price <- 1200000
saved <- 10000
salary <- 10000
return <- 0.02 
rate <- 0.01

downpayment <- 0.25*price
savedsalary <- 0.4*salary

# Step 2: While loop to find months

month <- 0 # start at month 0

while (saved < downpayment) {
  
  saved <- saved + ((return * saved) + (savedsalary))
  
  month <- month + 1 # month increases by 1 each time we loop
  
  # increase salary every 4 months
  if (month %% 4 == 0) { 
    salary <- salary + (salary * rate) 
    savedsalary <- 0.4*salary # update savedsalary
  }
}

month # 43 months 

################### OFFSITE QUESTION 2 #############################

# Step 1: Define variables (except salary)
price <- 1200000
downpayment <- 0.25*price

# Step 2: Create function F1
F1 <- function(salary) {
  
  saved <- 10000 # saved inside function cause it always starts at $10000
  savedsalary <- 0.4*salary
  month <- 0
  
  while (saved < downpayment) {
    saved <- saved + ((return * saved) + (savedsalary))
    month <- month + 1 # month increases by 1 each time we loop
  }
  
  return(month)
}

F1(7000) # 55
F1(10000) # 44


################### OFFSITE QUESTION 2 (optional argument) #############################


F1 <- function(salary, price = 1200000) {
  
  downpayment <- 0.25*price
  saved <- 10000
  savedsalary <- 0.4*salary
  month <- 0
  
  while (saved < downpayment) {
    saved <- saved + ((return * saved) + (savedsalary))
    month <- month + 1 # month increases by 1 each time we loop
  }
  
  return(month)
}

F1(7000) # 55
F1(10000) # 44

################### OFFSITE QUESTION 3 #############################

F2 <- function(salary, price = 1200000, rate = 0.02) {
  
  downpayment <- 0.25*price
  saved <- 10000
  savedsalary <- 0.4*salary
  month <- 0
  
  while (saved < downpayment) {
    saved <- saved + ((return * saved) + (savedsalary))
    month <- month + 1 # month increases by 1 each time we loop
    
    if (month %% 4 == 0) {
      salary <- salary + (salary * rate)
      savedsalary <- 0.4*salary # update savedsalary
    }
  }
  
  return(month)
}

F2(salary = 7000, rate = 0.02) # answer: 52
F2(salary = 10000, rate = 0.01) # answer: 43

################### EXTRA QUESTIONS #############################

vec <- c(1,2,3,4)

for (element in vec) {
  print(vec)
  print(element)
  vec <- vec[-element]
}

print(vec)