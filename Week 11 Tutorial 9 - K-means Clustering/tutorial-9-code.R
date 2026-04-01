setwd("~/My Drive (krlee7001@gmail.com)/NUS (Acads Related)/AY2526 Sem 2/DSA1101 TA")

########## ONSITE QUESTION PREP ##############

iris = read.csv("Data/iris.csv")

head(iris)
str(iris)
dim(iris) # 150 rows

########## ONSITE QUESTION 1 ##############

"
Q: Run K-means for iris dataset from k = 1...10
and obtain the WSS values
"

# Step 0: Set seed (even though official ans did not)
set.seed(1505)

# Step 1: Standardize the input variables
scaled_data = scale(iris[1:4])
head(scaled_data)

# Step 2: Perform K-means
K = 10 

wss = numeric(K) # to store wss values

for (k in 1:K) {
  model <- kmeans(scaled_data, 
                  centers = k, 
                  nstart = 20)
  
  wss[k] = model$tot.withinss
}
wss # view wss values

"
Recall that one weakness of K-means 
is that its final clusters can vary 
a lot depending on the initialization 
of centroids!

nstart = 20 here will run the kmeans 
20 times with diff initialization
and returns the one is lowest wcss 
"


########## ONSITE QUESTION 2 ##############

"
Plot the WSS obtained against k. 
State which k you would choose and 
briefly explain why
"

plot(x = 1:K, y = wss, 
     col = "red", type ="b", 
     xlab = "Number of Clusters", 
     ylab = "Within Sum of Squares")

"
Using the elbow method, a possible best k 
is k = 3 as the reduction in WSS from k = 3 to 
k = 4 is not too significant.
"


########## ONSITE QUESTION 3 ##############

"
With the best k chosen, report centroids of 
each cluster and no of data points in each cluster
"

# Apply K-means with k = 3 (best k)
kout = kmeans(scaled_data, 
              centers = 3,
              nstart = 20)

# Obtain the 3 centroids 
kout$centers

## Note: centroids here are still standardized
# EXTRA: to unstandardize
centers_unscaled = sweep(kout$centers, 
                         MARGIN = 2, 
                         attr(scaled_data, "scaled:scale"), "*")
centers_unscaled = sweep(centers_unscaled, 
                         MARGIN = 2, 
                         attr(scaled_data, "scaled:center"), "+")
centers_unscaled

# Number of points in each cluster
kout$size # 47 50 53



########## OFFSITE QUESTION 2 PREP ##############

readLines("data/hdb-2012-to-2014.csv", n = 3)

hdb = read.csv("data/hdb-2012-to-2014.csv")

hdb_selected = hdb[,c("resale_price", "floor_area_sqm")]

head(hdb_selected)
str(hdb_selected)
dim(hdb_selected) # 6047 rows 

"
Both numerical, great!
*recall kmeans only takes in num input (like knn)

But their ranges are very diff
So for this we MUST MUST standardize!
"


########## OFFSITE QUESTION 2a ##############

"
Q: Run kmeans on resale_price and floor_area_sqm
then pick the best k (using WSS as criterion)
"

scaled_hdb = scale(hdb_selected)
head(scaled_hdb)

set.seed(5)

K = 15 

wss = numeric(K)

for (k in 1:K) {
  wss[k] = kmeans(scaled_hdb, 
                  centers = k,
                  iter.max = 30, 
                  nstart = 50)$tot.withinss
}

plot(1:K, wss,
     col = "steelblue", type = "b",
     xlab = "Number of clusters",
     ylab = "Within Sum of Squares") # k = 3 is a good choice


########## OFFSITE QUESTION 2b ##############

"With optimal k, plot data points in the k clusters determined"

kout = kmeans(scaled_hdb, centers = 3, nstart = 50)

plot(x = hdb_selected$floor_area_sqm,
     y = hdb_selected$resale_price,
     col = kout$cluster)

kout$size # 3297  754 1996
