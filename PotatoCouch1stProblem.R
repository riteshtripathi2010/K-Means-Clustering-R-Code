#Retail Analytics: Roy
# Started with K - Means CLustering: Couch Potato Data set
#Learnt:
  # Normalization
  # Correlations
  # Get number of centroids, elbow method
  # run k means algorithm

mydata <- read.csv(file.choose(), header = TRUE)
mydata

summary(mydata)#data doesnt make that much of sense its not normalized

# Scalling
data2 <- scale(mydata, center = TRUE, scale = TRUE)
library(datasets)
summary(data2)
head(data2)

# Correlations
c <- cor(data2[,-c(1)])
c
# 0 means no correlation
# -1: perfect negative correlation
# 1: perfect positive correlation

#we see correlation between TV_View and Income are strong, 
#and rest are fairly weak
# we will use Income, Age, TV_Views
# There is K Means, and Then K-Mode: which looks after the count
# K Mode is good for categorical data, for segmenting

#Lets do plotting
library(ggplot2)
data3 <- data.frame(vec = data2)#converting to dataframe
ggplot(data3, aes(x = vec.Age)) + 
  geom_histogram(binwidth = 0.1, fill = "blue", 
                 color = "black") #Histogram of Age
#What i make out from this distribution, i see more of Younger People below average group
#Distribution is Skewed towards left

ggplot(data3, aes(y = vec.Income, x = vec.Age)) + 
  geom_point() #Age and Income

#Lets decide how many clusters are to be there:
rng <- 2:20 #k from 2 to 20
tries <- 100 #run k means algorithm 100 times
avg.totw.ss <- integer(length(rng)) #set up an empty vector to hold all of points
for(v in rng) #for each value of the range variable
{
  v.totw.ss <- integer(tries) #set up an empty vector to hold the 100 tries
  for(i in 1:tries){
    k.temp <- kmeans(data2[,-c(1,2,3)], centers = v) #dropping id, and others
    v.totw.ss[i] <- k.temp$tot.withinss #store the total withinss
    
  }
  avg.totw.ss[v-1] <- mean(v.totw.ss) #average the 100 total withinss
    
}
plot(rng, avg.totw.ss, type = "b", main = "Total within ss by various k",
     ylab = "Average Total with in sum of squares",
     xlab = "Value of K")

#Lets apply K Means Clustering
set.seed(500)
k <- kmeans(data2[,-c(1,2,3)], centers = 3)#create 3 clusters, remove coln 1,2,3
  # first coln is the serial number, then gender and marital status
k$centers #display cluster centers
table(k$cluster) #give count of data points in each cluster
k#give all important results

# --------------------END --------------------_


#Skipped this part
# With same data set, we do Hierarcheal Clustering
data <- read.csv(file.choose(), header = TRUE)
data
summary(data)
colMeans(data) # Column Means

data2 <- scale(data, center = TRUE, scale = TRUE)
library(datasets)
summary(data2)

# -------Hierarcheal ------------
clusters <- hclust(dist(data2[,4:6])) #Using age, income, Tv View
plot(clusters)#looks more dense
#this is a dendogram

#Finding the elbow method
#Optimal number of clusters
# professor's code doesnt work
#have to check internet for getting elbows method
data3 <- (data2[,4:6])
pkgs <- c("factoextra", "NbClust")

install.packages("pkgs")
#initially this package didnt install, 
#https://stackoverflow.com/questions/59055291/problem-installing-factoextra-package-in-r
#used above link to download the older version of facto and then re run the command to install facto

install.packages("factorextra")
library(factoextra)
library(NbClust)

#Elbow method
fviz_nbclust(data3, FUNcluster = hcut, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) +
  labs(subtitle = "Elbow Method")

# Cut the tree at 4 Clusters
clustercut <- cutree(clusters, k = 4) #cut to get 4 clusters
table(clustercut) #number of members in each cluster
clustercut

#Cluster Characteristic
data2 <- cbind(data2, clustercut)
data21 <- subset(data2, clustercut == 1)
data22 <- subset(data2, clustercut == 2)
data23 <- subset(data2, clustercut == 3)
data23 <- subset(data2, clustercut == 4)
rbind(colmeans(data21), colMeans(data22), colMeans(data23), colMeans(data24))


















































