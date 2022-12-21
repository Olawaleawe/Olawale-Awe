# Cluster Analysis Using R
#creating factors
library("mvtnorm")
dat <- rbind(rmvnorm(25, mean = c(3,3)),
             rmvnorm(20, mean = c(10, 8)),
             rmvnorm(10, mean = c(20, 1)))
plot(abs(dat), xlab = expression(x[1]), ylab = expression(x[2]))



###################
data(iris)
# rename the dataset
mydata <- iris



#mydata <- read.csv("~/Dropbox/RFiles/utilities.csv")
str(mydata)
head(mydata)
pairs(mydata[,1:4])

# Scatter plot 
#plot(mydata$Sepal.Length~ mydata$Species, data = mydata)
#with(mydata,text(mydata$Fuel_Cost ~ mydata$Sales, labels=mydata$Company,pos=4))

# Normalize 
z = mydata[,-c(5)]
means = apply(z,2,mean)
sds = apply(z,2,sd)
nor = scale(z,center=means,scale=sds)

##calculate distance matrix (default is Euclidean distance)
distance = dist(nor)

# Hierarchical agglomerative clustering using default complete linkage 
mydata.hclust = hclust(distance)
plot(mydata.hclust)
plot(mydata.hclust,labels=mydata$Species,main='Default from hclust')
plot(mydata.hclust,hang=-1)

# Hierarchical agglomerative clustering using "average" linkage 
mydata.hclust<-hclust(distance,method="average")
plot(mydata.hclust,hang=-1)

# Cluster membership
member = cutree(mydata.hclust,3)
table(member)

#Characterizing clusters 
aggregate(nor,list(member),mean)
aggregate(mydata[,-c(5)],list(member),mean)

# Scree Plot
wss <- (nrow(nor)-1)*sum(apply(nor,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(nor, centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") 

# K-means clustering
kc<-kmeans(nor,3)

summary(kc)
kc$cluster

table(kc$cluster)

