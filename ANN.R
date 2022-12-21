## Creating index variable 

# The objective is to predict rating of the cereals variables such as calories, proteins, fat etc.

# Read the Data
data = read.csv("C:/Users/Alireza/Desktop/Arabi/PISTAR444/rcode/cereals.csv", header=T)

head(data)
dim(data)
# Random sampling
samplesize = 0.60 * nrow(data)
set.seed(80)
index = sample( seq_len ( nrow ( data ) ), size = samplesize )

# Create training and test set
datatrain = data[ index, ]
datatest = data[ -index, ]

## Scale data for neural network

max = apply(data , 2 , max)
min = apply(data, 2 , min)
scaled = as.data.frame(scale(data, center = min, scale = max - min))

##.................................... Fit neural network 

# install library
#install.packages("neuralnet")

# load library
library(neuralnet)

# creating training and test set
trainNN = scaled[index , ]
testNN = scaled[-index , ]
data1=read.csv("ViCDATA.csv", header=TRUE)
dim(data1)
data1
dat1=data1[1:41,-c(1,8)]
dat1
as.data.frame(data1)
attach(data1)
dat1
# fit neural network
set.seed(2)
NN = neuralnet(AO ~ .,
               dat1, hidden = c(3) , act.fct = "logistic", linear.output = T )
?neuralnet
# plot neural network
plot(NN)
# The weights are calculated using the back propagation algorithm explained earlier.
# The blue line is the displays the bias term.

##...................... Prediction using neural network

predict_testNN = compute(NN, testNN[,c(1:5)])
predict_testNN = (predict_testNN$net.result * (max(data$rating) - min(data$rating))) + min(data$rating)

plot(datatest$rating, predict_testNN, col='blue', pch=16, ylab = "predicted rating NN", xlab = "real rating")

abline(0,1, col = 2)

# Calculate Root Mean Square Error (RMSE)
RMSE.NN = (sum((dat1$AO - predict_testNN)^2) / nrow(data1)) ^ 0.5

##.................................... Cross validation of neural network model

# install relevant libraries
#install.packages("boot")
#install.packages("plyr")

# Load libraries
library(boot)
library(plyr)

# Initialize variables
set.seed(50)
k = 100
RMSE.NN = NULL

List = list( )

# Fit neural network model within nested for loop
for(j in 10:65){
  for (i in 1:k) {
    index = sample(1:nrow(data),j )
    
    trainNN = scaled[index,]
    testNN = scaled[-index,]
    datatest = data[-index,]
    
    NN = neuralnet(rating ~ calories + protein + fat + sodium + fiber, trainNN, hidden = 3, linear.output= T)
    predict_testNN = compute(NN,testNN[,c(1:5)])
    predict_testNN = (predict_testNN$net.result*(max(data$rating)-min(data$rating)))+min(data$rating)
    
    RMSE.NN [i]<- (sum((datatest$rating - predict_testNN)^2)/nrow(datatest))^0.5
  }
  List[[j]] = RMSE.NN
}

Matrix.RMSE = do.call(cbind, List)

#save(Matrix.RMSE, file = "Matrix_RMSE.RData")
load("C:/Users/Alireza/Documents/Matrix_RMSE.RData")
## Prepare boxplot
boxplot(Matrix.RMSE[,56], ylab = "RMSE", main = "RMSE BoxPlot (length of traning set = 65)")


##....................................... Variation of median RMSE 
# We calculate the median RMSE for each of the training set length and plot them.

#install.packages("matrixStats")
library(matrixStats)

med = colMedians(Matrix.RMSE)

X = seq(10,65)

plot (med~X, type = "l", xlab = "length of training set", ylab = "median RMSE", main = "Variation of RMSE with length of training set")
# The performance of neural network model is sensitive to training-test split.
