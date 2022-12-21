
#########Correlation##################



##########################################
######FEATURE SELECTION#######################
##########################################
set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the data
data(PimaIndiansDiabetes)
# calculate correlation matrix
correlationMatrix <- cor(PimaIndiansDiabetes[,1:8])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)

##############################################################
###########LEARNING VECTOR QUANTIZATION
###############################################################
library(mlbench)
library(caret)
install.packages('e1071')
library(e1071)
# load the dataset
data(PimaIndiansDiabetes)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(diabetes~., data=PimaIndiansDiabetes, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance

#########################################################################
###########RECURSIVE FEATURE ELIMINATION
#########################################################################
set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the data
data(PimaIndiansDiabetes)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(PimaIndiansDiabetes[,1:8], PimaIndiansDiabetes[,9], sizes=c(1:8), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))


2000*550
5000*550

#########################################################################
###Agric Output
########################################################################
ag=read.csv("Vicdata.csv", header=T)
ag
ag=ag[,-1]
ag
colnames(ag)= c("LR","ER","GDP","Inf","OP","AO")
ag
attach(ag)

data1=read.csv("ViCDATA.csv", header=TRUE)
dim(data1)
data1
dat1=data1[1:41,-c(1,8)]
dat1
as.data.frame(dat1)
attach(data1)
dat1
###############################################################
library(mlbench)
library(caret)
install.packages('e1071')
library(e1071)
# load the dataset
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(GDP ~., data=dat1, method="lm", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
 plot(importance)
 
 
 d<-head(mtcars)
 plot(d[,'wt'], d[,'mpg'], 
      main="Milage vs. Car Weight\n~~~~~~~~~~~~~~~~~~~",
      xlab="Weight", ylab="Miles/(US) gallon",
      pch=19, col="darkgreen")
 text(d[,'wt'], d[,'mpg'],  row.names(d),
      cex=0.65, pos=3,col="red")
 
 text(2.4, 21, expression(hat(beta) == (X^t * X)^{-1} * X^t * y))
 text(2.7, 19, expression(bar(x) == sum(frac(x[i], n), i==1, n)))
 
 
 heart1 = function(name){
   t = seq(0,60,len=100)
   plot(c(-8,8),c(0,20),type='n',axes=FALSE,xlab='',ylab='')
   x = -.01*(-t^2+40*t+1200)*sin(pi*t/180)
   y = .01*(-t^2+40*t+1200)*cos(pi*t/180)
   lines(x,y, lwd=4)
   lines(-x,y, lwd=4)
   text(0,7,"Happy Valentine's Day",col='red',cex=2.5)
   text(0,5.5,name,col='red',cex=2.5)
 }
 
 
 dat<- data.frame(t=seq(0, 2*pi, by=0.1) )
 xhrt <- function(t) 16*sin(t)^3
 yhrt <- function(t) 13*cos(t)-5*cos(2*t)-2*cos(3*t)-cos(4*t)
 dat$y=yhrt(dat$t)
 dat$x=xhrt(dat$t)
 plot(y ~ x, data=dat, type="l", bty="n", xaxt="n", yaxt="n", ann=FALSE)
 with(dat, polygon(x,y, col="hotpink"))
 points(c(10,-10, -15, 15), c(-10, -10, 10, 10), pch=169, font=5)
 text(0,7,"Have a Nice Day\n Class!",col='red',cex=2.5)
box() 


t<-rnorm(10000,0,1.8)
x<-16*(sin(t)^3)
y<-13*cos(t)-5*cos(2*t)-2*cos(3*t)-cos(4*t)
plot(x,y,type='h', col=2)


seq(0, 1, length.out = 11)
seq(stats::rnorm(20)) # effectively 'along'
seq(1, 9, by = 2)     # matches 'end'
seq(1, 9, by = pi)    # stays below 'end'
seq(1, 6, by = 3)
seq(1.575, 5.125, by = 0.05)
seq(17) # same as 1:17
k=seq(1,100,pi);k
?seq
