getwd()
##
################################################################
####Load Libraries
library(caret)
library(pROC)
library(psych)
#######################################################################
############Train the Boosting Classification Model###########################################
##############################################################
#####Load the Data
################################################################
odata2 = read.csv("odata2.csv", header=TRUE, stringsAsFactors = F)
names(odata2)
attach(odata2)
odata2=odata2[,-c(32:43)]
dim(odata2)
names(odata2)
##########################DATA PARTITION##########################
set.seed(222)
ind <- sample(2, nrow(odata2), replace = T, prob = c(0.7, 0.3))
train <- odata2[ind==1,]
test <- odata2[ind==2,]

######Plot Target Variable.
plot(factor(odata2[,30]), names=c('Normal', "High"),ylab='Number of Respondents', xlab="Body Mass Index", col=c("blue","red"), ylim=c(0,600))
box()
k=odata2[,30]

table(k)

#####Train the Boosting Tree Classification Data
modelbst <- train(factor(overweight)~., data=train, method="sda", trControl=control)
modelbst
plot(modelbst)
modelbst$finalModel
predbst=predict(modelbst,newdata = test)
predbst
confusionMatrix(predbst,as.factor(test$overweight), mode='everything')
modelbst$results
plot(varImp(modelbst, scale=T))
########################################################################
#####Train the Boosting Tree Classification Data
modelb1 <- train(factor(overweight)~., data=train, method="AdaBoost.M1", trControl=control)
modelknn
plot(modelknn)
modelknn$finalModel
predknn=predict(modelknn,newdata = test)
predknn
confusionMatrix(predknn,as.factor(test$overweight), mode='everything')
modelknn$results
plot(varImp(modelknn, scale=T))
##########################################################################
##########################################################################

#####Train the Boosting Tree Classification Data
modelknn <- train(factor(overweight)~., data=train, method="bstTree", trControl=control)
modelknn
plot(modelknn)
modelknn$finalModel
predknn=predict(modelknn,newdata = test)
predknn
confusionMatrix(predknn,as.factor(test$overweight), mode='everything')
modelknn$results
plot(varImp(modelknn, scale=T))
###########################################################################
###########################################################################
#####Train the Boosting Tree Classification Data
modelknn <- train(factor(overweight)~., data=train, method="bstTree", trControl=control)
modelknn
plot(modelknn)
modelknn$finalModel
predknn=predict(modelknn,newdata = test)
predknn
confusionMatrix(predknn,as.factor(test$overweight), mode='everything')
modelknn$results
plot(varImp(modelknn, scale=T))
#################################################################################
#################################################################################

