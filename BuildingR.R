#################################################################
###LOAD DATA
#################################################################
bdata = read.csv("Building.csv", header=TRUE)
names(bdata)
attach(bdata)
#################################################################
##############LOAD PACKAGES######################################
##################################################################
library(rpart)
library(psych)
library(MASS)
library(class)
library(e1071)
library(ROSE)
library(randomForest)
library(lattice)
library(ggplot2)
library(caretEnsemble)
library(tidyverse)
library(mlbench)
library(caret)
#names(getModelInfo())
#modelnames <- paste(names(getModelInfo()), collapse=', ')
#modelnames
##################################################################
####Feature Plot
##################################################################
featurePlot(x=bdata[,1:6],y=bdata[,7],auto.key=list(columns=4))#featurePlot(x=fdata[,2:24],y=fdata[,1], plot = "box")
#featurePlot(x=x, y=y, plot="density", scales = scales)
plot(bdata[,7],type='l',col=2, ylab = 'Number of Casuality', xlab='Number of Collapsed Buildings')
#############################################################
######Correlation Plot##################################
########################################################
library(corrplot)
k=cor(bdata, method = "spearman")
corrplot(k)
corrplot(k, type='upper',method='ellipse')
corrplot(k, type='upper',method='pie')
#################################################################
attach(bdata)
######FEATURE SELECTION FOR PREDICTION
##### 1. Boruta algorithm to determine the best variables
library(Boruta)
borC = Boruta(Casuality~., data = bdata, doTrace = 2, maxRuns=500)
print(borC)
par(pty='m')
plot(borC,las=2,cex.axis=0.7)
plotImpHistory(borC)
bor1=TentativeRoughFix(borC)
attStats(bor1)
####################################################################
###DATA PARTITION
##################################################################
#ind=sample(2, nrow(fdata),replace=T,prob=c(0.70,0.30))
#train=fdata[ind==1,]
#test= fdata[ind==2,]
#dim(train)
#dim(test)
###########################################################
#######Alternatively
###########################################################
#inTrain=createDataPartition(y=INFY,p=0.75,list=FALSE)
#traing=fdata[inTrain,]
#testg=fdata[-inTrain,]
#attach(traing)
#attach(testg)
#dim(traing)
dim(testg)
####################################################################
#########################################################################
# Prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
######################################################################

#####################################################################
##Start Training the models
####################################################################
# Robust Linear model
set.seed(1)
modelrlm <- train(Casuality~., data=bdata, method="rlm", trControl=control)
modelrlm
modelrlm$finalModel
plot(varImp(modelrlm, scale=T))
#########################################################################
################################################################################
# train the SVM model
set.seed(2)
modelsvm <- train(Casuality~., data= bdata, method="svmRadial", trControl=control)
modelsvm
plot(modelsvm)
modelsvm$finalModel
plot(varImp(modelsvm, scale=T))
###############################################################################
######DT
set.seed(3)
fit.DT <- train(Casuality~., data = bdata, method = "rpart2",
                #metric = metric,
                trControl = control)
fit.DT
plot(fit.DT$finalModel, uniform=TRUE,
     main="Classification Tree")
text(fit.DT$finalModel, use.n.=TRUE, all=TRUE, cex=.8)
suppressMessages(library(rattle))

fancyRpartPlot(fit.DT$finalModel)

library(rpart.plot)

rpart.plot(fit.DT$finalModel)
plot(varImp(fit.DT, scale=T))

################################################################################
###############################KNN
set.seed(4)
modelknn <- train(Casuality~., data = bdata,  method = "knn",
                  trControl = control)

modelknn
plot(varImp(modelknn, scale=T))
###################################################################################
###############################################################################
#######Bayesian Ridge Regression
set.seed(5)
modelbridge<- train(Casuality~., data=bdata, method="bridge", trControl=control)
modelbridge
plot(varImp(modelbridge, scale=T))
#############################################################################
###Elastic Net
#############################################################################
set.seed(6)
modelenet <- train(Casuality~., data=bdata, method="enet", trControl=control)
modelenet
modelenet$finalModel
plot(varImp(modelenet, scale=T))
############################################################################
###########Ridge Regression
set.seed(7)
modelridge <- train(Casuality~., data=bdata, method="ridge", trControl=control)
modelridge
modelridge$finalModel
plot(varImp(modelridge, scale=T))
##########################################################################
###Random Forest
############################################################################
set.seed(8)
modelrf <- train(Casuality~., data=bdata, method="rf", trControl=control)
modelrf$results
modelrf
modelrf$finalModel
plot(varImp(modelrf, scale=T))
############################################################################
####################Bayesian LASSO###########################################
set.seed(9)
modelblasso <- train(Casuality~., data=bdata, method="blasso", trControl=control)
modelblasso
modelblasso$finalModel
plot(varImp(modelblasso, scale=T))
############################################################################
set.seed(10)
modellasso <- train(Casuality~., data=bdata, method="lasso", trControl=control)
plot(modellasso)
modellasso$finalModel
modellasso
plot(varImp(modellasso, scale=T))

############################################################################
###VARIABLE IMPORTANCE PLOTS
############################################################################
par(mfrow=c(2,5))
par(mfrow=c(2,2))
plot(varImp(modelrlm, scale=T))
plot(varImp(modelsvm, scale=T))
plot(varImp(fit.DT, scale=T))
plot(varImp(modelknn, scale=T))
plot(varImp(modelbridge, scale=T))
plot(varImp(modelenet, scale=T))
plot(varImp(modelridge, scale=T))
plot(varImp(modelrf, scale=T))
plot(varImp(modelblasso, scale=T))
plot(varImp(modellasso, scale=T))


##########################################################################
# collect resamples
results <- resamples(list(Lasso=modellasso, Bridge=modelbridge, ENET=modelenet, RLM=modelrlm,Blasso=modelblasso,RF=modelrf,Ridge=modelridge, SVM=modelsvm, DT=fit.DT,KNN=modelknn))

# summarize the distributions
summary(results)
# boxplots of results
bwplot(results)
# dot plots of results
dotplot(results)
