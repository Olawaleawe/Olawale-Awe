###########################################################################
###########################################################################
########## COMPARISON OF MULTIPLE ML MODELS FOR PREDICTING INFLATION RATES
###########################################################################
###############CODES WRITTEN AND DEPLOYED FOR fEMI AWOPEGBA, TECH4DEV, LTD###
##################BY OLAWALE AWE, PhD##########################################
###########################################################################
###LOAD DATA
#################################################################
fdata = read.csv("ngainfl3.csv", header=TRUE)
names(fdata)
attach(fdata)
fdata=scale(fdata)
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
####Feature Plot/Data Pre-processing
##################################################################
featurePlot(x=fdata[,2:28],y=fdata[,1],auto.key=list(columns=4))
#featurePlot(x=fdata[,2:24],y=fdata[,1], plot = "box")
#featurePlot(x=x, y=y, plot="density", scales = scales)
#############################################################
###### Data Visualisazion: Correlation Plot##################################
########################################################
library(corrplot)
k=cor(fdata)
corrplot(k)
corrplot(k, type='upper',method='ellipse')
corrplot(k, type='upper',method='pie')
#################################################################

######FEATURE SELECTION#####################################
##### 1. Boruta algorithm to determine the best variables
library(Boruta)
borC = Boruta(INFY~., data = fdata, doTrace = 2, maxRuns=200)
print(borC)
par(pty='m')
plot(borC,las=2,cex.axis=0.7)
plotImpHistory(borC)
bor1=TentativeRoughFix(borC)
attStats(bor1)
####################################################################
####################################################################
#########################################################################
# Prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
######################################################################
formula1= (INFY~ RNONOILG + EXRBDC + MS3 + MLR + CRGOV + CRPS + RPROF + OILP)
formula2=(INFY~ OILP + RPROF + MPR + TGE + PLR + RGCON + RYG + MLR + CIC + EXRNF + EXRIB + RNONOILG + CRPS + MS2 + CRGOV + MS3 + RSERG + EXRBDC)
#####################################################################
##Start Training the Supervised ML models
####################################################################
# Robust Linear model
set.seed(7)
modelrlm <- train(formula1, data=fdata, method="rlm", trControl=control)
modelrlm
modelrlm$finalModel
plot(varImp(modelrlm, scale=T))
#########################################################################
################################################################################
# train the SVM model
set.seed(7)
modelsvm <- train(INFY~., data= fdata, method="svmRadial", trControl=control)
modelsvm
modelsvm$finalModel
plot(varImp(modelsvm, scale=T))
###############################################################################
######Decision Tree
set.seed(7)
modelDT <- train(INFY~., data = fdata, method = "rpart",
                 #metric = metric,
                 trControl = control)
modelDT
plot(modelDT$finalModel, uniform=TRUE,
     main="Classification Tree")
text(modelDT$finalModel, use.n.=TRUE, all=TRUE, cex=.8)
suppressMessages(library(rattle))
fancyRpartPlot(modelDT$finalModel)
library(rpart.plot)
rpart.plot(modelDT$finalModel)
plot(varImp(modelDT, scale=T))
################################################################################
##############  TRAIN THE KNN MODEL ##########################################
###############################KNN###################################
set.seed(7)
modelknn <- train(INFY~., data = fdata ,  method = "knn",
                                 trControl = control)

modelknn
######PLOT VARIABLE IMPORTANCE
plot(varImp(modelknn, scale=T))
###################################################################################
###############################################################################
#######Bayesian Ridge Regression#########################################
set.seed(7)
modelbridge<- train(INFY~., data=fdata, method="bridge", trControl=control)

modelbridge
plot(varImp(modelbridge, scale=T))
#############################################################################
#######################Elastic Net##########################################
#############################################################################
set.seed(7)
modelenet <- train(formula2, data=fdata, method="enet", trControl=control)
modelenet
plot(varImp(modelenet, scale=T))
############################################################################
###########Ridge Regression#################################################
############################################################################
set.seed(7)
modelridge <- train(INFY~., data=fdata, method="ridge", trControl=control)
modelridge
plot(varImp(modelridge, scale=T))
##########################################################################
######################Random Forest
############################################################################
set.seed(7)
modelrf <- train(INFY~., data=fdata, method="rf", trControl=control)
modelrf$results
modelrf
plot(varImp(modelrf, scale=T))
############################################################################
####################Bayesian LASSO###########################################
#########################################################################
set.seed(7)
modelblasso <- train(INFY~., data=fdata, method="blasso", trControl=control)
modelblasso
plot(varImp(modelblasso, scale=T))
############################################################################
##################################LASSO
############################################################################
set.seed(7)
modellasso <- train(INFY~., data=fdata, method="lasso", trControl=control)
plot(modellasso)
plot(varImp(modellasso, scale=T))

############################################################################
# collect resamples and list the models here. Use only 10 models
results <- resamples(list(Lasso=modellasso, Bridge=modelbridge, Enet=modelenet, RLM=modelrlm,Blasso=modelblasso,RF=modelrf,Ridge=modelridge, SVM=modelsvm, CART=fit.DT,KNN=modelknn))

# summarize the metric distributions of the models
summary(results)
# boxplots of results
bwplot(results)
# dot plots of results
dotplot(results)

####The Support vector Machine is then suggested because it is the the best algorithm.
################################################################################
