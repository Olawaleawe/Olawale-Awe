setwd( "C:/Users/rsewpaul/Documents/Ronel Cdrive new/BRICS anemia grant/P4 ML obesity in adolesc")
getwd()

#####Input Data from Excel into R
####Obesity Data - SANHANES adolescents aged 15-17 years with bmi measured - input dataframe in R called odata
datao = read.csv("odata.csv")
datao
attach(datao)
head(datao)
names(datao)
dim(datao)
summary(datao)
set.seed(123)

library(Boruta)
borC = Boruta(factor(Overweight)~., data = datao, doTrace = 2, maxRuns=500)
print(borC)
par(pty='m')
plot(borC,las=2,cex.axis=0.7)
plot(factor(Overweight), names=c("Not overweight", "Overweight"),ylim=c(0,600),col=c("blue","red"),ylab="Number of Observations", xlab="Weight Status")
box()

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
library(naivebayes)

####################################################################
###DATA PARTITION
##################################################################
ind=sample(2, nrow(datao),replace=T,prob=c(0.70,0.30))
train=datao[ind==1,]
test= datao[ind==2,]
dim(train)
dim(test)
###########################################################
#######Alternatively
###########################################################
#inTrain=createDataPartition(y=INFY,p=0.75,list=FALSE)
#traing=fdata[inTrain,]
#testg=fdata[-inTrain,]
#attach(traing)
#attach(testg)
#dim(traing)
#dim(testg)
####################################################################
#########################################################################
# Prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=5)
######################################################################

#####################################################################
##Start Training the models
#BEFORE FEATURE SELECTION
# train the SVM model
set.seed(7)
Overweight=factor(Overweight)
modelsvm <- train(factor(Overweight)~., data= train, method="svmRadial", trControl=control)
modelsvm
modelsvm$finalModel
preddwd=predict(modelsvm,newdata = test)
preddwd
confusionMatrix(preddwd,as.factor(test$Overweight),mode="everything")
plot(varImp(modelsvm, scale=T))
###############################################################################
######DECISION TREE CLASSIFIER - CART
set.seed(123)
fit.DT <- train(factor(Overweight)~., data = train, method = "rpart",
                #metric = metric,
                trControl = control)
fit.DT
preddt=predict(fit.DT,newdata = test)
preddt
confusionMatrix(preddt,as.factor(test$Overweight),mode="everything")
plot(fit.DT$finalModel, uniform=TRUE,
     main="Classification Tree")
text(fit.DT$finalModel, use.n.=TRUE, all=TRUE, cex=.8)
suppressMessages(library(rattle))

fancyRpartPlot(fit.DT$finalModel)

library(rpart.plot)

rpart.plot(fit.DT$finalModel)
plot(varImp(fit.DT, scale=T))
##########################################################################
###Random Forest
set.seed(7)
Overweight=factor(Overweight)
modelrf <- train(factor(Overweight)~., data=train, method="rf", trControl=control)
modelrf
modelrf$finalModel
predrf=predict(modelrf,newdata = test)
predrf
confusionMatrix(predrf,as.factor(test$Overweight),mode="everything")
modelrf$results
plot(varImp(modelrf, scale=T))

#########################LINEAR DISCRIMINANT ANALYSIS (LDA)####################################
set.seed(7)
modellda <- train(factor(Overweight) ~., data=train, method="lda", trControl=control)
modellda
predlda=predict(modellda,newdata = test)
predlda
confusionMatrix(predlda,as.factor(test$Overweight),mode="everything")
plot(varImp(modellda, scale=T))
#########################NAIVE BAYES####################################
set.seed(7)
Overweight=factor(Overweight)
modelnb <- train(factor(Overweight) ~., data=train, method="naive_bayes", trControl=control)
modelnb
prednb=predict(modelnb,newdata = test)
prednb
confusionMatrix(prednb,as.factor(test$Overweight),mode="everything")
plot(varImp(modelnb, scale=T))

#########################KNN ####################################
set.seed(7)
Overweight=factor(Overweight)
modelknn <- train(factor(Overweight) ~., data=train, method="knn", trControl=control)
modelknn
predknn=predict(modelknn,newdata = test)
predknn
confusionMatrix(predknn,as.factor(test$Overweight),mode="everything")
plot(varImp(modelknn, scale=T))

#########################GLM ####################################
set.seed(7)
Overweight=factor(Overweight)
modelglm <- train(factor(Overweight) ~., data=train, method="glm", trControl=control)
modelglm
predglm=predict(modelglm,newdata = test)
predglm
confusionMatrix(predglm,as.factor(test$Overweight),mode="everything")
plot(varImp(modelglm, scale=T))
#########################Gradient boosting machine####################################
set.seed(7)
Overweight=factor(Overweight)
modelgbm <- train(factor(Overweight) ~., data=train, method="gbm", trControl=control)
modelgbm
predgbm=predict(modelgbm,newdata = test)
predgbm
confusionMatrix(predgbm,as.factor(test$Overweight),mode="everything")
plot(varImp(modelgbm, scale=T))

############################################################################
#####LOGISTIC REGRESSION
mymodel <- glm(factor(Overweight) ~ .,data = train, family = 'binomial')
summary(mymodel)
as.factor(Overweight)
#####PREDICT
p1 <- predict(mymodel, test, type = 'response')
head(p1)
####CONFUSION MATRIX
pred1 <- ifelse(p1>0.5, 1, 0)
tab1 <- table(Predicted = pred1, Actual = as.factor(test$Overweight))
tab1
#####eRROR
1 - sum(diag(tab1))/sum(tab1)
sum(diag(tab1))/sum(tab1)
plot(varImp(mymodel, scale=T))
####### COMPARE ALL MODELS TOGETHER
############################################################################
# collect resamples and list the models here. Use only 7 models
results <- resamples(list(KNN=modelknn, GLM=modelglm, RF=modelrf, DT=fit.DT, NB=modelnb, SVM=modelsvm, LDA=modellda))

# summarize the distributions
summary(results)
# boxplots of results
bwplot(results)
# dot plots of results
dotplot(results)
scales <- list(x=list(relation="free"), y=list(relation="free"))
densityplot(results, scales=scales, pch = "|")
splom(results)
#################COMPARE RESULTS
# difference in model predictions
diffs <- diff(results)
# summarize p-values for pair-wise comparisons
summary(diffs)
##############################################################################
# put in paper:
#We can see a table of pair-wise statistical significance scores. The lower diagonal of the table shows p-values for the null hypothesis (distributions are the same), smaller is better.
# The upper diagonal of the table shows the estimated difference between the algorithms. If we think that RF is the most accurate model from looking at the previous graphs, we can get an estimate of how much better than specific other models in terms of absolute accuracy.
#These scores can help with any accuracy claims we might want to make between specific algorithms.


######################################################################
################################################################################
#AFTER FEATURE SELECTION
# train the SVM model
set.seed(7)
Overweight=factor(Overweight)
modelsvma <- train(factor(Overweight)~ Sex + aindex + Wgain + Dwelling + Med.aid + Geotype + Agric_anim +FH.Diab, data= train, method="svmRadial", trControl=control)
modelsvma
modelsvma$finalModel
preddwda=predict(modelsvma,newdata = test)
preddwda
confusionMatrix(preddwda,as.factor(test$Overweight),mode="everything")
plot(varImp(modelsvma, scale=T))
###############################################################################
######DECISION TREE CLASSIFIER - CART
set.seed(123)
fit.DTa <- train(factor(Overweight)~Sex + aindex + Wgain + Dwelling + Med.aid + Geotype + Agric_anim +FH.Diab, data = train, method = "rpart",
                #metric = metric,
                trControl = control)
fit.DTa
preddta=predict(fit.DTa,newdata = test)
preddta
confusionMatrix(preddta,as.factor(test$Overweight),mode="everything")
plot(fit.DTa$finalModel, uniform=TRUE,
     main="Classification Tree")
text(fit.DTa$finalModel, use.n.=TRUE, all=TRUE, cex=.8)
suppressMessages(library(rattle))

fancyRpartPlot(fit.DTa$finalModel)

library(rpart.plot)

rpart.plot(fit.DTa$finalModel)
plot(varImp(fit.DTa, scale=T))
##########################################################################
###Random Forest
set.seed(7)
Overweight=factor(Overweight)
modelrfa <- train(factor(Overweight)~Sex + aindex + Wgain + Dwelling + Med.aid + Geotype + Agric_anim +FH.Diab, data=train, method="rf", trControl=control)
modelrfa
modelrfa$finalModel
predrfa=predict(modelrfa,newdata = test)
predrfa
confusionMatrix(predrfa,as.factor(test$Overweight),mode="everything")
modelrfa$results
plot(varImp(modelrfa, scale=T))

#########################LINEAR DISCRIMINANT ANALYSIS (LDA)####################################
set.seed(7)
modelldaa <- train(factor(Overweight) ~Sex + aindex + Wgain + Dwelling + Med.aid + Geotype + Agric_anim +FH.Diab, data=train, method="lda", trControl=control)
modelldaa
predldaa=predict(modelldaa,newdata = test)
predldaa
confusionMatrix(predldaa,as.factor(test$Overweight),mode="everything")
plot(varImp(modelldaa, scale=T))
#########################NAIVE BAYES####################################
set.seed(7)
Overweight=factor(Overweight)
modelnba <- train(factor(Overweight) ~Sex + aindex + Wgain + Dwelling + Med.aid + Geotype + Agric_anim +FH.Diab, data=train, method="naive_bayes", trControl=control)
modelnba
prednba=predict(modelnba,newdata = test)
prednba
confusionMatrix(prednba,as.factor(test$Overweight),mode="everything")
plot(varImp(modelnba, scale=T))
#########################KNN ####################################
set.seed(7)
Overweight=factor(Overweight)
modelknna <- train(factor(Overweight) ~Sex + aindex + Wgain + Dwelling + Med.aid + Geotype + Agric_anim +FH.Diab, data=train, method="knn", trControl=control)
modelknna
predknna=predict(modelknna,newdata = test)
predknna
confusionMatrix(predknna,as.factor(test$Overweight),mode="everything")
plot(varImp(modelknna, scale=T))
#########################GLM ####################################
set.seed(7)
Overweight=factor(Overweight)
modelglma <- train(factor(Overweight) ~Sex + aindex + Wgain + Dwelling + Med.aid + Geotype + Agric_anim +FH.Diab, data=train, method="glm", trControl=control)
modelglma
predglma=predict(modelglma,newdata = test)
predglma
confusionMatrix(predglma,as.factor(test$Overweight),mode="everything")
plot(varImp(modelglma, scale=T))
#########################Gradient boosting machine####################################
set.seed(7)
Overweight=factor(Overweight)
modelgbma <- train(factor(Overweight) ~Sex + aindex + Wgain + Dwelling + Med.aid + Geotype + Agric_anim +FH.Diab, data=train, method="gbm", trControl=control)
modelgbma
predgbma=predict(modelgbma,newdata = test)
predgbma
confusionMatrix(predgbma,as.factor(test$Overweight),mode="everything")
plot(varImp(modelgbma, scale=T))
############################################################################
#####LOGISTIC REGRESSION
mymodela <- glm(factor(Overweight) ~ Sex + aindex + Wgain + Dwelling + Med.aid + Geotype + Agric_anim +FH.Diab,data = train, family = 'binomial')
summary(mymodela)
as.factor(Overweight)
#####PREDICT
p1a <- predict(mymodela, test, type = 'response')
head(p1a)
####CONFUSION MATRIX
pred1a <- ifelse(p1>0.5, 1, 0)
tab1a <- table(Predicted = pred1a, Actual = as.factor(test$Overweight))
tab1a
#####eRROR
1 - sum(diag(tab1a))/sum(tab1a)
sum(diag(tab1a))/sum(tab1a)
plot(varImp(mymodela, scale=T))

####### COMPARE ALL MODELS TOGETHER
############################################################################
# collect resamples and list the models here. Use only 7 models
resultsa <- resamples(list(KNN=modelknna, GLM=modelglma, RF=modelrfa, DT=fit.DTa, NB=modelnba, SVM=modelsvma, LDA=modelldaa))

# summarize the distributions
summary(resultsa)
# boxplots of results
bwplot(resultsa)
# dot plots of results
dotplot(resultsa)
scalesa <- list(x=list(relation="free"), y=list(relation="free"))
densityplot(resultsa, scales=scalesa, pch = "|")
splom(resultsa)
#################COMPARE RESULTS
# difference in model predictions
diffsa <- diff(resultsa)
# summarize p-values for pair-wise comparisons
summary(diffsa)
##############################################################################





####BOOK cHAPTER#################################################
##############################################################################
setwd( "C:/Users/rsewpaul/Documents/Ronel Cdrive new/BRICS anemia grant/P4 ML obesity in adolesc")
getwd()

#####Input Data from Excel into R
#input dataframe in R called Ldata
ldata = read.csv("Lung.csv")
ldata
attach(ldata)
head(ldata)
names(ldata)
dim(ldata)
summary(ldata)
set.seed(123)

library(Boruta)
borC = Boruta(factor(LUNG_CANCER)~., data = ldata, doTrace = 2, maxRuns=500)
print(borC)
par(pty='m')
plot(borC,las=2,cex.axis=0.7)

####Plot Target Variable
plot(factor(LUNG_CANCER),ylim=c(0,300),col=c("blue",'red')) #names=c("Not overweight", "Overweight"),ylim=c(0,600),col=c("blue","red"),ylab="Number of Observations", xlab="Weight Status")
box()

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
library(naivebayes)

####################################################################
###DATA PARTITION
##################################################################
ind=sample(2, nrow(ldata),replace=T,prob=c(0.70,0.30))
train=ldata[ind==1,]
test= ldata[ind==2,]
dim(train)
dim(test)
###########################################################
#######Alternatively
###########################################################
#inTrain=createDataPartition(y=INFY,p=0.75,list=FALSE)
#traing=fdata[inTrain,]
#testg=fdata[-inTrain,]
#attach(traing)
#attach(testg)
#dim(traing)
#dim(testg)
####################################################################
#########################################################################
# Prepare training scheme
control <- trainControl(method="repeatedcv", number=100, repeats=5)
######################################################################

#####################################################################
##Start Training the models
#BEFORE FEATURE SELECTION
# train the SVM model
set.seed(7)

modelsvm <- train(factor(LUNG_CANCER)~., data= train, method="svmRadial", trControl=control)
modelsvm
modelsvm$finalModel
preddwd=predict(modelsvm,newdata = test)
preddwd
confusionMatrix(preddwd,as.factor(test$LUNG_CANCER),mode="everything")
plot(varImp(modelsvm, scale=T))
###############################################################################
######DECISION TREE CLASSIFIER - CART
set.seed(123)
fit.DT <- train(factor(LUNG_CANCER)~., data = train, method = "rpart",
                #metric = metric,
                trControl = control)
fit.DT
preddt=predict(fit.DT,newdata = test)
preddt
confusionMatrix(preddt,as.factor(test$LUNG_CANCER),mode="everything")
plot(fit.DT$finalModel, uniform=TRUE,
     main="Classification Tree")
text(fit.DT$finalModel, use.n.=TRUE, all=TRUE, cex=.8)
suppressMessages(library(rattle))

fancyRpartPlot(fit.DT$finalModel)

library(rpart.plot)

rpart.plot(fit.DT$finalModel)
plot(varImp(fit.DT, scale=T))
##########################################################################
###Random Forest
set.seed(7)
#Overweight=factor(Overweight)
modelrf <- train(factor(LUNG_CANCER)~., data=train, method="rf", trControl=control)
modelrf
modelrf$finalModel
predrf=predict(modelrf,newdata = test)
predrf
confusionMatrix(predrf,as.factor(test$LUNG_CANCER),mode="everything")
modelrf$results
plot(varImp(modelrf, scale=T))

#########################LINEAR DISCRIMINANT ANALYSIS (LDA)####################################
set.seed(7)
modellda <- train(factor(LUNG_CANCER) ~., data=train, method="lda", trControl=control)
modellda
predlda=predict(modellda,newdata = test)
predlda
confusionMatrix(predlda,as.factor(test$LUNG_CANCER),mode="everything")
plot(varImp(modellda, scale=T))
#########################NAIVE BAYES####################################
set.seed(7)
modelnb <- train(factor(LUNG_CANCER) ~., data=train, method="naive_bayes", trControl=control)
modelnb
prednb=predict(modelnb,newdata = test)
prednb
confusionMatrix(prednb,as.factor(test$LUNG_CANCER),mode="everything")
plot(varImp(modelnb, scale=T))

#########################KNN ####################################
set.seed(7)

modelknn <- train(factor(LUNG_CANCER) ~., data=train, method="knn", trControl=control)
modelknn
predknn=predict(modelknn,newdata = test)
predknn
confusionMatrix(predknn,as.factor(test$LUNG_CANCER),mode="everything")
plot(varImp(modelknn, scale=T))

#########################GLM ####################################
set.seed(7)
#Overweight=factor(Overweight)
modelmars <- train(factor(LUNG_CANCER) ~., data=train, method="earth", trControl=control)
modelmars
predmars =predict(modelmars,newdata = test)
predmars
confusionMatrix(predmars,as.factor(test$LUNG_CANCER), mode="everything")
plot(varImp(modelmars, scale=T))
#########################Penalized Logistic Regression####################################
set.seed(7)
#Overweight=factor(Overweight)
modelplr <- train(factor(LUNG_CANCER) ~., data=train, method="plr", trControl=control)
modelplr
predplr =predict(modelplr,newdata = test)
predplr
confusionMatrix(predplr,as.factor(test$LUNG_CANCER),mode="everything")
plot(varImp(modelplr, scale=T))

############################################################################

#########################Gradient boosting machine####################################
set.seed(7)
Overweight=factor(Overweight)
modelgbm <- train(factor(LUNG_CANCER) ~., data=train, method="gbm", trControl=control)
modelgbm
predgbm=predict(modelgbm,newdata = test)
predgbm
confusionMatrix(predgbm,as.factor(test$LUNG_CANCER),mode="everything")
plot(varImp(modelgbm, scale=T))

############################################################################


#########################Generalized Additive Model####################################
set.seed(7)
Overweight=factor(Overweight)
modelgam <- train(factor(LUNG_CANCER) ~., data=train, method="gam", trControl=control)
modelgam
predgam=predict(modelgam,newdata = test)
predgam
confusionMatrix(predgam,as.factor(test$LUNG_CANCER),mode="everything")
plot(varImp(modelgam, scale=T))

############################################################################













#####LOGISTIC REGRESSION
mymodel <- glm(factor(Overweight) ~ .,data = train, family = 'binomial')
summary(mymodel)
as.factor(Overweight)
#####PREDICT
p1 <- predict(mymodel, test, type = 'response')
head(p1)
####CONFUSION MATRIX
pred1 <- ifelse(p1>0.5, 1, 0)
tab1 <- table(Predicted = pred1, Actual = as.factor(test$Overweight))
tab1
#####eRROR
1 - sum(diag(tab1))/sum(tab1)
sum(diag(tab1))/sum(tab1)
plot(varImp(mymodel, scale=T))
####### COMPARE ALL MODELS TOGETHER
############################################################################
# collect resamples and list the models here. Use only 7 models
results <- resamples(list(KNN=modelknn, MARS=modelmars,PLR=modelplr,GAM=modelgam,GBM=modelgbm, RF=modelrf, DT=fit.DT, NB=modelnb, SVM=modelsvm, LDA=modellda))

# summarize the distributions
summary(results)
# boxplots of results
bwplot(results)
# dot plots of results
dotplot(results)
scales <- list(x=list(relation="free"), y=list(relation="free"))
densityplot(results, scales=scales, pch = "|")
splom(results)
#################COMPARE RESULTS
# difference in model predictions
diffs <- diff(results)
# summarize p-values for pair-wise comparisons
summary(diffs)
##############################################################################
# put in paper:
#We can see a table of pair-wise statistical significance scores. The lower diagonal of the table shows p-values for the null hypothesis (distributions are the same), smaller is better.
# The upper diagonal of the table shows the estimated difference between the algorithms. If we think that RF is the most accurate model from looking at the previous graphs, we can get an estimate of how much better than specific other models in terms of absolute accuracy.
#These scores can help with any accuracy claims we might want to make between specific algorithms.


######################################################################
################################################################################
#AFTER FEATURE SELECTION
# train the SVM model
set.seed(7)
Overweight=factor(Overweight)
modelsvma <- train(factor(Overweight)~ Sex + aindex + Wgain + Dwelling + Med.aid + Geotype + Agric_anim +FH.Diab, data= train, method="svmRadial", trControl=control)
modelsvma
modelsvma$finalModel
preddwda=predict(modelsvma,newdata = test)
preddwda
confusionMatrix(preddwda,as.factor(test$Overweight),mode="everything")
plot(varImp(modelsvma, scale=T))
###############################################################################
######DECISION TREE CLASSIFIER - CART
set.seed(123)
fit.DTa <- train(factor(Overweight)~Sex + aindex + Wgain + Dwelling + Med.aid + Geotype + Agric_anim +FH.Diab, data = train, method = "rpart",
                 #metric = metric,
                 trControl = control)
fit.DTa
preddta=predict(fit.DTa,newdata = test)
preddta
confusionMatrix(preddta,as.factor(test$Overweight),mode="everything")
plot(fit.DTa$finalModel, uniform=TRUE,
     main="Classification Tree")
text(fit.DTa$finalModel, use.n.=TRUE, all=TRUE, cex=.8)
suppressMessages(library(rattle))

fancyRpartPlot(fit.DTa$finalModel)

library(rpart.plot)

rpart.plot(fit.DTa$finalModel)
plot(varImp(fit.DTa, scale=T))
##########################################################################
###Random Forest
set.seed(7)
Overweight=factor(Overweight)
modelrfa <- train(factor(Overweight)~Sex + aindex + Wgain + Dwelling + Med.aid + Geotype + Agric_anim +FH.Diab, data=train, method="rf", trControl=control)
modelrfa
modelrfa$finalModel
predrfa=predict(modelrfa,newdata = test)
predrfa
confusionMatrix(predrfa,as.factor(test$Overweight),mode="everything")
modelrfa$results
plot(varImp(modelrfa, scale=T))

#########################LINEAR DISCRIMINANT ANALYSIS (LDA)####################################
set.seed(7)
modelldaa <- train(factor(Overweight) ~Sex + aindex + Wgain + Dwelling + Med.aid + Geotype + Agric_anim +FH.Diab, data=train, method="lda", trControl=control)
modelldaa
predldaa=predict(modelldaa,newdata = test)
predldaa
confusionMatrix(predldaa,as.factor(test$Overweight),mode="everything")
plot(varImp(modelldaa, scale=T))
#########################NAIVE BAYES####################################
set.seed(7)
Overweight=factor(Overweight)
modelnba <- train(factor(Overweight) ~Sex + aindex + Wgain + Dwelling + Med.aid + Geotype + Agric_anim +FH.Diab, data=train, method="naive_bayes", trControl=control)
modelnba
prednba=predict(modelnba,newdata = test)
prednba
confusionMatrix(prednba,as.factor(test$Overweight),mode="everything")
plot(varImp(modelnba, scale=T))
#########################KNN ####################################
set.seed(7)
Overweight=factor(Overweight)
modelknna <- train(factor(Overweight) ~Sex + aindex + Wgain + Dwelling + Med.aid + Geotype + Agric_anim +FH.Diab, data=train, method="knn", trControl=control)
modelknna
predknna=predict(modelknna,newdata = test)
predknna
confusionMatrix(predknna,as.factor(test$Overweight),mode="everything")
plot(varImp(modelknna, scale=T))
#########################GLM ####################################
set.seed(7)
Overweight=factor(Overweight)
modelglma <- train(factor(Overweight) ~Sex + aindex + Wgain + Dwelling + Med.aid + Geotype + Agric_anim +FH.Diab, data=train, method="glm", trControl=control)
modelglma
predglma=predict(modelglma,newdata = test)
predglma
confusionMatrix(predglma,as.factor(test$Overweight),mode="everything")
plot(varImp(modelglma, scale=T))
#########################Gradient boosting machine####################################
set.seed(7)
Overweight=factor(Overweight)
modelgbma <- train(factor(Overweight) ~Sex + aindex + Wgain + Dwelling + Med.aid + Geotype + Agric_anim +FH.Diab, data=train, method="gbm", trControl=control)
modelgbma
predgbma=predict(modelgbma,newdata = test)
predgbma
confusionMatrix(predgbma,as.factor(test$Overweight),mode="everything")
plot(varImp(modelgbma, scale=T))
############################################################################
#####LOGISTIC REGRESSION
mymodela <- glm(factor(Overweight) ~ Sex + aindex + Wgain + Dwelling + Med.aid + Geotype + Agric_anim +FH.Diab,data = train, family = 'binomial')
summary(mymodela)
as.factor(Overweight)
#####PREDICT
p1a <- predict(mymodela, test, type = 'response')
head(p1a)
####CONFUSION MATRIX
pred1a <- ifelse(p1>0.5, 1, 0)
tab1a <- table(Predicted = pred1a, Actual = as.factor(test$Overweight))
tab1a
#####eRROR
1 - sum(diag(tab1a))/sum(tab1a)
sum(diag(tab1a))/sum(tab1a)
plot(varImp(mymodela, scale=T))

####### COMPARE ALL MODELS TOGETHER
############################################################################
# collect resamples and list the models here. Use only 7 models
resultsa <- resamples(list(KNN=modelknn, GLM=modelglma, RF=modelrf, DT=fit.DTa, NB=modelnba, SVM=modelsvma, LDA=modelldaa))

# summarize the distributions
summary(resultsa)
# boxplots of results
bwplot(resultsa)
# dot plots of results
dotplot(resultsa)
scalesa <- list(x=list(relation="free"), y=list(relation="free"))
densityplot(resultsa, scales=scalesa, pch = "|")
splom(resultsa)
#################COMPARE RESULTS
# difference in model predictions
diffsa <- diff(resultsa)
# summarize p-values for pair-wise comparisons
summary(diffsa)