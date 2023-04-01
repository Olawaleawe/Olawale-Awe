#This Code will introduce you to using the caret #package for training machine learning models. The package caret is used for training both classification and regression models.
# We will consider simple cases of classification and regression using caret package. Feel free to adapt the codes to your datasets. 
getwd()
####Load packages
library(caret)
library(mlbench)
library(psych)
library(MASS)
#####LOAD DATASET
#data()###view various inbuilt data you can practice with in R
###Load data
lung=read.csv('store.csv', header=T)
lung
dim(lung)
names(lung)
attach(lung)
head(lung)
table(factor(LUNG_CANCER))


lung=read.csv('odata2.csv', header=T)
lung
dim(lung)
names(lung)
attach(lung)
head(lung)
table(factor(SBPFINAL))
#Check that there are no missing values
apply(lung,2,function(x) sum(is.na(x)))
###################################################
######FEATURE SELECTION BEFORE CLASSIFICATION
### 1. Boruta algorithm to determine the best variables for the model. Include the important variables in the final model.
library(Boruta)
borC = Boruta(factor(LUNG_CANCER)~., data = lung, doTrace = 2, maxRuns=25)
print(borC)
par(pty='m')
plot(borC,las=2,cex.axis=0.7)
plotImpHistory(borC)
bor1=TentativeRoughFix(borC)
attStats(bor1)
###############################################
#####################PARTITION THE DATA########################################
ind <- sample(2, nrow(lung), replace = T, prob = c(0.7, 0.3))
train <- lung[ind==1,]
test <- lung[ind==2,]
# prepare training scheme for cross-validation
control <- trainControl(method="repeatedcv", number=10, repeats=5)
###Training the Models
################################################
###################################################
# train the SVM model
set.seed(7)
modelSvm <- train(factor(LUNG_CANCER)~., data=train, method="svmRadial", trControl=control, preProc=('scale'),tuneLength=5)
modelSvm
predSvm=predict(modelSvm,newdata = test)
predSvm
cms=confusionMatrix(predSvm,as.factor(test$LUNG_CANCER))
cms
cms$table
fourfoldplot(cms$table, color = c("red", "blue"))
######################################################################
###################################################
####Logistic Regression
################################################
set.seed(7)
modelglm <- train(factor(LUNG_CANCER)~.,
                  data = train, 
                  method = "plr",
                  #metric = metric,
                  trControl = control)
modelglm
predglm=predict(modelglm,newdata = test)
predglm
cml=confusionMatrix(predglm,as.factor(test$LUNG_CANCER),mode='everything')
cml
cml$table
fourfoldplot(cml$table, color = c("red", "blue"))
#####################################################
#######KNN ############################
###set.seed(7)
fit.knn <- train(factor(LUNG_CANCER) ~.,
                 data = train , 
                 method = "knn",
                 #metric = 'ROC',
                 trControl = control)
fit.knn
predknn=predict(fit.knn,newdata = test)
predknn
cmK= confusionMatrix(predknn,as.factor(test$LUNG_CANCER))

cmK$table
fourfoldplot(cmK$table, color = c("red", "blue"))
#########################################################
#####Random Forest
set.seed(7)
model.rf <- train(factor(LUNG_CANCER)~.,
                  data = train, 
                  method = "rf",
                  #metric = 'ROC',
                  trControl = control)
model.rf
predrf=predict(model.rf,newdata = test)
predrf
cmr=confusionMatrix(predrf,as.factor(test$LUNG_CANCER))
cmr
cmr$table
fourfoldplot(cmr$table, color = c("red", "blue"))

varImp(model.rf, scale=T)
####Plot your result
plot(varImp(model.rf, scale=T))
##################################################
##############################################
# train the GBM model
set.seed(7)
modelGbm <- train(factor(LUNG_CANCER)~., data=train, method="gbm", trControl=control, verbose=FALSE)
modelGbm
predGbm=predict(modelGbm,newdata = test)
predGbm
cmg=confusionMatrix(predGbm,as.factor(test$LUNG_CANCER))
cmg
cmg$table
fourfoldplot(cmg$table, color = c("red", "blue"))


#######################################################
####Naive Bayes
modelnb <- train(factor(LUNG_CANCER)~., data=train, method="naive_bayes", trControl=control)
modelnb
prednb=predict(modelnb,newdata = test)
prednb
cmn=confusionMatrix(prednb,as.factor(test$LUNG_CANCER))
cmn
cmn$table
fourfoldplot(cmn$table, color = c("red", "blue"))
######################################################
##################LDA###################
set.seed(7)
model.lda <- train(factor(LUNG_CANCER)~.,
                   data = train, 
                   method = "lda",
                   #metric = metric,
                   trControl = control)
model.lda
predlda=predict(model.lda,newdata = test)
predlda
cmld=confusionMatrix(predlda,as.factor(test$LUNG_CANCER))
cmld
cmld$table
fourfoldplot(cmld$table, color = c("red", "blue"))
######################################################
######################################################################
#####Decision Tree
set.seed(7)
modelDT <- train(factor(LUNG_CANCER) ~.,
                 data = train, 
                 method = "rpart",
                 #metric = metric,
                 trControl = control)
modelDT
predDT=predict(modelDT,newdata = test)
predDT
cmd=confusionMatrix(predDT,as.factor(test$LUNG_CANCER))
cmd
cmd$table
fourfoldplot(cmd$table, color = c("red", "blue"))

#######################################################

# train the bagging model
set.seed(7)
modelbg <- train(factor(LUNG_CANCER)~., data=train, method="treebag", trControl=control)
modelbg
predbg=predict(modelbg,newdata = test)
predbg
cmb=confusionMatrix(predbg,as.factor(test$LUNG_CANCER), mode='everything')
cmb
cmb$table
fourfoldplot(cmb$table, color = c("red", "#6699CC"))
####Perform inference to determing most important variaables used for classification
varImp(modelbg, scale=T)
####Plot your result
plot(varImp(modelbg, scale=T))
###The graph shows that glucose level is the most important feature for classifying diabetic patients. 
#You can try this for other models

##########################################################################################################
####Neural Network
modelnn <- train(factor(LUNG_CANCER)~., data=train, method="nnet", trControl=control,hidden=c(5,3),linear.output=T)
modelnn
plot(modelnn)
prednn=predict(modelnn,newdata = test)
prednn
cmn=confusionMatrix(prednn,as.factor(test$LUNG_CANCER))
cmn
cmn$table
fourfoldplot(cmn$table, color = c("red", "#6699CC"))
######################################################################
##########################################################################################################
#### XTREME GRADIENT BOOSTING
#modelxb <- train(factor(LUNG_CANCER)~., data=train, method="xgbTree", trControl=control,hidden=c(5,3),linear.output=T)
#modelxb
#plot(modelxb)
#predxb=predict(modelxb,newdata = test)
#predxb
#cmxb=confusionMatrix(predxb,as.factor(test$LUNG_CANCER))
#cmxb
#cmxb$table
#fourfoldplot(cmxb$table, color = c("red", "#6699CC"))
################################################
##############################################
######LVQ###########################################

modellvq <- train(factor(LUNG_CANCER)~., data=train, method="lvq", trControl=control)
modellvq
plot(modellvq)
predllvq=predict(modellvq,newdata = test)
predllvq
cmlv=confusionMatrix(predllvq,as.factor(test$LUNG_CANCER))
cmlv
cmlv$table
fourfoldplot(cmlv$table, color = c("red", "#6699CC"))
################################################
##############################################
###############################################
# Collect all resamples and compare MODELS
results <- resamples(list(LVQ=modellvq, Bagging=modelbg,GBM=modelGbm,NB=modelnb,NN=modelnn,RF=model.rf,LDA=model.lda,SVM=modelSvm,GLM=modelglm,DT=modelDT,KNN=fit.knn))
# summarize the distributions of the results 
summary(results)
# boxplots of results
bwplot(results)
# dot plots of results
dotplot(results)
##################################################
##################################################
#####Comparison of the Confusion Matrices