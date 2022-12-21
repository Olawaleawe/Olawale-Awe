########SUPERVISED MACHINE LEARNING WITH CARET PACKAGE##########################
#############################################################
library(caret)
library(mlbench)
library(psych)
#####PRACTICE MACHINE LEARNING WITH IN-BUILT DATASET-PimaIndiansDiabetes####
data()
# load the inbuilt dataset
data(PimaIndiansDiabetes)
attach(PimaIndiansDiabetes)
pdata=PimaIndiansDiabetes
#####################PARTITION THE DATA########################################
ind <- sample(2, nrow(pdata), replace = T, prob = c(0.7, 0.3))
train <- pdata[ind==1,]
test <- pdata[ind==2,]
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=5)
# train the LVQ model
set.seed(7)
modelLvq <- train(factor(diabetes)~., data=train, method="lvq", trControl=control)
modelLvq
predLVq=predict(modelLvq,newdata = test)
predLVq
confusionMatrix(predLVq,as.factor(test$diabetes))
# train the GBM model
set.seed(7)
modelGbm <- train(diabetes~., data=train, method="gbm", trControl=control, verbose=FALSE)
modelGbm
predGbm=predict(modelGbm,newdata = test)
predGbm
confusionMatrix(predGbm,as.factor(test$diabetes))
# train the SVM model
set.seed(7)
modelSvm <- train(diabetes~., data=train, method="svmRadial", trControl=control)
modelSvm
predSvm=predict(modelSvm,newdata = test)
predSvm
confusionMatrix(predSvm,as.factor(test$diabetes))
######################################################################
modelnb <- train(diabetes~., data=train, method="naive_bayes", trControl=control)
modelnb
prednb=predict(modelnb,newdata = test)
prednb
confusionMatrix(prednb,as.factor(test$diabetes))
######################################################################
set.seed(7)
modelDT <- train(diabetes~.,
                 data = train, 
                 method = "rpart",
                 #metric = metric,
                 trControl = control)
modelDT
predDT=predict(modelDT,newdata = test)
predDT
confusionMatrix(predDT,as.factor(test$diabetes))
set.seed(7)
fit.knn <- train(diabetes~.,
                 data = train , 
                 method = "knn",
                 #metric = metric,
                 trControl = control)
fit.knn
predknn=predict(fit.knn,newdata = test)
predknn
confusionMatrix(predknn,as.factor(test$diabetes))
# collect resamples
results <- resamples(list(LVQ=modelLvq, GBM=modelGbm, NB=modelnb, SVM=modelSvm, DT=modelDT,KNN=fit.knn))
# summarize the distributions of the results 
summary(results)
# boxplots of results
bwplot(results)
# dot plots of results
dotplot(results)
#####################################################################
#####################################################################
################UNSUPERVISED MACHINE LEARNING########################
gdp = read.csv("realGDP.csv",header=T)
gdp
dim(gdp)
gdp = gdp[ ,-c(1)]
gdp
dim(gdp)
describe(gdp)
#####Clustering###################
# Loading the data set
df <- scale(gdp) # Scaling the data

dft <- as.data.frame(t(as.matrix(df)))
dft
kmeans(dft, 3, iter.max = 10, nstart = 1)

#install.packages("factoextra")
library(factoextra)
# Compute k-means with k = 4
set.seed(123)
km.res <- kmeans(dft, 3, nstart = 25)

km.res$cluster

head(km.res$cluster, 3)

km.res$centers

fviz_cluster(km.res, dft,
             palette = "Set2", ggtheme = theme_minimal())

# Show text only
fviz_cluster(km.res, dft, geom = "text")

# PAM clustering
# ++++++++++++++++++++
require(cluster)
pam.res <- pam(dft, 3)
# Visualize pam clustering
fviz_cluster(pam.res, geom = "text", ellipse.type = "norm")

# Hierarchical clustering
# ++++++++++++++++++++++++
# Use hcut() which compute hclust and cut the tree
hc.cut <- hcut(dft, k = 3, hc_method = "complete", cex=.7)
# Visualize dendrogram
fviz_dend(hc.cut, show_labels = T, rect = TRUE, cex=.8)###Dendrogram
# Visualize cluster
fviz_cluster(hc.cut, ellipse.type = "convex")