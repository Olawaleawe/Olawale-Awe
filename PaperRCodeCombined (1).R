library(MASS)
library(tidyverse)
library(leaps)
library(egg)
library(glmnet)
library(caret)
library(boot)
library(plotmo)
library(kableExtra)
library(tree)
library(randomForest)
library(gbm)
library(pROC)
library(corrplot)
library(earth)
library(rpart)
library(sda)
library(hda)

#STEP 0:Read in the data into the R enviroment:
odata <- read_csv("C:/Users/413/Desktop/OAE/ADA GLOBAL CONCEPT/odata.csv")
#View(odata)
#head(odata)
#dim(odata)

#STEP 1:Split the data into training and test set:
# Split the data into training (70%) and test set (30%)
set.seed(2503)
data <- model.matrix( ~ . , data = odata) %>% 
  as_tibble() %>% 
  dplyr::select(-"(Intercept)") 

trainIndex = sample(1:nrow(data), size = round(0.7*nrow(data)), replace=FALSE)
train = data[trainIndex ,]
test = data[-trainIndex ,]
table(train$overweight)
table(test$overweight)

#STEP 2: Best Subset Selection 
best_subset <- regsubsets(overweight ~ ., data = train, nvmax = 31) ##now considers all 31 variables
best_subset_sum <- summary(best_subset)
measures <- c("rsq", "adjr2", "cp", "bic")
our_names <- c("R2", "Adjusted R2", "Cp", "BIC")
size_seq <- 1:length(best_subset_sum$rsq)
my_plots <- NULL
for(mea_ind in seq_along(measures)){
  dat <- data.frame(d = size_seq, val = best_subset_sum[[measures[mea_ind]]])
  my_plots[[mea_ind]] <- ggplot(dat, mapping = aes(x = d, y = val)) + geom_point() + geom_line() +
    theme_bw() +
    ggtitle(our_names[mea_ind])
}
grid.arrange(grobs = my_plots, ncol = 2, top = "Figure 1. Optimal model for each size")

best_ind <- which.min(best_subset_sum$bic) #best index using bic
best_coef <- coef(best_subset, best_ind)
tr_x <- train %>% dplyr::select(names(best_coef)[-1])
tr_pred <- cbind(1, as.matrix(tr_x)) %*% best_coef
tr_error.best <- mean((tr_pred - train$overweight)^2)
te_x <- test %>% dplyr::select(names(best_coef)[-1])
te_pred <- cbind(1, as.matrix(te_x)) %*% best_coef
te_error.best <- mean((te_pred - test$overweight)^2)


##STEP 3: Forward Stepwise Selection 
forward_fit <- regsubsets(overweight ~ ., data = train, nvmax = 31, method = "forward")
forward_sum <- summary(forward_fit)
best_ind <- which.min(forward_sum$bic)
best_ind <- which.min(forward_sum$bic) #best index using bic
best_coef <- coef(forward_fit, best_ind)
tr_x <- train %>% dplyr::select(names(best_coef)[-1])
tr_pred <- cbind(1, as.matrix(tr_x)) %*% best_coef
tr_error.forward <- mean((tr_pred - train$overweight)^2)
te_x <- test %>% dplyr::select(names(best_coef)[-1])
te_pred <- cbind(1, as.matrix(te_x)) %*% best_coef
te_error.forward <- mean((te_pred - test$overweight)^2)

#STEP 4: Backward Stepwise Selection 
backward_fit <- regsubsets(overweight ~ ., data = train, nvmax = 31, method = "backward")
backward_sum <- summary(backward_fit)
best_ind <- which.min(backward_sum$bic)
best_ind <- which.min(backward_sum$bic) #best index using bic
best_coef <- coef(backward_fit, best_ind)
tr_x <- train %>% dplyr::select(names(best_coef)[-1])
tr_pred <- cbind(1, as.matrix(tr_x)) %*% best_coef
tr_error.backward <- mean((tr_pred - train$overweight)^2)
te_x <- test %>% dplyr::select(names(best_coef)[-1])
te_pred <- cbind(1, as.matrix(te_x)) %*% best_coef
te_error.backward <- mean((te_pred - test$overweight)^2)


#STEP 5: Ridge solution path 
x_tr <- as.matrix(train[, -32])
y_tr <- train[, 32, drop = T]
x_te <- as.matrix(test[, -32])
y_te <- test[, 32, drop = T]
std_fit <- preProcess(x_tr, method = c("center", "scale"))
x_tr_std <- predict(std_fit, x_tr)
x_te_std <- predict(std_fit, x_te)
fit_ridge <- glmnet(x_tr_std, y_tr, alpha = 0)

set.seed(25031)
cv_fit_ridge <- cv.glmnet(x_tr, y_tr, alpha = 0)
tr_pred <- predict(cv_fit_ridge, newx = x_tr)
te_pred <- predict(cv_fit_ridge, newx = x_te)
tr_error.ridge <- mean((tr_pred - y_tr)^2)
te_error.ridge <- mean((te_pred - y_te)^2)


#STEP 6: lasso Solution Path 
fit_lasso <- glmnet(x_tr_std, y_tr)

cv_fit_lasso <- cv.glmnet(x_tr, y_tr)
tr_pred <- predict(cv_fit_lasso, newx = x_tr)
te_pred <- predict(cv_fit_lasso, newx = x_te)
tr_error.lasso <- mean((tr_pred - y_tr)^2)
te_error.lasso <- mean((te_pred - y_te)^2)

stats <- data.frame(
  Method = c("Best Subset Selection", "Forward Regression", "Backward Regression",
             "Ridge Regression", "Lasso Regression"),
  Train.Error = c(tr_error.best, tr_error.forward, tr_error.backward,
                  tr_error.ridge, tr_error.lasso),
  Test.Error = c(te_error.best, te_error.forward, te_error.backward,
                 te_error.ridge, te_error.lasso)
)

stats <- stats %>% 
  mutate(across(where(~ is.numeric(.)), ~ round(., 3)))

stats

#STEP 7: Details of Data Set
summary(odata[,c(1:6)])
table(odata$overweight)
table(train$overweight)
table(test$overweight)

#STEP 8: Dectection of variable correlation

#Re-label values of outcomes (1 = overweight, 0 = underweight)
odata$Sex[odata$Sex ==1] <- "male"
odata$Sex[odata$Sex ==2] <- "female"
View(data)
ggplot(odata, aes(factor(overweight), fill = Sex)) +
  geom_histogram(stat = "count", position = "dodge") +
  theme_bw() +
  labs(title = "Figure 2. Histogram of overweight across Gender",
       x = "overweight", y = "Count") +
  scale_fill_manual(values = c("pink", "lightblue")) +
  theme(legend.position = "bottom")

corrplot(cor(data), title = "Figure 4. Correlation Plot", mar=c(0,0,1,0))


########MODELLING########

##Model 1: Logistic Regression 
mod <- as.formula(paste("overweight ~", 
                        paste(names(best_coef)[-1], collapse = " + ")))
logi.fit <- glm(mod, data = train, family='binomial')
logi.predict.train <- predict(logi.fit, train, type = 'response')
logi.train.error <- mean((logi.predict.train - train$overweight)^2)

logi.predict.test <- predict(logi.fit, test, type = 'response')
logi.test.error <- mean((logi.predict.test - test$overweight)^2)

logi.pred.test.label <- ifelse(logi.predict.test > 0.5, '1', '0')
table.lr <- table(true = test$overweight, predicted = logi.pred.test.label) #Confusion Matrix/Table
confusionMatrix(table.lr, mode = "everything")




#Support Vector Machine - svm
# Fit the model
#STEP 1:Split the data into training and test set:
# Split the data into training (70%) and test set (30%)
set.seed(25034)
training.samples <- odata$overweight %>%
  createDataPartition(p = 0.7, list = FALSE)
train.data <- odata[training.samples, ]
test.data <- odata[-training.samples, ]

#Re-label values of outcomes (1 = overweight, 0 = underweight)
train.data$overweight[train.data$overweight ==0] <- "underweight"
train.data$overweight[train.data$overweight ==1] <- "overweight"
test.data$overweight[test.data$overweight ==0] <- "underweight"
test.data$overweight[test.data$overweight ==1] <- "overweight"

#convert outcome variable to type factor
train.data$overweight <- as.factor(train.data$overweight)
test.data$overweight <- as.factor(test.data$overweight)
ctrl.svm <- trainControl(method ="repeatedcv", number = 10, repeats = 5, savePredictions = "all", classProbs = TRUE, sampling = "down")

model.svm <- train(overweight ~ .,
                   data = train.data, method = "svmLinear",
                   preProcess = c("center","scale"), 
                   trControl = ctrl.svm)

#Predict the outcome using model1 from train.data applied to thew test.data
predictions.svm <- predict(model.svm, newdata = test.data)

#create ConfusionMatrix
confusionMatrix(data = predictions.svm, test.data$overweight, mode = "everything")

#ROC
rocpred.svm <- predict(model.svm, newdata = test.data, type = 'prob')
alteredprob.svm <- rocpred.svm$underweight
labels <- test.data$overweight
pred.svm <- prediction(alteredprob.svm, labels)
pred.svm

perf.svm <- performance(pred.svm, "tpr", "fpr")

plot(perf.svm, avg = "threshold", col = 2, add = TRUE,
     lwd = 4)

#Area  under the curve (AUC)
auc.svm <- performance(pred.svm, "auc")
auc.svm <- unlist(slot(auc.svm, "y.values"))
auc.svm <- round(auc.svm, 4)
legend(.7, .2, auc.svm, title = "AUC-SVM", col = 4, pch = 4, lwd = 4)

###########################################################
#Generalized Partial Least Square-Logistic Regression - GPLS-LR
# Fit the model
ctrl.gplslr <- trainControl(method ="repeatedcv", number = 10, repeats = 5, savePredictions = "all", classProbs = TRUE, sampling = "down")

model.gplslr <- train(overweight ~ .,
                      data = train.data,
                      method = "plsRglm", # ncomp
                      trControl = ctrl.gplslr)

#Predict the outcome using model1 from train.data applied to thew test.data
predictions.gplslr <- predict(model.gplslr, newdata = test.data)

#create ConfusionMatrix
confusionMatrix(data = predictions.gplslr, test.data$overweight, mode = "everything")

#ROC
rocpred.gplslr <- predict(model.gplslr, newdata = test.data, type = 'prob')
alteredprob.gplslr <- rocpred.gplslr$overweight
labels <- test.data$overweight
pred.gplslr <- prediction(alteredprob.gplslr, labels)
pred.gplslr

perf.gplslr <- performance(pred.gplslr, "tpr", "fpr")

plot(perf.gplslr, avg = "threshold", col = 4, add = TRUE,
     lwd = 4)

#Area  under the curve (AUC)
auc.gplslr <- performance(pred.gplslr, "auc")
auc.gplslr <- unlist(slot(auc.gplslr, "y.values"))
auc.gplslr <- round(auc.gplslr, 4)
legend(.5, .2, auc.gplslr, title = "AUC-GPLS", col = 4, pch = 4, lwd = 4)


######### MARS ##########
ctrl.mars <- trainControl(method ="repeatedcv", number = 10, repeats = 5, savePredictions = "all", classProbs = TRUE, sampling = "down")
model.mars <- train(overweight ~ .,
                   data = train.data, method = "earth",
                   preProcess = c("center","scale"), 
                   trControl = ctrl.mars)

#Predict the outcome using model1 from train.data applied to thew test.data
predictions.mars <- predict(model.mars, newdata = test.data)

#create ConfusionMatrix
confusionMatrix(data = predictions.mars, test.data$overweight, mode = "everything")


######### Naive Bayes ##########
ctrl.nb <- trainControl(method ="repeatedcv", number = 10, repeats = 5, savePredictions = "all", classProbs = TRUE, sampling = "down")
model.nb <- train(overweight ~ .,
                    data = train.data, method = "nb",
                    preProcess = c("center","scale"), 
                    trControl = ctrl.nb)

#Predict the outcome using model1 from train.data applied to thew test.data
predictions.nb <- predict(model.nb, newdata = test.data)

#create ConfusionMatrix
confusionMatrix(data = predictions.nb, test.data$overweight, mode = "everything")

######### Tree ##########
ctrl.tree <- trainControl(method ="repeatedcv", number = 10, repeats = 5, savePredictions = "all", classProbs = TRUE, sampling = "down")
model.tree <- train(overweight ~ .,
                  data = train.data, method = "rpart",
                  preProcess = c("center","scale"), 
                  trControl = ctrl.tree)

#Predict the outcome using model1 from train.data applied to thew test.data
predictions.tree <- predict(model.tree, newdata = test.data)

#create ConfusionMatrix
confusionMatrix(data = predictions.tree, test.data$overweight, mode = "everything")








##Model 2:Linear Discriminant Analysis (LDA)
lda.fit <- lda(mod, data = train)
lda.predict.train <- predict(lda.fit, train)
lda.class.train <- lda.predict.train$class
lda.train.error <- mean(lda.class.train != train$overweight)
lda.predict.test <- predict(lda.fit, test)
lda.class.test <- lda.predict.test$class
lda.test.error <- mean(lda.class.test != test$overweight)

table.lda <- table(true = test$overweight, predicted = lda.class.test) #Confusion Matrix/Table
confusionMatrix(table.lda, mode = "everything")
importance(lda.fit)
varImpPlot(lda.fit, scale = TRUE)

##Model 3: Quadratic discriminant analysis (QDA)
qda.fit <- qda(mod, data = train)
qda.predict.train <- predict(qda.fit, train)
qda.class.train <- qda.predict.train$class
qda.train.error <- mean(qda.class.train != train$overweight)
qda.predict.test <- predict(qda.fit, test)
qda.class.test <- qda.predict.test$class
qda.test.error <- mean(qda.class.test != test$overweight)

table.qda <- table(true = test$overweight, predicted = qda.class.test) #Confusion Matrix/Table
confusionMatrix(table.qda, mode = "everything")


##Model 4:KNN
knn.fit <- knn3(mod, data = train, k = 5)
knn.predict.train <- predict(knn.fit, newdata = train, type = "prob")
knn.train.error <- mean((knn.predict.train[,2] - train$overweight)^2)
knn.predict.test <- predict(knn.fit, newdata = test, type = "prob")
knn.test.error <- mean((knn.predict.test[,2] - test$overweight)^2)

knn.predict.test.label <- ifelse(knn.predict.test[,2] > 0.1, '1', '0')
table.knn <- table(true = test$overweight, predicted = knn.predict.test.label) #Confusion Matrix/Table
confusionMatrix(table.knn, mode = "everything")


############# Trees for classification #################
data$overweight <- factor(data$overweight, 
                            levels = c(0,1),
                            labels = c(0,1))
train.class <- data[trainIndex,]
test.class <- data[-trainIndex,]
set.seed(25030)
tree.fit <- tree(mod, data = train.class)
cv.tree <- cv.tree(tree.fit)
cv.tree_df <- data.frame(size = cv.tree$size, deviance = cv.tree$dev)
best_size <- cv.tree$size[which.min(cv.tree$dev)]
tree.size.plot <- ggplot(cv.tree_df, mapping = aes(x = size, y = deviance)) + 
  geom_point(size = 3) + 
  geom_line() +
  geom_vline(xintercept = best_size, col = "red") +
  theme_bw()
tree.size.plot
## Visualizing the pruned classification tree
#cat('CV leads to the optimal tree size as ', best_size,'\n')
tree.fit.final <- prune.tree(tree.fit, best = best_size) #The subtree with best_size terminal nodes
plot(tree.fit.final)
text(tree.fit.final, cex = 1, col = "Darkblue")


##Model 5:Tree
tree.predict.train <- predict(tree.fit.final, newdata = train.class, type = "class")
tree.train.error <-mean(tree.predict.train != train.class$overweight)
tree.predict.test <- predict(tree.fit.final, newdata = test.class, type = "class")
tree.test.error <- mean(tree.predict.test != test.class$overweight)

table.tree <- table(true = test$overweight, predicted = tree.predict.test) #Confusion Matrix/Table
confusionMatrix(table.tree, mode = "everything")

##Model 6:Random Forest
set.seed(25032)
rf.fit <- randomForest(mod, data = data, subset=trainIndex, importance=TRUE)

rf.predict.train <- predict(rf.fit,newdata=train.class)
rf.train.error <- mean(rf.predict.train != train.class$overweight)

rf.predict.test <- predict(rf.fit,newdata=test.class)
rf.test.error <- mean(rf.predict.test != test.class$overweight)

importance(rf.fit)
varImpPlot(rf.fit, scale = TRUE)

table.rf <- table(true = test$overweight, predicted = rf.predict.test) #Confusion Matrix/Table
confusionMatrix(table.rf, mode = "everything")

##Model 7:Bagging
set.seed(25033)
p <- 4
## Setting mtry = p for bagging
bag.fit <- randomForest(mod, data = data, subset=trainIndex, mtry = p, importance=TRUE)

bag.predict.train <- predict(bag.fit,newdata=train.class)
bag.train.error <- mean(bag.predict.train != train.class$overweight)

bag.predict.test <- predict(bag.fit,newdata=test.class)
bag.test.error <- mean(bag.predict.test != test.class$overweight)

importance(bag.fit)
varImpPlot(bag.fit)

table.bagging <- table(true = test$overweight, predicted = bag.predict.test) #Confusion Matrix/Table
confusionMatrix(table.bagging, mode = "everything")

##Model 8:Boosting
set.seed(25031)
boost.fit <- gbm(mod, data = train, n.trees = 500, distribution = "bernoulli", 
                 interaction.depth = 1, cv.folds = 5, shrinkage = 0.2)
best_n_tress <- which.min(boost.fit$cv.error)
# summary(boost.fit)

boost.predict.train <- predict(boost.fit, newdata = train, 
                               n.trees = best_n_tress, type = "response")
boost.train.error <- mean((boost.predict.train - train$overweight)^2)
boost.predict.test <- predict(boost.fit, newdata = test, 
                              n.trees = best_n_tress, type = "response")
boost.test.error <- mean((boost.predict.test - test$overweight)^2)

boost.pred.test.label <- ifelse(boost.predict.test > 0.5, '1', '0')
table.boosting <- table(true = test$overweight, predicted = boost.pred.test.label) #Confusion Matrix/Table
confusionMatrix(table.boosting, mode = "everything")

################# ROC CURVE #############
rocobj_logi <- roc(test$overweight, logi.predict.test)
auc_logi <- auc(rocobj_logi)

lda.pred <- lda.predict.test$posterior[,2]
rocobj_lda <- roc(test$overweight, lda.pred)
auc_lda <- auc(rocobj_lda)

qda.pred <- qda.predict.test$posterior[,2]
rocobj_qda <- roc(test$overweight, qda.pred)
auc_qda <- auc(rocobj_qda)

rocobj_knn <- roc(test$overweight, knn.predict.test[,2])
auc_knn <- auc(rocobj_knn)

rocobj_tree <- roc(test$overweight, as.numeric(tree.predict.test)-1)
auc_tree <- auc(rocobj_tree)

rocobj_bagging <- roc(test$overweight, as.numeric(bag.predict.test)-1)
auc_bagging <- auc(rocobj_bagging)

rocobj_rf <- roc(test$overweight, as.numeric(rf.predict.test)-1)
auc_rf <- auc(rocobj_rf)

rocobj_boosting <- roc(test$overweight, boost.predict.test)
auc_boosting <- auc(rocobj_boosting)

rocobjs <- list(LDA = rocobj_lda, QDA = rocobj_qda, KNN = rocobj_knn, 
                Logistic = rocobj_logi, Tree = rocobj_tree, Bagging = rocobj_bagging,
                RF = rocobj_rf, Boosting = rocobj_boosting)

methods_auc <- paste(c("Logistic","LDA", "QDA","KNN","Tree1","Bagging","RF","Boosting"),
                     "AUC = ", 
                     round(c(auc_logi, auc_lda, auc_qda, auc_knn,
                             auc_tree, auc_bagging, auc_rf, auc_boosting),3))

ggroc(rocobjs, size = 2, alpha = 0.5) + 
  scale_color_discrete(labels = methods_auc) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(title = "Figure 1: ROC Curves Comparing The 8 Different Models") +
  guides(color=guide_legend(nrow=3,byrow=TRUE))



#############SENSITIVITY ANALYSIS ###############
sen = c(0.5250, 0.6250, 0.8113 , 0.7000, 0.6750,  0.6726, 0.801, 0.4340, 0.5849, 0.7990)
models = c("KNN", "RF", "NB", "SVM", "LR", "LDA", "GB", "MARS", "Tree", "Bagging")
plot(sen, xaxt = "n", xlab = "Algorithms", type = "b", ylab = "Sensitivity", col = 2, lwd = 5)
axis(side = 1, at = seq(1,10), labels = models)

###############################################
############# F1 Score ANALYSIS ###############
sen = c(0.5250, 0.6250, 0.8113 , 0.7000, 0.6750,  0.6726, 0.801, 0.4340, 0.5849, 0.7990)
models = c("KNN", "RF", "NB", "SVM", "LR", "LDA", "GB", "MARS", "Tree", "Bagging")
plot(sen, xaxt = "n", xlab = "Algorithms", type = "b", ylab = "Sensitivity", col = 2, lwd = 5)
axis(side = 1, at = seq(1,10), labels = models)

###############################################
#############ACURRACY ANALYSIS ###############
sen = c(0.5250, 0.6250, 0.8113 , 0.7000, 0.6750,  0.6726, 0.801, 0.4340, 0.5849, 0.7990)
models = c("KNN", "RF", "NB", "SVM", "LR", "LDA", "GB", "MARS", "Tree", "Bagging")
plot(sen, xaxt = "n", xlab = "Algorithms", type = "b", ylab = "Sensitivity", col = 2, lwd = 3)
axis(side = 1, at = seq(1,10), labels = models)

###############################################
############# BABALANCED ACCURACY ANALYSIS ###############
sen = c(0.5250, 0.6250, 0.8113 , 0.7000, 0.6750,  0.6726, 0.801, 0.4340, 0.5849, 0.7990)
models = c("KNN", "RF", "NB", "SVM", "LR", "LDA", "GB", "MARS", "Tree", "Bagging")
plot(sen, xaxt = "n", xlab = "Algorithms", type = "b", ylab = "Sensitivity", col = 2, lwd = 3)
axis(side = 1, at = seq(1,10), labels = models)



######### Shrinkage Discriminant Analysis ##########
ctrl.sda <- trainControl(method ="repeatedcv", number = 10, repeats = 5, savePredictions = "all", classProbs = TRUE, sampling = "down")
model.sda <- train(overweight ~ .,
                  data = train.data, method = "sda",
                  preProcess = c("center","scale"), 
                  trControl = ctrl.sda)

#Predict the outcome using model1 from train.data applied to thew test.data
predictions.sda <- predict(model.sda, newdata = test.data)

#create ConfusionMatrix
confusionMatrix(data = predictions.sda, test.data$overweight, mode = "everything")


######### Heterosckedastic Discriminant Analysis ##########
ctrl.hda <- trainControl(method ="repeatedcv", number = 10, repeats = 5, savePredictions = "all", classProbs = TRUE, sampling = "down")
model.hda <- train(overweight ~ .,
                   data = train.data, method = "hda",
                   preProcess = c("center","scale"), 
                   trControl = ctrl.hda)

#Predict the outcome using model1 from train.data applied to thew test.data
predictions.hda <- predict(model.hda, newdata = test.data)

#create ConfusionMatrix
confusionMatrix(data = predictions.hda, test.data$overweight, mode = "everything")




















##########################SECOND DATA SET ################################
#load libraries
library(readr)
library(tidyverse)
library(caret)
library(MASS)
library(klaR)
library(mda)
library(pROC)
library(gplots)
library(tidytuesdayR)
theme_set(theme_classic())


#STEP 0:Read in the data into the R enviroment:
sanhanes <- read_csv("C:/Users/413/Desktop/OAE/ADA GLOBAL CONCEPT/SANHANESDA.csv")
View(sanhanes)
head(sanhanes)
dim(sanhanes)

#STEP 1:Split the data into training and test set:
# Split the data into training (70%) and test set (30%)
set.seed(2503)
training.samples <- sanhanes$anemia %>%
  createDataPartition(p = 0.7, list = FALSE)
strain.data <- sanhanes[training.samples, ]
stest.data <- sanhanes[-training.samples, ]

#STEP 2: Normalize the data. Categorical variables are automatically ignored.
# Estimate preprocessing parameters
preproc.param <- strain.data %>% 
  preProcess(method = c("center", "scale"))
# Transform the data using the estimated parameters
strain.transformed <- preproc.param %>% predict(strain.data)
stest.transformed <- preproc.param %>% predict(stest.data)

dim(strain.transformed)
table(strain.transformed$anemia)
dim(stest.transformed)
table(stest.transformed$anemia)
###########################################################

###########################################################
#Linear discriminant analysis - slda
# Fit the model
ctrl.slda <- trainControl(method ="repeatedcv", number = 10, repeats = 5, savePredictions = "all", classProbs = TRUE, sampling = "down")

model.slda <- lda(anemia~., data = strain.transformed, trControl = ctrl.slda)
#plot(model.slda)

# Make predictions
predictions.slda <- model.slda %>% predict(stest.transformed)
names(predictions.slda)

#create ConfusionMatrix
table.slda <- table(predictions.slda$class,stest.transformed$anemia)
confusionMatrix(table.slda, mode = "everything")
lda.Sensitivity <- (0.9450+0.5172+0.55556)/3
lda.Specificity <- (0.6579+0.9237+0.98551)/3
lda.BalancedAccuracy <- (0.8014+0.7205+0.77053)/3
lda.F1 <- (0.9156+0.5660+0.62500)/3

#Area  under the curve (AUC)
roc.slda <- predict(model.slda, newdata = stest.transformed, anemia = "prob")
alteredprob.slda <- roc.slda$class
labels <- stest.transformed$anemia
pred.slda <- multiclass.roc(as.numeric(labels), as.numeric(alteredprob.slda))
#Multi-class area under the curve for LDA: 0.8277

###########################################################

###########################################################
#Quadratic discriminant analysis - qda
# Fit the model
ctrl.sqda <- trainControl(method ="repeatedcv", number = 10, repeats = 5, savePredictions = "all", classProbs = TRUE, sampling = "down")

model.sqda <- qda(anemia~., data = strain.transformed, trControl = ctrl.sqda)
#plot(model.sqda)
# Make predictions
predictions.sqda <- model.sqda %>% predict(stest.transformed)
names(predictions.sqda)

#create ConfusionMatrix
table.sqda <- table(predictions.sqda$class,stest.transformed$anemia)
confusionMatrix(table.sqda, mode = "everything")

#Area  under the curve (AUC)
roc.sqda <- predict(model.sqda, newdata = stest.transformed, anemia = "prob")
alteredprob.sqda <- roc.sqda$class
labels <- stest.transformed$anemia
pred.sqda <- multiclass.roc(as.numeric(labels), as.numeric(alteredprob.sqda))
pred.sqda
#Multi-class area under the curve for LDA: 1
###########################################################

###########################################################
#Mixture discriminant analysis - mda
# Fit the model
ctrl.smda <- trainControl(method ="repeatedcv", number = 10, repeats = 5, savePredictions = "all", classProbs = TRUE, sampling = "down")

model.smda <- mda(anemia~., data = strain.transformed, trControl = ctrl.sqda)

# Make predictions
predictions.smda <- model.smda %>% predict(stest.transformed)
names(predictions.smda)

#create ConfusionMatrix
table.smda <- table(predictions.smda,stest.transformed$anemia)
confusionMatrix(table.smda, mode = "everything")

#Area  under the curve (AUC)
alteredprob.smda <- predictions.smda
labels <- stest.transformed$anemia
pred.smda <- multiclass.roc(as.numeric(labels), as.numeric(alteredprob.smda))
pred.smda
#Multi-class area under the curve for MDA: 1

###########################################################

###########################################################
#Flexible discriminant analysis - fda
# Fit the model
ctrl.sfda <- trainControl(method ="repeatedcv", number = 10, repeats = 5, savePredictions = "all", classProbs = TRUE, sampling = "down")

model.sfda <- fda(anemia~., data = strain.transformed, trControl = ctrl.sfda)
# Make predictions
predictions.sfda <- model.sfda %>% predict(stest.transformed)
names(predictions.sfda)

#create ConfusionMatrix
table.sfda <- table(predictions.sfda,stest.transformed$anemia)
confusionMatrix(table.sfda, mode = "everything")

#Area  under the curve (AUC)
alteredprob.smda <- predictions.sfda
labels <- stest.transformed$anemia
pred.sfda <- multiclass.roc(as.numeric(labels), as.numeric(alteredprob.smda))
pred.sfda
#Multi-class area under the curve for FDA: 1
###########################################################

###########################################################
#Regularized discriminant analysis - rda
# Fit the model
ctrl.srda <- trainControl(method ="repeatedcv", number = 10, repeats = 5, savePredictions = "all", classProbs = TRUE, sampling = "down")

model.srda <- rda(anemia~., data = strain.transformed, trControl = ctrl.srda)
plot(model.srda)
# Make predictions
predictions.srda <- model.srda %>% predict(stest.transformed)
names(predictions.srda)

#create ConfusionMatrix
table.srda <- table(predictions.srda$class,stest.transformed$anemia)
confusionMatrix(table.srda, mode = "everything")
rda.Sensitivity <- (0.9174+0.34483+0.66667)/3
rda.Specificity <- (0.5263+0.91525+0.97826)/3
rda.BalancedAccuracy <- (0.7219+0.63004+0.82246)/3
rda.F1 <- (0.8811+0.40816+0.66667)/3

#Area  under the curve (AUC)
roc.srda <- predict(model.srda, newdata = stest.transformed, anemia = "prob")
alteredprob.srda <- roc.srda$class
labels <- stest.transformed$anemia
pred.srda <- multiclass.roc(as.numeric(labels), as.numeric(alteredprob.srda))
#Multi-class area under the curve for RDA:  0.7834
###########################################################



######### Shrinkage Discriminant Analysis ##########
ctrl.ssda <- trainControl(method ="repeatedcv", number = 10, repeats = 5, savePredictions = "all", classProbs = TRUE, sampling = "down")
model.ssda <- train(anemia~., data = strain.transformed, method = "sda",
                    preProcess = c("center","scale"), 
                    trControl = ctrl.ssda)

# Make predictions
predictions.ssda <- model.ssda %>% predict(stest.transformed)
names(predictions.ssda)

#create ConfusionMatrix
table.ssda <- table(predictions.ssda$class,stest.transformed$anemia)
confusionMatrix(table.ssda, mode = "everything")

######### Heterosckedastic Discriminant Analysis ##########
ctrl.shda <- trainControl(method ="repeatedcv", number = 10, repeats = 5, savePredictions = "all", classProbs = TRUE, sampling = "down")
model.shda <- train(anemia~., data = strain.transformed, method = "hda",
                    preProcess = c("center","scale"), 
                    trControl = ctrl.shda)

# Make predictions
predictions.srda <- model.srda %>% predict(stest.transformed)
names(predictions.srda)

#create ConfusionMatrix
table.srda <- table(predictions.srda$class,stest.transformed$anemia)
confusionMatrix(table.srda, mode = "everything")



######### Shrinkage Discriminant Analysis ##########
#STEP 1:Split the data into training and test set:
# Split the data into training (70%) and test set (30%)
set.seed(2503)
#data <- model.matrix( ~ . , data = sanhanes) %>% 
#as_tibble() %>% 
#dplyr::select(-"(Intercept)") 

trainIndex = sample(1:nrow(data), size = round(0.7*nrow(data)), replace=FALSE)
train = data[trainIndex ,]
test = data[-trainIndex ,]
table(train$anemia)
table(test$anemia)



ctrl.ssda <- trainControl(method ="repeatedcv", number = 10, repeats = 5, savePredictions = "all")
model.ssda <- train(factor(anemia)~., data = train, method = "sda",
                    preProcess = c("center","scale"), 
                    trControl = ctrl.ssda)

#Predict the outcome using model1 from train.data applied to thew test.data
predictions.ssda <- predict(model.ssda, newdata = test)

#create ConfusionMatrix
confusionMatrix(data = predictions.ssda, as.factor(test$anemia), mode = "everything")


######### Heterosckedastic Discriminant Analysis ##########
ctrl.shda <- trainControl(method ="repeatedcv", number = 10, repeats = 5, savePredictions = "all")
model.shda <- train(factor(anemia)~., data = train, method = "hda",
                    preProcess = c("center","scale"), 
                    trControl = ctrl.shda)

#Predict the outcome using model1 from train.data applied to thew test.data
predictions.shda <- predict(model.shda, newdata = test)

#create ConfusionMatrix
confusionMatrix(data = predictions.shda, as.factor(test$anemia), mode = "everything")

######### Mixture Discriminant Analysis ##########
ctrl.smda <- trainControl(method ="repeatedcv", number = 10, repeats = 5, savePredictions = "all")
model.smda <- train(factor(anemia)~., data = train, method = "mda",
                    preProcess = c("center","scale"), 
                    trControl = ctrl.smda)

#Predict the outcome using model1 from train.data applied to thew test.data
predictions.smda <- predict(model.smda, newdata = test)

#create ConfusionMatrix
confusionMatrix(data = predictions.smda, as.factor(test$anemia), mode = "everything")

######### Flexible Discriminant Analysis ##########
ctrl.sfda <- trainControl(method ="repeatedcv", number = 10, repeats = 5, savePredictions = "all")
model.sfda <- train(factor(anemia)~., data = train, method = "fda",
                    preProcess = c("center","scale"), 
                    trControl = ctrl.sfda)

#Predict the outcome using model1 from train.data applied to thew test.data
predictions.sfda <- predict(model.sfda, newdata = test)

#create ConfusionMatrix
confusionMatrix(data = predictions.sfda, as.factor(test$anemia), mode = "everything")




