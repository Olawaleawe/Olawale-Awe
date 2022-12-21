#Decision Trees
#Decision trees work by building a tree from the input data, splitting on a parameter in each inner node 
#according to a variable value. This can be splitting on whether a numerical value is above or below a certain 
#threshold or which level a factor has.
#Decision trees are implemented in the rpart package and models are fitted, just as linear models are:
library(rpart)
library(magrittr)
model <- cars %>% rpart(dist ~ speed, data = .)
rmse(predict(model, cars), cars$dist)
#Building a classifying model works very similar. We do not need to translate the cell thickness into a 
#numerical value, though; we can use the data frame as it is (but you can experiment with translating factors 
        #into numbers if you are interested in exploring this).
model <- BreastCancer %>%
  rpart(Class ~ Cl.thickness, data = .)
#The predictions when we used the glm() function were probabilities for the tumor being malignant. 
#The predictions made using the decision tree gives you the probabilities both for being benign and being 
#malignant:
  predict(model, BreastCancer) %>% head
  #To get a confusion matrix, we need to translate these probabilities into the corresponding classes. The 
  #output of predict() is not a data frame but a matrix so we first convert it into a data frame using the function 
  #as.data.frame() and then we use the %$% operator in the pipeline to get access to the columns by name in 
  #the next step.
  predicted_class <- predict(model, BreastCancer) %>%
    as.data.frame %$%
    ifelse(benign > 0.5, "benign", "malignant")
  table(BreastCancer$Class, predicted_class)
#Another implementation of decision trees is the ctree() function from the party package:
  library(party)
  model <- cars %>% ctree(dist ~ speed, data = .)
  rmse(predict(model, cars), cars$dist)
  
  model <- BreastCancer %>%
    ctree(Class ~ Cl.thickness, data = .)
  predict(model, BreastCancer) %>% head
  
  table(BreastCancer$Class, predict(model, BreastCancer))
  
  #I like this package slightly more since it can make plots of the fitted models
    cars %>% ctree(dist ~ speed, data = .) %>% plot
  
  #Random Forests    
    #Random forests generalize decision trees by building several of them and combining them. They are 
    #implemented in the randomForest package, as follows:
      library(randomForest)
    model <- cars %>% randomForest(dist ~ speed, data = .)
    rmse(predict(model, cars), cars$dist)
    
    #For classification, the predictions are the actual classes as a factor, so no translation is needed to get a 
    #confusion matrix:
      model <- BreastCancer %>%
      randomForest(Class ~ Cl.thickness, data = .)
    predict(model, BreastCancer) %>% head
    
    table(BreastCancer$Class, predict(model, BreastCancer))
    
    
  #Neural Networks
    #You can use a package called nnet to construct neural networks.
    library(nnet)
    #You can use it for both classification and regression. We can see it in action on the cars dataset:
      model <- cars %>% nnet(dist ~ speed, data = ., size = 5)
      rmse(predict(model, cars), cars$dist)
    #The neural networks require a size parameter specifying how many nodes you want in the inner layer 
    #of the network. Here I have used five.
    #For classification, you use a similar call:
    model <- BreastCancer %>%
      nnet(Class ~ Cl.thickness, data = ., size = 5)
    
    predict(model, BreastCancer) %>% head
    #We need to translate it into classes and, for this, we can use a lambda expression:
    predicted_class <- predict(model, BreastCancer) %>%
      { ifelse(. < 0.5, "benign", "malignant") }
    table(BreastCancer$Class, predicted_class)
    
    #Support Vector Machines
    library(kernlab)
    model <- cars %>% ksvm(dist ~ speed, data = .)
    rmse(predict(model, cars), cars$dist)
    #For classification, the output is again a factor we can use directly to get a confusion matrix:
      model <- BreastCancer %>%
      ksvm(Class ~ Cl.thickness, data = .)
    predict(model, BreastCancer) %>% head
    
    table(BreastCancer$Class, predict(model, BreastCancer))
    
    #Naive Bayes
    
    #Naive Bayes essentially assumes that each explanatory variable is independent of the others and uses the 
    #distribution of these for each category of data to construct the distribution of the response variable given the 
    #explanatory variables.
    #Naive Bayes is implemented in the e1071 package:
      library(e1071)
    
    #The package doesn't support regression analysis-after all, it needs to look at conditional distributions 
    #for each output variable value-but we can use it for classification. The function we need is naiveBayes()
    #and we can use the predict() output directly to get a confusion matrix:
    model <- BreastCancer %>%
      naiveBayes(Class ~ Cl.thickness, data = .)
    predict(model, BreastCancer) %>% head
    
    table(BreastCancer$Class, predict(model, BreastCancer))    
    
      