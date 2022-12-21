
#################################################################################################
#														
# TITLE:			Meta-analysis of diagnostic test accuracy studies				
# AUTHOR:			Christopher Partlett 										
# DATE CREATED:		15/05/2015								   				
# DATE MODIFIED:		07/04/2016								   						
# PURPOSE: 			Bivariate model for meta-analysis of CT and MRI for diagnosis of CAD	
#									 							
# ACKNOWLEDGEMENT:	Most of the code here has been modelled on previous work by  
# 				Yemisi Takwoingi for the equivalent stata programs  								
#																
#################################################################################################


## Set your working directory to the appropriate drive where you saved the file schuetz.csv. 
## Replace "C:/Users/partletc/Documents/DTA macros/DTA R". 

setwd("C:/Users/partletc/Documents/DTA macros/DTA R") 

############################ 1. DATA IMPORT #####################################################
## Read in the data from the .csv file ##

(X  = read.csv("schuetz.csv"))




############################ 2. META-ANALYSIS OF DTA - USING GLMER ##############################

## install lme4 package if required (to run remove the # and select an appropriate CRAN mirror).

# install.packages("lme4")

## load the package lme4 
library(lme4)

## In order to specify the generalized linear model, first, we need to set up the data 

## Set up the data
## Generate 5 new variables of type long. We need these before we can reshape the data.
# •	n1 is number diseased
# •	n0 is number without disease
# •	true1 is number of true positives
# •	true0 is the number of true negatives
# •	study is the unique identifier for each study. _n will generate a sequence of numbers. 

X$n1 <- X$tp+X$fn
X$n0 <- X$fp+X$tn
X$true1 <- X$tp
X$true0 <- X$tn 

X$study <- 1:108


## Reshape the data from wide to long format ###

Y = reshape(X, direction = "long", varying = list( c("n1" , "n0") , c( "true1","true0" ) ) ,
    timevar = "sens" , times = c(1,0) , v.names = c("n","true") ) 

## Sort data by study to cluster the 2 records per study together ###

Y = Y[order(Y$id),]  
Y$spec<- 1-Y$sens

## Generate a seperate data frame for each test type 

Y.CT  =  Y[Y$Test=="CT",]
Y.MRI =  Y[Y$Test=="MRI",]

## Perform meta-analysis for CT ## 

(MA_Y.CT = glmer( formula = cbind(  true , n - true ) ~ 0 + sens + spec + (0+sens + spec|study),
 data = Y.CT , family = binomial  ) ) 

## More detail can be obtained by using the summary command 

(ma_Y.CT = summary(MA_Y.CT)) 

## For the full list of outputs 

labels( ma_Y.CT ) 

## Therefore, to extract the coefficients 

ma_Y.CT$coeff 

(lsens.CT = ma_Y.CT$coeff[1,1])
(lspec.CT = ma_Y.CT$coeff[2,1]) 

se.lsens.CT = ma_Y.CT$coeff[1,2]
se.lspec.CT = ma_Y.CT$coeff[2,2] 

## Then we can manually create 95% confidence intervals for logit sens and spec

Sens.CT = c(lsens.CT, lsens.CT-qnorm(0.975)*se.lsens.CT, lsens.CT+qnorm(0.975)*se.lsens.CT ) 
Spec.CT = c(lspec.CT, lspec.CT-qnorm(0.975)*se.lspec.CT, lspec.CT+qnorm(0.975)*se.lspec.CT ) 

## Or as a dataframe 

logitCT = data.frame( estimate = c(lsens.CT , lspec.CT) , 
	lci =  c(lsens.CT-qnorm(0.975)*se.lsens.CT , lspec.CT-qnorm(0.975)*se.lspec.CT) , 
	uci = c(lsens.CT+qnorm(0.975)*se.lsens.CT , lspec.CT+qnorm(0.975)*se.lspec.CT) ,
		row.names = c("lSens", "lSpec") ) 

## 

## R has a built in logit and inv.logit function (use qlogis and plogis ) 

plogis( Sens.CT ) 
plogis( Spec.CT ) 


####### DOR and LRs 

(DOR = exp(lsens.CT+lspec.CT ) ) 

(LRp = plogis(lsens.CT)/(1-plogis(lspec.CT)) ) 

(LRm = ((1-plogis(lsens.CT))/plogis(lspec.CT)) ) 

## Standard errors and confidence intervals can be calculated using deltamethod. 
# This requires the package msm 


install.packages("msm")

library(msm)

se.DOR = deltamethod (~ exp(x1+x2) , mean = c(lsens.CT,lspec.CT) , cov = ma_Y.CT$vcov )
	
se.LRp = deltamethod (~ (exp(x1)/(1+exp(x1)))/(1-(exp(x2)/(1+exp(x2)))) , 
	mean = c(lsens.CT,lspec.CT) , cov = ma_Y.CT$vcov )
	
se.LRm = deltamethod (~ (1-(exp(x1)/(1+exp(x1))))/(exp(x2)/(1+exp(x2))) , 
	mean = c(lsens.CT,lspec.CT) , cov = ma_Y.CT$vcov ) 

data.frame( estimate = c(DOR , LRp , LRm) , 
	lci = c(DOR-qnorm(0.975)*se.DOR , LRp-qnorm(0.975)*se.LRp , LRm-qnorm(0.975)*se.LRm) , 
	uci = c(DOR+qnorm(0.975)*se.DOR , LRp+qnorm(0.975)*se.LRp , LRm+qnorm(0.975)*se.LRm),
		row.names = c("DOR", "LR+" , "LR-" )  ) 


#################################################

## unstructured 

glmer( formula = cbind(  true , n - true ) ~ 0 + sens + spec + (0+sens + spec|study),
 data = Y.CT , family = binomial  ,  nAGQ = 1 , verbose = 2  ) 

### no correlation 

glmer( formula = cbind(  true , n - true ) ~ 0 + sens + spec + (0+sens|study) + (0+spec|study),
 data = Y.CT , family = binomial  ,  nAGQ = 1 , verbose = 2  ) 

###



#################################################


# nAGQ controls number of points per axis for evaluating the adaptive Gauss-Hermite approximation 
# to the log-likelihood. Defaults to 1, corresponding to the Laplace approximation. 
 
# A value of zero uses a faster but less exact form of parameter estimation for GLMMs by 
# optimizing the random effects and the fixed-effects coefficients in the penalized iteratively 
# reweighted least squares step



############################ 5. META-REGRESSION - USING GLMER WITH A COVARIATE ##################



														
### Meta-analysis of CT ###

glmer( formula = cbind(  true , n - true ) ~ 0 + sens + spec + (0+sens + spec|study),
 data = Y.CT , family = binomial  ) 

### Meta-analysis of MRI ###
glmer( formula = cbind(  true , n - true ) ~ 0 + sens + spec + (0+sens + spec|study),
 data = Y.MRI , family = binomial  ) 

### Fit the model without the covariate ###

(A = glmer( formula = cbind(  true , n - true ) ~ 0 + sens + spec + (0+sens + spec|study),
 data = Y , family = binomial  ) ) 

### Add covariate terms to the model for both logit sensitivity and logit specificity. 
### This model assumes equal variances for both tests. ***


Y$CT  <- 2 - as.numeric(Y$Test) 
Y$MRI <- 1 - Y$CT 

Y$seCT  <- (Y$CT)*(Y$sens) 
Y$seMRI <- (Y$MRI)*(Y$sens) 

Y$spCT  <- (Y$CT)*(Y$spec) 
Y$spMRI <- (Y$MRI)*(Y$spec) 

(B = glmer( formula = cbind(  true , n - true ) ~ 0 + seCT + seMRI + spCT + spMRI + 
	(0+sens + spec|study), data = Y , family = binomial  )) 

### The models can be formally compared using the LR test 

install.packages("lmtest")

library(lmtest)

lrtest(A,B)



293/.8



## Is there a statistically significant difference in sensitivity between CT and MRI?  

(C = glmer( formula = cbind(  true , n - true ) ~ 0 + sens + spCT + spMRI + 
	(0+sens + spec|study), data = Y , family = binomial  )) 

lrtest(B,C)

## Is there a statistically significant difference in specificity between CT and MRI?  

(D = glmer( formula = cbind(  true , n - true ) ~ 0 + seCT + seMRI + spec + 
	(0+sens + spec|study), data = Y , family = binomial  )) 

lrtest(B,D)


### Using different variances for each test 

(E = glmer( formula = cbind(  true , n - true ) ~ 0 + seCT + seMRI + spCT + spMRI 
	+(0 +seMRI + spMRI |study) +(0 +seCT + spCT |study), data = Y , family = binomial  )) 


lrtest(B,E)


lrtest(A,E)

### To obtain the between study covariance between logit sensitivity and specificity 
# for each test use 

(summary(E))$vcov

###

# The overall logit-sensitivity and -specificity are given in 

cB = summary(B)$coefficients
cE = summary(E)$coefficients

## Therefore confidence intervals on the log scale can be calculated using 

seCT.B = c( cB[1,1] , cB[1,1] - qnorm(0.975)*cE[1,2] , cB[1,1] + qnorm(0.975)*cB[1,2]  ) 
seCT.E = c( cE[1,1] , cE[1,1] - qnorm(0.975)*cE[1,2] , cE[1,1] + qnorm(0.975)*cE[1,2]  ) 

spCT.B = c( cB[3,1] , cB[3,1] - qnorm(0.975)*cE[3,2] , cB[3,1] + qnorm(0.975)*cB[3,2]  ) 
spCT.E = c( cE[3,1] , cE[3,1] - qnorm(0.975)*cE[3,2] , cE[3,1] + qnorm(0.975)*cE[3,2]  ) 

seMRI.B = c( cB[2,1] , cB[2,1] - qnorm(0.975)*cE[2,2] , cB[2,1] + qnorm(0.975)*cB[2,2]  ) 
seMRI.E = c( cE[2,1] , cE[2,1] - qnorm(0.975)*cE[2,2] , cE[2,1] + qnorm(0.975)*cE[2,2]  ) 

spMRI.B = c( cB[4,1] , cB[4,1] - qnorm(0.975)*cE[4,2] , cB[4,1] + qnorm(0.975)*cB[4,2]  ) 
spMRI.E = c( cE[4,1] , cE[4,1] - qnorm(0.975)*cE[4,2] , cE[4,1] + qnorm(0.975)*cE[4,2]  ) 


## These confidence intervals can be transformed back to the original sacle by

plogis(  seCT.B) 
plogis(  seCT.E) 

plogis(  spCT.B) 
plogis(  spCT.E) 

plogis(  seMRI.B) 
plogis(  seMRI.E) 

plogis(  spMRI.B) 
plogis(  spMRI.E) 




