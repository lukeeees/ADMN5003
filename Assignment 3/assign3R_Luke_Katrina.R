#==========================================
# ADMN5003 - ASSIGNMENT 3
#==========================================
#------------------------------------------
# install.packages('caret')
# install.packages('InformationValue')
# install.packages('gains')
#==========================================
# LOAD STANDARD PACKAGES
#==========================================
library(readxl)
library(forecast)
library(ggplot2)
library(dplyr)
library(tidyr)
library(data.table)
library(testthat) #need to load this first to avoid problems with psych::describe
library(psych)
library(caret)
library(InformationValue)
library(gains)



#==========================================
# QUESTION 1
#For 2 normally distributed populations, assume the significance level is set at 0.05. The two-sided p-value is 0.536. 
#==========================================

#a) What is your null hypothesis of a two-sample t test? (0.5 mark)

#The null hypothesis is that the mean for population 1 and the mean for population 2 are equal. 



#b)	Is the statistical result significant? (0.5 mark) 

# The result is not statistically significant since the p > 0.05.



#c)	What will be your decision regarding the null hypothesis? (0.5 mark) 

# We fail to reject the null hypothesis and conclude that the means of both populations are equal.




#d)	What is your alternative hypothesis? (0.5 mark) 

# The alternate hypothesis is that the mean for population 1 is not equal to the mean for population 2. 



#e)	What is your decision on the alternative hypothesis? (0.5 mark)

# We reject the alternative hypothesis.







#==========================================
# QUESTION 2
# Interpret the following two lift charts. (Total 4.5 marks)
#==========================================


#a)	Is this lift chart generated from a training or a validation dataset? (0.5 mark)

#It is generated from the validation dataset.



#b)	How is the X axis (cumulative number of cases) sorted? (0.5 mark)

# In order of predicted value.



#c)	What does the straight diagonal line and the curved line represent, respectively? (1 mark)

# The straight diagonal line represents the cumulative spending plotted against the cumulative number of cases without using a machine learning model

#The curved line represents the cumulative spending plotted against the cumulative number of cases using predicted values from a machine learning model. 




#d)	What does THIS lift chart talk about a model’s predicted performance? (0.5 mark)

#We can do a better prediction using the model than without using a model. The higher the lift of the curve is, the better the predictive performance of the model. 




#e)	In this Decile-wise lift chart, what does the Mean Response = 1.0 mean? (0.5 mark) (Hint: it is the mean of what?)

# This refers to the mean result when we choose 10% of samples at random.



#f)	Interpret the height of the bar in the first 10 percentile. (0.5 mark)

#This bar represents the top 10% of the samples with the highest predicted results. The height of the bar is greated than 1.5, and we estimate it to be around 1.6 or 1.7. This means that we will gain 1.6-1.7 times the amount of the predicted results compared to choosing 10% of samples at random.



#g)	True or false: lift charts and decile lift charts can evaluate the performance of both prediction analyses (numeric output variable) and classification analyses (categorical output variable). (0.5 mark) Why? (0.5 mark)

# True; Lift chart and decile lift charts is widely used in predictive modeling and evaluating classification models.





#==========================================
# QUESTION 3
# Use the “ownerExample.csv” data, run a R code to generate a confusion matrix based on a cut-off value of 0.5. (1 mark) What is the accuracy? (0.5 mark) (Total 1.5 marks)
#==========================================

#Load Dataset
dfOwner <- read.csv("ownerExample.csv")

head(dfOwner)


dfOwner$Class <- ifelse(dfOwner$Class =="owner",1,0) #convert class to binary


confusionMatrix(dfOwner$Class,dfOwner$Probability)



#==========================================
# QUESTION 4
# Use all the data in “RidingMowers.csv” (no partition is needed) to answer the following questions (Total 3.5 marks)
#==========================================

#Load dataset
dfRidingMowers <- read.csv("RidingMowers.csv")

head(dfRidingMowers)


#a)	Fit a logistic regression of ownership on the two predictors: Lot size and Income. (1.5 mark)

dfRidingMowers$Ownership <- factor(dfRidingMowers$Ownership)

model <- glm(Ownership~Income+Lot_Size,family = "binomial",data = dfRidingMowers)

predicted <- predict(model,dfRidingMowers,type="response")

dfRidingMowers$Ownership <- ifelse(dfRidingMowers$Ownership =="Owner",1,0)


#b)	The logit as a function of the predictors (0.5 mark)

# This is just the same with model
logit.reg <-glm(Ownership~.,data=dfRidingMowers,family = 'binomial')
data.frame(summary(logit.reg)$coefficients,odds = exp(coef(logit.reg)))

pred <- predict(logit.reg,dfRidingMowers)
gain <-gains(dfRidingMowers$Ownership,logit.reg$fitted.values)

confusionMatrix(ifelse(pred > 0.5, 1,0),dfRidingMowers$Ownership)

#c)	The odds as a function of the predictors (0.5 mark)

# ?????

#d)	The probability as a function of the predictors (0.5 mark)

# ?????




#e)	Among nonowners, what is the percentage of households classified correctly? Hint: use the function confusionMatrix() in the package caret. (0.5 mark)

confusionMatrix(dfRidingMowers$Ownership,predicted)

accuracy = 20/24 # 83.3% were classified correctly.


#another way to compute accuracy
accuracy = 1- misClassError(dfRidingMowers$Ownership,predicted)

accuracy



