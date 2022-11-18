#==========================================
# ADMN5003 - ASSIGNMENT 4
#==========================================
#------------------------------------------
# install.packages('caret')
# install.packages('InformationValue')
# install.packages('gains')
# install.packages("FNN")
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
library(FNN)


#==========================================
# QUESTION 1
# Use “BostonHousing.csv” to predict the median house price in new tracts based on information such as crime rate, pollution, number of rooms and distance to employment centers. The dataset contains 13 predictors, and the response is the median house price (MEDV). 
#==========================================

#Load the dataset
dfBoston <- read.csv("BostonHousing.csv")



# a)	Partition the data. Use training data to build the following 2 models. (0.5 mark)

set.seed(0)

trainIndex_Boston <- sample(row.names(dfBoston), 0.6*dim(dfBoston)[1])  
validIndex_Boston <- setdiff(row.names(dfBoston), dfBoston)  
train_dfBoston <- dfBoston[Boston_trainIndex, ]
valid_dfBoston <- dfBoston[Boston_validIndex, ]



# b) Fit a multiple linear regression model to the median house price (MEDV) as a function of CRIM, CHAS, RM and DIS. (0.5 mark) Which predictors are significant? (0.5 mark)




# c) Find the best model of the above model using the “backward” method. (0.5 mark) Write the equation of the best model. (0.5 mark)  output variable = intercept + coeff 1 X1, coeff 2 X2…)




#d) Use correlation matrix to discuss the relationships among INDUS, NOX and TAX. (0.5 mark) 1 bonus mark if you can use one of the heatmaps we learned in class.





#e) Fit a logistic regression model to the categorized median house price (CAT.MEDV) as a function of CRIM, CHAS, RM and DIS. (0.5 mark) Which predictors are significant? (0.5 mark)



#f) List 3 differences between multiple linear regression and logistic regression. (1.5 marks)



#==========================================
# QUESTION 2
# Use “BostonHousing.csv” to predict the median house price in new tracts based on information such as crime rate, pollution, number of rooms and distance to employment centers. The dataset contains 13 predictors, and the response is the median house price (MEDV). 
#==========================================

# a) Perform a k-NN prediction with all 12 predictors (the output variable is MEDV, ignore the CAT.MEDV column), trying values of k from 1 to 5. Partition your data. (1 mark) 


#Normalize the data. (0.5 marks) 


#Choose function knn() from package class rather than package FNN. (0.5 mark) To make sure R is using the class package when both packages are loaded, use class::knn().


# b) What is the best k? (0.5 mark)



# c) What does the best k mean (i.e. how many nearest neighbors shall be used to determine the predicted value of MEDV, and how is the value determined)? (0.5 mark) 




#==========================================
# QUESTION 3
# Neural Nets (Total 3.5 marks)
#==========================================


# 1)	A neural net typically starts out with random coefficients; hence, it produces essentially random predictions when presented with its first case. What is the key ingredient by which the net evolves to produce a more accurate prediction? (0.5 mark)


# 2) Use data “ToyotaCorolla.csv” to predict the price of a used Toyota Corolla based on its specifications. Use the three predictors Age_08_04, KM, and Fuel_Type to build a neural network model. (1 mark) 


#Convert the categorical variable to dummies. (0.5 mark) 


#Scale the numerical predictor and outcome variables to a 0-1 scale. (0.5 mark) 


#Fit a neural network model to the data. Use a single hidden layer with 2 nodes. (0.5 mark) 


#Plot the neural network. (0.5 mark)
