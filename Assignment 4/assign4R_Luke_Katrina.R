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
library(reshape2)


#==========================================
# QUESTION 1
# Use “BostonHousing.csv” to predict the median house price in new tracts based on information such as crime rate, pollution, number of rooms and distance to employment centers. The dataset contains 13 predictors, and the response is the median house price (MEDV). 
#==========================================

#Load the dataset
dfBoston <- read.csv("BostonHousing.csv")
View(dfBoston)


# a)	Partition the data. Use training data to build the following 2 models. (0.5 mark)

set.seed(0)

trainIndex_Boston <- sample(row.names(dfBoston), 0.6*dim(dfBoston)[1])  
validIndex_Boston <- setdiff(row.names(dfBoston), dfBoston)  
train_dfBoston <- dfBoston[trainIndex_Boston, ]
valid_dfBoston <- dfBoston[validIndex_Boston, ]



# b) Fit a multiple linear regression model to the median house price (MEDV) as a function of CRIM, CHAS, RM and DIS. (0.5 mark) Which predictors are significant? (0.5 mark)

model <- lm(MEDV ~ CRIM+CHAS+RM+DIS, data = train_dfBoston)
summary(model)

#significant predictors include CRIM, CHAS and RM



# c) Find the best model of the above model using the “backward” method. (0.5 mark) Write the equation of the best model. (0.5 mark)  output variable = intercept + coeff 1 X1, coeff 2 X2…)

model_step <- step(model,direction = "backward")
summary(model_step)

#MEDV = -26.50 + -0.23 CRIM + 5.10 CHAS + 7.85 RM 



#d) Use correlation matrix to discuss the relationships among INDUS, NOX and TAX. (0.5 mark) 1 bonus mark if you can use one of the heatmaps we learned in class.

cormat_Boston <- round(cor(train_dfBoston),2)
cormat_Boston

melted_cormat_Boston <- melt(cormat_Boston)
head(melted_cormat_Boston)

ggheatmap <- ggplot(data = melted_cormat_Boston,aes(x = Var1, y = Var2, fill = value )) + 
  geom_tile(color = "white") + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Pearson\nCorrelation") + 
  theme_minimal()


ggheatmap + 
  geom_text(aes(Var1,Var2, label = value), color = "black", size = 2) +
  theme(legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,title.position = "top", title.hjust = 0.5))




#e) Fit a logistic regression model to the categorized median house price (CAT.MEDV) as a function of CRIM, CHAS, RM and DIS. (0.5 mark) Which predictors are significant? (0.5 mark)

model2 <- glm(CAT..MEDV ~ CRIM+CHAS+RM+DIS, data = train_dfBoston, family = "binomial")
options(scipen=999)
summary(model2)

#RM is the only significant predictor.



#f) List 3 differences between multiple linear regression and logistic regression. (1.5 marks)

#f-1) Output for multiple linear regression is numeric, while for logistic regression it is categorical
#f-2) Parameter estimation for multiple linear regression is least squares, while for logistic regression it is maximum likelihood
#f-3) Performance measures for multiple linear regression include lift charts and accuracy test, while for logistic regression it is a confusion matrix.


#==========================================
# QUESTION 2
# Use “BostonHousing.csv” to predict the median house price in new tracts based on information such as crime rate, pollution, number of rooms and distance to employment centers. The dataset contains 13 predictors, and the response is the median house price (MEDV). 
#==========================================
set.seed(0)

trainIndex_Boston <- sample(row.names(dfBoston), 0.6*dim(dfBoston)[1])  
validIndex_Boston <- setdiff(row.names(dfBoston), dfBoston)  
train_dfBoston <- dfBoston[trainIndex_Boston, ]
valid_dfBoston <- dfBoston[validIndex_Boston, ]

train_dfBoston
valid_dfBoston

# initialize normalized training, validation data, complete data frames to originals
train.norm.df <- train_dfBoston
valid.norm.df <- valid_dfBoston
boston.norm.df <- dfBoston

# a) Perform a k-NN prediction with all 12 predictors (the output variable is MEDV, ignore the CAT.MEDV column), trying values of k from 1 to 5. Partition your data. (1 mark) 

#Normalize the data. (0.5 marks)

#use preProcess() from the caret package
norm.values <- preProcess(train_dfBoston[, 1:12], method=c("center", "scale"))
norm.values
norm.values$dim

train.norm.df[, 1:12] <- predict(norm.values, train_dfBoston[, 1:12]) #predict (model, value)
valid.norm.df[, 1:12] <- predict(norm.values, valid_dfBoston[, 1:12])
boston.norm.df[, 1:12] <- predict(norm.values, dfBoston[, 1:12])

#Choose function knn() from package class rather than package FNN. (0.5 mark) To make sure R is using the class package when both packages are loaded, use class::knn().

nn <- class::knn(train= train.norm.df[,1:12], test = valid.norm.df[,1:12] , cl = train.norm.df[,13],k=1)

#or

grid = expand.grid(k = c(1,2,3,4,5))
model_nn <-train(MEDV~., data = train.norm.df[,1:13], method="knn", trControl = trainControl("cv", search = "grid"),tuneGrid = grid)
model_nn$results

# b) What is the best k? (0.5 mark)

accuracy_dfBoston <-data.frame(k = seq(1,5,1),RMSE = rep(0,5))
accuracy_dfBoston
# compute knn for different k on validation.

for(i in 1:5) {
  knn.pred <- class::knn(train = train.norm.df[, 1:12], test = valid.norm.df[, 1:12],
                 cl = train.norm.df[, 13], k = i)
  accuracy_dfBoston[i, 2] <-caret::RMSE(as.numeric(as.character(knn.pred)),as.numeric(as.character(valid.norm.df$MEDV)))
}

accuracy_dfBoston
#K=2, because when K = 2 it has the least RMSE across the 5 K's which is 4.58 RMSE. 
# The 2 solutions would differ in RMSE but it gives out K=2 has the lowest RMSE.


# c) What does the best k mean (i.e. how many nearest neighbors shall be used to determine the predicted value of MEDV, and how is the value determined)? (0.5 mark) 
nn <- class::knn(train= train.norm.df[,1:12], test = valid.norm.df[,1:12] , cl = train.norm.df[,13],k=2)
# The best K means how many nearest neighbor shall be used. In this case because its a numerical outcome the lowest error is used to give the best value of K.



#==========================================
# QUESTION 3
# Neural Nets (Total 3.5 marks)
#==========================================
#install.packages("neuralnet")
library(neuralnet)

# 1)	A neural net typically starts out with random coefficients; hence, it produces essentially random predictions when presented with its first case. What is the key ingredient by which the net evolves to produce a more accurate prediction? (0.5 mark)

#The iterative learning process in which rows are presented to the network one at a time, and the weights associated.

# 2) Use data “ToyotaCorolla.csv” to predict the price of a used Toyota Corolla based on its specifications.
#Use the three predictors Age_08_04, KM, and Fuel_Type to build a neural network model. (1 mark) 

dfToyotaCorolla <- read.csv("ToyotaCorolla.csv")
View(dfToyotaCorolla)


#Convert the categorical variable to dummies. (0.5 mark) 
unique(dfToyotaCorolla$Fuel_Type)
dfToyotaCorolla$Fuel_Type = factor(dfToyotaCorolla$Fuel_Type,levels = c('Diesel', 'Petrol', 'CNG'),labels = c(0, 1, 2))

dfToyotaCorolla.simp <-dfToyotaCorolla[,c('Age_08_04','KM','Fuel_Type','Price')]
dfToyotaCorolla.simp
#Scale the numerical predictor and outcome variables to a 0-1 scale. (0.5 mark) 

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

scale0to1 <- as.data.frame(lapply(dfToyotaCorolla.simp[,c("Age_08_04","KM","Price")], normalize))
scale0to1

dfToyotaCorolla.fin <-cbind(scale0to1,Fuel_Type=as.numeric(dfToyotaCorolla.simp$Fuel_Type))
finaldf<-as.data.frame(dfToyotaCorolla.fin)
finaldf
#Fit a neural network model to the data. Use a single hidden layer with 2 nodes. (0.5 mark) 

nn <- neuralnet::neuralnet(Price ~ ., data = finaldf, linear.output = F, hidden = 2)
nn$result.matrix
#Plot the neural network. (0.5 mark)
plot(nn)

#model validation

results<-data.frame(actual = finaldf$Price, prediction = n$net.result)
View(results)
