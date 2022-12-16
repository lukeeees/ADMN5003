#==========================================
# ADMN5003 - ASSIGNMENT 4
#==========================================
#------------------------------------------


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
library(e1071)
#install.packages("arules")
library(Matrix)
library(arules)
set.seed(0)
#==========================================
# QUESTION 1
# With the given pivot tables, compute probabilities for a) b) c) and d) by using the Naive Bayes method. Please provide detailed calculation such as (axb)x(cxd)x(exf) / [ (gxJ) + (kxt)] etc. (Total 2 marks, 0.5 mark each)
#==========================================

#a)	P(Delayed|Weather=Good, Destination=EWR)

  #((P(Delayed)*P(W=Good|Delayed))/P(W=Good))* P(D=EWR)
  
  ((428/2202)*(396/428)/(2169/2202))*(1182/2202)
    

#b)	P(Delayed|Weather=Good, Destination=JFK)

  #((P(Delayed)*P(W=Good|Delayed))/P(W=Good))* P(D=JFK)
  
  ((428/2202)*(396/428)/(2169/2202))*(1020/2202)


#c)	P(Delayed|Weather=Bad, Destination=EWR)

  #((P(Delayed)*P(W=Bad|Delayed))/P(W=Bad))* P(D=EWR)

  ((428/2202)*(32/428)/(33/2202))*(1182/2202)


#d)	P(Delayed|Weather=Bad, Destination=JFK)
      
  #((P(Delayed)*P(W=Bad|Delayed))/P(W=Bad))* P(D=JFK)

  ((428/2202)*(32/428)/(33/2202))*(1020/2202)





#==========================================
# QUESTION 2
# 2.	Association Rules. Question 14.2 (The Green Book, page 352). (Total 1 mark)
#==========================================
course.topics.df <- read.csv("Coursetopics.csv")
head(course.topics.df)

course.mat<-as.matrix(course.topics.df)
course.mat

#Run association rules on the data, with supp= 0.01, conf = 0.5. (0.5 mark) 
rules <- apriori(course.mat,parameter = list(sup = 0.01, conf = 0.5, target = "rules"))
head(course.topics.df)

#Interpret the top 3 association rules. Use the function inspect(sort(rules, by = "lift")) to sort resulting rules by lift. (0.5 mark)
inspect(head(sort(rules,by="lift"),n=3))


# the LHS are the frequent set of items which is the antecedent which in this case
# 1. Intro, Regression, Forecast
# 2. Intro, Survey, DOE
# 3. Intro, DataMining, Cat. Data

# the Rhs are the consequent which couses they'll likely take after getting the antecedents.

#==========================================
# QUESTION 3
# Using the dataset “Cereals.csv” to answer following questions on clustering. (4 marks)
#==========================================
Cereals <- read.csv("Cereals.csv")
View(Cereals)

# 1)	Remove all cereals with missing values. (0.5 mark) Normalize all predictor variables. (0.5 mark)

# removing all cereals with missing values
nrow(Cereals)
Cereals[Cereals==0] <- NA
Cereals <- na.omit(Cereals)
View(Cereals)

# Normalize all predictor values
norm.values <- preProcess(Cereals, method=c("center", "scale"))
cereals.df.norm <-predict(norm.values,Cereals)

row.names(cereals.df.norm) <- row.names(Cereals) 

# 2)	Apply hierarchical clustering to the data using Euclidean distance. (1 mark) Show the clustering results from complete linkage with 5 clusters. (0.5 mark)
d.norm <- dist(cereals.df.norm[,4:15], method = "euclidean")
d.norm

hc1 <- hclust(d.norm, method = "single") #single means minimum distance
plot(hc1, hang = -1, ann = FALSE)
hc2 <- hclust(d.norm, method = "average")
plot(hc2, hang = -1, ann = FALSE)

# 3)	Use K-means clustering with 5 clusters. (1 mark)

View(cereals.df.norm)
kcereals <- kmeans(cereals.df.norm[,4:15], 5)
kcereals$cluster

# plot an empty scatter plot
plot(c(0), xaxt = 'n', ylab = "", type = "l", 
     ylim = c(min(kcereals$centers), max(kcereals$centers)), xlim = c(0, 12))


# plot centroids
for (i in c(1:5))
  lines(kcereals$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1, 3, 5),
                                                       "black", "dark grey"))

# name clusters
text(x = 0.5, y = kcereals$centers[, 1], labels = paste("Cluster", c(1:6)))

# 4)	Compare the results from b and c. (Hint: exam cluster members, and compute mean for each cluster) Discuss your results.  (0.5 mark)
# comparing the results of b and c we can see that the clustering for b is a paired distance while 
# c it is clustered to its mean centroid.




#==========================================
# QUESTION 4
# Use the “assign2data.csv” data you created in Assignment 2 to answer the following questions. (Total 5 marks) 
#Note: this dataset has the missing values replaced with the median values, and has the outlier removed. The sample size is 24.
#==========================================


# 1)	Prepare your data (1 mark):
#------------------------------------------

df <- read.csv("assign2data_Luke_Katrina.csv")

head(df)
View(df)

median_danceability <- median(df$danceability, na.rm = TRUE)

median_tempo <- median(df$tempo, na.rm = TRUE)

df_clean <- df %>% replace_na(list(danceability = median_danceability,tempo = median_tempo))

View(df_clean)

head(df_clean)



#2)	Use two of the supervised-learning data mining methods (2 marks) - #kNN and logistic regression 
#------------------------------------------


#2.1) Data Preprocessing
#------------------------------------------
df_clean$danceabilityCode <- ifelse(df_clean$danceability > 0.5,1,0) #encode target (1 = danceable, 0 = not danceable)

df_clean$danceabilityActualCat <- ifelse(df_clean$danceabilityCode == 1, "Yes","No")

df_clean$albumCode <- ifelse(df_clean$album == "Taylor Swift", 1, 0)

#Note: Scaling the data is not needed since one predictor variable is categorical, and only one is numeric

View(df_clean)



trainIndex <- sample(row.names(df_clean), 0.6*dim(df_clean)[1])  
validIndex <- setdiff(row.names(df_clean), trainIndex)  
train_df <- df_clean[trainIndex, ]
valid_df <- df_clean[validIndex, ]

dim(df_clean)
dim(train_df)
dim(valid_df)






#2.2) Logistic Regression
#------------------------------------------


logit <- glm(data=train_df,  danceabilityCode ~ tempo+albumCode, family = binomial)


valid_df$probDance <- predict(logit, newdata = data.frame(valid_df[,c("tempo","albumCode","danceabilityCode")]),type="response")

valid_df$danceability_logitpred <-ifelse(valid_df$probDance >0.5,"Yes","No")


#Equation for Logistic Regression
summary(logit)

#logit_equation = 6.93159+(-0.04321*tempo)+(-0.70058*albumCode)



#Confusion Matrix
table(valid_df$danceability_logitpred, valid_df$danceabilityActualCat, dnn=c("Predicted", "Actual"))

mean(valid_df$danceability_logitpred  == valid_df$danceabilityActualCat)
#90% accuracy on test set


#Check for overfitting

train_df$probDance <- predict(logit, newdata = data.frame(train_df[,c("tempo","albumCode","danceabilityCode")]),type="response")

train_df$danceability_logitpred <-ifelse(train_df$probDance >0.5,"Yes","No")

table(train_df$danceability_logitpred, train_df$danceabilityActualCat, dnn=c("Predicted", "Actual"))

mean(train_df$danceability_logitpred  == train_df$danceabilityActualCat)
#92% accuracy on training set; since it it higher on the training set, this model is over-fitting

#lift chart
lift.logit <- lift(relevel(as.factor(danceability_logitpred), ref="Yes") ~ probDance, data = train_df)
xyplot(lift.logit, plot = "gain")




#2.3) K-Nearest Neighbors
#------------------------------------------


#Train Model
nn <- class::knn(train= train_df[,c("tempo","albumCode")], test = valid_df[,c("tempo","albumCode")] , cl = train_df[,"danceability"],k=1)

accuracy_df <-data.frame(k = seq(1,5,1),RMSE = rep(0,5))
accuracy_df

# compute knn for different k on validation.
for(i in 1:5) {
  knn.pred <- class::knn(train= train_df[,c("tempo","albumCode")], test = valid_df[,c("tempo","albumCode")] , cl = train_df[,"danceability"], k = i)
  
  accuracy_df[i, 2] <-caret::RMSE(as.numeric(as.character(knn.pred)),as.numeric(as.character(valid_df$danceability)))
}

accuracy_df 

train_df
#Check for overfitting
knn.pred.train <- class::knn(train= train_df[,c("tempo","albumCode")], test = train_df[,c("tempo","albumCode")] , cl = train_df[,"danceability"], k = 3)

caret::RMSE(as.numeric(as.character(knn.pred.train)),as.numeric(as.character(train_df$danceability)))
#RMSE of 0.0770



#Meaning of Best K and Corresponding Neighbors
#K=3, because it has the least RMSE across the 5 K's which is 0.0622. This is the optimal number of nearby records that the model uses to predict values.


#lift chart
lift.knn <- lift(relevel(as.factor(danceability_logitpred), ref="Yes") ~ probDance, data = train_df)
xyplot(lift.knn, plot = "gain")






#f)	What are the similarities and differences in the results of the two analyses? (simple descriptions are needed here)

#The 2 analyses are produced by different algorithms - Logistic Regression is used for classification, while K-NN is used for prediction of a numeric value. Logistic Regression provides a binary output that refers to a predicted class of the dataset. Meanwhile, K-NN provides a predicted value.

#The Performance of a Logistic Regression model depends on the classification accuracy (the higher the better), while K-NN performance is based on error (the lower the better).






#3)	Do a performance analysis for each above method using the 40% validation data (2 marks)
#------------------------------------------
#a&b) already provided above.

#c)	How is the performance of your models? Is there any overfitting? 

# The logistic regression model is over-fitting since the accuracy on the training set (92%) is higher than the test set (90%). 

# On the other hand, KNN is not over-fitting since the RMSE is lower on the test set (0.0622) compared to the training set RMSE(0.0770)





#d)	Among the two methods used, which one is better fitted to your data? Why? (simple descriptions are needed here)

#KNN may be better suited since it is not over-fitting to the data.



#e)	If your sample size increase from 24 to 2400, will you choose a different analytical method? Why? (simple descriptions are needed here)

#More samples will allow models to be trained more accurately. However, due to the nature of the dataset, it will be more appropriate to use KNN since it predicts a numeric value for danceability. Encoding danceability into a binary classification means there is an assumption regarding the threshold of danceability for each class. On the other hand, KNN will predict the numeric value of danceability, which leaves more room for interpretation.  
