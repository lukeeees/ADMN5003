#==========================================
# ADMN5003 - ASSIGNMENT 2
#==========================================
#------------------------------------------

#==========================================
# LOAD PACKAGES
#==========================================
library(readxl)
library(forecast)
library(ggplot2)
library(dplyr)
library(tidyr)
library(data.table)
library(testthat) #need to load this first to avoid problems with psych::describe
library(psych)



#==========================================
# QUESTION 1 - Random Sampling
#==========================================

#Our population includes Taylor Swift songs from her oldest and latest albums.This refers to songs from her self-titled album "Taylor Swift" in 2006 and her re-released "Fearless (Taylor's Version)" in 2021. For each of these 41 songs, we generated a random number using the RAND() function on Microsoft Excel and ranked the generated random numbers in descending order. 24 songs with the top random numbers were chosen as samples. 


#==========================================
# QUESTION 2 - Explore and Summarize Data
#==========================================

#Load the Dataset
df <- read.csv("assign2data_Luke_Katrina.csv")

head(df)

#a)	Replace the missing values with the median values of their corresponding columns. 

median_danceability <- median(df$danceability, na.rm = TRUE)

median_tempo <- median(df$tempo, na.rm = TRUE)

df_clean <- df %>% replace_na(list(danceability = median_danceability,tempo = median_tempo))



#b)	Compute the mean, median, min, max and standard deviation for each of the continuous variables, using sapply(). 
options(warn=-1)
df_cont = df_clean[,2:3] #select the continuous variable in the dataframe
sapply(df_cont, function(df_cont) c( "Mean"= mean(df_cont,na.rm=TRUE),
                                       "Median" = median(df_cont),
                                       "Min" = min(df_cont),
                                       "Max" = max(df_cont),
                                       "Std dev" = sd(df_cont)
                                       )
       )


#c)	Plot a side-by-side box plot of dependent variable (column 2) as a function of categorical independent variable (column 4). Include main title and the label of two axes. 

ggplot(df_clean, aes(x = album, y = danceability))+ ggtitle("Danceability in Taylor Swift Albums") +xlab("Album") + ylab("Danceability Rating")+ geom_boxplot() 




#d) Build a scatter plot between dependent variable (column 2) and independent continuous variable (column3). Include main title and the label of two axes. 

ggplot(df_clean, aes(x = tempo, y = danceability))+ ggtitle("Tempo and Danceability in Taylor Swift Songs") +xlab("Tempo") + ylab("Danceability")+ geom_point()


#==========================================
# QUESTION 3 - Partition Data and Predict Accuracy Measures
#==========================================


#a)	Partition the data into training (60%) and validation (40%) sets.

set.seed(1)

train.rows <- sample(rownames(df_clean), dim(df_clean)[1]*0.6) 

train.data <- df_clean[train.rows, ]

valid.rows <- setdiff(rownames(df_clean), train.rows) 
valid.data <- df_clean[valid.rows, ]

dim(train.data)
dim(valid.data)


#b)	Fit a linear regression model to the two continuous variables in the training data.

reg <- lm(danceability ~ tempo, data = train.data) 
pred <- predict(reg, newdata = valid.data)




#c)	Use the “forecast” package to compare the accuracy of the model on training and validation data. Is there any overfitting of the model? Why?

#checking accuracy on training set
acc_train <- accuracy(reg$fitted.values, train.data$danceability)

#checking accuracy on validation set
acc_valid <- accuracy(pred, valid.data$danceability)

acc_train

acc_valid

#There is no overfitting since the RMSE for the validation data (0.064) is lower than the train data (0.69)




#==========================================
# QUESTION 4 - Cereals Data
#==========================================
#Load convenience functions for PCA

#Load Dataset
dfCereals <- read.csv("Cereals.csv")

#a)	Perform a Principal Component Analysis on “any 7 variables” using normalized data. Remove missing values. (1 mark)

#Check NA
names(which(colSums(is.na(dfCereals)) > 0))

# replace NA with median by selecting columns
clean_cereal <- dfCereals %>%
  mutate_at(c('carbo','sugars','potass'), ~replace_na(.,median(.,na.rm=TRUE)))
print(clean_cereal)


#no more missing values
names(which(colSums(is.na(clean_cereal)) > 0)) 

pca_cereal <- prcomp(clean_cereal[,4:15], scale = T, center = T)
summary(pca_cereal)
barplot(pca_cereal$rotation[,1],main="PC Loading Plot",las =2)

plot(pca_cereal,type='barplot')


#b)	How many components in your future modelling are sufficient, and why?     (1 mark)

#5 will be enough, because the covariance is near zero as it moves down. The first 5 components explains about 81% of the variance.


