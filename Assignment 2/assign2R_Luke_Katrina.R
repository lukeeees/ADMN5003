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

View(df_clean)


#b)	Compute the mean, median, min, max and standard deviation for each of the continuous variables, using sapply(). 






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


#c)	Use the “forecast” package to compare the accuracy of the model on training and validation data. Is there any overfitting of the model? Why?




#==========================================
# QUESTION 4 - Cereals Data
#==========================================



#a)	Perform a Principal Component Analysis on “any 7 variables” using normalized data. Remove missing values. (1 mark)


#b)	How many components in your future modelling are sufficient, and why?     (1 mark)
