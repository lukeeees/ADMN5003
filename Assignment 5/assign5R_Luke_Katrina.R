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

#==========================================
# QUESTION 1
# With the given pivot tables, compute probabilities for a) b) c) and d) by using the Naive Bayes method. Please provide detailed calculation such as (axb)x(cxd)x(exf) / [ (gxJ) + (kxt)] etc. (Total 2 marks, 0.5 mark each)
#==========================================

#a)	P(Delayed|Weather=Good, Destination=EWR) = 
#b)	P(Delayed|Weather=Good, Destination=JFK) = 
#c)	P(Delayed|Weather=Bad, Destination=EWR) = ?
#d)	P(Delayed|Weather=Bad, Destination=JFK) = ?
      





#==========================================
# QUESTION 2
# 2.	Association Rules. Question 14.2 (The Green Book, page 352). (Total 1 mark)
#==========================================


#Run association rules on the data, with supp= 0.01, conf = 0.5. (0.5 mark) 


#Interpret the top 3 association rules. Use the function inspect(sort(rules, by = "lift")) to sort resulting rules by lift. (0.5 mark)






#==========================================
# QUESTION 3
# Using the dataset “Cereals.csv” to answer following questions on clustering. (4 marks)
#==========================================


# 1)	Remove all cereals with missing values. (0.5 mark) Normalize all predictor variables. (0.5 mark)

# 2)	Apply hierarchical clustering to the data using Euclidean distance. (1 mark) Show the clustering results from complete linkage with 5 clusters. (0.5 mark)

# 3)	Use K-means clustering with 5 clusters. (1 mark)

# 4)	Compare the results from b and c. (Hint: exam cluster members, and compute mean for each cluster) Discuss your results.  (0.5 mark)






#==========================================
# QUESTION 4
# Use the “assign2data.csv” data you created in Assignment 2 to answer the following questions. (Total 5 marks) 
#Note: this dataset has the missing values replaced with the median values, and has the outlier removed. The sample size is 24.
#==========================================

# 1)	Prepare your data (1 mark):
#a.	Transform a continuous variable into a categorical variable, or a categorical variable to continuous variable if necessary. Add an additional column for the transformed data.
#b.	Normalize your predictor variables if necessary.
#c.	Partition your data and use 60% training data for your analyses.
#d.	Choose the appropriate columns for your analyses.



#2)	Use two of the supervised-learning data mining methods (2 marks)
#a)	One method to analyze continuous output variable. Do not choose multiple linear regression for this, as you have used it in Assignment 2.
#b)	Another method to analyze categorical output variable. Note: Your output variable is continuous, therefore you need to transform it to categorical in Step 1).
#c)	Use the two predictor variables: one continuous and one categorical. Transform the data type, if necessary, in Step 1).
#d)	Plot the result if you use neural network.
#e)	Briefly describe the relationships between the two predictor variables and the output variable. For example, write the equation for logistic regression, describe the meaning of the best k and the identify the corresponding neighbors for k-NN, plot the best network for neural network, etc.
#f)	What are the similarities and differences in the results of the two analyses? (simple descriptions are needed here)



#3)	Do a performance analysis for each above method using the 40% validation data (2 marks)
#a)	Use either residual comparisons or a lift chart/decile lift chart for method a)-continuous output variable.
#b)	User either a confusion matrix or a lift chart/decile lift chart for method b)-categorical output variable.
#c)	How is the performance of your models? Is there any overfitting? 
#d)	Among the two methods used, which one is better fitted to your data? Why? (simple descriptions are needed here)
#e)	If your sample size increase from 24 to 2400, will you choose a different analytical method? Why? (simple descriptions are needed here)



