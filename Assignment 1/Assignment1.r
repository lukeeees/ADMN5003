# Date: 09/16/2022
# Authors: Katrina Ong, Luke Laylo
# Data Mining for Business Analytics text

#create dataframe
df <- data.frame(age = c(25,56,65,32,41,49),
                 income = c(49000,156000,99000,19200,39000,57000))
df

summary(df)

age_mean <- mean(df$age)
age_sd <- sd(df$age)

#find z-score
zScoreAge<- (df$age - age_mean) / age_sd

#display z-score
zScoreAge


income_mean <-mean(df$income)
income_sd <-sd(df$income)

zScoreIncome <- (df$income - income_mean)/income_sd

zScoreIncome

#or 
sapply(df,function(df) (df-mean(df))/sd(df))
