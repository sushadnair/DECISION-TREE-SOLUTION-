## DECISIONTREE EXERCISE..

# OBJECTIVE : The main objective of data set is to classify that wether a perticular person has diabetes or not with help of several independaent variables given in data set..

## INSTALLING PACKAGES..

install.packages('caTools')  #for train and test data split
install.packages('dplyr')    #for Data Manipulation
install.packages('ggplot2')  #for Data Visualization
install.packages('class')    #KNN 
install.packages('caret')    #Confusion Matrix
install.packages('corrplot') #Correlation Plot
install.packages("readr")
install.packages("C50")      # Training a model on the data
install.packages("tidyverse")
install.packages("funModeling")
install.packages("Hmisc")


library(caTools)
library(dplyr)
library(ggplot2)
library(caret)
library(class)
library(corrplot)
library(readr)
library(C50)
library(funModeling) 
library(tidyverse) 
library(Hmisc)

##         PERFORMING EDA , DATA PREPROCESSING AND CLEANING ,..........

summary(diabetes)

# Number of observations (rows) and variables, and a head of the first cases.

glimpse(diabetes)

# < SO WE HAVE Rows: 768 , Columns: 9..>

# Getting the metrics about data types, zeros, infinite numbers, and missing values:...

status(diabetes)

# Analyzing categorical variables...

freq(diabetes)

# Analyzing numerical variables....

plot_num(diabetes)


##   Analyzing numerical and categorical at the same time....

library(Hmisc)
describe(diabetes)

# BMI can not be exactly "zero" as its not possible practically so we need to replace these Zero values with NA to replace them with some value using kNN computation method....

library(dplyr) # to use select function

sum(is.na(diabetes)) #   To check total number is NA present 

 # ##############      Lets checK data types... 

str(diabetes)

# As we are perforaing classification hence we are converting integer dependent variable to factor....

diabetes$Class.variable=factor(diabetes$Class.variable)

# Normalization of dataset required as independent variable has diffrent ranges which will affect on our classification models

diabetes2<-diabetes[,-9]

normalize=function(x){
  return((x-min(x))/(max(x)-min(x)))
}
diabetes_norm<-normalize(diabetes2)

diabetes[,1:8]<-diabetes_norm[,1:8] # Replacing normalize value of data with orignal data set
head(diabetes) # checking head again

## Partition dataset in training dataset & testing data set to build model & check wether model has build correctly or not..

str(diabetes)

library(caret) # its a classification and regression training..

install.packages("lattice")
library(lattice)

install.packages("ggplot2")
library(ggplot2)


#   Read file diabetes...

diabetes<-read.csv(file.choose(),header = T) # header=T means it will consider first row of data set as header and it will not take it in computation part
head(diabetes) # shows first 6 rows of data set

##  PERFORMING EDA AND DATA PREPROCESSING AND CLEANING ...

diabetes$default <- as.factor(diabetes$default)
diabetes$checking_balance <- as.factor(diabetes$checking_balance)

# Shuffle the data
diabetes_rand <- diabetes[order(runif(768)), ]
str(diabetes_rand)

# split the data frames
diabetes_train <- diabetes_rand[1:678, ]
diabetes_test  <- diabetes_rand[679:768, ]

# check the proportion of class variable
prop.table(table(diabetes_rand$default))
prop.table(table(diabetes_train$default))
prop.table(table(diabetes_test$default))

diabetes_model <- C5.0(diabetes_train[, -9], diabetes_train$Class.variable)


windows()
plot(diabetes_model) 

# Display detailed information about the tree
summary(diabetes_model)


# Step 4: Evaluating model performance
# Test data accuracy
test_res <- predict(diabetes_model, diabetes_test)
test_acc <- mean(diabetes_test$Class.variable == test_res)
test_acc


# On Training Dataset
train_res <- predict(diabetes_model, diabetes_train)
train_acc <- mean(diabetes_train$Class.variable == train_res)
train_acc

table(diabetes_train$Class.variable, train_res)



 # END...