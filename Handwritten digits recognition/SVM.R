#Clear the Environment - To avoid any testing issues.
rm(list = ls())

############################ SVM Handwritten Digit Recogniser #######################
# 1. Business Understanding
# 2. Data Understanding
# 3. Data Preparation
# 4. Model Building 
#  4.1 Linear kernel
#  4.2 RBF Kernel
# 5 Hyperparameter tuning and cross validation

#####################################################################################

# 1. Business Understanding: 

#The objective is to identify handwritten digits based on pixel values

#####################################################################################

# 2. Data Understanding: 
#http://corochann.com/mnist-dataset-introduction-1138.html

setwd('C:/pgdds/Course 4/svm assign')
train = read.csv('mnist_train.csv')
test = read.csv('mnist_test.csv')

#3. Data Preparation (EDA): 

#Lets view the structure of the train and test datasets.

str(train) #785 columns (first column is the digit classification and rest 28x28 = 784)
str(test) #785 columns (first column is the digit classification and rest 28x28 = 784)

#Now, lets see a summary of the file which will help in performing EDA - 

summary(train)
summary(test)

# Next step is to determine columns where everything is NA (such columns can be removed)
# Every EDA step is done on both train and test files.
which(sapply(train,function(x) sum(is.na(x)))!= 0) # No issues.
# Similarly for the test dataset
which(sapply(test,function(x) sum(is.na(x)))!=0) # No issues.

# Check both the files (opening takes time, hence validate using head... few records)
head(train) # Viewing train dataset.
head(test) # Viewing test dataset.

# Lets rename the first column identically in the 2 datasets (auto names assigned by R seem different)

# This is the column to be predicted, hence lets name it Hand_digit
colnames(train)[1] <- 'Hand_digit'
colnames(test)[1] <- 'Hand_digit'

# Lets rename the rest of the columns in both test and train so they are consistent

for(i in 2:785){
  colnames(train)[i] <- paste("X",i,sep="")
  colnames(test)[i] <- paste("X",i,sep="")
}

# Similarly check if every row is 0 for any columns, such columns can be removed
# since they provide no input to determine the digit.
# Storing into variable because same columns need to be removed from test dataset also
cols_to_be_removed <- which(unlist(lapply(train, function(x) ! all(x == 0))))

train <-
  train[, cols_to_be_removed] #Remove all columns with everything NA
# Check first few rows (opening whole file takes lot of time in slower pc's)
head(train)


# Since we need to remove the same columns in the test data set also, we can use same variables.

test <-
  test[, cols_to_be_removed] #Remove all columns with everything NA
# Check first few rows (opening whole file takes lot of time in slower pc's)
head(test)

#Check for duplicate values
sum(duplicated(train))   #0 - hence no rows are duplicates.
sum(duplicated(test))   #0 - hence no rows are duplicates.


#Making our target class to factor
train$Hand_digit<-as.factor(train$Hand_digit)
test$Hand_digit<-as.factor(test$Hand_digit)

# EDA is complete. In the next step, lets build model
# 4 - Next step is to construct model 

# Install Libraries

# install.packages('kernlab')
# install.packages('dplyr') etc.

library(caret)
library(kernlab)
library(dplyr)
library(readr)
library(ggplot2)
library(gridExtra)
library(MASS)
library(car)
library(e1071)
library(ggplot2)
library(cowplot)
library(caTools)


#Take stratified samples (take 5000 out of 60000), using sample.split
# The whole train dataset is too large to use for training the model (due to 
# computational power required, its not feasible)
set.seed(100)

indices = sample.split(train, SplitRatio = 1/12) #5k samples is 1/12 of 60K samples

train_not_used = train[!(indices),]

train = train[indices,]


nrow(train) # There are 4900+ records in the train dataset (which is close to 5K)
head(train)
#Constructing Model

#Using Linear Kernel
Model_linear <- ksvm(Hand_digit~., data = train, scale = FALSE, kernel = "vanilladot")
Eval_linear<- predict(Model_linear,test) # Test Accuracy : 91.45%

#confusion matrix - Linear Kernel
confusionMatrix(Eval_linear,test$Hand_digit)


#Using RBF Kernel
Model_RBF <- ksvm(Hand_digit~ ., data = train, scale = FALSE, kernel = "rbfdot")
Eval_RBF<- predict(Model_RBF, test)
Eval_train_RBF <- predict(Model_RBF, train)

#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF,test$Hand_digit) # Test Accuracy : 95.17%
confusionMatrix(Eval_train_RBF,train$Hand_digit) # Train Accuracy : 97.91%

############   Hyperparameter tuning and Cross Validation #####################

# We will use the train function from caret package to perform Cross Validation. 

#traincontrol function Controls the computational nuances of the train function.
# i.e. method =  CV means  Cross Validation.
#      Number = 2 implies Number of folds in CV.

trainControl <- trainControl(method="cv", number=5)


# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.

metric <- "Accuracy"

#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.

set.seed(7)
grid <- expand.grid(.sigma=c(0.000001, 0.000002), .C=c(0.1,0.5,1,2) )


#train function takes Target ~ Prediction, Data, Method = Algorithm
#Metric = Type of metric, tuneGrid = Grid of Parameters,
# trcontrol = Our traincontrol method.

fit.svm <- train(Hand_digit~., data=train, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

print(fit.svm)

plot(fit.svm) #They key observation is that with lower sigma the accuracy is better.
# With the current parameters we used in grid, it seems that the accuracy has dropped
# compared to the RBF model with kernel rbfdot. We could try to improve the accruacy of
# the svmRadial method by reducing the sigma more than 0.000001  (10^ -6), but
# since the accuracy is already 95+% using the rbfdot kernel on the train as well test 
# dataset, lets keep the rbfdot as the final model.
#                        -------------------------

# Output of above print(fit.svm)

# Support Vector Machines with Radial Basis Function Kernel 
# 
# 4928 samples
# 717 predictor
# 10 classes: '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' 
# 
# No pre-processing
# Resampling: Cross-Validated (5 fold) 
# Summary of sample sizes: 3941, 3943, 3945, 3941, 3942 
# Resampling results across tuning parameters:
#   
#   sigma  C    Accuracy   Kappa     
# 1e-06  0.1  0.5466588  0.49389432
# 1e-06  0.5  0.8774293  0.86376724
# 1e-06  1.0  0.9320203  0.92444625
# 1e-06  2.0  0.9344559  0.92715183
# 2e-06  0.1  0.1848644  0.08630173
# 2e-06  0.5  0.3415244  0.26385853
# 2e-06  1.0  0.7262450  0.69567062
# 2e-06  2.0  0.7459337  0.71756292
# 
# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were sigma = 1e-06 and C = 2.
