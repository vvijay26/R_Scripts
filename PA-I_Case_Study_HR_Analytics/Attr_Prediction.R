#Clear the Environment - To avoid any testing issues.
rm(list = ls())

# Install and Load the required packages
#install.packages("MASS")
#install.packages("car")
#install.packages("e1071")
#install.packages("caret", dependencies = c("Depends", "Suggests"))
#install.packages("cowplot")
#install.packages("GGally")
#install.packages('caTools')

library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)

# Loading the 5 files
setwd('C:/pgdds/Course 3/Group CS/PA-I_Case_Study_HR_Analytics')
emp_survey<- read.csv("employee_survey_data.csv", stringsAsFactors = F)
general_data<- read.csv("general_data.csv", stringsAsFactors = F)
in_time<- read.csv("in_time.csv", stringsAsFactors = F)
manager_survey<- read.csv("manager_survey_data.csv", stringsAsFactors = F)
out_time<- read.csv("out_time.csv", stringsAsFactors = F)

# Lets view the structures of these 5 files to get a general view and 
# rule out any glaring issues.

str(emp_survey) #4410 obs. of  4 variables (All integers)
str(general_data) #4410 obs. of  24 variables (high level, looks ok)
str(in_time) # 4410 obs. of  262 variables (Column 1 seems to be emp id but not named)
str(manager_survey) # 4410 obs. of  3 variables (All integers)
str(out_time) # 4410 obs. of  262 variables (Column 1 seems to be emp id but not named)

# The next step is to ensure that the Employee Id is same across all these 5 files 
# (i.e., it can be assumed as the key for further analysis)


# Rename the first column to EmployeeID in in_time and out_time files (currently its blanks!)
names(in_time)[1]<-"EmployeeID"
names(out_time)[1]<-"EmployeeID"

# Lets check for count of unique employee Id across these 5 files.

length(unique(tolower(emp_survey$EmployeeID))) # 4410 unique
length(unique(tolower(general_data$EmployeeID))) # 4410 unique
length(unique(tolower(in_time$EmployeeID))) # 4410 unique
length(unique(tolower(manager_survey$EmployeeID))) # 4410 unique
length(unique(tolower(out_time$EmployeeID))) # 4410 unique

# Though, the unique number of EmployeeID is same across the 5 files,
# we still need to compare the "Actual" employee id's across these 5 files to ensure
# consistency.

# Compare the EmployeeID in emp_survey file with each of the other 4 files.

setdiff(emp_survey$EmployeeID,general_data$EmployeeID) # Identical EmployeeID across these datasets
setdiff(emp_survey$EmployeeID,in_time$EmployeeID) # Identical EmployeeID across these datasets
setdiff(emp_survey$EmployeeID,manager_survey$EmployeeID) # Identical EmployeeID across these datasets
setdiff(emp_survey$EmployeeID,out_time$EmployeeID) # Identical EmployeeID across these datasets

# So, now we have ensured that the EmployeeID is consistent across each 5 files. 
# Merging on EmployeeID becomes easy now!

# Lets merge the 3 files except in and out times.
empdata<- merge(general_data,emp_survey, by="EmployeeID", all = F)
empdata<- merge(empdata,manager_survey, by="EmployeeID", all = F)

# Lets focus on EDA and Data preparation for these 3 files, once done, this file would be
# merged with in-time and out-time data files.

str(empdata) # 4410 objects of 29 variables.

# We can see that some of the variables are constant throughout the file, these
# can be removed as they have no impact on the outcome (attrition!)
# 
unique(empdata$EmployeeCount) # Only one value - 1
unique(empdata$StandardHours) # Only one value - 8
unique(empdata$Over18) # Only one value - "Y"

# Thus, these 3 can be removed.
empdata[,c("EmployeeCount","StandardHours","Over18")] <- NULL

# Lets create some plots to visually analyze the data of the 3 merged files.

# Barcharts for categorical features with stacked Attrition information
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")


plot_grid(ggplot(empdata, aes(x=Gender,fill=Attrition))+ geom_bar(), 
          ggplot(empdata, aes(x=BusinessTravel,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(empdata, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(empdata, aes(x=EducationField,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(empdata, aes(x=JobRole,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(empdata, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")



# Need to scale continous variables - 
# Age
# DistanceFromHome
# MonthlyIncome
# NumCompaniesWorked
# PercentSalaryHike
# TotalWorkingYears
# TrainingTimesLastYear
# YearsAtCompany
# YearsSinceLastPromotion
# YearsWithCurrManager

empdata$Age<- scale(empdata$Age)
empdata$Age<- scale(empdata$DistanceFromHome)
empdata$MonthlyIncome <- scale(empdata$MonthlyIncome)
empdata$NumCompaniesWorked <- scale(empdata$NumCompaniesWorked)
empdata$PercentSalaryHike <- scale(empdata$PercentSalaryHike)
empdata$TotalWorkingYears <- scale(empdata$TotalWorkingYears)
empdata$TrainingTimesLastYear <- scale(empdata$TrainingTimesLastYear)
empdata$YearsAtCompany <- scale(empdata$YearsAtCompany)
empdata$YearsSinceLastPromotion <- scale(empdata$YearsSinceLastPromotion)
empdata$YearsWithCurrManager <- scale(empdata$YearsWithCurrManager)


# Lets conver the factors with two levels to 1 and 0 first, then we will handle
# factors with more than 2 levels using dummy variables concept!

#Convert Attrition to 1 (Yes) and 0 (No)
# Attrition
unique(empdata$Attrition) # two values = Yes and No
empdata$Attrition<- ifelse(empdata$Attrition=="Yes",1,0)
unique(empdata$Attrition) # two values = 1 (Yes) and 0 (No)

# Gender
unique(empdata$Gender) # two values = Female and Male
empdata$Gender<- ifelse(empdata$Gender=="Female",1,0)
unique(empdata$Gender) # two values = 1 (Female) and 0 (Male)

#
unique(empdata$Attrition) # verified that there are only 2 levels
empdata$Attrition<- ifelse(empdata$Attrition=="Yes",1,0)
unique(empdata$Attrition) # two values = 1 (Yes) and 0 (No)

# Need to create dummy variables for factor attributes with more than 2 levels.
#
# BusinessTravel
# Department
# Education
# EducationField
# JobLevel
# JobRole
# MaritalStatus

# Lets create a new dataframe and move the data for above columns into it.
# This will enable to quickly use sapply and apply model.matrix function on these columns
# in a single-go.
empdata_chr <- empdata[,c("BusinessTravel","Department","Education",
                           "EducationField","JobLevel","JobRole",
                           "MaritalStatus")]
# Next step is to convert the columns in empdata_fact to factors, so that
# model.matrix function can be applied on it to generate dummy variable identifiers!

empdata_fact<- data.frame(sapply(empdata_chr, function(x) factor(x)))
str(empdata_fact) # all have been converted to factors with upto 9 levels.

# Next step is to create dummy variables for these using model.matrix
emp_dummies<- data.frame(sapply(empdata_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =empdata_fact))[,-1]))


# Next step is to remove the original columns from the file that were converted to dummies
empdata[,c("BusinessTravel","Department","Education",
                          "EducationField","JobLevel","JobRole",
                          "MaritalStatus")] <- NULL

# Finally, lets merge the dummies data.frame with empdata

empdata <- cbind(empdata,emp_dummies)
str(empdata) # 4410 observations of 46 variables.

# Now, Lets merge the in-time and out-time data as well

empdata<- merge(empdata,in_time, by="EmployeeID", all = F)
empdata<- merge(empdata,out_time, by="EmployeeID", all = F)

str(empdata) # 551 columns (ensures that merging happend correctly.)
