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
library(chron) 

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
# Lets plot how Attrition correlates with other categorical variables - 
# Attrition              
# BusinessTravel
# Department
# Education
# EducationField
# Gender                 
# KobLevel
# JobRole
# MaritalStatus          
# EnvironmentSatisfaction
# JobSatisfaction
# WorkLifeBalance        
# JobInvolvement
# PerformanceRating


# Barcharts for categorical features with stacked Attrition information
bartheme <- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")


empdata_orig <- empdata # Keeping a backup of the file (before scaling/dummy etc..)

plot_grid(ggplot(empdata_orig, aes(x=Education,fill=Attrition))+ geom_bar(),
          ggplot(empdata_orig, aes(x=BusinessTravel,fill=Attrition))+ geom_bar()+bartheme, 
          ggplot(empdata_orig, aes(x=Department,fill=Attrition))+ geom_bar()+bartheme,
          ggplot(empdata_orig, aes(x=EducationField,fill=Attrition))+ geom_bar()+bartheme,
          ggplot(empdata_orig, aes(x=Gender,fill=Attrition))+ geom_bar()+bartheme,
          ggplot(empdata_orig, aes(x=JobLevel,fill=Attrition))+ geom_bar()+bartheme,
          align = "h")
# EducationField (Life Sciences and Medical seem to have higher Attritions). For rest, 
# attrition seems to be evenly distributed.

plot_grid(ggplot(empdata_orig, aes(x=MaritalStatus,fill=Attrition))+ geom_bar(),ggplot(empdata_orig, aes(x=JobRole,fill=Attrition))+ geom_bar()+bartheme, 
          ggplot(empdata_orig, aes(x=JobInvolvement,fill=Attrition))+ geom_bar()+bartheme,
          ggplot(empdata_orig, aes(x=PerformanceRating,fill=Attrition))+ geom_bar()+bartheme,
          align = "h")
# Lower Performance Rating seems to have higher Attrition %.

plot_grid(ggplot(empdata_orig, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar(),
          ggplot(empdata_orig, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar()+bartheme,
          ggplot(empdata_orig, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar()+bartheme,
          align = "h") # There are a few NA's in this data. Values for these need to be imputed.
# Environment and Job Satisfaction, 1 (low) seems to have higher % attrition than 2 (medium).
# Which actually makes sense.

#----

# As there are a few NA's in the data from emp_survey, lets analyze and impute/fix/delete them.
length(which(is.na(empdata$EnvironmentSatisfaction))) # 25, A very small Percentage
length(which(is.na(empdata$JobSatisfaction))) # 20, A very small Percentage
length(which(is.na(empdata$WorkLifeBalance))) # 38, A very small Percentage

# Since there are a lot of other attributes available for every employee, just a non-availabilty of 
# few records does not warrant a complete delete. 

# To determine what we need to impute these with, lets find the frequency of each -
table(empdata$EnvironmentSatisfaction) # 3 has the highest frequency, hence we impute NA to 3.
table(empdata$JobSatisfaction) # 3& 4  have almost same frequency, hence we impute NA to 3.
table(empdata$WorkLifeBalance) # 3 has the highest frequency, hence we impute NA to 3.


#Impute missing values to 3 as analyzed above.

empdata$EnvironmentSatisfaction[which(is.na(empdata$EnvironmentSatisfaction))] <- 3

empdata$JobSatisfaction[which(is.na(empdata$JobSatisfaction))] <- 3

empdata$WorkLifeBalance[which(is.na(empdata$WorkLifeBalance))] <- 3

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

empdata$Age <- scale(empdata$Age)
empdata$DistanceFromHome <- scale(empdata$DistanceFromHome)
empdata$MonthlyIncome <- scale(empdata$MonthlyIncome)
empdata$NumCompaniesWorked <- scale(empdata$NumCompaniesWorked)
empdata$PercentSalaryHike <- scale(empdata$PercentSalaryHike)
empdata$TotalWorkingYears <- scale(empdata$TotalWorkingYears)
empdata$TrainingTimesLastYear <- scale(empdata$TrainingTimesLastYear)
empdata$YearsAtCompany <- scale(empdata$YearsAtCompany)
empdata$YearsSinceLastPromotion <- scale(empdata$YearsSinceLastPromotion)
empdata$YearsWithCurrManager <- scale(empdata$YearsWithCurrManager)


# Lets convert the factors with two levels to 1 and 0 first, then we will handle
# factors with more than 2 levels using dummy variables concept!

#Convert Attrition to 1 (Yes) and 0 (No)
# Attrition
unique(empdata$Attrition) # two values = Yes and No
empdata$Attrition<- ifelse(empdata$Attrition =="Yes",1,0)
unique(empdata$Attrition) # two values = 1 (Yes) and 0 (No)

# Gender
unique(empdata$Gender) # two values = Female and Male
empdata$Gender<- ifelse(empdata$Gender=="Female",1,0)
unique(empdata$Gender) # two values = 1 (Female) and 0 (Male)


# Need to create dummy variables for factor attributes with more than 2 levels.
#
# BusinessTravel
# Department
# Education
# EducationField
# JobLevel
# JobRole
# MaritalStatus
# ...AND....
# EnvironmentSatisfaction
# JobSatisfaction
# WorkLifeBalance
# ...AND....
# JobInvolvement
# PerformanceRating

# Lets create a new dataframe and move the data for above columns into it.
# This will enable to quickly use sapply and apply model.matrix function on these columns
# in a single-go.
empdata_chr <- empdata[,c("BusinessTravel","Department","Education",
                           "EducationField","JobLevel","JobRole",
                           "MaritalStatus","EnvironmentSatisfaction",
                          "JobSatisfaction","WorkLifeBalance",
                          "JobInvolvement","PerformanceRating")]
# Next step is to convert the columns in empdata_fact to factors, so that
# model.matrix function can be applied on it to generate dummy variable identifiers!

empdata_fact<- data.frame(sapply(empdata_chr, function(x) factor(x)))
str(empdata_fact) # all have been converted to factors with upto a max. of 9 levels.

# Next step is to create dummy variables for these using model.matrix
emp_dummies<- data.frame(sapply(empdata_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =empdata_fact))[,-1]))


# Next step is to remove the original columns from the file that were converted to dummies
empdata[,c("BusinessTravel","Department","Education",
                          "EducationField","JobLevel","JobRole",
                          "MaritalStatus","EnvironmentSatisfaction",
                          "JobSatisfaction","WorkLifeBalance",
                          "JobInvolvement","PerformanceRating")] <- NULL

# Finally, lets merge the dummies data.frame with empdata

empdata <- cbind(empdata,emp_dummies)
str(empdata) # 4410 observations.

# Lets average the in-time and out-time records for each employee to determine
# his or her average swipe in and swipe out times. We will merge this data with the empdata
# data.frame. Otherwise it will be nearly impossible to perform logistic regression.

#Convert char object to Posixct for date time manipulations.
in_time_without_Emp <- data.frame(sapply(in_time[,2:262], 
       function(x) as.POSIXlt(x, format = "%Y-%m-%d %H:%M:%S")))
out_time_without_Emp <- data.frame(sapply(out_time[,2:262], 
       function(x) as.POSIXlt(x, format = "%Y-%m-%d %H:%M:%S")))

#Just keep the HH:MM:SS (As we are only concerned with the in-times). 
in_time_without_Emp <- data.frame(lapply(in_time_without_Emp, 
       function(x) times(strftime(x,"%H:%M:%S")))) 
out_time_without_Emp <- data.frame(lapply(out_time_without_Emp, 
       function(x) times(strftime(x,"%H:%M:%S"))))

#replace NA to 0 for easier calculations
in_time_update1 <- data.frame(lapply(in_time_without_Emp,  
                                     function(x){out <- x; out[is.na(out)] <- 0; out})) 
out_time_update1 <- data.frame(lapply(out_time_without_Emp, 
                                      function(x){out <- x; out[is.na(out)] <- 0; out}))

# calculate difference in intime and outtime 
intime_outtime_diff1 <-  (out_time_update1 - in_time_update1)*24 

#Avg working hours of employee
emp_average_hours <- rowMeans(intime_outtime_diff1,na.rm = T)
Emp_Average <- data.frame(cbind(out_time[,1],emp_average_hours))

colnames(Emp_Average) <- c("EmployeeID","Average_no_of_hours")

# Now, Lets merge the Emp average data as well

empdata<- merge(empdata,Emp_Average, by="EmployeeID", all = F)

str(empdata) # 47 columns (ensures that merging happend correctly.)

# Now, we are ready to perform Logistic Regression as EDA is complete.

# Lets check the Attrition rate first 

Attrition <- sum(empdata$Attrition)/nrow(empdata)
Attrition # 16.12 % Attrition Rate.

# Lets first split the file into train and test files.

########################################################################
# splitting the data between train and test
set.seed(100)

indices = sample.split(empdata$Attrition, SplitRatio = 0.7)

train = empdata[indices,]

test = empdata[!(indices),]

########################################################################


# First Model

# Lets remove EmployeeID from the file before creating first model
empdata$EmployeeID <- NULL

model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) # AIC: 2129.1

# Lets perform StepAIC and create Model_2

model_2<- stepAIC(model_1, direction="both")

summary(model_2)

# Identify multicollinearity through VIF check.

vif(model_2)

# Lets create model_3 using parameters identified as significant from previous step

model_3<- glm(formula = Attrition ~ MonthlyIncome+ YearsAtCompany+
                DistanceFromHome+ EnvironmentSatisfaction.x2+
                EnvironmentSatisfaction.x3+EnvironmentSatisfaction.x4+
                JobInvolvement.x3+JobRole.xResearch.Scientist+
                Average_no_of_hours+Department.xSales+
                JobSatisfaction.x2+JobSatisfaction.x4+
                NumCompaniesWorked+TotalWorkingYears+
                WorkLifeBalance.x3+BusinessTravel.xTravel_Rarely+
                Department.xResearch...Development+
                JobSatisfaction.x3+WorkLifeBalance.x2+
                WorkLifeBalance.x4+JobRole.xLaboratory.Technician+
                JobRole.xResearch.Director+MaritalStatus.xSingle+
                TrainingTimesLastYear+YearsWithCurrManager+
                BusinessTravel.xTravel_Frequently+
                EducationField.xOther+JobLevel.x2+
                Education.x3+Education.x5+
                MaritalStatus.xMarried+YearsSinceLastPromotion+
                Age+Education.x4+
                JobRole.xSales.Executive, family = "binomial", data = train) 

summary(model_3) # Following variables have high p (> 0.1) - Lets remove these and create Model_4
# EducationField.xOther
# Education.x3
# MaritalStatus.xMarried

vif(model_3)  # None of them have a vif > 5, lets not exclude because of vif in this step.

# Remove 3 variables identified as low significance from model_3 and create model_4

model_4<- glm(formula = Attrition ~ MonthlyIncome+ YearsAtCompany+
                DistanceFromHome+ EnvironmentSatisfaction.x2+
                EnvironmentSatisfaction.x3+EnvironmentSatisfaction.x4+
                JobInvolvement.x3+JobRole.xResearch.Scientist+
                Average_no_of_hours+Department.xSales+
                JobSatisfaction.x2+JobSatisfaction.x4+
                NumCompaniesWorked+TotalWorkingYears+
                WorkLifeBalance.x3+BusinessTravel.xTravel_Rarely+
                Department.xResearch...Development+
                JobSatisfaction.x3+WorkLifeBalance.x2+
                WorkLifeBalance.x4+JobRole.xLaboratory.Technician+
                JobRole.xResearch.Director+MaritalStatus.xSingle+
                TrainingTimesLastYear+YearsWithCurrManager+
                BusinessTravel.xTravel_Frequently
                +JobLevel.x2
                +Education.x5
                +YearsSinceLastPromotion+
                Age+Education.x4+
                JobRole.xSales.Executive, family = "binomial", data = train) 

summary(model_4) # Education.x5 and Education.x4 have low significance (> 0.1)
vif (model_4) # None has a VIF > 5 so lets not remove anything because of VIF at this time.


# Lets remove Education.x5 and Education.x4 and create model 5

model_5<- glm(formula = Attrition ~ MonthlyIncome+ YearsAtCompany+
                DistanceFromHome+ EnvironmentSatisfaction.x2+
                EnvironmentSatisfaction.x3+EnvironmentSatisfaction.x4+
                JobInvolvement.x3+JobRole.xResearch.Scientist+
                Average_no_of_hours+Department.xSales+
                JobSatisfaction.x2+JobSatisfaction.x4+
                NumCompaniesWorked+TotalWorkingYears+
                WorkLifeBalance.x3+BusinessTravel.xTravel_Rarely+
                Department.xResearch...Development+
                JobSatisfaction.x3+WorkLifeBalance.x2+
                WorkLifeBalance.x4+JobRole.xLaboratory.Technician+
                JobRole.xResearch.Director+MaritalStatus.xSingle+
                TrainingTimesLastYear+YearsWithCurrManager+
                BusinessTravel.xTravel_Frequently+
                +JobLevel.x2+
                YearsSinceLastPromotion+
                Age+
                JobRole.xSales.Executive, family = "binomial", data = train)

summary(model_5)

