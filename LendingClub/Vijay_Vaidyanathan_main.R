#Clear the Environment - To avoid any testing issues.
rm(list = ls())

#Include stringr, dplyr and plyr libraries to perform stting & data manipulations
#Include ggplot2 to create plots. These packages can be installed using
# install.packages("ggplot2"), install.packages("chron") etc.
library(stringr) #string manipulations
library(dplyr) #string manipulations
library(plyr) #string manipulations
library(ggplot2) #for easy plotting
library(chron) #for time functions
library(tidyr) #for gather
library(reshape2) #for melt etc.

#Set working directory to import file

setwd("C:/pgdds/Course 2/Project")
#Read the file into a dataframe 
#(Read with stringAsFactors TRUE - to identify factors such as grades etc.)
loan <-
  read.csv("loan.csv",
           sep = ",",
           stringsAsFactors = TRUE)

#Check for duplicate values
sum(duplicated(loan$id))   #0 - hence no rows are duplicates.
# We could also use to check which row is duplicate 
loan[which(duplicated(loan$id) == T), ] #none

#Data understanding and Cleaning
#-------------------------------
# As this data set contains information on loan data, its important to analyze for
# missing values in key fields such as loan amount, member id, loan id, int_rate etc.
# Any null values in these fields need to be analyzed and then it needs to be determined
# if the value needs to be or can be imputed, or whether the data in the particular row 
# should not be considered for analysis

#Check for NA/blanks values for the critical fields 
# (Ideally these values should be present, if not, 
# these values cannot be imputed and the record cannot be analyzed)
sum(is.na(loan$id),length(which(loan$id == ""))) 
sum(is.na(loan$member_id),length(which(loan$member_id == "")))
sum(is.na(loan$loan_amnt),length(which(loan$loan_amnt == "")))
sum(is.na(loan$funded_amnt),length(which(loan$funded_amnt == "")))
sum(is.na(loan$funded_amnt_inv),length(which(loan$funded_amnt_inv == "")))
sum(is.na(loan$term),length(which(loan$term == "")))
sum(is.na(loan$int_rate),length(which(loan$int_rate == "")))
sum(is.na(loan$installment),length(which(loan$installment == "")))
sum(is.na(loan$grade),length(which(loan$grade == "")))
sum(is.na(loan$sub_grade),length(which(loan$sub_grade == "")))
sum(is.na(loan$verification_status),length(which(loan$verification_status == "")))
sum(is.na(loan$issue_d),length(which(loan$issue_d == "")))
sum(is.na(loan$loan_status),length(which(loan$loan_status == "")))

#Output is 0 for all the above columns, hence none of these are nulls or blanks ("").

#The information is for loans between 2007 to 2011 
#but the issue_d column date format needs to be corrected 
#as its not in the correct format
# The column currently reflects Dec 2011 as Dec-11, lets split it into two columns
# so we can analyze the data by months and years separately if required.

#Split the Request and Drop timestamp columns into Date and Time (Also convert YY to YYYY etc.)
loan$issue_month <-   str_split_fixed(loan$issue_d, "-", 2)[, 1]
loan$issue_year<- paste("20",str_split_fixed(loan$issue_d, "-", 2)[, 2],sep="")

#Remove issue_d column as its no longer needed
loan$issue_d <- NULL

# Data Cleaning :- Check the structure of the file
# Summary of the file can be verified to check for uppercase/lowercase issues etc.  
str(loan)
#identify uppercase/lowercase issues in fields such as grade, subgrade etc. This is
# important since otherwise lower case and upper case may be interpreted as different
# when they are actually the same.
summary(loan$grade) #no issues.
summary(loan$sub_grade) #no issues.
summary(loan$emp_length) #no issues. There are some with n/a, its not null, just not available.
summary(loan$home_ownership) #no issues.
summary(loan$verification_status) #no issues. Only 3 status available.
summary(loan$loan_status) #no issues. 5627 loans charged off.
summary(loan$addr_state) #no issues.
length(unique(loan$addr_state)) #Data for 50 states in US
length(unique(loan$application_type)) #Data for only one type of application. #no issues.

#Create a new df for only the loans that have been charged off
loan_charged_off <- filter(loan,loan_status == "Charged Off")
nrow(loan_charged_off) #5627 rows

#---------------------------------
#Meanings of variable

#Data issues Identification 
#Data Cleaning (Missing Values imputation, Outlier treatment, Treatment of Data redundancies)
#Data Conversion to usable formats as required

#

#Univariate

#Segmented Univariate

#Derived (Business, Type, Data)

#Analysis and determination of at least 5 important driver fields 
# (Variables that are strong indicators of default)

#Bivariate analysis to identify the important combinations of driver variables. 
# Combinations to make business and analytical sense.

#Explain insights in comments

#ggplots for insights along with axis labels

#Create ppt listing actionable and realistic recommendations.