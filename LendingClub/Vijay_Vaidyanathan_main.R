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

#Data cleaning :- 
# There are no header, footers, summary etc. in the file
# None of the column names are missing and all are meaningful names
colnames(loan)
# Unnecessary columns - Columns where each instance is NA , Blanks etc.
# These columns have no relevance to the analysis, hence remove.
library(data.table)
temp <- as.data.table(loan)
loan_data <- temp[,which(unlist(lapply(temp, function(x)!all(is.na(x))))),with=F]

ncol(loan_data) #Down to 57 columns (manageable as compared to 111 column dataset)

#write.csv(loan_data, file = "loan_data.csv",row.names = F)

#Check for duplicate values
sum(duplicated(loan_data$id))   #0 - hence no rows are duplicates.
# We could also use to check which row is duplicate 
loan[which(duplicated(loan_data$id) == T), ] #none

#Data understanding and Cleaning
#-------------------------------
# Summary of the file can be verified to check for uppercase/lowercase issues etc.  
str(loan_data)
#identify uppercase/lowercase issues in fields such as grade, subgrade etc. This is
# important since otherwise lower case and upper case may be interpreted as different
# when they are actually the same.

# Lets traverse each through each column and check for data issues and clean accordingly
# id - 
sum(is.na(loan_data$id),length(which(loan_data$id == ""))) 
# member_id
sum(is.na(loan_data$member_id),length(which(loan_data$member_id == "")))
# loan_amnt
sum(is.na(loan_data$loan_amnt),length(which(loan_data$loan_amnt == "")))
# funded_amnt
sum(is.na(loan_data$funded_amnt),length(which(loan_data$funded_amnt == "")))
# funded_amnt_inv
sum(is.na(loan_data$funded_amnt_inv),length(which(loan_data$funded_amnt_inv == "")))
# term
sum(is.na(loan_data$term),length(which(loan_data$term == "")))
# int_rate
sum(is.na(loan_data$int_rate),length(which(loan_data$int_rate == "")))
# installment
sum(is.na(loan_data$installment),length(which(loan_data$installment == "")))
# grade
sum(is.na(loan_data$grade),length(which(loan_data$grade == "")))
summary(loan_data$grade) #no issues.
# sub_grade
sum(is.na(loan_data$sub_grade),length(which(loan_data$sub_grade == "")))
summary(loan_data$sub_grade) #no issues.
# emp_title- no data cleaning required.

# emp_length
summary(loan_data$emp_length) #no issues. There are some with n/a, its not null, just not available.
# home_ownership
summary(loan_data$home_ownership) #no issues.
# annual_inc
summary(loan$annual_inc) # Range of 4000 to 6 million...we can look at this more during univariate analysis.
which(is.na(loan$annual_inc)) # none of the values are NA , 

# verification_status
sum(is.na(loan_data$verification_status),length(which(loan_data$verification_status == "")))
# issue_d
sum(is.na(loan_data$issue_d),length(which(loan_data$issue_d == "")))
# loan_status
sum(is.na(loan_data$loan_status),length(which(loan_data$loan_status == "")))
summary(loan_data$loan_status) #no issues. 5627 loans charged off.
# pymnt_plan
summary(loan_data$pymnt_plan) #There is a single variable "n" in this column, 
# we can remove this whole column - disguised missing value case.
loan_data$pymnt_plan <- NULL

# url,desc - no data cleaning required.

# purpose
summary(loan_data$purpose) #no issues. no issues with case of letters etc.
# title - no cleaning reqd. Not a key field.

# zip_code - no data cleaning required (State data is enough for analysis)

# addr_state
summary(loan_data$addr_state) #no issues.
length(unique(loan_data$addr_state)) #Data for 50 states in US
length(unique(loan_data$application_type)) #Data for only one type of application. #no issues.


# dti - This is a very important characteristic, it basically is the ratio of 
# monthly debt payment on all debt obligations (excluding mortgage)/ Personal income
# If this ratio is high, it means that the individual is in high debt (probably increases risk of default)
summary(loan$dti) # Range of 0 to 30%...seems reasonable.
which(is.na(loan$dti)) # none of the values are NA , 
# but lets see how many are 0 (since 0 does not make sense, if there is an open loan)
length(which(loan$dti == 0)) #183 out of ~40K records (which is <0.5%, not significant)
#Decision is to not impute this to some non-zero number, but leave it as-is at 0.

# delinq_2yrs - number of 30+ days deliquency (delayed non payments)
summary(loan$delinq_2yrs)
which(is.na(loan$delinq_2yrs)) # none of the values are NA , 

# Some other key fields ->

# revol_bal - The total credit revolving balance
summary(loan$revol_bal)
which(is.na(loan$revol_bal)) # none of the values are NA , summary looks fine.
# total_acc - # of credit lines, higher the credit lines signals higher chance of default
summary(loan$total_acc)
which(is.na(loan$total_acc)) # none of the values are NA , summary looks fine. 
# out_prncp
summary(loan$out_prncp) # Outstanding principal
which(is.na(loan$out_prncp)) # none of the values are NA , summary looks fine. 
# total_pymnt - #Payment done by the individual till now. Higher it is, lower chance of default.
summary(loan$total_pymnt)
which(is.na(loan$total_pymnt)) # none of the values are NA , summary looks fine. 


#Validated the below fields with a cursory look in the file and we see some NA's etc, which could
# actually be valid (since we wont have a months since last deliquency if someone has never been 
# deliquent in the first place etc.)

# We take a judicious call to ignore certain fields from detailed analysis because as we know, 
# EDA will never stop, we need to take a judicious call at a poiint, beyond which, lot of hours of
# analyis effort would be required for marginal improvements in analysis output (efficiency will 
# become very low)

# earliest_cr_line
# inq_last_6mths
# mths_since_last_delinq, 
# mths_since_last_record
# open_acc
# pub_rec
# revol_util
# initial_list_status
# out_prncp
# out_prncp_inv
# total_pymnt
# total_pymnt_inv
# total_rec_prncp
# total_rec_int
# total_rec_late_fee
# recoveries
# collection_recovery_fee
# last_pymnt_d
# last_pymnt_amnt
# next_pymnt_d
# last_credit_pull_d
# collections_12_mths_ex_med
# policy_code
# application_type
# acc_now_delinq
# chargeoff_within_12_mths
# delinq_amnt
# pub_rec_bankruptcies
# tax_liens


#The information is for loans between 2007 to 2011 
#but the issue_d column date format needs to be corrected 
#as its not in the correct format
# The column currently reflects Dec 2011 as Dec-11, lets split it into two columns
# so we can analyze the data by months and years separately if required.

#Split the Request and Drop timestamp columns into Date and Time (Also convert YY to YYYY etc.)
loan_data$issue_month <-   str_split_fixed(loan_data$issue_d, "-", 2)[, 1]
loan_data$issue_year<- paste("20",str_split_fixed(loan_data$issue_d, "-", 2)[, 2],sep="")

#Remove issue_d column as its no longer needed
loan_data$issue_d <- NULL


# I) Univariate Analysis

#Lets do a summary of the dataframe and determine if there are any outliers
# This might also give some inputs around need for derived metrics, if any.
summary(loan_data)

#Some insights from above summary ->
# 1) loan amount range is 0.9K to 35k (Does not look like there have been any typos/extremely large or small values)
# 2) Only 2 loan terms - 3 years and 5 years
# 3) Further Research on interest rate needs to be done.
# 4) Installments look reasonable (22$ to 1305$ per month)
# 5) Employment lengths look reasonable without any outliers. 
# 6) Annual income might need more research (depending on plots later.)
# 7) Unordered Categorical variable -> "purpose" looks ok.
# 8) Ordered categorical variable - pub_rec_bankruptcies seems to be an 
#    interesting field and looks ok (no outliers)



#----********-------------**********--END OF CASE STUDY------*****-------**************-------***

##Delete below once complete

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
