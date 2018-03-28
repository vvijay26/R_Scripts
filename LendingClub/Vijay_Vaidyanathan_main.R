#Clear the Environment - To avoid any testing issues.
rm(list = ls())

#Include stringr, dplyr and plyr libraries to perform string & data manipulations
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
# There are no header, footers, summary rows etc. in the file
# None of the column names are missing and all are meaningful names
colnames(loan)
# Unnecessary columns - Columns where each instance is NA , Blanks etc.
# These columns have no relevance to the analysis, hence remove.
library(data.table)
temp <- as.data.table(loan)
loan_data <- temp[,which(unlist(lapply(temp, function(x)!all(is.na(x))))),with=F] #Remove all columns with everything NA

ncol(loan_data) #Down to 57 columns (manageable as compared to 111 column dataset)

#write.csv(loan_data_final, file = "loan_data.csv",row.names = F)

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

#Identify columns containing multiple data values.
#The information is for loans between 2007 to 2011 
#but the issue_d column date format needs to be corrected 
#as its not in the correct format
# The column currently reflects Dec 2011 as Dec-11, lets split it into two columns
# so we can analyze the data by months and years separately if required.

#Split the Issue_d into Date and Time (Also convert YY to YYYY etc.)
loan_data$issue_month <- str_split_fixed(loan_data$issue_d, "-", 2)[, 2]
loan_data$issue_year <- as.numeric(str_split_fixed(loan_data$issue_d, "-", 2)[, 1])

loan_data$issue_year <-
  as.numeric(ifelse(
    loan_data$issue_year > 9,
    paste("20",str_split_fixed(loan_data$issue_d, "-", 2)[, 1],sep=""),
    paste("200",str_split_fixed(loan_data$issue_d, "-", 2)[, 1],sep="")
  ))

#Remove issue_d column as its no longer needed
loan_data$issue_d <- NULL

# Lets traverse each through each column and check for data issues and clean accordingly
# id - 
sum(is.na(loan_data$id),length(which(loan_data$id == ""))) #No issues
# member_id
sum(is.na(loan_data$member_id),length(which(loan_data$member_id == "")))#No issues
# loan_amnt
sum(is.na(loan_data$loan_amnt),length(which(loan_data$loan_amnt == "")))#No issues
# funded_amnt
sum(is.na(loan_data$funded_amnt),length(which(loan_data$funded_amnt == "")))#No issues
# funded_amnt_inv
sum(is.na(loan_data$funded_amnt_inv),length(which(loan_data$funded_amnt_inv == "")))#No issues
# term
sum(is.na(loan_data$term),length(which(loan_data$term == "")))#No issues
# int_rate
sum(is.na(loan_data$int_rate),length(which(loan_data$int_rate == "")))#No issues
# installment
sum(is.na(loan_data$installment),length(which(loan_data$installment == "")))#No issues
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

# url,desc - no data cleaning required.visual check is good enough.

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
# inq_last_6mths
summary(loan_data$inq_last_6mths) #looks ok. This is one of the key fields usually in analyzing credit risk

#Validated the below fields with a cursory look in the file and we see some NA's etc, which could
# actually be valid (since we wont have a months since last deliquency if someone has never been 
# deliquent in the first place etc.)

# We do a summary of the whole file and validate the rest of fields using cursory look
summary(loan_data)

# Overall rest of the columns look ok. 
# We take a judicious call to ignore these fields from detailed analysis because as we know, 
# EDA will never stop, we need to take a judicious call at a poiint, beyond which, 
# additional analysis hours do not justify the marginal improvements in output (efficiency will 
# become very low)

#The following fields are being left as-is 
#(later during plots if we realize any issues we can come back to these and perform additional 
# data cleaning as necessary) :- 
-----
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


# I) Univariate Analysis
#-=-=-=-=-=-=-=-=-=-=-=-=-=-

#Lets do a summary of the dataframe and determine again to determine if there are any outliers
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

# Lets create few histograms of key variables and determine their trend (Univariate Analysis)

# The primary objective is to understand the driving factors
# behind loan default, i.e. the variables which are strong indicators of default.  
# The company can utilise this knowledge for its portfolio and risk assessment for future loans. 

# For purposes of meaningful analysis and more manageble data, lets remove the columns
# that might not usually have some impact on the default status (Such as id, url etc.)
# Also, lets filter the file to remove In progress loans, we only want to analyze loans
# that are either paid in full (loan_status = "Fully Paid") or defaulted ("Charged Off").

loan_data_temp <- filter(loan_data,loan_status == "Fully Paid" | loan_status == "Charged Off") #Removing "Current" Loans

# Following fields are selected for analysis :- Based on analysis of public websites which have
# data on how banks, p2p lends etc. along with info from the data dictionary

#----BUSINESS UNDERSTANDING---------#
# Reasoning on why the specific data point is selected for analysis is given in parenthesis

# funded_amnt (Amount of loan funded)
# term (3 vs 5 year repayment period)
# int_rate (higher interest rate usually signals lower credit score)
# installment (size of the installment)
# grade (loan grade assigned by LendingClub)
# sub_grade (loan subgrade assigned by LendingClub (may be based on credit score etc.))
# emp_length (higher the emp length signals higher stability)
# home_ownership (Rent vs. Mortgage is expected to have a bearing on the loan_status)
# annual_inc (annual income)
# verification_status (status regarding verification of annual income)
# loan_status (defaulted vs. payments in progress)
# purpose ( purpose for the loan - car, house etc.)
# addr_state (To determine if defaults are originating from specific states)
# dti (explained in comments above)
# delinq_2yrs (higher the deliquency, probably higher chances of default)
# inq_last_6mths ( higher inquires usually signal lower credit scores due to hard inquiries etc.)
# mths_since_last_delinq (higher this period signals lower chance of default)
# open_acc ( higher number of credit lines signals higher utilization of credit, thus higher chance of default)
# pub_rec ( derogatory records - such as missed payments etc. - lowers credit score)
# revol_bal ( how much of credit is revolving - that is credit line - pending payments)
# revol_util (how much of credit line is utilized, probably higher % is not good)
# earliest_cr_line (higher age of credit history is usually a positive for credit score)
# issue_year (derived column from issue_d, used to determine age of credit history from loan date)

loan_data_final <- loan_data_temp[,c('funded_amnt','term','int_rate','installment','grade','sub_grade','emp_length','home_ownership','annual_inc','verification_status','loan_status','purpose','addr_state','dti','delinq_2yrs','inq_last_6mths','mths_since_last_delinq','open_acc','pub_rec','revol_bal','revol_util','earliest_cr_line','issue_year')]

#Lets just extract the year from the earliest_cr_line field (since its having different formats
# such as MMM-YY or Y-MMM etc., lets use regex to extract numbers, which will be the year)
# if the number is >10, we can assume the century to be 1900, else century is 2000

loan_data_final$cr_line_year <- gsub("[^0-9]", "", loan_data_final$earliest_cr_line) #Substitute all characters, "-" etc. with ""

loan_data_final$issue_year <- as.numeric(loan_data_final$issue_year) #Convert to numeric to perform numeric functions

loan_data_final$cr_line_year <- as.numeric(loan_data_final$cr_line_year) #Convert to numeric to perform numeric functions

loan_data_final$cr_line_year <-
  ifelse(
    loan_data_final$cr_line_year > 9,
    paste("19",loan_data_final$cr_line_year,sep=""),
    paste("200",loan_data_final$cr_line_year,sep="")
  )
loan_data_final$cr_line_year <- as.numeric(loan_data_final$cr_line_year)  #Convert to numeric to perform numeric functions

#Validate the lowest and highest credit line years
max(loan_data_final$cr_line_year) #2008, looks ok.
min(loan_data_final$cr_line_year) #1946, looks ok.


# Now, we are down to 25 fields from the initial 111 fields, lets begin UNIVARIATE analysis

# Next step is to determine a derived column - issue_year minus earliest_cr_line (in Years)
# We can categorize these into <5 years, 5-10 years, 10-20 years and >20 years. This is being
# done to bucket the credit history into different sets, for drawing insights from analysis
loan_data_final$age_of_credit_history <- loan_data_final$issue_year - loan_data_final$cr_line_year

#Bucket age of credit history using ifelse statements

loan_data_final$credit_history_buckets <-
  ifelse(
    loan_data_final$age_of_credit_history <= 5,
    "0 - 5 Years",
    ifelse (
      loan_data_final$age_of_credit_history > 5 & loan_data_final$age_of_credit_history <= 10,
      "5-10 Years",
      ifelse (
        loan_data_final$age_of_credit_history > 10 & loan_data_final$age_of_credit_history <= 15,
        "10-15 Years",
        ifelse (
          loan_data_final$age_of_credit_history > 15 & loan_data_final$age_of_credit_history <= 20,
          "15-20 Years",
          ifelse (loan_data_final$age_of_credit_history > 20 &
                    loan_data_final$age_of_credit_history <= 25, "20-25 Years", "> 25 Years")
        )
      )
    )
  )


#Lets create few bar plots to analyze the effect of fields on loan_status (segmented univariate)

ggplot(data = loan_data_final, aes(
  x = factor(loan_status),
  fill = factor(loan_data_final$sub_grade)
)) +
  geom_bar(alpha = 0.7, position = "dodge")+
  xlab("Loan Status") +ylab("Count") +labs(fill = 'Sub-Grade')
  
#Histogram plots showing the number of loans for each grade, status and credit age
ggplot(data = loan_data_final, aes(
  x = factor(grade),
  fill = factor(credit_history_buckets)
)) +
  geom_bar(alpha = 0.7, position = "dodge")+ 
  xlab("Loan Status") +ylab("Count") +labs(fill = 'Credit History Range') +facet_grid(.~loan_data_final$loan_status, scales = "free_x")

#A Histogram plot showing the number of loans for each subgrade for each loan_status 
ggplot(loan_data_final, aes(x = sub_grade)) + geom_histogram(binwidth = 1,
                                                             fill = "black",
                                                             ,
                                                             stat = "count") +
  facet_grid(. ~ loan_data_final$loan_status, scales = "free_x")

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
