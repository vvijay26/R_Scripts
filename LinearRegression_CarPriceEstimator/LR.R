#LR
#Clear the Environment - To avoid any testing issues.
rm(list = ls())
#Set the working dir (This will vary depending on which system/directory the files are present)
setwd('C:/pgdds/Course 3/CS')
#EDA

#Include stringr, dplyr and plyr libraries to perform string & data manipulations
#Include ggplot2 to create plots. These packages can be installed using
# install.packages("ggplot2") etc.
library(stringr) #string manipulations
library(dplyr) #string manipulations
library(plyr) #string manipulations
library(ggplot2) #for easy plotting


# load the data on cars (We use string as factors true and will later convert 
# the string variables that are not factors (i.e., categorical) into string).
# This is much faster than NOT reading string as factors and then converting each
# column into factor.
cars <- read.csv("CarPrice_Assignment.csv",
                 sep = ",",
                 stringsAsFactors = TRUE)
str(cars)

#carCompany (we need to split the carName into car company name and car name and covert the car
# company name to factor
cars$carcompany <-
  str_split_fixed(cars$CarName, " ", 2)[, 1]
cars$carName <-
  str_split_fixed(cars$CarName, " ", 2)[, 2] 
#Drop the existing variable CarName
cars$CarName <- NULL

#Reorder the dataframe so that the car Company name becomes the second column

cars <- cars[,c(1,26,2:25,27)]

#Check data for how many unique Car companies is present in the input file.

unique(cars$carcompany)

#we can see that there are some erros present in the car company names.
# maxda should be mazda
# Nissan should be nissan
# porcshce should be porsche
# toyouta should be toyota
# vokswagen should be volkswagen
# vw should be volkswagen

# Lets correct these errors.
cars$carcompany <- str_replace_all(cars$carcompany, "maxda", "mazda")
cars$carcompany <- str_replace_all(cars$carcompany, "Nissan", "nissan")
cars$carcompany <- str_replace_all(cars$carcompany, "porcshce", "porsche")
cars$carcompany <- str_replace_all(cars$carcompany, "toyouta", "toyota")
cars$carcompany <- str_replace_all(cars$carcompany, "vokswagen", "volkswagen")
cars$carcompany <- str_replace_all(cars$carcompany, "vw", "volkswagen")

#check for any remaining errors in car Company name -

unique(cars$carcompany) #No duplicates/junk data left.
table(cars$carcompany) #Looks ok. Maximum entries are for toyota
length(summary(as.factor(cars$carcompany),maxsum=50000)) #22 Companies.


#Lets validate the structure of the cars dataframe one more time.
str(cars)

#Convert categorical variables to factors based on Data Dictionary and Business Understanding
# Since we used string as Factors True, except for symboling and car company, rest of the
# Categorical variables are already read as Factors in the cars dataframe.

#The following need to be converted to factor
#carcompany
cars$carcompany <- as.factor(cars$carcompany)
#Symboling
cars$symboling <- as.factor(cars$symboling)

#Lets validate the structure of the cars dataframe one more time.
str(cars)

#=================
# Since we need to perform regression analysis on these variables, the next step is to 
# convert factors into numeric variables
#=================

# FACTORS with 2 levels.
#=========================
# fueltype 
levels(cars$fueltype)<-c(1,0) 
cars$fueltype <- as.numeric(levels(cars$fueltype))[cars$fueltype]
# aspiration
levels(cars$aspiration)<-c(1,0) 
cars$aspiration <- as.numeric(levels(cars$aspiration))[cars$aspiration]
# doornumber
levels(cars$doornumber)<-c(1,0) 
cars$doornumber <- as.numeric(levels(cars$doornumber))[cars$doornumber]
# enginelocation
levels(cars$enginelocation)<-c(1,0) 
cars$enginelocation <- as.numeric(levels(cars$enginelocation))[cars$enginelocation]
#=====================
#Lets validate the structure of the cars dataframe one more time.
str(cars)
# FACTORS with more than 2 levels (dummy variables to be used for these)

#carcompany (22 levels)
summary(factor(cars$carcompany))
dummy_carcompany <- data.frame(model.matrix( ~carcompany, data = cars)) #Converting "carscompany" into dummies . 
dummy_carcompany <- dummy_carcompany[,-1] #This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "carscompany". 
cars$carcompany <- NULL
cars <- cbind(cars, dummy_carcompany)

#Repeat the other commands to convert the following also to dummy variables.

#symboling (6 levels)
summary(factor(cars$symboling))
dummy_symboling <- data.frame(model.matrix( ~symboling, data = cars))
dummy_symboling <- dummy_symboling[,-1]
cars$symboling <- NULL
cars <- cbind(cars, dummy_symboling)
#carbody (5 levels)
summary(factor(cars$carbody))
dummy_carbody <- data.frame(model.matrix( ~carbody, data = cars))
dummy_carbody <- dummy_carbody[,-1]
cars$carbody <- NULL
cars <- cbind(cars, dummy_carbody)
#drivewheel (3 levels)
summary(factor(cars$drivewheel))
dummy_drivewheel <- data.frame(model.matrix( ~drivewheel, data = cars))
dummy_drivewheel <- dummy_drivewheel[,-1]
cars$drivewheel <- NULL
cars <- cbind(cars, dummy_drivewheel)
#enginetype (7 levels)
summary(factor(cars$enginetype))
dummy_enginetype <- data.frame(model.matrix( ~enginetype, data = cars))
dummy_enginetype <- dummy_enginetype[,-1]
cars$enginetype <- NULL
cars <- cbind(cars, dummy_enginetype)
#cylindernumber (7 levels)
summary(factor(cars$cylindernumber))
dummy_cylindernumber <- data.frame(model.matrix( ~cylindernumber, data = cars))
dummy_cylindernumber <- dummy_cylindernumber[,-1]
cars$cylindernumber <- NULL
cars <- cbind(cars, dummy_cylindernumber)

# At this point, we do a quick validation and see that there are 65 variables in the 
# cars dataframe (which is correct, considering we have created dummy variables
# and removed the orignal variables using which the dummies were created.)
#EDA is complete.
