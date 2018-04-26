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

#Remove car_ID since its just an identifier
cars$car_ID <- NULL

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
#fuelsystem (7 levels)
summary(factor(cars$fuelsystem))
# Since mfi and mpfi are same, we need to change mfi to mpfi (Both refer to multip point fuel injection.)
cars$fuelsystem <- str_replace_all(cars$fuelsystem, "mfi", "mpfi")
summary(factor(cars$fuelsystem))
dummy_fuelsystem <- data.frame(model.matrix( ~fuelsystem, data = cars))
dummy_fuelsystem <- dummy_fuelsystem[,-1]
cars$fuelsystem <- NULL
cars <- cbind(cars, dummy_fuelsystem)

# At this point, we do a quick validation and see that there are 68 variables in the 
# cars dataframe (which is correct, considering we have created dummy variables
# and removed the orignal variables using which the dummies were created.)

# Now, at this point, we need to add some derived columns which make sense
# For example, few key metrics on which car performance is measured are -
# 1. horsepower/curbweight (i.e., horsepower per unit weight)
cars$hp_by_weight <- cars$horsepower/cars$curbweight
# 2. horsepower/enginesize (i.e., horsepower per unit cubic capacity - or, enginesize)
cars$hp_by_cc <- cars$horsepower/cars$enginesize

#EDA is complete. Total variables in cars df is 70 now.


#=========================================
#Linear Regression
#=========================================

# Build model 1 containing all variables
model_1 <-lm(price~.,data=cars)
summary(model_1)
#######

#4 variables seem totally insignificant due to singluaties (we will remove these) -
# 1. enginetypeohcf
# 2. cylindernumberthree
# 3. cylindernumbertwo
# 4. fuelsystemidi
cars$enginetypeohcf <- NULL
cars$cylindernumberthree <- NULL 
cars$cylindernumbertwo <- NULL
cars$fuelsystemidi <- NULL

# Lets model again
# Build model 1 containing all variables
model_2 <-lm(price~.,data=cars)
summary(model_2)


# Using stepAIC to perform elimination of variables.

# Now that the second model is ready. Lets use that in StepAIC function and
# remove insignificant variables automatically.

# Lets load the library 
library(MASS)
step <- stepAIC(model_2, direction="both")
step #It can be seen that stepAIC has removed the insignificant variables. The following
# 15 variables are determined as significant : -
#
#carcompanybuick
#+ drivewheelfwd
#+ symboling0
#+ fuelsystem4bbl
#+ cylindernumberfive
#+ doornumber
#+ symboling2
#+ cylindernumbertwelve
#+ symboling1
#+ fuelsystemspfi
#+ enginetyperotor
#+ carcompanysaab
#+ stroke
#+ citympg
#+ symboling3  
#
# The next step would be to create model_3 and identify 
# the variables based on p-value (lower than 0.05 implies significant)
# 

# Create model_3 using the 15 variables identified by stepAIC

model_3 <-lm(formula = price ~ carcompanybuick + drivewheelfwd
             + symboling0 + fuelsystem4bbl + cylindernumberfive
             + doornumber + symboling2 + cylindernumbertwelve
             + symboling1 + fuelsystemspfi + enginetyperotor
             + carcompanysaab + stroke + citympg
             + symboling3,data=cars)

summary(model_3) # The R-Squared of this model is 70% and the Adj R-sq is 68% [ Quite good as per industry standards]

# All variables, except 5 variables have P > 0.05 (suggesting insignificancy), lets remove them and create model_4

# Removing all except these 5 ->carcompanybuick + drivewheelfwd + cylindernumbertwelve+ stroke + citympg

model_4 <-lm(formula = price ~ carcompanybuick + drivewheelfwd + cylindernumbertwelve
             + stroke + citympg,
             data=cars)

summary(model_4) # Just keep these 5 variables, the R-squared & Adj.R.sq is ~67%]. 
#Since there is not much difference from model_3, this model looks good for now, 

# Lets use model_3 and try removing only those variables that have a p > 0.2

model_5 <-lm(formula = price ~ carcompanybuick + carcompanysaab +drivewheelfwd + 
               cylindernumbertwelve + stroke + citympg + symboling3,
             data=cars)

summary(model_5) # This model has an adj-R-squared of 66.89%, NOT better than model_4

# As of now, the model_4 looks the best with the following predictors ->
# 1.carcompanybuick
# 2.drivewheelfwd 
# 3.cylindernumbertwelve
# 4.stroke
# 5.citympg
# Let's identify collinearity using VIF now.

library(car)
vif(model_4) # All the 5 variables have a VIF below 2, suggests no mluticollinearity,
# hence, lets not remove any variables at the moment and try predicting the price and
# comparing with the actual price values.

# Lets Test the model_4

cars$predicted_price_model_4 <- predict(model_4, cars)
cor(cars$price,cars$predicted_price_model_4) 
# The correlation between actual price and predicted price looks satisfactory (82%)
rsquared <- cor(cars$price,cars$predicted_price_model_4)^2
rsquared #(As expected, same as the R-Squared shown summary(model_4))

# Final value of R-Squared is 67%. This looks like a satisfactory model.

# Lets add few more cylinder parameters and see what happens. Lets add cylinders 4 and 6

model_6 <-lm(formula = price ~ carcompanybuick + drivewheelfwd + cylindernumbertwelve
             + cylindernumberfour + cylindernumbersix + stroke + citympg,
             data=cars)

summary(model_6) # [The R-squared & Adj.R.sq is ~75%].  We see good improvement.
#Model_6 also indicates that stroke might be an insignificant parameter. Lets remove it.

model_7 <-lm(formula = price ~ carcompanybuick + drivewheelfwd + cylindernumbertwelve
             + cylindernumberfour + cylindernumbersix + citympg,
             data=cars)

summary(model_7) # So stroke can be removed, since The R-squared & Adj.R.sq is still ~75%

# HENCE -> Model_7 looks good.

vif(model_7) # This suggests to remove cylinders 4 and 6 as their vif > 2, lets see that once.

model_8 <-lm(formula = price ~ carcompanybuick + drivewheelfwd + cylindernumbertwelve
            + citympg,
             data=cars)

summary(model_8) # Not good -> The adj R-squared dropped to 66%. Hence, lets ignore model_8

# Lets calculate the predicted price using model 7

cars$predicted_price_model_7 <- predict(model_7, cars)
cor(cars$price,cars$predicted_price_model_7) 
# The correlation between actual price and predicted price looks satisfactory (82%)
rsquared <- cor(cars$price,cars$predicted_price_model_7)^2
rsquared #(As expected, same as the R-Squared shown summary(model_7))

#### FINAL SUMMARY  ########

summary(model_7)

#####END OF CODE ###########
