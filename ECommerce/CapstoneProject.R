#Clear the Environment - To avoid any testing issues.
rm(list = ls())

#Set working directory to import file
setwd('C:/pgdds/Project')

#Read the file into a dataframe
electronics<-read.csv("ConsumerElectronics.csv")

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
library(lubridate) #date functions.
library(sqldf) #sql functions.

#Check structure of the data 
str(electronics)

#Drop columns with only/mostly \n values in it 
electronics<-within(electronics, rm(deliverybdays,deliverycdays))
#electronics$deliverybdays <- NULL
#electronics$deliverycdays <- NULL

#Finding the NA's in the data frame
sapply(electronics, function(x) sum(is.na(x)))

#dropping rows which have NA's in customer id, pincode or gmv 
# (these orders probably didnt go through)
electronics<-electronics[!is.na(electronics$gmv),]
electronics<-electronics[!is.na(electronics$cust_id),]
electronics<-electronics[!is.na(electronics$pincode),]

# Convert Month and Year to numeric for arithmetic comparisons.

electronics$Month <- as.numeric(electronics$Month)
electronics$Year <- as.numeric(electronics$Year)

# Next step is to verify if dates are within the range of July-15 to June-16
electronics$validDate <-
  ifelse((electronics$Month < 7 & electronics$Year == 2015)||(electronics$Month > 6 & electronics$Year == 2016),
         "INVALID",
         "VALID")

electronics <-electronics %>% filter(!(electronics$Month ==5 & electronics$Year==2015|electronics$Month ==6 & electronics$Year==2015
                                       |electronics$Month ==7 & electronics$Year==2016))
unique(electronics$validDate) #Only Yes, so all dates are within range. No issues.

week(electronics$order_date)
# Identify week of the Year
electronics$weekYear<-week(electronics$order_date)
electronics$weekYear
str(electronics)
summary(electronics)

#year 2016 will start from 1st week again so cleaning the data such a way that weeks should start
#with 54,55 etc.,
electronics$weekYear<- ifelse(electronics$Year==2016&electronics$weekYear<=26,electronics$weekYear+53,
                              electronics$weekYear)

#Removing products with or 0 or -ve MRP
electronics<-subset(electronics,electronics$product_mrp > 0)

# Lets make some plots to analyze the data to identify any outliers.
quantile(electronics$gmv)
#    0%    25%    50%    75%   100% 
#    0    338    779   1699 218464
quantile(electronics$units)
#0%  25%  50%  75% 100% 
#1    1    1    1   50 
quantile(electronics$product_mrp)
#0%    25%    50%    75%   100% 
#75    700   1500   3199 259995

#quantile of units, gmv and mrp looks reasonable.

#KPI Function for all the 3 categories

# Next step is to identify some KPI's.
# KPI 1 - Price per product
# KPI 2 - Discount % on product (MRP vs. Price its sold at)
# KPI 3 - NPS (As indicated NPS is basically a translation of how good is the ecomm store - a proxy for customer vote)
# KPI 4 - SLA to deliver the product 
# KPI 5 - Special Sale days
# KPI 6 - Prepaid vs. COD

# Lets implement the above KPI's now -
#KPI 1
electronics$price_per_Product <- electronics$gmv/electronics$units

#KPI 2
electronics$Discount_Percentage <- (electronics$product_mrp - electronics$price_per_Product)/electronics$product_mrp

#KPI 3
# We first need to populate the monthly NPS values that are available. Lets copy it into
# a csv and read it into R
nps<- read.csv("NPS.csv", stringsAsFactors = F)
# Lets merge the NPS values into the electronics df by Month and Year
electronics<- merge(electronics,nps, by=c("Year","Month"), all.x = T)
distinct(electronics,electronics$NPS)
#KPI 4 - No calculation needed - SLA column can be used as-is

# KPI 5

Special<- read.csv("Special.csv", stringsAsFactors = F)
#Add a week column in Special days
Special$weekYear <- week(Special$SpecialDays)
# Convert week to 54 for 2016 onwards, as done earlier
Special$weekYear<- ifelse(year(Special$SpecialDays) == 2016 ,Special$weekYear+53,
                          Special$weekYear)

summary(Special)
str(Special)
# Add a new column in Special dataframe which can be used while merging.
Special$SpecialDay <- 1
# Drop SpecialDays column (Not needed in final since we already know special day week)
Special$SpecialDays <-  NULL
Special <- unique(Special) # Delete duplicate rows (since multiple special days are part of same week)
Special

# Merge special days to electronics data set to identify special days

electronics <- merge(electronics,Special, by="weekYear", all.x = TRUE)

str(electronics)

# Replace Non special days as 0 (from NA) - now only 2 values in specialdays
electronics$SpecialDay[is.na(electronics$SpecialDay)] <- 0

summary(electronics$SpecialDay)

class(electronics$weekYear)
class(Special$weekYear)
unique(electronics$SpecialDay)

# KPI 6
electronics$Pay_type <- ifelse(electronics$s1_fact.order_payment_type=="Prepaid",1,0)

# After calculating the KPI's, we can divide the ecomm df into the 3 product sub-categories
# that we need to analyze as part of the Project.

cameraAccessory<-subset(electronics,electronics$product_analytic_sub_category=='CameraAccessory')
gamingAccessory<-subset(electronics,electronics$product_analytic_sub_category=='GamingAccessory') 
homeAudio<-  subset(electronics,electronics$product_analytic_sub_category=='HomeAudio')

# Lets Create some basic EDA plots

#Reading caluculated adstock
cameraAccessory_adstock<-read.csv("final_adstock_camera_accessory.csv")
gamingAccessory_adstock<-read.csv("final_adstock_game_accessory.csv")
homeAudio_adstock<-read.csv("final_adstock_home_audio.csv")

#Mergeing adstock with each product
cameraAccessory <- merge(cameraAccessory,cameraAccessory_adstock, by="weekYear", all.x = TRUE)
gamingAccessory <- merge(gamingAccessory,gamingAccessory_adstock, by="weekYear", all.x = TRUE)
homeAudio <- merge(homeAudio,homeAudio_adstock, by="weekYear", all.x = TRUE)

#plotting the graphs for adstock rate vs gmv 
ggplot(cameraAccessory, aes(TV,gmv)) + geom_point(colour = "red", size = 1)
ggplot(gamingAccessory, aes(TV,gmv)) + geom_point(colour = "red", size = 1)
ggplot(homeAudio, aes(TV,gmv)) + geom_point(colour = "red", size = 1)

ggplot(cameraAccessory, aes(Affiliates,gmv)) + geom_point(colour = "red", size = 1)
ggplot(gamingAccessory, aes(Affiliates,gmv)) + geom_point(colour = "red", size = 1)
ggplot(homeAudio, aes(Affiliates,gmv)) + geom_point(colour = "red", size = 1)


ggplot(cameraAccessory, aes(Content.Marketing,gmv)) + geom_point(colour = "red", size = 1)
ggplot(gamingAccessory, aes(Content.Marketing,gmv)) + geom_point(colour = "red", size = 1)
ggplot(homeAudio, aes(Content.Marketing,gmv)) + geom_point(colour = "red", size = 1)

ggplot(cameraAccessory, aes(Digital,gmv)) + geom_point(colour = "red", size = 1)
ggplot(gamingAccessory, aes(Digital,gmv)) + geom_point(colour = "red", size = 1)
ggplot(homeAudio, aes(Digital,gmv)) + geom_point(colour = "red", size = 1)

ggplot(cameraAccessory, aes(Online.marketing,gmv)) + geom_point(colour = "red", size = 1)
ggplot(gamingAccessory, aes(Online.marketing,gmv)) + geom_point(colour = "red", size = 1)
ggplot(homeAudio, aes(Online.marketing,gmv)) + geom_point(colour = "red", size = 1)

ggplot(cameraAccessory, aes(Other,gmv)) + geom_point(colour = "red", size = 1)
ggplot(gamingAccessory, aes(Other,gmv)) + geom_point(colour = "red", size = 1)
ggplot(homeAudio, aes(Other,gmv)) + geom_point(colour = "red", size = 1)

ggplot(cameraAccessory, aes(Radio,gmv)) + geom_point(colour = "red", size = 1)
ggplot(gamingAccessory, aes(Radio,gmv)) + geom_point(colour = "red", size = 1)
ggplot(homeAudio, aes(Radio,gmv)) + geom_point(colour = "red", size = 1)

ggplot(cameraAccessory, aes(Sponsorship,gmv)) + geom_point(colour = "red", size = 1)
ggplot(gamingAccessory, aes(Sponsorship,gmv)) + geom_point(colour = "red", size = 1)
ggplot(homeAudio, aes(Sponsorship,gmv)) + geom_point(colour = "red", size = 1)


ggplot(electronics,aes(electronics$weekYear,electronics$gmv, fill = as.factor(ifelse(electronics$SpecialDay>0,1,0))))+geom_bar(stat="identity") + labs(fill = "Special_Day", x = "Week", y = "GMV") + ggtitle("name")

# The next step is to create a basic linear model.
#=========================================
#Linear Regression (Below analysis will be done separately for each of the 3 sub-category)
#=========================================
# Build model 1 containing all key variables (Model to predict gmv - since thats the whole point!)

# original data frame
library("MASS")
library("car")
set.seed(100)
#camera <- cameraAccessory[,-c(1:7,9:10,12:12,14:17,20:20)]
#cameraAccessory <-cameraAccessory %>% filter((product_mrp*units)>=gmv)

# Lets scale the variables
cameraAccessory$sla<- scale(cameraAccessory$sla)
cameraAccessory$pincode<- scale(cameraAccessory$pincode)
cameraAccessory$product_mrp<- scale(cameraAccessory$product_mrp)
cameraAccessory$product_procurement_sla<- scale(cameraAccessory$product_procurement_sla)
cameraAccessory$price_per_Product<- scale(cameraAccessory$price_per_Product)
cameraAccessory$Discount_Percentage<- scale(cameraAccessory$Discount_Percentage)
cameraAccessory$NPS<- scale(cameraAccessory$NPS)
cameraAccessory$SpecialDay<- scale(cameraAccessory$SpecialDay)
cameraAccessory$Pay_type<- scale(cameraAccessory$Pay_type)

trainindices= sample(1:nrow(cameraAccessory), 0.7*nrow(cameraAccessory))
train = cameraAccessory[trainindices,]
test = cameraAccessory[-trainindices,]

model_1 <-lm(gmv~ sla + pincode + product_mrp + product_procurement_sla + price_per_Product
             + Discount_Percentage + NPS + SpecialDay + Pay_type+Affiliates+Content.Marketing
             +Digital+Online.marketing+Other+Radio+SEM+Sponsorship+TV,data=train)

summary(model_1)

step1 <- stepAIC(model_1, direction="both")

# Remove insignificant variables.

model_2 <-lm(gmv~ sla + product_mrp + product_procurement_sla + price_per_Product + 
               Discount_Percentage + SpecialDay + Pay_type + Digital + Other + 
               Radio + SEM + TV,data=train)

summary(model_2)

vif(model_2)

# Now remove price_per_Product, Digital, Other, Radio, SEM and TV as it has high vif

model_3 <-lm(gmv~ sla + product_mrp + product_procurement_sla 
             + Discount_Percentage + NPS + SpecialDay + Pay_type,data=train)

summary(model_3)
vif (model_3)

# Lets try some other models

model_4 <-lm(gmv~ sla + product_mrp + product_procurement_sla 
             + Discount_Percentage + NPS + SpecialDay,data=train)
summary(model_4)

model_5 <-lm(gmv~ sla + product_mrp + product_procurement_sla 
             + Discount_Percentage + NPS,data=train)
summary(model_5)

model_6 <-lm(gmv~ sla + product_mrp + Discount_Percentage + NPS,data=train)
summary(model_6)
# Adjusted R squared is 90%

s<-predict(model_6,test)

cor(s,test$gmv)^2  # is 89.5%

#####################
#SIMILARLY TO BE DONE FOR OTHER 2 DATASETS
#####################

###### Lets build the next model - Multiplicative Model

LogCam <- cameraAccessory[,-c(1:7,9:10,12:17,20:20)]

#Before taking logs, convert 0 to 0.001 - As log of 0 will cause issues.

LogCam[LogCam == 0] <- 0.001

LogCam <- log(LogCam)

set.seed(100)
trainindices= sample(1:nrow(LogCam), 0.7*nrow(LogCam))
train = LogCam[trainindices,]
test = LogCam[-trainindices,]

LogCam_model1 <- lm(gmv~ sla + product_mrp + product_procurement_sla + 
                      price_per_Product + Discount_Percentage + NPS + SpecialDay + 
                      Pay_type + Affiliates + Content.Marketing + 
                      Digital + Online.marketing + Other + Radio + SEM + 
                      Sponsorship + TV, data = train)

summary(LogCam_model1) # R-squared is 89%

# Lets try other models

LogCam_model2 <- lm(gmv~ sla + product_mrp + product_procurement_sla + 
                       Discount_Percentage + NPS + SpecialDay + 
                      Pay_type + Affiliates + Content.Marketing + 
                      Digital + Online.marketing + Other + Radio + SEM + 
                      Sponsorship + TV, data = train)
summary(LogCam_model2) # Adj R-Square 80%

LogCam_model3 <- lm(gmv~ sla + product_mrp + product_procurement_sla + 
                      Discount_Percentage + NPS + SpecialDay + 
                      Pay_type + Affiliates + Content.Marketing + 
                      Digital + Online.marketing + Other + Radio + SEM , data = train)
summary(LogCam_model3) # Still 80%


LogCam_model4 <- lm(gmv~ sla + product_mrp + product_procurement_sla + 
                      Discount_Percentage + NPS + SpecialDay + 
                      Pay_type + Affiliates + Content.Marketing , data = train)
summary(LogCam_model4) # Still ~80%

LogCam_model5 <- lm(gmv~ product_mrp + 
                      Discount_Percentage + NPS + SpecialDay + 
                      Pay_type + Affiliates + Content.Marketing , data = train)
summary(LogCam_model5) # 74%


LogCam_model6 <- lm(gmv~  price_per_Product + Discount_Percentage + NPS + SpecialDay + 
                      Pay_type + Affiliates + Content.Marketing , data = train)
summary(LogCam_model6) # Removing product_mrp and replace with price_per_Product gives best 84%

################
#SIMILARLY FOR OTHERS
################


