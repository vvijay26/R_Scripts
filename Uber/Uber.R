#Uber Assignment---

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

setwd("C:/pgdds/Course 2/Module 4 - Uber/Uber")
#Read the file into a dataframe
uber <-
  read.csv("Uber Request Data.csv",
           sep = ",",
           stringsAsFactors = FALSE)
#-----------------------------------
# 1. Data Cleaning and Preparation
# 1.1	Identify the data quality issues and clean the data so that you can use it for analysis
#-----------------------------------
#Split the Request and Drop timestamp columns into Date and Time
uber$Req.dt <-
  stringr::str_split_fixed(uber$Request.timestamp, " ", 2)[, 1]
uber$Req.tm <-
  stringr::str_split_fixed(uber$Request.timestamp, " ", 2)[, 2]
uber$Drop.dt <-
  stringr::str_split_fixed(uber$Drop.timestamp, " ", 2)[, 1]
uber$Drop.tm <-
  stringr::str_split_fixed(uber$Drop.timestamp, " ", 2)[, 2]

#-----------------------------------
# 1. Data Cleaning and Preparation
# 1.1	Identify the data quality issues and clean the data so that you can use it for analysis
#-----------------------------------
#Convert the Date values to standard R format (YYYY-MM-DD)
uber$Req.dt <-
  as.Date(str_replace_all(uber$Req.dt, "/", "-"), format = "%d-%m-%Y")
uber$Drop.dt <-
  as.Date(str_replace_all(uber$Drop.dt, "/", "-"), format = "%d-%m-%Y")

#-----------------------------------
# 1.Data Cleaning and Preparation 
# 1.2	Ensure that the dates and time are in the proper format. 
#     Derive new variables which will be useful for analysis
#-----------------------------------
# Split the time value into a new column to identify the hour of the day (for both
# Request and drop times.)
# This will help us analyze the data in blocks of hours, which can also be
# later used to identify time of day (morning, afternoon, evening, night, late night etc.)
uber$Req.Hr <-
  as.numeric(stringr::str_split_fixed(uber$Req.tm, "\\:", 2)[, 1])
uber$Drop.Hr <-
  as.numeric(stringr::str_split_fixed(uber$Drop.tm, ":", 2)[, 1])

#-----------------------------------
# 1.Data Cleaning and Preparation
# 1.1	Identify the data quality issues and clean the data so that you can use it for analysis
# 1.2	Ensure that the dates and time are in the proper format. 
#     Derive new variables which will be useful for analysis
#-----------------------------------
#The request and drop times are currently in different formats in the input file
# Few of them have no padding for the hour field, some dont have the seconds field etc.
# Hence, the next step is to convert the request and pick up time fields to TIME format, 
# so that calculations can be done on the same using R

# The below command uses the chron package to conver the time to R time format. Seconds, if 
# not there are defaulted to 00
uber$Req.tm <- chron(times=ifelse (nchar(uber$Req.tm) <= 5, paste(uber$Req.tm,":00",sep=""),uber$Req.tm))
uber$Drop.tm <- chron(times=ifelse (nchar(uber$Drop.tm) <= 5, paste(uber$Drop.tm,":00",sep=""),uber$Drop.tm))

#Drop the Request and Drop timestamp values (got from the initial file), as these
# are no longer required (since they are already split into useful columns).
uber$Request.timestamp <- NULL
uber$Drop.timestamp <- NULL

#-----------------------------------
# 1.Data Cleaning and Preparation
# 1.1	Identify the data quality issues and clean the data so that you can use it for analysis
# 1.2	Ensure that the dates and time are in the proper format. 
#     Derive new variables which will be useful for analysis
#-----------------------------------
#Add a columns to classify the time slot
#(3-7 as Early Mornings (Early Morn))
#(7-11 as Mornings(Morn))
#(11-15 as Afternoon (Anoon))
#(15-19 as Evenings(Eve))
#(19-23 as Late Evenings(Late Eve))
#(23-3 as Night)
# This approach is followed as its easy to understand this code and the file is not too big

uber$Req.timeslot <-
  ifelse(
    uber$Req.Hr > 3 & uber$Req.Hr <= 7,
    "Early Morn",
    ifelse (
      uber$Req.Hr > 7 & uber$Req.Hr <= 11,
      "Morn",
      ifelse (
        uber$Req.Hr > 11 & uber$Req.Hr <= 15,
        "Anoon",
        ifelse (
          uber$Req.Hr > 15 & uber$Req.Hr <= 19,
          "Eve",
          ifelse (uber$Req.Hr > 19 &
                    uber$Req.Hr <= 23, "Late Eve", "Night")
        )
      )
    )
  )

# The file is being sorted on Status, Timeslot and Pickup location
uber <- arrange(uber, Status, Req.timeslot, Pickup.point)

#-----------------------------------
#2.Results
#2.1.	Visually identify the most pressing problems for Uber
#-----------------------------------
#Plot the Count of trips against the Timeslot and Status of the trip
# using ggplot2
# Key points :-
#a)dodge is used in geom_bar to allow side by side comparison and
# not stacked view (Which is little difficult to comprehend)
#b) facet_grid ensures the 3 plots (1 for each Status) are printed in single view
#c) free_x is used for a free flow and not have all x axis values in each of the facet
#d) labels are used appropriately for x, y axis and legent (Pickup point in this case)
#e) Though we could add the actual count of trips on the graph itself using geom_text but
# that would make the graph cluttered.

ggplot(data = uber, aes(
  x = factor(Req.timeslot),
  fill = factor(uber$Pickup.point)
)) +
  geom_bar(alpha = 0.7, position = "dodge")+ 
  xlab("Timeslot") +ylab("Count") +labs(fill = 'Pickup') +facet_grid(.~uber$Status, scales = "free_x")

#Write back the file to local system (for spot checking/validation)
#write.csv(uber, file = "Uber_Analyzed.csv",row.names = F)

#-----------------------------------
#2.Results
#2.2.	Find out the gap between supply and demand and show the same using plots
#-----------------------------------
# Count the number of trips by Pickup location (Airport or City), Timeslot and Status
#
# We are not taking date into the analysis of Supply-Demand gap, because, based on the analysis
# of distribution of pickup requests among the 5 days(11 to 15 July), it can be seen that the 
# distribution of requests among different timeslots/pickup location is pretty even.
#
# This will help make general observations on a whole 
#(Analysis will get cluttered if we go at date level)
#
uber_stats <- setNames(count(uber, c('Status','Pickup.point','Req.timeslot')),c('Status','Pickup.point','Req.timeslot','Actual.shortage'))
# Filter the stats file to keep only "Cancelled" and "No Cars Available" status.
#
# This is done to determine the timeslots when the difference between Supply and Demand
# is the highest
#
uber_stats_no_Availability <- filter(uber_stats,Status == "Cancelled" | Status == "No Cars Available")
uber_stats_no_Availability <- arrange(uber_stats_no_Availability,desc(Actual.shortage))

# Output of top 4 uber_stats_no_Availability dataframe -> 
uber_top4_issues <- head(uber_stats_no_Availability,4)
# These are the issue areas Uber needs to concentrate on ->
#
#Output :-
#
# Status                 Pickup.point    Req.timeslot          Actual.shortage
#--------                ------------    ------------          ----
# No Cars Available      Airport         Eve (3 - 7 pm)        801
# No Cars Available      Airport         Late Eve (7-11 pm)    665
# Cancelled              City            Early Morn (3 - 7 am) 526
# Cancelled              City            Morn (7-11 am)        406

#Analysis of individual timeslots from the above table
# Lets analyze this slot first - 
# No Cars Available      Airport         Eve (3 - 7 pm)        801
#
# It makes sense to assume that the drivers with Pickup from City between
# 11 am to 3 pm (Afternoon timeslot), service the Airport pickup requests 
# between 3pm to 7pm (Evening).
# Because they would already be waiting at the airport
# 
# Pickup requests from City is 373 and of these, completed requests is 213 as calculated below -
Available_from_Anoon_slot <- filter(uber_stats,Pickup.point == "City" & Status == "Trip Completed" & Req.timeslot == "Anoon")[,4]
#
# Total number of requests from Airport between 3 to 7 pm is 1140 
# (calculated using the initial data provided, as below) ->
Req_at_Aiport_Evening_slot <- count(filter(uber,Pickup.point == "Airport" & Req.timeslot == "Eve"), c('Pickup.point'))[,2]
# Total "No Cars Available" at Aiport in Evening slot is 801 
# (calculated using the initial data provided, as below) ->
No_cars_Airport_Evening_Slot <- filter(uber_stats,Pickup.point == "Airport" & Status == "No Cars Available" & Req.timeslot == "Eve")[,4]

#The caculated Difference between Requested at Airport and Available at airport 
# (Eve slot) is 927,
#
Difference_Supply_Demand_Evening_slot <- Req_at_Aiport_Evening_slot - Available_from_Anoon_slot
#
# which closely matches the "Actual (from input file)" No_cars_Airport_Evening_Slot = 801
#
# This justifies the below reasoning.
#
#It can be seen that the Non availability of cars at Airport in the Evening Slot is because
#very few (less than 20% of the demand) cars travel from City to Airport in the previous timeslot, and
# hence large mismatch between Supply and Demand.

#Similarly, the issue of large Supply demand mismatch in the Late Evening Slot at the airport
# No Cars Available      Airport         Late Eve (7-11 pm)    665
# is due to low inflow of cabs from city in the previous timeslot, as shown below
Available_from_Evening_slot <- filter(uber_stats,Pickup.point == "City" & Status == "Trip Completed" & Req.timeslot == "Eve")[,4]
# Total number of requests from Airport between 3 to 7 pm is 1002->
Req_at_Aiport_Late_Eve_slot <- count(filter(uber,Pickup.point == "Airport" & Req.timeslot == "Late Eve"), c('Pickup.point'))[,2]
# Total "No Cars Available" at Aiport in Evening slot is 665->
No_cars_Airport_Late_Eve_Slot <- filter(uber_stats,Pickup.point == "Airport" & Status == "No Cars Available" & Req.timeslot == "Late Eve")[,4]
#Difference between Requested at Airport and Available at airport (Late Eve slot) is 706,
#which closely matches the "Actual (from input file)" = 665
Difference_Supply_Demand_Late_Eve_slot <- Req_at_Aiport_Late_Eve_slot - Available_from_Evening_slot
#
# This justifies the below reasoning.
#
#It can be seen that the Non availability of cars at Airport in the Late Evening Slot is because
#very few (less than 30% of the demand) cars travel from City to Airport in the previous timeslot, 
# and hence large mismatch between Supply and Demand.
#
# ***Key Analysis notes :- ***
#-----------------------------------
#2.Results
#2.3.	Key reasons for the Supply vs. Demand mismatch at Airport
#-----------------------------------
#
# Most of the cancellations in the City (it is to be noted that "No Cars Available" does not
# happen in the city), happen in the mornings (3 AM to 11 AM). The reason for this may be 
# because, the number of flights that arrive in the morning/afternoon is very less (Which is also
# evident from the low number of Pick up requests from Airport between 3 AM to 3 PM), hence,
# none of the drivers are actually interested to go to Airport from City during these hours 
#(because they might not get a ride back). Also, the frequency of requests with intercity travels 
# might be high at these hours (though this data is not available - but we can speculate). 
# People might travel to offices/colleges etc. and the cab drivers might be able to do multiple 
# rides during these hours in the city itself and hence avoid Airport drops during these times.

#In-short, it looks like the whole problem is because of low number of flights arriving in the 
#early morning to early afternoon leading to a cyclic issue (rides not available from airport 
#to city in afternoon/evening -- AND -- rides getting cancelled in morning from City to Airport)

# Status                 Pickup.point    Req.timeslot          freq
#--------                ------------    ------------          ----
# Cancelled              City            Early Morn (3 - 7 am) 526
# Cancelled              City            Morn (7-11 am)        406
 
#Plot the Supply vs. Demand issues for pickup location as Airport
Supply <- c(Available_from_Anoon_slot,Available_from_Evening_slot)
Demand <- c(Req_at_Aiport_Evening_slot,Req_at_Aiport_Late_Eve_slot)
uber_aiport_issues <- filter(uber_top4_issues,Pickup.point == "Airport")
uber_aiport_issues <- cbind(uber_aiport_issues,Supply,Demand)
  
#Convert to long format (to help with plotting) using melt function of reshape2 package

uber_aiport_issues_long <- melt(uber_aiport_issues)

#-----------------------------------
#2.Results
#2.2.	Find out the gap between supply and demand and show the same using plots
#-----------------------------------
ggplot(uber_aiport_issues_long, aes(x = factor(Req.timeslot), y = value,fill=variable)) +
  geom_bar(stat='identity',alpha = 0.7, position = "dodge")

#Actually, no need to plot the Supply vs Demand for City 
#(since, this is happening due to cab drivers estimating that they might
# not get a pickup from Airport as explained above)
#***Key Analysis notes :- ***
#-----------------------------------
#2.Results
#2.4.	Recommendation of some ways to resolve the supply-demand gap. 
# Some key suggestions/points ->
# Since, the arrivals and departures of flights is not in control of the cab company,
# following are some thoughts to alleviate these issues :-
# a) Incentivize cab drivers to accept trips to Airport during the mornings (3AM - Noon), 
#    this will help alleviate the issue with cab cancellations during morning hours. The cost
#    of this needs to be born by the commuters(50%). Hence the cost of the trip needs to be 
#    increased accordinlgy for morning hours. Since the cab drivers would be waiting almost 
#    half a day for a return trip (as traffic increases only in the evening/late evening period 
#    from Airport)
# b) The other 50% of the increased cost as mentioned in point a, needs to be born by the 
#    customers in evening and late evening trips from the Airport (since the cab drivers would
#    be waiting since morning/afternoon for these pickups)
#-----------------------------------

#===End===

