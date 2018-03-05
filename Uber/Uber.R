#Uber Assignment---
library(stringr)

#Set working directory to import file
setwd("C:/pgdds/Kaggle")
#Read the file into a dataframe
uber <- read.csv("Uber Request Data.csv", sep=",",stringsAsFactors =FALSE)

#Split the Request and Drop timestamp columns into Date and Time
uber$Req.dt <- stringr::str_split_fixed(uber$Request.timestamp," ",2)[,1]
uber$Req.tm <- stringr::str_split_fixed(uber$Request.timestamp," ",2)[,2]
uber$Drop.dt <- stringr::str_split_fixed(uber$Drop.timestamp," ",2)[,1]
uber$Drop.tm <- stringr::str_split_fixed(uber$Drop.timestamp," ",2)[,2]

#Convert the Date values to standard R format (YYYY-MM-DD)
uber$Req.dt <- as.Date(str_replace_all(uber$Req.dt, "/", "-"),format="%d-%m-%Y")
uber$Drop.dt <- as.Date(str_replace_all(uber$Drop.dt, "/", "-"),format="%d-%m-%Y")

# Split the time value into a new column to identify the hour of the day (for both 
# Request and drop times.)
# This will help us analyze the data in blocks of hours, which can also be
# later used to identify time of day (morning, afternoon, evening, night, late night etc.)
uber$Req.Hr <- stringr::str_split_fixed(uber$Req.tm,"\\:",2)[,1]
uber$Drop.Hr <- stringr::str_split_fixed(uber$Drop.tm,":",2)[,1] 

#Drop the Request and Drop timestamp values (got from the initial file), as these 
# are no longer required.
uber$Request.timestamp <- NULL
uber$Drop.timestamp <- NULL