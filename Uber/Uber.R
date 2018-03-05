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
uber$Req.Hr <- as.numeric(stringr::str_split_fixed(uber$Req.tm,"\\:",2)[,1])
uber$Drop.Hr <- as.numeric(stringr::str_split_fixed(uber$Drop.tm,":",2)[,1])

#Drop the Request and Drop timestamp values (got from the initial file), as these 
# are no longer required.
uber$Request.timestamp <- NULL
uber$Drop.timestamp <- NULL

#Add a columns to classify the time slot
#(3-7 as Early Mornings (Early Morn))
#(7-11 as Mornings(Morn))
#(11-15 as Afternoon (Anoon))
#(15-19 as Evenings(Eve))
#(19-23 as Late Evenings(Late Eve))
#(23-3 as Night)
# This approach is followed as its easy to understand this code and the file is not too big

uber$Req.timeslot <- ifelse(uber$Req.Hr>3 & uber$Req.Hr <= 7,"Early Morn",
                        ifelse (uber$Req.Hr>7 & uber$Req.Hr <= 11,"Morn",
                            ifelse (uber$Req.Hr>11 & uber$Req.Hr <= 15,"Anoon",
                                ifelse (uber$Req.Hr>15 & uber$Req.Hr <= 19,"Eve",
                                    ifelse (uber$Req.Hr>19 & uber$Req.Hr <= 23,"Late Eve","Night")
                                    )
                                )
                            )
                        )

#Plot the Status, Hour of the day (delete later)

# ggplot(uber, aes(x = factor(Req.timeslot), fill = factor(uber$Status))) +
#   geom_bar(alpha = 0.5,position = "dodge")+
#   xlab("Timeslot")+ylab("Count")+ labs(fill='Status')

#Plot the Status, Req.id and Hour of the day

ggplot(data = uber, aes(x=factor(Req.timeslot), fill = factor(uber$Pickup.point))) + 
  geom_bar(alpha = 0.5,position = "dodge") + 
  facet_wrap(~uber$Status) +
  xlab("Timeslot")+ylab("Count")+ labs(fill='Status')

#Write back the file to local system (for spot checking/validation)
write.csv(uber,file="Uber_Analyzed.csv")



