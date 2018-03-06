#Uber Assignment---

#Include stringr, dplyr and plyr libraries to perform data manipulations
#Include ggplot2 to create plots. These packages can be installed using
# install.packages("ggplot2") etc.
library(stringr)
library(dplyr)
library(plyr)
library(ggplot2)

#Set working directory to import file

setwd("C:/pgdds/Kaggle")
#Read the file into a dataframe
uber <-
  read.csv("Uber Request Data.csv",
           sep = ",",
           stringsAsFactors = FALSE)

#Split the Request and Drop timestamp columns into Date and Time
uber$Req.dt <-
  stringr::str_split_fixed(uber$Request.timestamp, " ", 2)[, 1]
uber$Req.tm <-
  stringr::str_split_fixed(uber$Request.timestamp, " ", 2)[, 2]
uber$Drop.dt <-
  stringr::str_split_fixed(uber$Drop.timestamp, " ", 2)[, 1]
uber$Drop.tm <-
  stringr::str_split_fixed(uber$Drop.timestamp, " ", 2)[, 2]

#Convert the Date values to standard R format (YYYY-MM-DD)
uber$Req.dt <-
  as.Date(str_replace_all(uber$Req.dt, "/", "-"), format = "%d-%m-%Y")
uber$Drop.dt <-
  as.Date(str_replace_all(uber$Drop.dt, "/", "-"), format = "%d-%m-%Y")

# Split the time value into a new column to identify the hour of the day (for both
# Request and drop times.)
# This will help us analyze the data in blocks of hours, which can also be
# later used to identify time of day (morning, afternoon, evening, night, late night etc.)
uber$Req.Hr <-
  as.numeric(stringr::str_split_fixed(uber$Req.tm, "\\:", 2)[, 1])
uber$Drop.Hr <-
  as.numeric(stringr::str_split_fixed(uber$Drop.tm, ":", 2)[, 1])

#Drop the Request and Drop timestamp values (got from the initial file), as these
# are no longer required (since they are already split into useful columns).
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
  geom_bar(alpha = 0.5, position = "dodge")+ 
  xlab("Timeslot") +ylab("Count") +labs(fill = 'Pickup') +facet_grid(.~uber$Status, scales = "free_x")

#Write back the file to local system (for spot checking/validation)
write.csv(uber, file = "Uber_Analyzed.csv")

?aes
