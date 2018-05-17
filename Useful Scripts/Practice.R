# TO-DO
# Write a function which identifies blank characters representing the unavailable values, and replaces them with NA 
# Make sure that the resultant vector is a numeric vector
# Consider these as blank values :  " ", "", "-"

remove_blank <- function(vector){
  # store the resulting vector in corrected_vector
  # Please enter your code below
  
  corrected_vector <- as.numeric(gsub("^$|^ $|^-$", NA, vector))
    
    
    #---DO NOT EDIT THE CODE BELOW---#
    return(corrected_vector)
}

# Test - Passing c(21,""," " ,34,"-",78, 98) to your function
remove_blank(c(21,""," " ,34,"-",78, 98))
# The ideal output should look like: 21 NA NA 34 NA 78 98



# TODO
# Write a function which identifies and removes the "*" character after some numeric values in a vector.
# Make sure that the resultant vector is a numeric vector.


remove_string <- function(vector){
  # store the resulting vector in corrected_vector
  # write all your code here
  
  
  corrected_vector <- as.numeric(gsub("\\*","" , vector))
    
    
    #---DO NOT EDIT THE CODE BELOW---#
    return(corrected_vector)
}

# Test - Passing a vector - c(21,34,99*,56,90*, 45*), to your function
remove_string(c(21,34,"99*",56,"90*", "45*"))
# The ideal output should be numeric and look like: 21 34 99 56 90 45

# TODO
# Write a function which takes a vector of marks and replaces every value above 100 with NA

remove_invalid <- function(marks_vector){
  # store the resulting vector in Corrected_marks
  # Please enter your code below
  
  
  corrected_marks <- replace(marks_vector,marks_vector>100,NA)
    
    
    #---DO NOT EDIT THE CODE BELOW---#
    return(corrected_marks)
}


# Test - Passing a vector - c(89, 90, 108, 56), to your function
remove_invalid(c(89, 90, 108, 56))
# The ideal output should look like: 89 90 NA 56


# To-DO
# Complete the function which takes in a vector & replaces the phone numbers not having exactly 10 digits with NA

phone <- function(phone_vector){
  
  # Store the cleaned vector in clean_vector
  # Please enter your code below
  
  
  clean_vector <- replace(phone_vector,phone_vector<=999999999|phone_vector>9999999999,NA)
    
    
    #---DO NOT EDIT THE CODE BELOW---#
    return(clean_vector)
}

# Test - Passing a vector - c(99887766, 998877665521, 9897932453) to your function
phone(c(99887766, 998877665521, 9897932453))
# The ideal output should look like: NA NA 9897932453
#============
# TO-DO
# some_df is a dataframe which may contain duplicate rows
# Complete the function such that it returns a dataframe after removing them from some_df

remove_dup <- function(some_df){
  # remove duplicate values from some_df and store the resulting dataframe in new_df
  
  # Please enter your code below
  
  new_df <- unique(some_df)
  #new_df <- some_df[-which(duplicated(some_df)), ]
    
    
    
    #---DO NOT EDIT THE CODE BELOW---#
    return(new_df)
}


# Passing a dataframe - data.frame(rbind(c(2,9,6),c(4,6,7),c(4,6,7),c(4,6,7),c(2,9,6))), to your function
remove_dup(data.frame(rbind(c(2,9,6),c(4,6,7),c(4,6,7),c(4,6,7),c(2,9,6))))
# The ideal output is:   X1 X2 X3
#                      1  2  9  6
#                      2  4  6  7


library(ggplot2)
EDA_nas <- read.delim("EDA_nas.csv", sep = ",")

EDA_nas$Science <- EDA_nas$Science..

ggplot(EDA_nas, aes(x = Watch.TV, y = Science)) +
  geom_boxplot()

ggplot(EDA_nas,aes(x=factor(EDA_nas$Watch.TV),y=(EDA_nas$Science..),col=factor(EDA_nas$Watch.TV)))+
  geom_boxplot()+
  geom_jitter(width = .3)+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red")

ggplot(EDA_nas, aes(x = Mother.edu, y = Reading..)) +
  geom_boxplot()+
geom_jitter(width = .3)


census <- read.xlsx(
  file = "EDA_census.xlsx",
  sep = "\t",
  header = TRUE,
  fill = TRUE,
  comment.char = "",
  stringsAsFactors = FALSE,
  quote = ""
)

census <- readxl::read_excel("EDA_census.xlsx")

census_india <- filter(census, AreaName == "INDIA")

census_india$literacy <- (census_india$illFemales/census_india$TotalFemales) * 100

census_india$totallit <- (census_india$litPersons/census_india$TotalPersons) * 100

census_allages <- filter(census, Agegroup == "All ages")

census_allages_totals <- filter(census_allages,Tots == "Total")

census_allages_totals$femalelit <- (census_allages_totals$litFemales/census_allages_totals$TotalFemales) * 100

census_allages_totals$totallit <- (census_allages_totals$litPersons/census_allages_totals$TotalPersons) * 100

q3_ans <- arrange(census_allages_totals,desc(femalelit))

q4_ans <- arrange(census_allages_totals,totallit)

#PRactice bivariate

cur <- read.delim("currencies.csv", sep = ",")
cur1 <- cur[,c("Chinese.Yuan","Euro","Japanese.Yen","U.K..Pound.Sterling","U.S..Dollar","Australian.Dollar","Indian.Rupee")]
cur2 <- na.omit(cur1)
z <- cor(cur2)
require(lattice)
levelplot(z)



# TO-DO
# A vector contains the scores of a player in various innings
# Write a function that takes in the vector as input and returns the number of centuries scored by the player. 

centuries <- function(player_vector){
  # store the centuries scored by the player in centuries
  # Please enter your code below
  
  sum_centuries <- sum(player_vector >= 100)
  #sum_centuries <- sum(ifelse(player_vector>=100,1,0))
    
    
    #---DO NOT EDIT THE CODE BELOW---#
    return(sum_centuries)
}

# Test - Passing a vector - c(120 ,30, 134, 16,102) to your function
centuries(c(120 ,30, 134, 16,102))
# The ideal output should be: 3

#PRactice - derived metrics chapter
odi <- read.delim("odi-batting.csv", sep = ",")

odi$centuries <- ifelse(odi$Runs>=100,"1","0")

install.packages("sqldf")
library(sqldf)
cent <- sqldf('  select Player,sum(centuries) from odi group by Player order by sum(centuries) desc  ')

odi$new_variable<- ifelse(odi$Runs>=100,1,0) 
c <- aggregate(new_variable~Country+Player,odi,sum) 

cent2 <- sqldf('  select * from odi where centuries = 1  ')
cent2$strk <- (cent2$Runs/cent2$Balls)*100

cent3 <- arrange(cent2,desc(cent2$strk))

odi$Year <- substr(odi$MatchDate,7,10)
cent4 <- sqldf('select substr,sum(centuries) from odi group by Player order by sum(centuries) desc  ')

c <- filter(odi,Country == "India")
d <- aggregate(new_variable~Country+Year,c,sum) 
e <- arrange(d,desc(new_variable))
