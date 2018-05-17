#Module 6

setwd("C:/Superstore Sales Data")

getwd()

market_fact <- read.csv("market_fact.csv", stringsAsFactors = FALSE)
cust_dimen <- read.csv("cust_dimen.csv", stringsAsFactors = FALSE)
orders_dimen <- read.csv("orders_dimen.csv", stringsAsFactors = FALSE)
prod_dimen <- read.csv("prod_dimen.csv", stringsAsFactors = FALSE)
shipping_dimen <- read.csv("shipping_dimen.csv", stringsAsFactors = FALSE)

?merge

merge1 <- merge(market_fact,cust_dimen,by="Cust_id")
merge2 <- merge(merge1,orders_dimen,by="Ord_id")
merge3 <- merge(merge2,prod_dimen,by="Prod_id")
merge4 <- merge(merge3,shipping_dimen,by="Ship_id")


#slice operation
subset(merge4, merge4$Customer_Name = "JOHN CASTELL")

#dice operation
dice1 <- subset(merge4, merge4$Region == "ATLANTIC" & merge4$Order_Priority == "MEDIUM")subset(merge4, merge4$Customer_Name = "JOHN CASTELL")

#rollup operation
rollup1 <- aggregate(merge4$Sales, by=list(merge4$Province), FUN=sum)

rollup2 <- aggregate(merge4$Sales, by=list(merge4$Ship_Mode), FUN=sum)
rollup3 <- aggregate(merge4$Profit, by=list(merge4$Product_Sub_Category), FUN=sum)
