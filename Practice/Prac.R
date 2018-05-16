setwd('C:/pgdds/Course 3/Clustering')

## Importing the dataset

Online.Retail <- read.csv("Online Retail.csv", stringsAsFactors=FALSE)

## NA value treatment

order_wise <- na.omit(Online.Retail)

## Making RFM data

Amount <- order_wise$Quantity * order_wise$UnitPrice
order_wise <- cbind(order_wise,Amount)

order_wise <- order_wise[order(order_wise$CustomerID),]

monetary <- aggregate(Amount~CustomerID, order_wise, sum)

frequency <- order_wise[,c(7,1)]

k<-table(as.factor(frequency$CustomerID))

k<-data.frame(k)

colnames(k)[1]<-c("CustomerID")

master <-merge(monetary,k,by="CustomerID")

recency <- order_wise[,c(7,5)]

recency$InvoiceDate<-as.Date(recency$InvoiceDate,"%m/%d/%Y %H:%M")

maximum<-max(recency$InvoiceDate)

maximum<-maximum+1

maximum$diff <-maximum-recency$InvoiceDate

recency$diff<-maximum$diff

df<-aggregate(recency$diff,by=list(recency$CustomerID),FUN="min")

colnames(df)[1]<- "CustomerID"

colnames(df)[2]<- "Recency"

RFM <- merge(monetary, k, by = ("CustomerID"))

RFM <- merge(RFM, df, by = ("CustomerID"))

RFM$Recency <- as.numeric(RFM$Recency)

## Outlier treatment

box <- boxplot.stats(RFM$Amount)
out <- box$out

RFM1 <- RFM[ !RFM$Amount %in% out, ]

RFM <- RFM1

box <- boxplot.stats(RFM$Freq)
out <- box$out

RFM1 <- RFM[ !RFM$Freq %in% out, ]

RFM <- RFM1

box <- boxplot.stats(RFM$Recency)
out <- box$out

RFM1 <- RFM[ !RFM$Recency %in% out, ]

RFM <- RFM1

## Standardisation of data

RFM_norm1<- RFM[,-1]

RFM_norm1$Amount <- scale(RFM_norm1$Amount)
RFM_norm1$Freq <- scale(RFM_norm1$Freq)
RFM_norm1$Recency <- scale(RFM_norm1$Recency)

# ## Implementing K-Means algorithm
# cluster: This stores the cluster IDs for each data point
# centers: This gives the location of the cluster centres
# totss: This measures the total spread in the data by calculating the total sum of squares of distance
# withinss: This is a measure of the within-cluster sum of squares, one component per cluster
# tot.withinss: This gives the total within-cluster sum of squares, i.e. sum(withinss)
# betweenss: The inter-cluster sum of squares of the distance, i.e. totss-tot.withinss
# size: The number of points in each cluster
# iter: The number of (outer) iterations
# ifault: integer indicator of a possible algorithm problem - for experts

# min intra cluster distance => reduce withinss/totss
# max inter cluster distance => increase betweenss/totss

# R-sq value which is the ratio of (betweenss/totss) -> Maximize this.
# 
# Pseudo F statistic, which is given by (between-cluster-sum-of-squares / (c-1)) / (within-cluster-sum-of-squares / (n-c)),
# where c is the number of clusters and n is the total number of data points


clus3 <- kmeans(RFM_norm1, centers = 3, iter.max = 50, nstart = 50)

## Finding the optimal value of K

r_sq<- rnorm(20)

for (number in 1:20){clus <- kmeans(RFM_norm1, centers = number, nstart = 50)
r_sq[number]<- clus$betweenss/clus$totss
}

plot(r_sq)

warnings()

## Running the K-Means algorithm for K =4,5,6

clus4 <- kmeans(RFM_norm1, centers = 4, iter.max = 50, nstart = 50)

clus5 <- kmeans(RFM_norm1, centers = 5, iter.max = 50, nstart = 50)

clus6 <- kmeans(RFM_norm1, centers = 6, iter.max = 50, nstart = 50)

## Appending the ClusterIDs to RFM data

RFM_km <-cbind(RFM,clus5$cluster)

colnames(RFM_km)[5]<- "ClusterID"

## Cluster Analysis

library(dplyr)
library(ggplot2)

km_clusters<- group_by(RFM_km, ClusterID)

tab1<- summarise(km_clusters, Mean_amount=mean(Amount), Mean_freq=mean(Freq), Mean_recency=mean(Recency))

ggplot(tab1, aes(x= factor(ClusterID), y=Mean_amount)) + geom_bar(stat = "identity")
ggplot(tab1, aes(x= factor(ClusterID), y=Mean_freq)) + geom_bar(stat = "identity")
ggplot(tab1, aes(x= factor(ClusterID), y=Mean_recency)) + geom_bar(stat = "identity")


###Graded####

graded <- read.csv("Cricket.csv", stringsAsFactors=FALSE)

graded$SR <- scale(graded$SR)
graded$Ave <- scale(graded$Ave)

grad <- graded[,c("SR","Ave")]

clus_g1 <- kmeans(grad, centers = 4, iter.max = 50, nstart = 50)


## Finding the optimal value of K

r_sq2<- rnorm(20)

for (number in 1:20){clus_g2 <- kmeans(grad, centers = number, nstart = 50)
r_sq2[number]<- clus_g2$betweenss/clus_g2$totss
}

plot(r_sq2)

grad_km <-cbind(graded,clus_g1$cluster)


colnames(RFM_km)[5]<- "ClusterID"


#############

## Hierarchical clustering

## Calcualting the distance matrix

RFM_dist<- dist(RFM_norm1)

## Constructing the dendrogram using single linkage

RFM_hclust1<- hclust(RFM_dist, method="single")
plot(RFM_hclust1)

## Constructing the dendrogram using complete linkage

RFM_hclust2<- hclust(RFM_dist, method="complete")
plot(RFM_hclust2)

## Visualising the cut in the dendrogram

rect.hclust(RFM_hclust2, k=5, border="red")

## Making the cut in the dendrogram

clusterCut <- cutree(RFM_hclust2, k=5)

## Appending the ClusterIDs to RFM data

RFM_hc <-cbind(RFM,clusterCut)

colnames(RFM_hc)[5]<- "ClusterID"

## Cluster Analysis

hc_clusters<- group_by(RFM_hc, ClusterID)

tab2<- summarise(hc_clusters, Mean_amount=mean(Amount), Mean_freq=mean(Freq), Mean_recency=mean(Recency))
ggplot(tab2, aes(x= factor(ClusterID), y=Mean_recency)) + geom_bar(stat = "identity")
ggplot(tab2, aes(x= factor(ClusterID), y=Mean_amount)) + geom_bar(stat = "identity")
ggplot(tab2, aes(x= factor(ClusterID), y=Mean_freq)) + geom_bar(stat = "identity")