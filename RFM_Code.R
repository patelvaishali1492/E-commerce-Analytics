rm(list=ls())
gc()
setwd("C:/Users/upadh/Desktop/Ankita Upadhyay/577")

install.packages('readxl')
install.packages("dplyr")
library(readxl)
library(plyr)
library(dplyr)
library(ggplot2)


retail<-read_excel('Online Retail.xlsx')
retail2<-retail
#Data Cleaning Code
#this code is to remove all rows with missing values
table(is.na(retail))

retail<-retail[complete.cases(retail),]
table(is.na(retail))
class(retail)

str(retail)
retail

#this code is to remove only rows with negative quantity
retail <- retail[retail$Quantity > 0,]

#this code is to remove only rows where unit price = 0 
retail <- retail[!(retail$UnitPrice == '0'),]

#this code is to remove all rows without customer ID
retail <- retail[!is.na(retail$CustomerID), ]

#this code is to create a new parameter Revenue
retail$Amount <- retail$Quantity*retail$UnitPrice

head(retail)

retail <- retail[,c(-2,-3,-4)]

#Approach I-RFM Class

firstPart <- unique(retail[,c(1,4,2)])
head(firstPart)

secondPart <- aggregate(list(Amount=retail$Amount),
                        by=list(InvoiceNo=retail$InvoiceNo), FUN=sum)
head(secondPart)

dataRFM <- merge(firstPart,secondPart, by = "InvoiceNo")
head(dataRFM)

dataRFM$InvoiceDate <- as.Date(dataRFM$InvoiceDate, format = "%m/%d/%Y")
head(dataRFM)

dataRFM <- na.omit(dataRFM)
library(didrooRFM)

resultsRFM <-findRFM(dataRFM,recencyWeight = 4, frequencyWeight = 4,
                     monetoryWeight = 4)
head(resultsRFM)

head(resultsRFM[,c(1,8:10,16)])

write.csv(resultsRFM, "ResultsRFM_Correct.CSV")

tblClass <- table(resultsRFM$FinalCustomerClass)
tblClass
barplot(tblClass)

head(retail)

retail.nondup <- retail[,c(4,5)]
head(retail.nondup)
retail.nondup <- unique(retail.nondup[,c(1,2)])
head(retail.nondup)

RFMCountry <-merge(resultsRFM[,c(1,8:10,16)],retail.nondup, by="CustomerID")
colnames(RFMCountry) <- c("ID","Money","Frequency","Recency","Class","Country")
head(RFMCountry)

table(RFMCountry$Country,RFMCountry$Class)

#Approach II -clustering
totwss <- NULL
for (i in 2:15){
  totwss <- append(totwss,kmeans(resultsRFM[,8:10],centers=i)$tot.withinss)
}
plot(x=2:15, y=totwss, type="b", xlab="Clusters", ylab= "Total Within SS")

install.packages("rattle")
library(rattle)
rattle()
rounded.clusters <- as.data.frame.matrix(round(crs$kmeans$centers))
rounded.clusters
with(rounded.clusters, rounded.clusters[order(-RecencyScore,-FrequencyScore, -
                                                MonetoryScore),])
table(crs$kmeans$size)

table(Cluster=crs$kmeans$cluster,Class=resultsRFM$FinalCustomerClass)


chisq.test(table(Cluster=crs$kmeans$cluster,
                 Class=resultsRFM$FinalCustomerClass))

table(crs$kmeans$cluster)

k1<-crs$kmeans
Clusters <- k1$cluster
Centres <- k1$centers
str(Clusters)
Clusters
head(Clusters)
head(k1)
write.csv(Clusters,"Clusters_k2.csv")
write.csv(Centres,"Centres_k2.csv")

#classification by Tree
library(lubridate)
head(resultsRFM)
RFM_data1<- resultsRFM[,c(1,2,3,4)]
head(RFM_data1)
currentdate <- '2012-01-01'
RFM_data1$Recency<-difftime(ymd(currentdate), ymd(RFM_data1$LastTransaction))
RFM_data1$AvgMonetary<- RFM_data1$MeanValue
RFM_data1$Frequency<- RFM_data1$NoTransaction
head(RFM_data1)
list(RFM_data1)
as.numeric(RFM_data1$CustomerID)
as.numeric(RFM_data1$Recency)
as.numeric(RFM_data1$AvgMonetary)
as.numeric(RFM_data1$Frequency)
str(RFM_data1)
as.data.frame.matrix(RFM_data1)
head(RFM_data1)
RFM_data1 <- RFM_data1[,c(-2,-3,-4)]
RFM_data_Classification1<- RFM_data1[,c(1,3,4,2)]
head(Clusters)
head(RFM_data_Classification1)
Clusters<- as.data.frame(Clusters)
Clusters<- Clusters[1:4338,]
Clusters <- as.data.frame(Clusters)
str(Clusters)
head(Clusters)
#str(RFM_kmeans_cluster)
write.csv(Clusters,"Clusters_k2_withclassification.csv")

head(RFM_data_Classification1)
RFM_data_Classification1$Cluster <- Clusters$Clusters
write.csv(RFM_data_Classification1,"RFM_data_Classificationk2.csv")
classification_data1 <- RFM_data_Classification1
classification_data1 <- classification_data1[,-1]
head(classification_data1)
str(classification_data1)
install.packages("RWeka")
install.packages("rJava")
library(RWeka)
library(rJava)
library(party)
library(partykit)
library("arules")
classification_data1$Cluster<-(discretize(classification_data1$Cluster, method = "interval", breaks = ))
head(classification_data1)
#train data 70%, test data 30%
train_data <- classification_data1[1:3060,]
train_data <- train_data[,-1]
head(train_data)
write.csv(train_data,"train_data_k2.csv")
test_data <- classification_data1[3061:4338,]
test_data <- test_data[,-1]
write.csv(test_data,"test_data_k2.csv")
list(train_data)
toString(train_data)
decision_tree1 <- J48(Cluster~., data = train_data, control = Weka_control(M = 5, C = 0.1, B = F))
decision_tree1

dt1 <- read.arff(system.file("arff", "train_data.arff",
                             package = "RWeka"))
dt2 <- J48(Cluster~ ., data = dt1,control = Weka_control(M = 5, C = 0.1, B = F))

plot(as.party.Weka_tree(decision_tree1))

evaluate_Weka_classifier(decision_tree1, class = T, numFolds = 10)
# Use new test dataset
evaluate_Weka_classifier(decision_tree1, newdata = test_data, class = T)
# Both decision tree models have high accuracies

install.packages("party")
library(party)
install.packages("partykit")
library(partykit)

library(rpart)
tree2<-rpart(Cluster~.,method="class", data = train_data, cp=1e-2, minsplit=20)
printcp(tree2)
plotcp(tree2)
summary(tree2)
plot(tree2, uniform=T, main='Classification tree')
text(tree2, use.n=T,all=T,cex=0.8)
# prune the tree
pfit = prune(tree2, cp = tree2$cptable[which.min(tree2$cptable[,"xerror"]),"CP"])

# plot the pruned tree
plot(pfit, uniform=T, main="Pruned Classification Tree")
text(pfit, use.n=T, all=T, cex=.8)
