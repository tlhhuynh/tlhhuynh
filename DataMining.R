# Homework 1 Data Mining
#load required data mining libraries
library(datasets)
library(arules)
library(arulesViz)
library(ggplot2)
library(dplyr)
library(rpart)
library(rpart.plot)
library(TH.data)
library(ISLR2)
library(lattice)
library(stats)
library(rattle)
library(RColorBrewer)
library(caret)
library(ROCR)
library(tidyverse)  
library(cluster)  
library(factoextra) 
library(gridExtra)
library(NbClust)
library(dendextend)
library(class)
library(ClustOfVar)
library(MASS)
library(kableExtra)
library(partykit)
library(dbscan)
library(knitr)
library(mosaic)

#load orderData csv from github
orderData <- read.table("https://raw.githubusercontent.com/sjsimmo2/DataMining-Fall/master/orderData.csv",sep=',',header=T)
orderData1 <- split(orderData$item, orderData$orderNo)

#subsetting main entrees for summary stats
entreeSum <- orderData %>% filter(row_number() %% 3 == 1)
#summary info on main entrees
table(entreeSum$item)
#bar graph of main entrees ordered
ggplot(entreeSum, aes(x = item)) + geom_bar() +  geom_text(aes(label = ..count..), stat = "count", vjust = -0.2) + labs(x = "Main Entrees", y = "Counts")

#subsetting wine for summary stats
wineSum <- orderData %>% filter(row_number() %% 3 == 2)
#summary info on wines 
table(wineSum$item)

#subsetting sides for summary stats
sidesSum <- orderData %>% filter(row_number() %% 3 == 0)
#summary info on wines 
table(sidesSum$item)

#create transactional data set
trans.order <- as(split(orderData$item, list(orderData$seatNo,orderData$orderNo),drop=TRUE), "transactions") 
#drop = TRUE drops all transactions that are empty created because some customer orders have more seat #s than others  
#prints out list of each transaction per customer [main entree, wine, side item]
inspect(trans.order)

#The following code runs the association analysis and stores the results in the object rules. Once you have this stored object, you can view it and partition it in different ways
trans.order@itemInfo$labels
# Create an item frequency plot for all items
itemFrequencyPlot(trans.order,topN = length(unique(orderData$item)),type="relative")

# partitioning the rules into rules.order (all rules for every combo of entree/wine/side)
# partitioning the rules into individual sets categorized by main entree
#rules.order is rules for all combination of menu items not categorized by main entree
rules.order <- apriori(trans.order, parameter = list(supp = 0.001, conf = 0.001, target="rules")) 
duck <- apriori(trans.order, parameter = list(supp = 0.001, conf = 0.001), appearance = list(lhs="Duck Breast",default="rhs"),minlen=2) 
#minlen option forces main entree to lhs, without it there were rules showing up with nothing on lhs
filet <- apriori(trans.order, parameter = list(supp = 0.001, conf = 0.001), appearance = list(lhs="Filet Mignon",default="rhs"),minlen=2)
porkChop <- apriori(trans.order, parameter = list(supp = 0.001, conf = 0.001), appearance = list(lhs="Pork Chop",default="rhs"),minlen=2)
porkTen <- apriori(trans.order, parameter = list(supp = 0.001, conf = 0.001), appearance = list(lhs="Pork Tenderloin",default="rhs"),minlen=2)
chicken <- apriori(trans.order, parameter = list(supp = 0.001, conf = 0.001), appearance = list(lhs="Roast Chicken",default="rhs"),minlen=2)
salmon <- apriori(trans.order, parameter = list(supp = 0.001, conf = 0.001), appearance = list(lhs="Salmon",default="rhs"),minlen=2)
bass <- apriori(trans.order, parameter = list(supp = 0.001, conf = 0.001), appearance = list(lhs="Sea Bass",default="rhs"),minlen=2)
sword <- apriori(trans.order, parameter = list(supp = 0.001, conf = 0.001), appearance = list(lhs="Swordfish",default="rhs"),minlen=2)

# sort each partition by decreasing lift - we want to see lifts greater than 1 
rules.order <-sort(rules.order, by="lift", decreasing=TRUE) 
duck <-sort(duck, by="lift", decreasing=TRUE)
filet <-sort(filet, by="lift", decreasing=TRUE)
porkChop <-sort(porkChop, by="lift", decreasing=TRUE)
porkTen <-sort(porkTen, by="lift", decreasing=TRUE)
chicken <-sort(chicken, by="lift", decreasing=TRUE)
salmon <-sort(salmon, by="lift", decreasing=TRUE)
bass <-sort(bass, by="lift", decreasing=TRUE)
sword <-sort(sword, by="lift", decreasing=TRUE)

# print out top 10 partitioned sorted rules
rules.order1 <- inspect(rules.order[1:200]) 
duck1 <- inspect(duck[1:10])
filet1 <- inspect(filet[1:10])
pork1 <- inspect(porkChop[1:10])
pork2 <- inspect(porkTen[1:10])
chicken1 <- inspect(chicken[1:10])
salmon1 <- inspect(salmon[1:10])
bass1 <- inspect(bass[1:10])
sword1 <-inspect(sword[1:10])

# creating data frame of rules for main entrees with lift greater than 1
entree.wine <- data.frame(matrix(ncol = 0, nrow = 0))
entree.wine <- bind_rows(duck1[duck1$lift > 1,], filet1[filet1$lift > 1,], pork1[pork1$lift > 1,], 
                         pork2[pork2$lift > 1,], chicken1[chicken1$lift > 1,], salmon1[salmon1$lift > 1,], 
                         bass1[bass1$lift > 1,], sword1[sword1$lift > 1,])

#remove brackets {} before & after entree & wine/side text
entree.wine$rhs <- gsub("[{}]", "", entree.wine$rhs) 
entree.wine$lhs <- gsub("[{}]", "", entree.wine$lhs)
#write to csv​
# write.csv(entree.wine, file = "entree.wine.csv")

# creating data frame of rules for all combination of menu items 
rules.order2 <- as.data.frame(rules.order1)
#removing brackets {} from lhs & rhs
rules.order2$rhs <- gsub("[{}]", "", rules.order2$rhs) 
rules.order2$lhs <- gsub("[{}]", "", rules.order2$lhs)
#write to csv
# write.csv(rules.order2, file = "rules.order.csv")

filet2 <- apriori(trans.order, parameter = list(supp = 0.001, conf = 0.001), appearance = list(lhs=c("Filet Mignon", "Seasonal Veg"),default="rhs"),minlen=2)
porkChop2 <- apriori(trans.order, parameter = list(supp = 0.001, conf = 0.001), appearance = list(lhs=c("Pork Chop", "Seasonal Veg"),default="rhs"),minlen=2)
porkTen2 <- apriori(trans.order, parameter = list(supp = 0.001, conf = 0.001), appearance = list(lhs=c("Pork Tenderloin", "Seasonal Veg"),default="rhs"),minlen=2)
chicken2 <- apriori(trans.order, parameter = list(supp = 0.001, conf = 0.001), appearance = list(lhs=c("Roast Chicken", "Seasonal Veg"),default="rhs"),minlen=2)
salmon2 <- apriori(trans.order, parameter = list(supp = 0.001, conf = 0.001), appearance = list(lhs=c("Salmon","Seasonal Veg"),default="rhs"),minlen=2)
bass2 <- apriori(trans.order, parameter = list(supp = 0.001, conf = 0.001), appearance = list(lhs=c("Sea Bass","Seasonal Veg"),default="rhs"),minlen=2)
sword2 <- apriori(trans.order, parameter = list(supp = 0.001, conf = 0.001), appearance = list(lhs=c("Swordfish","Seasonal Veg"),default="rhs"),minlen=2)

# sort each partition by decreasing lift - we want to see lifts greater than 1 
rules.order <-sort(rules.order, by="lift", decreasing=TRUE) 
duck <-sort(duck, by="lift", decreasing=TRUE)
filet2 <-sort(filet2, by="lift", decreasing=TRUE)
porkChop <-sort(porkChop, by="lift", decreasing=TRUE)
porkTen <-sort(porkTen, by="lift", decreasing=TRUE)
chicken <-sort(chicken, by="lift", decreasing=TRUE)
salmon <-sort(salmon, by="lift", decreasing=TRUE)
bass <-sort(bass, by="lift", decreasing=TRUE)
sword <-sort(sword, by="lift", decreasing=TRUE)


#Homework 2 Churn
#load orderData csv from github
Telechurn <- read.table("https://raw.githubusercontent.com/sjsimmo2/DataMining-Fall/master/TelcoChurn.csv",sep=',',header=T)

#classification
for (i in 2:ncol(Telechurn)){
  print(table(Telechurn[i],exclude=NULL))
}# did not identify NA from table entries

#make training and test data with random sample
set.seed(123)
perm=sample(1:7043)
churn_randomOrder = Telechurn[perm,]
c.train = churn_randomOrder[1:floor(0.8*7043),]
c.test = churn_randomOrder[(floor(0.8*7043)):7043,]

#classification tree removing customer ID as predictor
c.tree = rpart(Churn ~ . - customerID, data = c.train, method = 'class', parms = list(split='gini'))
summary(c.tree)
print(c.tree)
#plot tree, type=4 labels all nodes
rpart.plot(c.tree, type=4, main="Decision Tree for Customer Churn Prediction", cex.main=1)

#check variable importance
c.tree$variable.importance #identified 12 important variables
#plot variable importance
c.varimp=data.frame(c.tree$variable.importance)
c.varimp$names=as.character(rownames(c.varimp))
ggplot(data=c.varimp,aes(x=names,y=c.tree.variable.importance)) + geom_bar(stat="identity")+coord_flip() + labs(x="Variable Name",y="Variable Importance")

#decision tree predictions
c.tscores = predict(c.tree,type='class')
c.scores = predict(c.tree, c.test, type='class')

##Training missclassification rate:
sum(c.tscores!=c.train$Churn)/nrow(c.train)

##Test missclassification rate:
sum(c.scores!=c.test$Churn)/nrow(c.test)


##########
# Class Lab
load("/Users/lhuynh/Documents/NCSU-IAA/Statistics & Data/Data Mining/TeenSNS4.rdata")
#check for NA values
for (i in c(1:ncol(teens4))){
  print(table(teens4[i],exclude=NULL))
}

summary(teens4)
#found 2724 NAs in gender
#imputing missing values to category "none"
teens4$gender = teens4$gender %>% as.character()
teens4$gender = teens4$gender %>% replace_na('missing')
teens4$gender = ifelse(teens4$gender == 'F', 0, ifelse(teens4$gender == 'M', 1, 2))

# histogram of data
for (i in 4:ncol(teens4)){
  hist(teens4[,i])
}

#standardization by making proportion of answers for each row/entry
individCount = rowSums(teens4[,4:ncol(teens4)])
for (i in 1:length(individCount)){if (individCount[i] == 0){individCount[i] = 1}}
teen = teens4
for (i in 4:ncol(teen)){
  for(j in 1:nrow(teen)){
    teen[j,i] = teen[j,i]/individCount[j]
  }
}

set.seed(1234)
fviz_nbclust(teen,kmeans,method = 'silhouette') #returns 2 clusters as optimal
fviz_nbclust(teen,kmeans,method = 'wss', k.max = 5) #exhausting vector/k
fviz_nbclust(teen,kmeans,method = 'gap_stat', k.max = 5, nboot = 50)

clustProp = kmeans(teen, centers = 3, nstart = 50)
clustProp
fviz_cluster(clustProp, data = teen, geom = "point")

NbClust(teen,method="kmeans",min.nc=2,max.nc = 4)
#z score scaling
teen.scale = scale(teens4) 

fviz_nbclust(teen.scale,kmeans,method = 'silhouette') #returns 2 clusters as optimal
fviz_nbclust(teen.scale,kmeans,method = 'wss') #exhausting vector/k
fviz_nbclust(teen.scale,kmeans,method = 'gap_stat')

clustZ = kmeans(teen.scale, centers = 3, nstart = 50) 
clustZ
fviz_cluster(clustZ, data = teen.scale, geom = "point")


#PCA scaling
teen.PCA = prcomp(teens4, center = TRUE, scale = TRUE) 
summary(teen.PCA)

teen.transform = as.data.frame(-teen.PCA$x[,1:3])
fviz_nbclust(teen.transform,kmeans,method = 'wss') #exhausting vector/k
fviz_nbclust(teen.transform,kmeans,method = 'silhouette') #returns 2 clusters as optimal
fviz_nbclust(teen.transform,kmeans,method = 'gap_stat')

clustPCA = kmeans(teen.transform, centers = 2, nstart = 25)
clustPCA
fviz_cluster(clustPCA, data = teen.transform, geom = "point")

teens4$gender = ifelse(teens4$gender == 0, "F", ifelse(teens4$gender == 1, "M", "missing"))

profile.k = cbind(teens4,clustProp$cluster)
all.k = profile.k %>% group_by(clustProp$cluster) %>% summarise_all(mean)
all.k
write_csv(all.k,"/Users/lhuynh/Downloads/TeenMeanSummary.csv")

