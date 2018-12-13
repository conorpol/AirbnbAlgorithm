# Random forest 
airdata = read.csv("FinalCleanedAirbnb.csv")
airdata = airdata[,-1]
attach(airdata)

# Removing outliers first from data  & unusable predictor from the data
preds = airdata[,-c(1:7,9,10,12,13,18:33,36,41:43)]
low_outliers = preds[preds$IncomePerMonth< (airdata$price),] # 142 where IncomePerMonth is lower than just one night stay
high_outliers = preds[preds$IncomePerMonth > (airdata$price*30),] # none 
air = preds[preds$IncomePerMonth > (airdata$price),] # air dataset includes the final set with predictors and without outliers
summary(air$IncomePerMonth) # min = $28.80, max = $12533.90

# get a look at the min and max in terms of IncomePerMonth. What do each of these listings look like? 
maxIndex = which.max(air$IncomePerMonth)
minIndex = which.min(air$IncomePerMonth)
extremes = air[c(minIndex,maxIndex),] # includes the min and max in terms of IncomePerMonth seen in the dataset

# separating occupancy rate 
occupancyRateSet = air
air = air[,-9]

# checking data types
lapply(air,class)
which("factor" == lapply(air, class)) # no factors... yet.
which("numeric" == lapply(air, class)) # numeric = bathrooms, square_meter, propetyPrice, IncomePerMonth 
which("integer" == lapply(air, class))

# trying host_listings_count, accommodates, beds, bedrooms as numeric instead of factor
air[,c(1,3,5,6)] = lapply(air[c(1,3,5,6)], as.numeric)
which("numeric" == lapply(air, class))

# change most data types to factor that were integer (and bathrooms)
integerIndex = which("integer" == lapply(air,class))
air[,c(integerIndex)] = lapply(air[c(integerIndex)], factor) # changing integer values to factor and bathrooms(4)


#TODO: segment data based on clusters
# creating clusters based on occupancy, host total listings count
set.seed(20)
occupancyClusterSet = occupancyRateSet[,9]
plot(occupancyRateSet$occupancy_rate)

km.3 = kmeans(occupancyClusterSet, 3) #3 cluster
plot(occupancyRateSet$occupancy_rate, col = (km.3$cluster))
air$cluster = km.3$cluster

# random forest on non outlier datas to predict IncomePerMonth using all predictors available
library(randomForest)
air = air[,-c(7,8,21:27,43)] # remove unusble inputs, bed types, cancellation policy, property price 
forest1=randomForest(IncomePerMonth~., ntree=500, data=air, importance=TRUE, do.trace = 100) 
#print(forest1)
#plot(forest1) # show the improvement in error with the number of tree

imp1 = data.frame(importance(forest1, type = 1)) #1 = mean decrease in accuracy
imp1$VarName = rownames(imp1)
imp1 = imp1[order(-imp1$X.IncMSE),]
head(imp1,20) # top 10 important vars based on imp1 
plot(imp1$X.IncMSE)

least = imp1[order(imp1$X.IncMSE),] # least important predictors
head(least,10)

imp2 = data.frame(importance(forest1, type = 2)) #2 = mean decrease in node impurity
imp2$VarName = rownames(imp2)
imp2 = imp2[order(-imp2$IncNodePurity),]
head(imp2,10) # in this case, I think it makes more sense to use the type=1 as we are predicting not classifying

varImpPlot(forest1) #visualz

# doing simple decision tree with most impotant variables as discovered above
library(tree)
library(rpart)
library(rpart.plot)

#TODO: Overfit the tree with a low cp then trim it back
importantVars = air[,c(2:9,19,22,24,26,28,29,31,32,33,35,34)]
attach(importantVars)
importantVars$cluster = as.factor(importantVars$cluster)

# changing Property Types to a single factor variables
importantVars$propertyType = ifelse(importantVars$EntirePlace == 1, "Entire Apartment", 
                                    ifelse(importantVars$PrivateRoom == 1, "Private Room",
                                           ifelse(importantVars$SharedRoom == 1, "Shared Room","ERROR")
                                    )
)
importantVars$propertyType = as.factor(importantVars$propertyType)
table(importantVars$propertyType)
importantVars = importantVars[,-c(6:8)]

# splitting data into test and train 
smp_size <- floor(0.75 * nrow(air))
## set the seed to make partition reproducible
set.seed(123)
train_indTree <- sample(seq_len(nrow(importantVars)), size = smp_size)

trainTree <- importantVars[train_indTree, ]
testTree <- importantVars[-train_indTree, ]

# tree 
tree=rpart(IncomePerMonth~., data = trainTree,control=rpart.control(cp=0.0001))
# rpart.plot(tree) # shows overfittedness - takes a long time to plot

# plot the out of sample performance 
plotcp(tree)

# find best value of cp for out of sample performance
bestCP = tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"] 
minMSE = min(tree$cptable[,"xerror"]) 
bestCP
minMSE

# optimal tree
bestTree=rpart(IncomePerMonth~.,data=trainTree,control=rpart.control(cp=bestCP))
rpart.plot(bestTree) # plot of the best tree 
# short tree
shortTree=rpart(IncomePerMonth~.,data=trainTree,control=rpart.control(cp=0.01))
rpart.plot(shortTree)

# calculating MAE for the decision tree
treePredictions = data.frame(predict(bestTree,testTree))
treePredictions$Actual = testTree$IncomePerMonth
tree.MAE = mean(abs(treePredictions$Actual - treePredictions$predict.bestTree..testTree.))
tree.MAE  # off by the MAE on average

# random forest with our actual inputs as predictors 
clusterInputs = importantVars
table(clusterInputs$cluster) # show clusters in each

# Splitting data into train and test set 
smp_size <- floor(0.75 * nrow(air))

## set the seed to make partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(air)), size = smp_size)

train <- clusterInputs[train_ind, ]
test <- clusterInputs[-train_ind, ]

clusterForest = randomForest(IncomePerMonth~., ntree=500, data=train, importance=TRUE, do.trace = 100)
print(clusterForest)

# variable importance
imp1.1 = data.frame(importance(clusterForest, type = 1)) #1 = mean decrease in accuracy
imp1.1$VarName = rownames(imp1.1)
imp1.1 = imp1.1[order(-imp1.1$X.IncMSE),]
head(imp1.1,20) # top 10 important vars based on imp1 
plot(imp1.1$X.IncMSE)

test$pred = predict(clusterForest, test)
forest.MAE = mean(abs(test$IncomePerMonth - test$pred))
forest.MAE  # off by the MAE on average

# forest with all data to use to predict new observations
attach(importantVars)
fullForest = randomForest(IncomePerMonth~., ntree=500, data=importantVars, importance=TRUE, do.trace = 100)
print(fullForest)

importantVars1 = importantVars
importantVars1$IncomePerMonth = (importantVars$IncomePerMonth / 1000)

fullForest1 = randomForest(IncomePerMonth~., ntree=500, data=importantVars1, importance=TRUE, do.trace = 100)
print(fullForest1)
summary(importantVars1$IncomePerMonth)

# Test Prediction
tester = data.frame(zipcode = 75001, accommodates = 4, bathrooms = 2, bedrooms = 2, 
                    beds = 2, hasTV = 0, hasShampoo = 1, hasAC = 1, hasDesk = 1, hasIron = 1, 
                    hasHairDryer = 0, hasKitchen = 1, isFamilyFriendly = 1,square_meter = 200, 
                    cluster = 1, propertyType = "Entire Apartment")

tester$zipcode = as.factor(tester$zipcode)
tester$hasTV = as.factor(tester$hasTV)
tester$hasShampoo = as.factor(tester$hasShampoo)
tester$hasAC = as.factor(tester$hasAC)
tester$hasDesk = as.factor(tester$hasDesk)
tester$hasIron = as.factor(tester$hasIron)
tester$hasHairDryer = as.factor(tester$hasHairDryer)
tester$hasKitchen = as.factor(tester$hasKitchen)
tester$isFamilyFriendly = as.factor(tester$isFamilyFriendly)
tester$cluster = as.factor(tester$cluster)
tester$propertyType = as.factor(tester$propertyType)

# fixing factor levels
tester$zipcode <- factor(tester$zipcode, levels = levels(importantVars$zipcode))
tester$hasTV <- factor(tester$hasTV, levels = levels(importantVars$hasTV))
tester$hasShampoo <- factor(tester$hasShampoo, levels = levels(importantVars$hasShampoo))
tester$hasAC <- factor(tester$hasAC, levels = levels(importantVars$hasAC))
tester$hasDesk <- factor(tester$hasDesk, levels = levels(importantVars$hasDesk))
tester$hasIron <- factor(tester$hasIron, levels = levels(importantVars$hasIron))
tester$hasHairDryer <- factor(tester$hasHairDryer, levels = levels(importantVars$hasHairDryer))
tester$hasKitchen <- factor(tester$hasKitchen, levels = levels(importantVars$hasKitchen))
tester$isFamilyFriendly <- factor(tester$isFamilyFriendly, levels = levels(importantVars$isFamilyFriendly))
tester$cluster <- factor(tester$cluster, levels = levels(importantVars$cluster))
tester$propertyType <- factor(tester$propertyType, levels = levels(importantVars$propertyType))

# predict
predict(fullForest,tester)

# getting variable ranges, etc. for R Shiny App user interface
summary(importantVars$accommodates)
table(importantVars$bedrooms)
summary(importantVars$square_meter)
summary(importantVars$bathrooms)
summary(importantVars$beds)
summary(importantVars$square_meter)

#TODO: confidence intervals (+- error)
library(stargazer)
stargazer(imp1, type = "html", out = "./impPlot.html")
