###################################
#                                 #
#         Machine Learning:       #
#         Homework Code           #
#                                 #
###################################

# Needed Libraries for Analysis 
library(tidyverse)
library(caret)
library(leaps)
library(glmnet)
library(ggplot2)
library(earth)
library(mgcv)
library(AmesHousing)
library(InformationValue)
library(dplyr)
library(randomForest)
library(xgboost)
library(Ckmeans.1d.dp)
library(pdp)
library(varhandle)
library(imputeMissings)

# Load ins_t dataset for piece-wise linear regression example
ins_t <- read_csv("~/Documents/NCSU-IAA/Statistics & Data/Machine Learning/insurance_t.csv")

binary <- c("DDA","DIRDEP","NSF","SAV","ATM","CD","IRA","MM",
            "SDB","INAREA","INV","CC")
ordinal <- c("MMCRED","CCPURC")
nominal <- c("BRANCH")
continuous <- c("ACCTAGE","DDABAL","DEP","DEPAMT","CHECKS","NSFAMT","PHONE","TELLER",
                "SAVBAL","ATMAMT","POS","POSAMT","CDBAL","IRABAL","INVBAL",
                "MMBAL","CCMBAL","INCOME","LORES","HMVAL","AGE","CRSCORE")

ins_t[binary] =lapply(ins_t[binary],as.factor)
ins_t[ordinal] =lapply(ins_t[ordinal],as.factor)
ins_t[nominal] =lapply(ins_t[nominal],as.factor)

original_variables <- names(insurance_t)

#IMPUTATION
sapply(ins_t,function(x)sum(is.na(x))) #find columns with missing values & create flagged variable if imputed
library(imputeMissings)
ins_t = imputeMissings::impute(ins_t,method="median/mode",flag = TRUE)
#check all NAs removed
sapply(ins_t,function(x)sum(is.na(x)))
                                                                                                  
# EARTH on all variables
mars_model <- earth(INS ~ ., data = ins_t, glm=list(family=binomial))
summary(mars_model)
evimp(mars_model)
#ROC Curve 
training$p_hat <- predict(mars_model, type = "response")
plotROC(training$INS, training$p_hat)

#create ROC curve for MARS
training$p_hat <- predict(mars_model, type = "response")
plotROC(training$INS, training$p_hat)

#GAM model with splines
gam <- mgcv::gam(as.factor(INS) ~ s(ACCTAGE)+as.factor(DDA)+s(DDABAL)
                 + as.factor(DEP) + s(DEPAMT) + s(CHECKS) + as.factor(DIRDEP) + as.factor(NSF) + s(NSFAMT)
                 + s(PHONE)+s(TELLER)+as.factor(SAV)+s(SAVBAL)+as.factor(ATM)+s(ATMAMT)+s(POS)+s(POSAMT)+as.factor(CD)+s(CDBAL)
                 + as.factor(IRA)+s(IRABAL)+as.factor(INV)+s(INVBAL)+as.factor(MM)+s(MMBAL)+as.factor(MMCRED)+as.factor(CC)
                 + s(CCBAL)+as.factor(CCPURC)+as.factor(SDB)
                 + s(INCOME)+s(LORES)+s(HMVAL)+s(AGE)+s(CRSCORE)+as.factor(INAREA)+BRANCH
                 , data = ins_t,family=binomial,select="TRUE")

summary(gam)

#create ROC curve for GAM
training$p_hatGAM <- predict.gam(gam, type = "response")
plotROC(training$INS, training$p_hatGAM)

#random forest model
ins_t$INS <- as.factor(ins_t$INS)
set.seed(626)

rf.insurance <- randomForest(INS ~ ., data = ins_t, ntree = 500, importance = TRUE)
# Plot the change in error across different number of trees 
plot(rf.insurance, main = "Number of Trees Compared to MSE") #posssily 100-150 trees is good enough

#Look at variable importance
varImpPlot(rf.insurance,
           sort = TRUE,
           n.var = 10,
           main = "Top 10 - Variable Importance")
importance(rf.insurance)

# Tune an random forest mtry value
set.seed(626)
ins_t <- as.data.frame(ins_t)
tuneRF(x = ins_t[,-37], y = ins_t[,37], 
       plot = TRUE, ntreeTry = 500, stepFactor = 0.5) #tuned mtry = 7

set.seed(626)
rf.insurance1 <- randomForest(INS ~ ., data = ins_t, ntree = 500, mtry = 7, importance = TRUE)

varImpPlot(rf.insurance1,
           sort = TRUE,
           n.var = 52,
           main = "Order of Variables")
RF.imp <- importance(rf.insurance1, type = 1)
write.csv(RF.imp,"~/Documents/NCSU-IAA/Statistics & Data/Machine Learning/RFimp.csv")

#create ROC curve for random forest
ins_t$INS = unfactor(ins_t$INS)

p_hatRF <- predict(rf.insurance1, type = "prob")[,2]

plotROC(ins_t$INS, p_hatRF) #AUC 0.7944
summary(rf.insurance1)

# Prepare data for XGBoost function - similar to what we did for glmnet
train_x <- model.matrix(INS ~ ., data = ins_t)[, -1]
train_y <- ins_t$INS

# Build XGBoost model
set.seed(626)
xgb.insurance <- xgboost(data = train_x, label = train_y, subsample = 0.5, nrounds = 100, objective = "binary:logistic")

# Tuning an XGBoost nrounds parameter - 8 was lowest!
xgbcv.insurance <- xgb.cv(data = train_x, label = train_y, subsample = 0.5, nrounds = 100, nfold = 10,objective = "binary:logistic")

# Tuning through caret
tune_grid <- expand.grid(
  nrounds = 8,
  eta = c(0.1, 0.15, 0.2, 0.25, 0.3),
  max_depth = c(1:10),
  gamma = c(0),
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = c(0.25, 0.5, 0.75, 1)
)

set.seed(626)
train_y = as.factor(train_y)
xgb.insurance.caret <- train(x = train_x, y = train_y,
                        method = "xgbTree",
                        tuneGrid = tune_grid,
                        trControl = trainControl(method = 'cv', # Using 10-fold cross-validation
                                                 number = 10))

plot(xgb.insurance.caret)
xgb.insurance.caret$bestTune
#    nrounds max_depth  eta gamma colsample_bytree min_child_weight subsample
#      8         5   0.2     0                1                1         1

# Variable importance
train_y = unfactor(train_y)
xgb.insurance1 <- xgboost(data = train_x, label = train_y, subsample = 1, nrounds = 8, eta = 0.2, max_depth = 5,objective = "binary:logistic")

XGB.imp <- xgb.importance(feature_names = colnames(train_x), model = xgb.insurance1)
write.csv(XGB.imp,"~/Documents/NCSU-IAA/Statistics & Data/Machine Learning/XGBimp.csv")

xgb.ggplot.importance(xgb.importance(feature_names = colnames(train_x), model = xgb.insurance1))

#create ROC curve for xgboost
p_hatXGB <- predict(xgb.insurance1,train_x)
plotROC(ins_t$INS, p_hatXGB) #AUC 0.8442



