###################################
#                                 #
#         Machine Learning:       #
#       Homework 3 Code           #
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
library(nnet)
library(NeuralNetTools)
library(reshape2)
library(e1071)
library(pdp)
library(ALEPlot)
library(lime)
library(iml)
library(patchwork)

# Load ins_t dataset for piece-wise linear regression example
ins_t <- read_csv("~/Documents/NCSU-IAA/Statistics & Data/Machine Learning/insurance_t.csv")

binary <- c("DDA","DIRDEP","NSF","SAV","ATM","CD","IRA","MM",
            "SDB","INAREA","INV","CC")
ordinal <- c("MMCRED","CCPURC")
nominal <- c("BRANCH")
continuous <- c("ACCTAGE","DDABAL","DEP","DEPAMT","CHECKS","NSFAMT","PHONE","TELLER",
                "SAVBAL","ATMAMT","POS","POSAMT","CDBAL","IRABAL","INVBAL",
                "MMBAL","CCBAL","INCOME","LORES","HMVAL","AGE","CRSCORE")

ins_t[binary] =lapply(ins_t[binary],as.factor)
ins_t[ordinal] =lapply(ins_t[ordinal],as.factor)
ins_t[nominal] =lapply(ins_t[nominal],as.factor)

original_variables <- names(insurance_t)

#IMPUTATION
sapply(ins_t,function(x)sum(is.na(x))) #find columns with missing values & create flagged variable if imputed
ins_t = imputeMissings::impute(ins_t,method="median/mode",flag = TRUE)
#check all NAs removed
sapply(ins_t,function(x)sum(is.na(x)))
# Standardizing Continuous Variables
ins_t_scale <- ins_t %>%
  mutate(s_ACCTAGE = scale(ACCTAGE),
         s_DDABAL = scale(DDABAL),
         s_DEP = scale(DEP),
         s_DEPAMT = scale(DEPAMT),
         s_CHECKS = scale(CHECKS),
         s_NSFAMT = scale(NSFAMT),
         s_PHONE = scale(PHONE),
         s_TELLER = scale(TELLER),
         s_SAVBAL = scale(SAVBAL),
         s_ATMAMT = scale(ATMAMT),
         s_POS = scale(POS),
         s_POSAMT = scale(POSAMT),
         s_CDBAL = scale(CDBAL),
         s_IRABAL = scale(IRABAL),
         s_INVBAL = scale(INVBAL),
         s_MMBAL = scale(MMBAL),
         s_CCBAL = scale(CCBAL),
         s_INCOME = scale(INCOME),
         s_LORES = scale(LORES),
         s_HMVAL = scale(HMVAL),
         s_AGE = scale(AGE),
         s_CRSCORE = scale(CRSCORE))

#remove continuous variables not scaled
training <- ins_t_scale[ , ! names(ins_t_scale) %in% continuous]
training$INS <- as.factor(training$INS)
# Neural Network model
set.seed(626)
nn.ins <- nnet(INS ~ ., data = training, size = 5, linout = FALSE)

# Plot the network
plotnet(nn.ins)

# Optimize Number of Hidden Nodes and Regularization (decay option)
tune_grid <- expand.grid(
  .size = c(3, 4, 5, 6, 7),
  .decay = c(0, 0.5, 1)
)

set.seed(626)
nn.ins.caret <- train(INS ~ .
                       , data = training,
                       method = "nnet", # Neural network using the nnet package
                       tuneGrid = tune_grid,
                       trControl = trainControl(method = 'cv', # Using 10-fold cross-validation
                                                number = 10),
                       trace = FALSE, linout = FALSE)

nn.ins.caret$bestTune #size = 3 decay = 1

nn.ins <- nnet(INS ~.
                , data = training, size = 3, decay = 1, linout = FALSE)

plotnet(nn.ins)

# Hinton Diagram
nn_weights <- matrix(data = nn.ins$wts[1:225], ncol = 3, nrow = 75)
rownames(nn_weights) <- c("bias", nn.ins$coefnames)
colnames(nn_weights) <- c("h1", "h2", "h3")

ggplot(melt(nn_weights), aes(x=Var1, y=Var2, size=abs(value), color=as.factor(sign(value)))) +
  geom_point(shape = 15) +
  scale_size_area(max_size = 40) +
  labs(x = "", y = "", title = "Hinton Diagram of NN Weights") +
  theme_bw()

#create ROC curve for neural network with training data
p_hatNN <- predict(nn.ins, type = "raw")

plotROC(training$INS, p_hatNN) #AUC 0.809

#test on validation data
ins_v <- read_csv("~/Documents/NCSU-IAA/Statistics & Data/Machine Learning/insurance_v.csv")

ins_v[binary] =lapply(ins_v[binary],as.factor)
ins_v[ordinal] =lapply(ins_v[ordinal],as.factor)
ins_v[nominal] =lapply(ins_v[nominal],as.factor)

#IMPUTATION
sapply(ins_v,function(x)sum(is.na(x))) #find columns with missing values & create flagged variable if imputed
ins_v = imputeMissings::impute(ins_v,method="median/mode",flag = TRUE)
# Standardizing Continuous Variables
validation <- ins_v %>%
  mutate(s_ACCTAGE = scale(ACCTAGE),
         s_DDABAL = scale(DDABAL),
         s_DEP = scale(DEP),
         s_DEPAMT = scale(DEPAMT),
         s_CHECKS = scale(CHECKS),
         s_NSFAMT = scale(NSFAMT),
         s_PHONE = scale(PHONE),
         s_TELLER = scale(TELLER),
         s_SAVBAL = scale(SAVBAL),
         s_ATMAMT = scale(ATMAMT),
         s_POS = scale(POS),
         s_POSAMT = scale(POSAMT),
         s_CDBAL = scale(CDBAL),
         s_IRABAL = scale(IRABAL),
         s_INVBAL = scale(INVBAL),
         s_MMBAL = scale(MMBAL),
         s_CCBAL = scale(CCBAL),
         s_INCOME = scale(INCOME),
         s_LORES = scale(LORES),
         s_HMVAL = scale(HMVAL),
         s_AGE = scale(AGE),
         s_CRSCORE = scale(CRSCORE))

validation <- validation[ , ! names(validation) %in% continuous]
validation$INS <- as.factor(validation$INS)
#ROC for validation data
p_hatNN1 <- predict(nn.ins,newdata = validation, type = "raw")

plotROC(validation$INS, p_hatNN1) #AUC 0.7904

# Naive Bayes model
ins_t$INS <- as.factor(ins_t$INS)
set.seed(626)
nb.ins <- naiveBayes(INS ~ ., data = ins_t, laplace = 0, usekernel = TRUE)

# Optimize laplace and kernel - CARET ONLY ABLE TO TUNE CLASSIFICATION PROBLEMS FOR NAIVE BAYES
tune_grid <- expand.grid(
  usekernel = c(TRUE, FALSE),
  fL = c(0, 0.5, 1),
  adjust = c(0, 0.5, 1)
)

set.seed(626)
nb.ins.caret <- caret::train(INS ~ ., data = ins_t,
                       method = "nb", 
                       tuneGrid = tune_grid,
                       trControl = trainControl(method = 'cv', # Using 10-fold cross-validation
                                                number = 10))

nb.ins.caret$bestTune
nb.ins <- naiveBayes(INS ~ ., data = ins_t, laplace = 0, usekernel = TRUE,adjust=0.5)

#create ROC curve for naive bayes
p_hatNB <- predict(nb.ins, ins_t,type = "raw")

plotROC(ins_t$INS, p_hatNB[,2]) #AUC 0.6365

#create ROC curve for naive bayes validation
p_hatNB1 <- predict(nb.ins, ins_v,type = "raw")

plotROC(ins_v$INS, p_hatNB1[,2]) #AUC 0.6243

#create ROC for models created in Phase 1 & 2
ins_v_adjust <- ins_v[-c(295,355),] #removed observations 295 & 355 from validation
#training data did not include DEP 12 & 17
ins_t_adjust = subset(ins_t,MMCRED !=5) #removed 1 observation from training as validation did not have MMCRED level 5
ins_t_adjust$MMCRED = droplevels(ins_t_adjust$MMCRED) #drop factor level

#ROC Curve for MARS
mars_model <- earth(INS ~ ., data = ins_t_adjust, glm=list(family=binomial(link = logit)))
summary(mars_model)
p_hatMARS1 <- predict(mars_model,ins_v, type = "response")
plotROC(ins_v$INS, p_hatMARS1) #AUC 0.7814

#create ROC curve for GAM
gam <- mgcv::gam(as.factor(INS) ~ s(ACCTAGE)+as.factor(DDA)+s(DDABAL)
                 + as.factor(DEP) + s(DEPAMT) + s(CHECKS) + as.factor(DIRDEP) + as.factor(NSF) + s(NSFAMT)
                 + s(PHONE)+s(TELLER)+as.factor(SAV)+s(SAVBAL)+as.factor(ATM)+s(ATMAMT)+s(POS)+s(POSAMT)+as.factor(CD)+s(CDBAL)
                 + as.factor(IRA)+s(IRABAL)+as.factor(INV)+s(INVBAL)+as.factor(MM)+s(MMBAL)+as.factor(MMCRED)+as.factor(CC)
                 + s(CCBAL)+as.factor(CCPURC)+as.factor(SDB)
                 + s(INCOME)+s(LORES)+s(HMVAL)+s(AGE)+s(CRSCORE)+as.factor(INAREA)+BRANCH
                 , data = ins_t_adjust,family=binomial(link=logit),select="TRUE")

summary(gam)
p_hatGAM1 <- predict(gam,newdata = ins_v_adjust, type = "response")
plotROC(ins_v_adjust$INS, p_hatGAM1) #AUC 0.7811

#create ROC curve for random forest
rf.insurance2 <- randomForest(INS ~ ., data = ins_t_adjust, ntree = 500, mtry = 7, importance = TRUE)

ins_v_adjust$INS = as.factor(ins_v_adjust$INS)
ins_v_adjust = as.data.frame(ins_v_adjust)

p_hatRF1 <- predict(rf.insurance2,newdata=ins_v_adjust, type = "prob")[,2]

plotROC(ins_v_adjust$INS, p_hatRF1) #AUC 0.7990 BEST ROC VALUE - FINAL MODEL

#create ROC curve for xgboost
train_x_adjust <- model.matrix(INS ~ ., data = ins_t_adjust)[, -1]
train_y_adjust <- ins_t_adjust$INS
train_y_adjust <- unfactor(train_y_adjust)
xgb.insurance2 <- xgboost(data = train_x_adjust, label = train_y_adjust, subsample = 1, nrounds = 8, eta = 0.2, max_depth = 5,objective = "binary:logistic")

train_x1 <- model.matrix(INS ~ ., data = ins_v_adjust)[, -1]

p_hatXGB1 <- predict(xgb.insurance2,train_x1)
plotROC(ins_v_adjust$INS, p_hatXGB1) #AUC 0.7869

#final model extra interpretation
#Look at variable importance
varImpPlot(rf.insurance2,
           sort = TRUE,
           n.var = 10,
           main = "Top 10 - Variable Importance")
RFvar.imp = importance(rf.insurance2, type=1)

# Permutation Importance for Linear Regression 
# (used validation data for PDP & training for Shapley)
ins_v_adjust$INS = unfactor(ins_v_adjust$INS)
lm.ins <- lm(INS ~ ., data = ins_v_adjust)

summary(lm.ins)

linear_pred <- Predictor$new(lm.ins, data = ins_v_adjust[,-37], 
                             y = ins_v_adjust$INS, type = "response")
plot(FeatureImp$new(linear_pred, loss = "mse"))

# PDP for Random Forest on ACCTAGE
ins_v_adjust = ins_v_adjust[,-53]

forest_pred <- Predictor$new(rf.insurance2, data = ins_v_adjust[,-37], 
                                      y = ins_v_adjust$INS, type = "response")
pd_plot <- FeatureEffects$new(forest_pred, method = "pdp")
pd_plot$plot(c("ACCTAGE"))
pd_plot$plot()

# Shapley Values for Random Forest observation 732 on training
ins_t_adjust = ins_t_adjust[,-53]
forest_pred_training <- Predictor$new(rf.insurance2, data = ins_t_adjust[,-37], 
                                      y = ins_t_adjust$INS, type = "response")

shap <- Shapley$new(forest_pred_training, x.interest = ins_t_adjust[point,-37])
shap$plot()
predict(rf.insurance2, type = "prob")[732,2] #random forest prediction on observation 732

