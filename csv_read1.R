###Start -----
rm(list = ls())
if(!is.null(dev.list())) dev.off()
shell("cls")
setwd("c:/users/asus/documents")
load(file = "data_csv_vars.Rdata")
###Reading Data from CSV -----
data_csv = read.csv("COPPER_1440_logfile.csv" , header = TRUE)
dim(data_csv)
class(data_csv)
head(data_csv , 3)
tail(data_csv , 3)
sum(is.na(data_csv))
sum(is.infinite(data_csv))
View(data_csv)

#Transform data to factor
data_csv$Day_of_Week <- factor(data_csv$Day_of_Week)
data_csv$Trend_Factor <- factor(data_csv$Trend_Factor)

#features formula and vector
x_formula = as.formula(Trend_Factor ~ ATR + MA_Fast + MA_Slow + OsMA + 
                       RSI_Fast + RSI_Slow + Stochastic + 
                       ADX_Main + DMI_Pos + DMI_Neg + CCI +
                       Bulls_Power + Bears_Power + Momentum +
                       WPR + RVI_Main + RVI_Signal + Force_Index + MFI +
                       ReturnLag1 + ReturnLag2 + ReturnLag3 +
                       ReturnLag4 + ReturnLag5)
x_vector = c("ATR","MA_Fast","MA_Slow","OsMA",
             "RSI_Fast","RSI_Slow",
             "Stochastic","ADX_Main","DMI_Pos","DMI_Neg",
             "CCI","Bulls_Power","Bears_Power",
             "Momentum","WPR","RVI_Main","RVI_Signal",
             "Force_Index","MFI" ,"ReturnLag1" , "ReturnLag2" , 
             "ReturnLag3" , "ReturnLag4" , "ReturnLag5")
###Create Train & Test Data -----
train <- data_csv[data_csv$Year %in% c(2009:2018),]
head(train , 3)
tail(train , 3)
dim(train)
sum(is.na(train))
View(train)


test <- data_csv[data_csv$Year %in% c(2019:2020),]
head(test , 3)
tail(test , 3)
dim(test)
sum(is.na(test))
View(test)

#save basic variables in R (data , train , test , formula , . . . )
save.image(file = "data_csv_vars.Rdata")
load(file = "data_csv_vars.Rdata")

###Build Prediction Model in R
###Some Data Plot -----
#Plot Data Close Points
library("ggplot2")
ggplot(data = data_csv, aes(x = c(1:nrow(data_csv)), y = Close)) +
  geom_line() +
  xlab("Time") +
  scale_y_continuous(name = "Close Point of Data_csv")

#Data Close Points Statistics
summary(data_csv$Close)
hist(data_csv$Close, breaks = 100 , main = "Close Points")
mean(data_csv$Close)
sd(data_csv$Close)

#Plot Data Return
ggplot(data = data_csv, aes(x = c(1:nrow(data_csv)), y = Return)) +
  geom_line() +
  xlab("Time") +
  scale_y_continuous(name = "Return of Data_csv")

#Data Return Statistics
summary(data_csv$Return)
hist(data_csv$Return, breaks = 100 , main = "Candle Return")
mean(data_csv$Return)
sd(data_csv$Return)

#Plot Data RSI
ggplot(data = data_csv, aes(x = c(1:nrow(data_csv)), y = RSI_Slow)) +
  geom_line() +
  xlab("Time") +
  scale_y_continuous(name = "RSI of Data_csv")

#Data RSI statistics
summary(data_csv$RSI_Slow)
hist(data_csv$RSI_Slow, breaks = 100 , main = "RSI")
mean(data_csv$RSI_Slow)
sd(data_csv$RSI_Slow)

#Descriptive statistics by Skimr
install.packages("skimr")
library("skimr")
skimmed <- skim(data_csv)
skimmed
View(skimmed)

###Feature Selection -----
##Correlation Check & Redundant Features
library("corrplot")
correlations = cor(data_csv[,c("ATR","MA_Fast","MA_Slow","OsMA",
                               "RSI_Fast","RSI_Slow",
                               "Stochastic","ADX_Main","DMI_Pos","DMI_Neg",
                               "CCI","Bulls_Power","Bears_Power",
                               "Momentum","WPR","RVI_Main","RVI_Signal",
                               "Force_Index","MFI")])
print(head(correlations) , 3)
corrplot(correlations , method = "circle")

##Correlation Check for x & y
correlations = cor(data_csv[,c("Return",
                               "ATR","MA_Fast","MA_Slow","OsMA",
                               "RSI_Fast","RSI_Slow",
                               "Stochastic","ADX_Main","DMI_Pos","DMI_Neg",
                               "CCI","Bulls_Power","Bears_Power",
                               "Momentum","WPR","RVI_Main","RVI_Signal",
                               "Force_Index","MFI")])
print(head(correlations) , 3)
corrplot(correlations , method = "circle")

##Correlation Check for x & y
correlations = cor(data_csv[,c("Return",
                               "ATR","MA_Fast","MA_Slow","OsMA",
                               "RSI_Fast","RSI_Slow",
                               "Stochastic","ADX_Main","DMI_Pos","DMI_Neg",
                               "CCI","Bulls_Power","Bears_Power",
                               "Momentum","WPR","RVI_Main","RVI_Signal",
                               "Force_Index","MFI","ReturnLag1","ReturnLag2",
                               "ReturnLag3","ReturnLag4","ReturnLag5")])
print(head(correlations) , 3)
corrplot(correlations , method = "circle")

##Correlation Check for x & y
correlations = cor(data_csv[,c("Close",
                               "ATR","MA_Fast","MA_Slow","OsMA",
                               "RSI_Fast","RSI_Slow",
                               "Stochastic","ADX_Main","DMI_Pos","DMI_Neg",
                               "CCI","Bulls_Power","Bears_Power",
                               "Momentum","WPR","RVI_Main","RVI_Signal",
                               "Force_Index","MFI",
                               "Return","ReturnLag1","ReturnLag2",
                               "ReturnLag3","ReturnLag4","ReturnLag5")])
print(head(correlations) , 3)
corrplot(correlations , method = "circle")

##1. Rank Features By Boruta
#https://www.machinelearningplus.com/machine-learning/feature-selection/
install.packages('Boruta')
library(Boruta)

# Perform Boruta search
boruta_output <- Boruta(Trend_Factor ~ ATR + MA_Fast + MA_Slow + OsMA + 
                        RSI_Fast + RSI_Slow +
                        Stochastic + ADX_Main + DMI_Pos + DMI_Neg + 
                        CCI + Bulls_Power + Bears_Power +
                        Momentum + WPR + RVI_Main + RVI_Signal + Force_Index + MFI +
                        ReturnLag1 + ReturnLag2 + ReturnLag3 + ReturnLag4 + ReturnLag5, 
                        data=na.omit(data_csv), 
                        doTrace=0 , 
                        pValue = 0.05,
                        maxRuns = 20)
names(boruta_output)

# Get significant variables including tentatives
boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif) 

# Do a tentative rough fix
roughFixMod <- TentativeRoughFix(boruta_output)
boruta_signif <- getSelectedAttributes(roughFixMod)
print(boruta_signif)

# Variable Importance Scores
imps <- attStats(roughFixMod)
imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]

head(imps2[order(-imps2$meanImp), ])  # descending sort

# Plot variable importance
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")

##2.Variable Importance from Machine Learning Algorithms
# Train an rpart model and compute variable importance.
library(caret)
set.seed(100)
model_rpart <- train(Trend_Factor ~ ATR + MA_Fast + MA_Slow + OsMA + 
                  RSI_Fast + RSI_Slow +
                  Stochastic + ADX_Main + DMI_Pos + DMI_Neg + 
                  CCI + Bulls_Power + Bears_Power +
                  Momentum + WPR + RVI_Main + RVI_Signal + Force_Index + MFI +
                  ReturnLag1 + ReturnLag2 + ReturnLag3 + ReturnLag4 + ReturnLag5,
                  data = na.omit(data_csv), 
                  method = "rpart")
rpartImp <- varImp(model_rpart)
print(model_rpart)

# Train an RRF (Regulized Random Forrest) model and compute variable importance.
install.packages("RRF")
library("RRF")
set.seed(100)
model_rrf <- train(Trend_Factor ~ ATR + MA_Fast + MA_Slow + OsMA + 
                RSI_Fast + RSI_Slow +
                Stochastic + ADX_Main + DMI_Pos + DMI_Neg + 
                CCI + Bulls_Power + Bears_Power +
                Momentum + WPR + RVI_Main + RVI_Signal + Force_Index + MFI +
                ReturnLag1 + ReturnLag2 + ReturnLag3 + ReturnLag4 + ReturnLag5,
                data = data_csv, 
                method = "RRF")

# Some of the other algorithms available in train() 
# that you can use to compute varImp are the following:
# ada, AdaBag, AdaBoost.M1, adaboost, bagEarth, bagEarthGCV, 
# bagFDA, bagFDAGCV, bartMachine, blasso, BstLm, bstSm, C5.0, 
# C5.0Cost, C5.0Rules, C5.0Tree, cforest, chaid, ctree, ctree2, 
# cubist, deepboost, earth, enet, evtree, extraTrees, fda, 
# gamboost, gbm_h2o, gbm, gcvEarth, glmnet_h2o, glmnet, 
# glmStepAIC, J48, JRip, lars, lars2, lasso, LMT, LogitBoost, 
# M5, M5Rules, msaenet, nodeHarvest, OneR, ordinalNet, ORFlog, 
# ORFpls, ORFridge, ORFsvm, pam, parRF, PART, penalized, PenalizedLDA, 
# qrf, ranger, Rborist, relaxo, rf, rFerns, rfRules, rotationForest, 
# rotationForestCp, rpart, rpart1SE, rpart2, rpartCost, rpartScore, 
# rqlasso, rqnc, RRF, RRFglobal, sdwd, smda, sparseLDA, spikeslab, 
# wsrf, xgbLinear, xgbTree.

rrfImp <- varImp(rrfMod, scale = F)
rrfImp
plot(rrfImp, top = 20, main='Variable Importance')

##3. Lasso Regression
# Least Absolute Shrinkage and Selection Operator (LASSO) regression 
install.packages("glmnet")
library(glmnet)

x = as.matrix(data_csv[,c("ATR","MA_Fast","MA_Slow","OsMA",
                          "RSI_Fast","RSI_Slow",
                          "Stochastic","ADX_Main","DMI_Pos","DMI_Neg",
                          "CCI","Bulls_Power","Bears_Power",
                          "Momentum","WPR","RVI_Main","RVI_Signal",
                          "Force_Index","MFI")])
y = as.double(data_csv[,c("Trend_Factor")])

# Fit the LASSO model (Lasso: Alpha = 1)
set.seed(100)
cv.lasso <- cv.glmnet(x, 
                      y, 
                      family='binomial', 
                      alpha=1, 
                      parallel=TRUE, 
                      standardize=TRUE, 
                      type.measure='auc')

# Results
plot(cv.lasso)

cv.lasso$lambda.min
# plot(cv.lasso$glmnet.fit, xvar="lambda", label=TRUE)
cat('Min Lambda: ', cv.lasso$lambda.min, '\n 1Sd Lambda: ', cv.lasso$lambda.1se)
df_coef <- round(as.matrix(coef(cv.lasso, s=cv.lasso$lambda.min)), 2)

# See all contributing variables
df_coef[df_coef[, 1] != 0, ]

###Classification with the KNN in R -----
load(file = "data_csv_vars.Rdata")
library("class")
#the normalization function is created
nor <-function(x) { (x -min(x))/(max(x)-min(x))   }
#Normalizing Features
train_nor = as.data.frame(lapply(train[,c("MA_Fast","MA_Slow","RSI_Fast","RSI_Slow",
                                       "Stochastic","ADX_Main","DMI_Pos","DMI_Neg",
                                       "CCI","ReturnLag1","ReturnLag2","ReturnLag3",
                                       "ReturnLag4","ReturnLag5")], nor))
test_nor = as.data.frame(lapply(test[,c("MA_Fast","MA_Slow","RSI_Fast","RSI_Slow",
                                        "Stochastic","ADX_Main","DMI_Pos","DMI_Neg",
                                        "CCI","ReturnLag1","ReturnLag2","ReturnLag3",
                                        "ReturnLag4","ReturnLag5")], nor))
model_knn <- knn(train = train_nor,
                 test = test_nor,
                 cl = train$Trend_Factor,
                 k = 13,
                 prob = FALSE)
View(model_knn)
test$pred_knn <- model_knn
#create confusion matrix
table(test$pred_knn,test$Trend_Factor)
mean(test$pred_knn == test$Trend_Factor) * 100

###Classification with the Logistic Regression in R -----
load(file = "data_csv_vars.Rdata")
library("caret")
modelLookup("glm")

model_logreg1 <- glm(formula = x_formula ,
                 family = "binomial", 
                 data = train)

summary(model_logreg1) 
coefficients(model_logreg1)

#Prediction on train data
train$probs_logreg <- predict(model_logreg1, train, type = "response")
head(train)

train$pred_logreg <- ifelse(train$probs_logreg >= 0.5, 1, 0)
mean(train$pred_logreg == train$Trend_Factor) * 100

#confusion matrix
table(actual = train$Trend_Factor, prediction = train$pred_logreg)
sum(train$Trend_Factor == 1)
sum(train$Trend_Factor == 0)

#Prediction on test
test$probs_logreg <- predict(model_logreg1, test, type = "response")
head(test)
test$pred_logreg <- ifelse(test$probs_logreg >= 0.5, 1, 0)
mean(test$pred_logreg == test$Trend_Factor) * 100 
View(test)

#confusion matrix
table(actual = test$Trend_Factor, prediction = test$pred_logreg)

#Using "caret" Library for Evaluating Classification Model
#https://www.machinelearningplus.com/machine-learning/evaluation-metrics-classification-models-r/
library("caret")
caret::confusionMatrix(as.factor(test$pred_logreg), 
                       test$Trend_Factor, 
                       positive="1", 
                       mode="everything")

###Classification with the LDA in R -----
library("MASS")
model_lda <- MASS::lda(formula = x_formula ,
                       data = train,
                       method = "moment")

model_lda

#Prediction on test
predict(model_lda, test)
test$pred_lda <- predict(model_lda, test)$class
head(test)
mean(test$pred_lda == test$Trend_Factor) * 100

#confusion matrix
confm_lda <- table(actual = test$Trend_Factor, prediction = test$pred_lda)
confm_lda

###Classification with the SVM in R -----
library("e1071")
set.seed(1234)
model_svm <- e1071::tune(method = "svm", 
                         train.x = Trend_Factor ~ RSI_Fast + RSI_Slow + MA_Fast + MA_Slow + 
                                   Stochastic + ADX_Main + DMI_Pos + DMI_Neg + CCI +
                                   ReturnLag1 + ReturnLag2 + ReturnLag3 + ReturnLag4 + ReturnLag5 , 
                         train.y = NULL,
                         data = train, 
                         kernel = "polynomial",
                         ranges = list(degree = c(2, 3, 4, 5, 10)))

summary(model_svm)
model_svm$best.performance
model_svm <- model_svm$best.model

#Prediction on test
test$pred_svm <- predict(model_svm, test)
head(test)
mean(test$pred_svm == test$Trend_Factor) * 100

#confusion matrix
confm_svm <- table(actual = test$Trend_Factor, prediction = test$pred_svm)
confm_svm

###Classification with the Naive Bayes in R -----
library(e1071)
train_x_nb = train[,c("MA_Fast","MA_Slow","RSI_Fast","RSI_Slow",
                     "Stochastic","ADX_Main","DMI_Pos","DMI_Neg",
                     "CCI","ReturnLag1","ReturnLag2","ReturnLag3",
                     "ReturnLag4","ReturnLag5")]
train_y_nb = train[,c("Trend_Factor")]

model_nb = naiveBayes(x = train_x_nb,
                      y = train_y_nb)
test$pred_nb = predict(model_nb,
                       test[,c("MA_Fast","MA_Slow","RSI_Fast","RSI_Slow",
                               "Stochastic","ADX_Main","DMI_Pos","DMI_Neg",
                               "CCI","ReturnLag1","ReturnLag2","ReturnLag3",
                               "ReturnLag4","ReturnLag5")])
mean(test$pred_nb == test$Trend_Factor) * 100 

#confusion matrix
confm_rf <- table(actual = test$Trend_Factor, prediction = test$pred_nb)
confm_rf

###Classification with the Random Forrest in R -----
library("randomForest")
set.seed(1234)
model_rf <- randomForest::randomForest(Trend_Factor ~ RSI_Fast + RSI_Slow + MA_Fast + MA_Slow + 
                                         Stochastic + ADX_Main + DMI_Pos + DMI_Neg + CCI +
                                         ReturnLag1 + ReturnLag2 + ReturnLag3 + ReturnLag4 + ReturnLag5 ,
                                       data = train, mtry = 5, ntree = 1000)

#Prediction on test
test$pred_rf <- predict(model_rf, test)
head(test)
mean(test$pred_rf == test$Trend_Factor) * 100 

#confusion matrix
confm_rf <- table(actual = test$Trend_Factor, prediction = test$pred_rf)
confm_rf

###Classification with the Gradient Boosting GBM in R -----
install.packages("gbm")
library("gbm")

train_gbm <- train[,c("Trend_Factor",
                      "RSI_Fast","RSI_Slow","MA_Fast","MA_Slow",
                      "Stochastic","ADX_Main","DMI_Pos","DMI_Neg","CCI",
                      "ReturnLag1","ReturnLag2","ReturnLag3",
                      "ReturnLag4","ReturnLag5")]

View(train_gbm)
model_gbm = gbm(Trend_Factor ~.,
              data = train_gbm,
              distribution = "huberized", #multinomial , bernoulli , adaboost
              cv.folds = 10,
              shrinkage = .01,
              n.minobsinnode = 10,
              n.trees = 200)

pred_gbm = predict.gbm(object = model_gbm,
                   newdata = test,
                   n.trees = 200,
                   type = "response")
View(pred_gbm)

test$pred_gbm = colnames(pred_gbm)[apply(pred_gbm, 1, which.max)]
mean(test$pred_gbm == test$Trend_Factor) * 100

###Classification with the Adabag Boosting in R -----
install.packages("adabag")
library(adabag)

model_adaboost = boosting(formula = Trend_Factor ~ RSI_Fast + RSI_Slow + 
                                    MA_Fast + MA_Slow + Stochastic + ADX_Main + 
                                    DMI_Pos + DMI_Neg + CCI + ReturnLag1 + ReturnLag2 + 
                                    ReturnLag3 + ReturnLag4 + ReturnLag5 , 
                          data = train, 
                          boos = TRUE, 
                          mfinal = 100,
                          coeflearn = "Breiman")

print(names(model_adaboost))
print(model_adaboost$trees[1])

pred_adaboost = predict(model_adaboost, test)
test$pred_adaboost = pred_adaboost$class
View(test)
mean(test$pred_adaboost == test$Trend_Factor) * 100

###Classification with the XGBoost Boosting in R -----
library("xgboost")

train_x <- data.matrix(train[,c("RSI","MA_Fast","MA_Slow","Stochastic",
                                "ADX_Main","DMI_Pos","DMI_Neg","CCI",
                                "ReturnLag1","ReturnLag2","ReturnLag3",
                                "ReturnLag4","ReturnLag5")])
train_y <- train[,"Trend_Factor"]
train_y <- as.numeric(train_y)-1

xgb_train = xgb.DMatrix(data=train_x, label=train_y)

set.seed(123)
model_xgb <- xgboost(data=xgb_train, 
                     eta = 0.1,
                     lambda = 0,
                     max.depth=3, 
                     nrounds=100,
                     subsample = 0.65,
                     objective = "binary:logistic",
                     verbose = 0)

test_x <- data.matrix(test[,c("RSI","MA_Fast","MA_Slow","Stochastic",
                               "ADX_Main","DMI_Pos","DMI_Neg","CCI",
                               "ReturnLag1","ReturnLag2","ReturnLag3",
                               "ReturnLag4","ReturnLag5")])
test_y <- test[,"Trend_Factor"]
test_y <- as.numeric(test_y)-1

xgb_test = xgb.DMatrix(data=test_x, label=test_y)
test$probs_xgb <- predict(model_xgb,xgb_test)
test$pred_xgb <- ifelse(test$probs_xgb >= 0.5, 1, 0)
mean(test$pred_xgb == test$Trend_Factor) * 100

###Classification with the CatBoost Boosting in R -----
library("catboost")
x_train = train[,x_vector]
y_train <- train[,"Trend_Factor"]
y_train <- as.numeric(y_train)-1

train_pool <- catboost.load_pool(data  = x_train ,
                                 label = y_train)
model_cat <- catboost.train(learn_pool = train_pool,
                            test_pool  = NULL,
                            params = list(loss_function = 'Logloss', #Precision
                                          iterations = 1000, #1000
                                          metric_period=1, #1
                                          use_best_model = TRUE,
                                          depth = 6, #6
                                          learning_rate = 0.03, #0.03
                                          rsm = 1,
                                          prediction_type = "Class",
                                          logging_level = "Verbose"))

model_cat$feature_importances

catboost.get_feature_importance(model_cat, 
                                pool = NULL, 
                                type = 'FeatureImportance',
                                thread_count = -1)

catboost.get_feature_importance(model_cat, 
                                pool = train_pool, 
                                type = 'FeatureImportance',
                                thread_count = -1)

catboost.get_feature_importance(model_cat, 
                                pool = train_pool, 
                                type = 'Interaction',
                                thread_count = -1)

catboost.get_feature_importance(model_cat, 
                                pool = train_pool, 
                                type = 'PredictionDiff',
                                thread_count = -1)

model_cat$tree_count

x_test = test[,x_vector]

y_test <- test[,"Trend_Factor"]
y_test <- as.numeric(y_test)-1
test_pool <- catboost.load_pool(x_test)


test$cat_probs = catboost.predict(model_cat,
                                 test_pool,
                                 prediction_type = "Probability")
View(test$cat_probs)

test$cat_pred = catboost.predict(model_cat,
                                  test_pool,
                                  prediction_type = "Class") 
View(test$cat_pred)
mean(test$cat_pred == test$Trend_Factor) * 100



save.image(file = "data_csv_vars.Rdata")

