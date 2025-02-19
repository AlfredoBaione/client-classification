#-----------------------------------INTRO

rm(list=ls())
options(java.parameters = "-Xmx8g")

# libraries
library(dplyr)
library(mlr) 
library(FSelector) 
library(rpart.plot)
library(corrplot)
library(bartMachine)

# Reading
setwd( "/Users/user/Desktop/polito/Business analytics/2021/consegne/laboratorioclassificazione/")
dtab = read.csv("pilgrimABCmod.csv", header=TRUE)
attach(dtab)

# control for the clients who left the bank in 2000
sum(is.na(Online0))
sum(is.na(Profit0))
which(is.na(Profit0) != is.na(Online0))
which(is.na(Profit0) < is.na(Online0))
which(is.na(Profit0) > is.na(Online0))

#----------------------------------PREPROCESSING

# train and test creation
set.seed(100)
train     = sample(nrow(dtab), 0.5*nrow(dtab), replace = FALSE)
TrainSet  = dtab[train ,]
ValidSet  = dtab[-train ,]
summary(TrainSet)
summary(ValidSet)

summarizeColumns(TrainSet)
summarizeColumns(ValidSet)


# handling outliers (remove profits over 50000)
plot(TrainSet$Profit0,TrainSet$Profit9)
TrainSet=TrainSet[-c(6669,2457,2363,9527,4192,5437,6823,4397),]

plot(ValidSet$Profit0,ValidSet$Profit9)
ValidSet=ValidSet[-c(14429),]

# handling missing data 
TrainSet$AgeZero9 = ifelse(is.na(TrainSet$Age9),0,TrainSet$Age9)
mm1 = mean(TrainSet$Age9, na.rm=TRUE)
TrainSet$AgeAverage9 = ifelse(is.na(TrainSet$Age9),mm1,TrainSet$Age9)
TrainSet$AgeCategory9=as.factor(TrainSet$AgeZero9)
TrainSet$AgeGiven9 = as.factor(ifelse(is.na(TrainSet$Age9),0,1))

ValidSet$AgeZero9 = ifelse(is.na(ValidSet$Age9),0,ValidSet$Age9)
mm2 = mean(ValidSet$Age9, na.rm=TRUE)
ValidSet$AgeAverage9 = ifelse(is.na(ValidSet$Age9),mm2,ValidSet$Age9)
ValidSet$AgeCategory9=as.factor(ValidSet$AgeZero9)
ValidSet$AgeGiven9 = as.factor(ifelse(is.na(ValidSet$Age9),0,1))


TrainSet$IncomeZero9 = ifelse(is.na(TrainSet$Income9),0,TrainSet$Income9)
TrainSet$IncomeGiven9 = as.factor(ifelse(is.na(TrainSet$Income9),0,1))
mmm1= mean(TrainSet$Income9, na.rm=TRUE)
TrainSet$IncomeAverage9=ifelse(is.na(TrainSet$Income9),mmm1,TrainSet$Income9)
TrainSet$IncomeCategory9=as.factor(TrainSet$IncomeZero9)

ValidSet$IncomeZero9 = ifelse(is.na(ValidSet$Income9),0,ValidSet$Income9)
ValidSet$IncomeGiven9 = as.factor(ifelse(is.na(ValidSet$Income9),0,1))
mmm2= mean(ValidSet$Income9, na.rm=TRUE)
ValidSet$IncomeAverage9=ifelse(is.na(ValidSet$Income9),mmm2,ValidSet$Income9)
ValidSet$IncomeCategory9=as.factor(ValidSet$IncomeZero9)


# handling variables
TrainSet$District11009 = as.factor(ifelse(TrainSet$District9==1100,1,0))
TrainSet$District12009 = as.factor(ifelse(TrainSet$District9==1200,1,0))
TrainSet$District9=as.factor(TrainSet$District9)

ValidSet$District11009 = as.factor(ifelse(ValidSet$District9==1100,1,0))
ValidSet$District12009 = as.factor(ifelse(ValidSet$District9==1200,1,0))
ValidSet$District9=as.factor(ValidSet$District9)


TrainSet$Online9=as.factor(TrainSet$Online9)
TrainSet$Online0=as.factor(TrainSet$Online0)

ValidSet$Online9=as.factor(ValidSet$Online9)
ValidSet$Online0=as.factor(ValidSet$Online0)


max(TrainSet$Profit9)
min(TrainSet$Profit9)
TrainSet$ProfitCategory9[TrainSet$Profit9>=-221 & TrainSet$Profit9<=7]='0' 
TrainSet$ProfitCategory9[TrainSet$Profit9>=8 & TrainSet$Profit9<=236]='1'
TrainSet$ProfitCategory9[TrainSet$Profit9>=237 & TrainSet$Profit9<=465]='2'
TrainSet$ProfitCategory9[TrainSet$Profit9>=466 & TrainSet$Profit9<=694]='3'
TrainSet$ProfitCategory9[TrainSet$Profit9>=695 & TrainSet$Profit9<=923]='4'
TrainSet$ProfitCategory9[TrainSet$Profit9>=924 & TrainSet$Profit9<=1152]='5'
TrainSet$ProfitCategory9[TrainSet$Profit9>=1153 & TrainSet$Profit9<=1381]='6'
TrainSet$ProfitCategory9[TrainSet$Profit9>=1382 & TrainSet$Profit9<=1610]='7'
TrainSet$ProfitCategory9[TrainSet$Profit9>=1611 & TrainSet$Profit9<=1839]='8'
TrainSet$ProfitCategory9[TrainSet$Profit9>=1840 & TrainSet$Profit9<=2071]='9'
TrainSet$ProfitCategory9=as.factor(TrainSet$ProfitCategory9)

max(ValidSet$Profit9)
min(ValidSet$Profit9)
ValidSet$ProfitCategory9[ValidSet$Profit9>=-220 & ValidSet$Profit9<=7]='0' 
ValidSet$ProfitCategory9[ValidSet$Profit9>=8 & ValidSet$Profit9<=235]='1'
ValidSet$ProfitCategory9[ValidSet$Profit9>=236 & ValidSet$Profit9<=463]='2'
ValidSet$ProfitCategory9[ValidSet$Profit9>=464 & ValidSet$Profit9<=691]='3'
ValidSet$ProfitCategory9[ValidSet$Profit9>=692 & ValidSet$Profit9<=919]='4'
ValidSet$ProfitCategory9[ValidSet$Profit9>=920 & ValidSet$Profit9<=1147]='5'
ValidSet$ProfitCategory9[ValidSet$Profit9>=1148 & ValidSet$Profit9<=1375]='6'
ValidSet$ProfitCategory9[ValidSet$Profit9>=1376 & ValidSet$Profit9<=1603]='7'
ValidSet$ProfitCategory9[ValidSet$Profit9>=1604 & ValidSet$Profit9<=1831]='8'
ValidSet$ProfitCategory9[ValidSet$Profit9>=1832 & ValidSet$Profit9<=2063]='9'
ValidSet$ProfitCategory9=as.factor(ValidSet$ProfitCategory9)


TrainSet$Retain = ifelse(is.na(TrainSet$Profit0),0,1)
ValidSet$Retain = ifelse(is.na(ValidSet$Profit0),0,1)
prop.table(table(TrainSet$Retain))
prop.table(table(ValidSet$Retain))


corrplot(cor(TrainSet-c(1,7,9,10,11,12,13,14,15,16,17,18,19,20,21,22)), type="upper")
corrplot(cor(ValidSet-c(1,7,9,10,11,12,13,14,15,16,17,18,19,20,21,22)), type="upper")


TrainSet$Retain[TrainSet$Retain=="0"]<-"Gone"
TrainSet$Retain[TrainSet$Retain=="1"]<-"Retained"
TrainSet$Retain=as.factor(TrainSet$Retain)

ValidSet$Retain[ValidSet$Retain=="0"]<-"Gone"
ValidSet$Retain[ValidSet$Retain=="1"]<-"Retained"
ValidSet$Retain=as.factor(ValidSet$Retain)

TrainSet <- normalizeFeatures(TrainSet,method = "standardize")
ValidSet <- normalizeFeatures(ValidSet,method = "standardize")


(model_task=makeClassifTask(data=TrainSet, target="Retain", positive= 'Gone'))


generateFilterValuesData(model_task, method = c("FSelector_information.gain")) %>% plotFilterValues()
generateFilterValuesData(model_task, method = c("FSelector_chi.squared")) %>% plotFilterValues()
generateFilterValuesData(model_task, method = c("FSelector_gain.ratio")) %>% plotFilterValues()

TrainSetmodel <- select(TrainSet, -c(1,3,4,5,8,9,10,11,12,13,15,16,17,19,20,21,22)) 
ValidSetmodel<- select(ValidSet,  -c(1,3,4,5,8,9,10,11,12,13,15,16,17,19,20,21,22)) 

list( train = summary(TrainSetmodel), test = summary(ValidSetmodel) )




#---------------------------------PROCESSING

#optimized model
set.seed(1000)
(model_task <- makeClassifTask(data=TrainSetmodel, target="Retain", positive="Gone"))



#logistic regression

# learner
(rl_prob<- makeLearner('classif.logreg', predict.type='prob'))

#tuning hyperparameters
getParamSet("classif.logreg")# non sono modificabili 

# validation
kFold <- makeResampleDesc(method = "RepCV", folds = 10, reps = 50,
                          stratify = TRUE)

(rlogisticvalidation <- resample(rl_prob, model_task,
                             resampling = kFold,
                             measures = list(acc, fpr, fnr)))



# train model
set.seed(1000) 
rlogistic_train <- train(learner=rl_prob, task=model_task)
rlogisticmodeldata<-getLearnerModel(rlogistic_train)
 coef(rlogisticmodeldata)

#converting model parameters into odds ratio
exp(cbind(Odds_Ratio = coef(rlogisticmodeldata), confint(rlogisticmodeldata)))

# prevision
set.seed(1000)
(rlogistic_predict<- predict(rlogistic_train, newdata = ValidSetmodel, type='prob'))


# confusion matrix
calculateConfusionMatrix(rlogistic_predict)
rlogistic_predict %>% calculateROCMeasures()


# roc curve and auc
df = generateThreshVsPerfData(rlogistic_predict, measures = list(fpr, tpr, mmce))
plotROCCurves(df)
mlr::performance(rlogistic_predict, mlr::auc)


# idea for a threshold
plotThreshVsPerf(df)

Logisticregression <- rlogistic_predict %>% setThreshold(0.35) 
(rl_performance <- Logisticregression %>%performance(measures = list(tpr,auc, mmce,tnr)) )
(rl_cm <- Logisticregression %>% calculateROCMeasures() )
#? molto migliorata la previsione dei 'Gone'


# DECISION TREE


# learner
(dt_prob <- makeLearner('classif.rpart', predict.type="prob"))

#tuning hyperparameters
getParamSet("classif.rpart")


dt_param <- makeParamSet( makeDiscreteParam("minsplit", values=seq(5,10,1)), 
                          makeDiscreteParam("minbucket", values=seq(round(5/3,0), round(10/3,0), 1)), 
                          makeNumericParam("cp", lower=0, upper=0.02),
                          makeDiscreteParam("maxcompete", values=6), 
                          makeDiscreteParam("usesurrogate", values=0), 
                          makeDiscreteParam("maxdepth", values=seq(5,6,1) ))
dt_param

# tipo di ricerca: Grid
(ctrl = makeTuneControlGrid())



# evaluation of hyperparameters: resampling 
rdesc = makeResampleDesc("CV", iters = 3L, stratify=TRUE)

# evaluation
set.seed(1000) 
(dt_tuneparam <- tuneParams(learner=dt_prob, 
                            resampling=rdesc, 
                            measures=list(tpr, auc, fnr, mmce, tnr, setAggregation(tpr, test.sd)), 
                            par.set=dt_param, 
                            control=ctrl, 
                            task=model_task, 
                            show.info = TRUE) ) 
# best hyperparam
dt_tuneparam

list( `Optimal HyperParameters` = dt_tuneparam$x, 
      `Optimal Metrics` = dt_tuneparam$y )



# selecting best control param
## minsplit = 8,
##minbucket = 2,
##maxdepth = 6,
##maxcompete=6,
##usesurrogate = 0,
#cp = 0

dtree <- setHyperPars(dt_prob, par.vals = dt_tuneparam$x)

# best model
set.seed(1000) 
dtree_train <- train(learner=dtree, task=model_task) 
getLearnerModel(dtree_train)


# some plots
rpart.plot(dtree_train$learner.model, roundint=FALSE, varlen=3, type = 3, clip.right.labs = FALSE, yesno = 2)
rpart.rules(dtree_train$learner.model, roundint = FALSE)



# class prediction
set.seed(1000)
(dtree_predict<- predict(dtree_train, newdata = ValidSetmodel))


# confusion matrix
calculateConfusionMatrix(dtree_predict)
dtree_predict %>% calculateROCMeasures()

# roc curve and auc
df = generateThreshVsPerfData(dtree_predict, measures = list(fpr, tpr, mmce))
plotROCCurves(df)
mlr::performance(dtree_predict, mlr::auc)


# idea for a threshold
plotThreshVsPerf(df)

DecisionTree <- dtree_predict %>% setThreshold(0.2) 
(dt_performance <- DecisionTree %>%performance(measures = list(tpr,auc, mmce,tnr)) )
(dt_cm <- DecisionTree %>% calculateROCMeasures() )
#? leggermente migliorata la previsione dei 'Gone'


# RANDOM FOREST
# learner
(rf_prob <- makeLearner('classif.randomForest', predict.type="prob"))

# tuning hyperparameters
getParamSet("classif.randomForest")

# selecting intervals
rf_param <- makeParamSet( makeDiscreteParam("ntree", values=seq(100,600,100)), 
                          makeDiscreteParam("mtry", values=seq(2,5,1)))

rf_param

# searching: Grid Search
(ctrl = makeTuneControlGrid())

# evaluation of hyperparameters: resampling 
rdesc = makeResampleDesc("CV", iters = 3L, stratify=TRUE)

# evaluation
set.seed(1000) 
(rf_tuneparam <- tuneParams(learner=rf_prob, 
                            resampling=rdesc, 
                            measures=list(tpr, auc, fnr, mmce, tnr, setAggregation(tpr, test.sd)), 
                            par.set=rf_param, 
                            control=ctrl, 
                            task=model_task, 
                            show.info = TRUE) ) 
# best param
rf_tuneparam

list( `Optimal HyperParameters` = rf_tuneparam$x, 
      `Optimal Metrics` = rf_tuneparam$y )



# selecting best param
## ntree =400 ,
##mtry = 6,
rforest <- setHyperPars(rf_prob, par.vals = rf_tuneparam$x)

# best model
set.seed(1000) 
rforest_train <- train(learner=rforest, task=model_task) 
getLearnerModel(rforest_train)



# class prediction
set.seed(1000)
(rforest_predict<- predict(rforest_train, newdata = ValidSetmodel))


# confusion matrix
calculateConfusionMatrix(rforest_predict)
rforest_predict %>% calculateROCMeasures()

# roc curve and auc
df = generateThreshVsPerfData(rforest_predict, measures = list(fpr, tpr, mmce))
plotROCCurves(df)
mlr::performance(rforest_predict, mlr::auc)


# idea for a threshold
plotThreshVsPerf(df)

RandomForest <- rforest_predict %>% setThreshold(0.4) 
(rf_performance <- RandomForest %>%performance(measures = list(tpr,auc, mmce,tnr)) )
(rf_cm <- RandomForest %>% calculateROCMeasures() )
#?  migliorata la previsione dei 'Gone'


# BAYES TREE
# learner
(bt_prob<-makeLearner('classif.bartMachine', predict.type='prob'))

# tuning hyperparameters
getParamSet("classif.bartMachine")

# chosing intervals
bt_param <- makeParamSet( makeDiscreteParam("num_trees", values=seq(40,60,10)),
                          makeDiscreteParam("num_burn_in", values=seq(240,260,10)))


bt_param

# searching: Grid Search
(ctrl = makeTuneControlGrid())

# evaluation of hyperparameters: resampling 
rdesc = makeResampleDesc("CV", iters = 2, stratify=TRUE)

# evaluation
set.seed(1000) 
(bt_tuneparam <- tuneParams(learner=bt_prob, 
                            resampling=rdesc, 
                            measures=list(tpr, auc, fnr, mmce, tnr, setAggregation(tpr, test.sd)), 
                            par.set=bt_param, 
                            control=ctrl, 
                            task=model_task, 
                            show.info = TRUE) ) 
# best param
bt_tuneparam

list( `Optimal HyperParameters` = bt_tuneparam$x, 
      `Optimal Metrics` = bt_tuneparam$y )



# selecting best param
## num_trees =40 ,
##nu_burn_in = 240,
btrees <- setHyperPars(bt_prob, par.vals = bt_tuneparam$x)

# best model
set.seed(1000) 
btrees_train <- train(learner=btrees, task=model_task) 
getLearnerModel(btrees_train)



# class prediction
set.seed(1000)
(btrees_predict<- predict(btrees_train, newdata = ValidSetmodel))


#confusion matrix
calculateConfusionMatrix(btrees_predict)
btrees_predict %>% calculateROCMeasures()

#roc curve and auc
df = generateThreshVsPerfData(btrees_predict, measures = list(fpr, tpr, mmce))
plotROCCurves(df)
mlr::performance(btrees_predict, mlr::auc)


# idea for a threshold
plotThreshVsPerf(df)

Bayestrees <- btrees_predict %>% setThreshold(0.2) 
(bt_performance <- Bayestrees %>%performance(measures = list(tpr,auc, mmce,tnr)) )
(bt_cm <- Bayestrees %>% calculateROCMeasures() )

# Comments: The prevision of the GONE clients has largely improved



