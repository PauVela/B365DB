####Install Packages####
install.packages("pacman")
pacman::p_load(dplyr,caret,ggplot2,reshape2,corrplot,party,rpart,rpart.plot) 
####Install Database####
library(readr)
forestfires <- read_csv("ubiqum/BET365DB/forestfires.csv")
View(forestfires)
FFires<-forestfires
####Pre-Process####
str(forestfires)
summary(forestfires)
hist(forestfires$area)
plot(forestfires$area)
forestfires$month <-NULL
forestfires$day <-NULL
##each column plotted and log for area,RH,wind and Y ##
forestfires$log.area<- log(forestfires$area +1)
hist(forestfires$log.area)
forestfires$log.RH<- log(forestfires$RH)
forestfires$log.wind<- log(forestfires$wind)
forestfires$log.Y<- log(forestfires$Y)
forestfires$log.DC<- log(forestfires$DC)
##decision tree and plots to know which variables explain log.area##
decisionTreeFFires <-rpart(log.area~.-area, data= forestfires)
decisionTreeFFires
ff <- melt(forestfires)
ggplot(ff,aes(x = value)) + 
  facet_wrap(~variable, scales = "free_x") + 
  geom_histogram()
##Correlations##
correlationFF<-cor(forestfires)
correlationFF
View(correlationFF)
corrplot(correlationFF, method="circle", type="lower", order="hclust")
write.csv(correlationFF, file= "CorrelationFF.csv")

####Data Partition####

TrainFF<- createDataPartition(forestfires$log.area, p=0.80, list= FALSE)
trainForest<- forestfires[TrainFF,]
testForest<- forestfires[-TrainFF,]

####Prediction####
fitControl <- trainControl(method = "cv",number = 10)
####lm#####
LinearModel <- lm(log.area ~temp+DMC+X + RH ,
         data = forestfires)
LinearModel
summary(LinearModel)
predictionLM <- predict(LinearModel, select(testForest,temp ,DMC,X,RH))
predictionNoLog<- exp(predictionLM)-1
predictionLMNoLog<- data.frame(pred= predictionNoLog, obs=testForest$area)
View(predictionNoLog)
##LM prediction plot##
ggplot(predictionLMNoLog, aes(x = pred, y = obs)) + geom_point(alpha = 0.5,
position = position_jitter(width=0.2)) + geom_abline(colour = "blue") + ggtitle("LM Prediction")


####SVM####
FFSVM<-train(log.area ~ 0+temp+DMC+X+RH,
                  data=trainForest,
                  method='svmRadial',
                  trControl = fitControl,
                  preProcess = c("zv", "center", "scale"))

print(FFSVM)

predictionFFSVM<-predict(FFSVM,testForest)
predictionFFNoLog<- exp(predictionFFSVM)-1
predictionFFSVMNoLog<- data.frame(pred= predictionFFNoLog, obs=testForest$area)
View(predictionFFSVMNoLog)
testForest$predictionFFSVMNoLog<-predictionFFSVM
testForest$DifPredRealSVM <- testForest$predictionFFSVM - testForest$area
##plots SVM##
ggplot(predictionFFSVMNoLog, aes(x = pred, y = obs)) + geom_point(alpha = 0.5,position = position_jitter(width=0.2)) + geom_abline(colour = "blue") + ggtitle("SVM Prediction")
FFSVMerror<-plot(testForest$DifPredRealSVM)

####RandomForest####
RFFF<-train(log.area~ 0+temp+DMC+X+RH, data= trainForest, method="rf",
              trControl=fitControl, ntree=50, do.trace=10)
predictionRFFF<-predict(RFFF,testForest)
View(predictionRFFF)
predictionRFNoLog<- exp(predictionRFFF)-1
predictionFFRFNoLog<- data.frame(pred= predictionRFNoLog, obs=testForest$area)
View(predictionFFRFNoLog)
testForest$DifPredRealRF <- testForest$predictionFFRFNoLog - testForest$area
##plots RF##
ggplot(predictionFFRFNoLog, aes(x = pred, y = obs)) + geom_point(alpha = 0.5,position = position_jitter(width=0.2)) + geom_abline(colour = "blue") + ggtitle("RF Prediction")
FFRFerror<-plot(testForest$DifPredRealRF)

