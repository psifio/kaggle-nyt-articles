writesubmission <- function (myunique, mypredictions ,myfilename){
  MySubmission = data.frame(UniqueID = myunique, Probability1 = mypredictions)
  write.csv(MySubmission, myfilename, row.names=FALSE)
  #MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTestWordCount)
  #write.csv(MySubmission, "SubmissionWordCount.csv", row.names=FALSE)
}
########
calcTrainAcc <-function(myvalues,mypredictions,mypercentage){
  tempTable<-table(myvalues,mypredictions>mypercentage)
  t11<-tempTable[1,1];t22<-tempTable[2,2];t12<-tempTable[1,2];t21<-tempTable[2,1]
  return( (t11 +t22)/(t11+t22+t21+t12) )
}
########
getAcc<-function(mypred,myvals){
  return(as.numeric(performance(prediction(mypred,pop),"auc")@y.values))
}
########

showAccs<-function(){
  df<-data.frame(method=methods,Accuracy=trainAccs,ROCRS=rocrs)
  return(df)
}
######## FUNCTIONS ######## ########





# We are adding in the argument stringsAsFactors=FALSE, since we have some text fields
setwd('Y:/Mooc/0 took/01 Data Science n math/edx-15.071x The Analytics Edge/kaggle')
NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)
#Dates
NewsTrain$PubDate = strptime(NewsTrain$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTest$PubDate = strptime(NewsTest$PubDate, "%Y-%m-%d %H:%M:%S")
#(0 = Sunday, 1 = Monday, etc.) ?POSIXlt
NewsTrain$Weekday = NewsTrain$PubDate$wday
NewsTest$Weekday = NewsTest$PubDate$wday

#001 logistic WordCount
logWordCount = glm(Popular ~ WordCount, data=NewsTrain, family=binomial)
#predictions on the test set:
PredTestWordCount = predict(logWordCount, newdata=NewsTest, type="response")
PredTrainWordCount= predict(logWordCount, type="response")
#submit
#writesubmission(NewsTest$UniqueID,PredTestWordCount, "1.SubmissionWordCount.csv" )

#002 HeadlineWordsLog
library(tm);library(SnowballC)
CorpusHeadline = Corpus(VectorSource(c(NewsTrain$Headline, NewsTest$Headline)))
CorpusHeadline = tm_map(CorpusHeadline, tolower)
# Remember this extra line is needed after running the tolower step:
CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)
CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)
CorpusHeadline = tm_map(CorpusHeadline, removeWords, stopwords("english"))
CorpusHeadline = tm_map(CorpusHeadline, stemDocument)
dtm = DocumentTermMatrix(CorpusHeadline)
#choose other threshold
sparse = removeSparseTerms(dtm, 0.99)
HeadlineWords = as.data.frame(as.matrix(sparse))
# Let's make sure our variable names are okay for R:
colnames(HeadlineWords) = make.names(colnames(HeadlineWords))
HeadlineWordsTrain = head(HeadlineWords, nrow(NewsTrain))
HeadlineWordsTest = tail(HeadlineWords, nrow(NewsTest))
#We'll add back the dependent variable to the training set, and the WordCount variable
#to both datasets. You might want to add back more variables to use in your model 
HeadlineWordsTrain$Popular = NewsTrain$Popular
HeadlineWordsTrain$WordCount = NewsTrain$WordCount
HeadlineWordsTest$WordCount = NewsTest$WordCount
# Now let's create a logistic regression model using all of the variables:
HeadlineWordsLog = glm(Popular ~ ., data=HeadlineWordsTrain, family=binomial)
#sos check warning
PredHeadlineWordsLog = predict(HeadlineWordsLog, newdata=HeadlineWordsTest, type="response")
PredHeadlineTrainLog = predict(HeadlineWordsLog, type="response")
#writesubmission(NewsTest$UniqueID,PredHeadlineWordsLog, "2.SubmissionHeadlineWordsLog.csv" )

#003 Head+NewsDesk
HeadDeskTrain=HeadlineWordsTrain
HeadDeskTest = HeadlineWordsTest
HeadDeskTrain$NewsDesk=as.factor(NewsTrain$NewsDesk)
HeadDeskTest$NewsDesk=as.factor(NewsTest$NewsDesk)

logHeadDesk = glm(Popular ~ ., data=HeadDeskTrain, family=binomial)
PredHeadDeskTest = predict(logHeadDesk, newdata=HeadDeskTest, type="response")
PredHeadDeskTrain = predict(logHeadDesk, type="response")
#writesubmission(NewsTest$UniqueID,PredHeadDeskTest, "3.SubmissionHeadDesk.csv" )

#004 Head+NewsDesk + Hour + Day
HhrdayTrain = HeadDeskTrain
HhrdayTest = HeadDeskTest

HhrdayTrain$Day = NewsTrain$PubDate$wday
HhrdayTest$Day= NewsTest$PubDate$wday
HhrdayTrain$Hour = NewsTrain$PubDate$hour
HhrdayTest$Hour= NewsTest$PubDate$hour

logHhrday = glm(Popular ~ ., data=HhrdayTrain, family=binomial)
predHhrdayTest = predict(logHhrday, newdata=HhrdayTest, type="response")
predHhrdayTrain = predict(logHhrday, type="response")
#writesubmission(NewsTest$UniqueID,predHhrdayTest, "4.SubmissionHeadDeskHD.csv" )


#005 @@@@@@@@@@@@@@@@@@@@ + Snippet
CorpusSnippet = Corpus(VectorSource(c(NewsTrain$Snippet, NewsTest$Snippet)))
CorpusSnippet = tm_map(CorpusSnippet, PlainTextDocument)
CorpusSnippet = tm_map(CorpusSnippet, removePunctuation)
CorpusSnippet = tm_map(CorpusSnippet, removeWords, stopwords("english"))
CorpusSnippet = tm_map(CorpusSnippet, stemDocument)
dtmSnippet = DocumentTermMatrix(CorpusSnippet)
sparseSnippet = removeSparseTerms(dtmSnippet, 0.85)
SnippetWords = as.data.frame(as.matrix(sparseSnippet))
# Let's make sure our variable names are okay for R:
colnames(SnippetWords) = make.names(colnames(SnippetWords))
colnames(SnippetWords) = paste("snip.", colnames(SnippetWords))
SnippetWordsTrain = head(SnippetWords, nrow(NewsTrain))
SnippetWordsTest = tail(SnippetWords, nrow(NewsTest))

#We'll add back the dependent variable to the training set, and the WordCount variable
#to both datasets. You might want to add back more variables to use in your model 
SnippetWordsTrain2 =cbind(SnippetWordsTrain,HhrdayTrain)
SnippetWordsTest2 =cbind(SnippetWordsTest,HhrdayTest)
# Now let's create a logistic regression model using all of the variables:
SnippetLog = glm(Popular ~ ., data=SnippetWordsTrain2, family=binomial)
PredSnippetTest = predict(SnippetLog, newdata=SnippetWordsTest2, type="response")
PredSnippetTrain = predict(SnippetLog, type="response")

library(ROCR);pop=NewsTrain$Popular
getAcc(PredSnippetTrain,pop)
#writesubmission(NewsTest$UniqueID,PredHeadlineWordsLog, "5.Snippet85.csv" )
#@@@@@@@@@@@@@@@@@@@@
#006 @@@@@@ rpart trees with corss
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)

# Define cross-validation experiment
numFoldsH = trainControl( method = "cv", number = 10 )
cpGridH = expand.grid( .cp = seq(0.0001,0.5,0.005)) 
# Perform the cross validation
train(Popular ~ ., data = HhrdayTrain, method = "rpart", trControl = numFoldsH, tuneGrid = cpGridH )
# Create a new CART model
hTreeCV = rpart(Popular ~ ., data = HhrdayTrain, method="class", cp = 0.001)
#prp(StevensTreeCV)
PredHTreeTest = predict(hTreeCV, newdata = HhrdayTest)
PredHTreeTrain = predict(hTreeCV)

summary(PredHTreeTrain)
summary(PredHTreeTrain[,2])
summary(PredHTreeTrain[,1])
summary(predHhrdayTest)

#writesubmission(NewsTest$UniqueID,PredHTreeTest[,2], "6.rpartCrossHead_new.csv" )
getAcc(PredHTreeTrain[,2],pop)
#@@@@@@@@@@@@@@@@@@@@
#007 @@@@@@ rpart trees with snippet

#numFoldsS = trainControl( method = "cv", number = 10 )
#cpGridS = expand.grid( .cp = seq(0.001,0.5,0.005)) 
#train(Popular ~ .,data=SnippetWordsTrain2, method="rpart",trControl=numFoldsS,tuneGrid=cpGridS )

sTreeCV = rpart(Popular ~ ., data = SnippetWordsTrain2, method="class", cp = 0.001)
#prp(StevensTreeCV)
PredSTreeTest = predict(sTreeCV, newdata = SnippetWordsTest2)
PredSTreeTrain = predict(sTreeCV)

summary(PredSTreeTrain)
summary(PredSTreeTrain[,2])
summary(PredSTreeTrain[,1])
summary(predHhrdayTest)

writesubmission(NewsTest$UniqueID,PredSTreeTest[,2], "7.rpartCrossSnip.csv" )
getAcc(PredSTreeTrain[,2],pop)
#@@@@@@@@@@@@@@@@@@@@
#008 @@@@@@ random forest
library(randomForest)
hForestTrain =HhrdayTrain
hForestTest  =HhrdayTest


##############very important 
NewsTrain$NewsDesk[NewsTrain$NewsDesk == ''] = 'nonewsdesk'
NewsTest$NewsDesk[NewsTest$NewsDesk == ''] = 'nonewsdesk'
NewsTrain$SectionName[NewsTrain$SectionName == ''] = 'nosection'
NewsTest$SectionName[NewsTest$SectionName == ''] = 'nosection'
NewsTrain$SubsectionName[NewsTrain$SubsectionName == ''] = 'nosubsection'
NewsTest$SubsectionName[NewsTest$SubsectionName == ''] = 'nosubsection'


hForestTrain$NewsDesk=as.factor(NewsTrain$NewsDesk)
hForestTest$NewsDesk=as.factor(NewsTest$NewsDesk)
levels(hForestTest$NewsDesk) <- levels(hForestTrain$NewsDesk)

hForestTrain$SectionName=as.factor(NewsTrain$SectionName)
hForestTest$SectionName=as.factor(NewsTest$SectionName)
levels(hForestTest$SectionName) <- levels(hForestTrain$SectionName)

hForestTrain$SubsectionName=as.factor(NewsTrain$SubsectionName)
hForestTest$SubsectionName=as.factor(NewsTest$SubsectionName)
levels(hForestTest$SubsectionName) <- levels(hForestTrain$SubsectionName)

hForestTrain$Month=as.factor(NewsTrain$PubDate$mon)
hForestTest$Month=as.factor(NewsTest$PubDate$mon)
levels(hForestTest$Month) <- levels(hForestTrain$Month)

hForestTrain$Day = as.factor(NewsTrain$PubDate$wday)
hForestTest$Day = as.factor(NewsTest$PubDate$wday)
levels(hForestTest$Day) <- levels(hForestTrain$Day)

hForestTrain$Hour = as.factor(NewsTrain$PubDate$hour)
hForestTest$Hour= as.factor(NewsTest$PubDate$hour)
levels(hForestTest$Hour) <- levels(hForestTrain$Hour)

#makis (0 = Sunday, 1 = Monday, etc.) ?POSIXlt
##############
hForestTrain$Popular=as.factor(NewsTrain$Popular)
#method="class",   data = Train, ntree=200, nodesize=25
set.seed(100)
hForest = randomForest(Popular ~ ., data = hForestTrain,method="class")
#prp(StevensTreeCV)
PredHForestTest = predict(hForest, newdata = hForestTest,type="prob")
PredHForestTrain = predict(hForest,type="prob")
writesubmission(NewsTest$UniqueID,PredHForestTest[,2], "8.HForest4.csv" )

#10 cluster
#cluster then predict
#remove depedent variable before clustering
set.seed(144)
#km<-kmeans(hForestTrain,centers=3)
limTrain=hForestTrain
limTrain$Popular=NULL

modmat<-model.matrix(~.+0, data=limTrain)
modtest<-model.matrix(~.+0, data=hForestTest)
km<-kmeans(modmat, centers=5)
table(km$cluster)

install.packages('flexclust')
library(flexclust)
km.kcca = as.kcca(km, modmat)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=modtest)


enhTrain=hForestTrain
enhTrain$UniqueID= NewsTrain$UniqueID
enhTest=hForestTest
enhTest$UniqueID= NewsTest$UniqueID
clglm=glm(Popular  ~ . - UniqueID, data=enhTrain, family=binomial)
PredClustTest = predict(clglm, newdata = enhTest,type="response")
PredClustTrain = predict(clglm,type="response")

train1 = subset(enhTrain, clusterTrain == 1)
train2 = subset(enhTrain, clusterTrain == 2)
train3 = subset(enhTrain, clusterTrain == 3)
train4 = subset(enhTrain, clusterTrain == 4)
train5 = subset(enhTrain, clusterTrain == 5)
test1 = subset(enhTest, clusterTest == 1)
test2 = subset(enhTest, clusterTest == 2)
test3 = subset(enhTest, clusterTest == 3)
test4 = subset(enhTest, clusterTest == 4)
test5 = subset(enhTest, clusterTest == 5)

train1$NewsDesk<-factor(train1$NewsDesk)
levels(train1$NewsDesk) <- levels(test1$NewsDesk)
train1$SectionName<-factor(train1$SectionName)
levels(train1$SectionName) <- levels(test1$SectionName)
train1$SubsectionName<-factor(train1$SubsectionName)
levels(train1$SubsectionName) <- levels(test1$SubsectionName)
train2$NewsDesk<-factor(train2$NewsDesk)
levels(train2$NewsDesk) <- levels(test2$NewsDesk)
train2$SectionName<-factor(train2$SectionName)
levels(train2$SectionName) <- levels(test2$SectionName)
train2$SubsectionName<-factor(train2$SubsectionName)
levels(train2$SubsectionName) <- levels(test2$SubsectionName)
train3$NewsDesk<-factor(train3$NewsDesk)
levels(train3$NewsDesk) <- levels(test3$NewsDesk)
train3$SectionName<-factor(train3$SectionName)
levels(train3$SectionName) <- levels(test3$SectionName)
train3$SubsectionName<-factor(train3$SubsectionName)
levels(train3$SubsectionName) <- levels(test3$SubsectionName)
train4$NewsDesk<-factor(train4$NewsDesk)
levels(train4$NewsDesk) <- levels(test4$NewsDesk)
train4$SectionName<-factor(train4$SectionName)
levels(train4$SectionName) <- levels(test4$SectionName)
train4$SubsectionName<-factor(train4$SubsectionName)
levels(train4$SubsectionName) <- levels(test4$SubsectionName)

train5$NewsDesk<-factor(train5$NewsDesk)
test5$NewsDesk<-factor(test5$NewsDesk)

levels(train5$NewsDesk) <- levels(test5$NewsDesk)
train5$SectionName<-factor(train5$SectionName)
levels(train5$SectionName) <- levels(test5$SectionName)
train5$SubsectionName<-factor(train5$SubsectionName)
levels(train5$SubsectionName) <- levels(test5$SubsectionName)




clglm1=glm(Popular  ~ . - UniqueID, data=train1, family=binomial)
PredClustTrain1 = predict(clglm1,type="response")
PredClustTest1= predict(clglm1, newdata = test1,type="response")

test2$day=as.numeric(test2$day)
train2$day=as.numeric(train2$day)


clglm2=glm(Popular  ~ . - UniqueID, data=train2, family=binomial)
PredClustTrain2 = predict(clglm2,type="response")
PredClustTest2= predict(clglm2, newdata = test2,type="response")

clglm3=glm(Popular  ~ . - UniqueID, data=train3, family=binomial)
PredClustTrain3 = predict(clglm3,type="response")
PredClustTest3= predict(clglm3, newdata = test3,type="response")

clglm4=glm(Popular  ~ . - UniqueID, data=train4, family=binomial)
PredClustTrain4 = predict(clglm4,type="response")
PredClustTest4= predict(clglm4, newdata = test4,type="response")

clglm5=glm(Popular  ~ . - UniqueID, data=train5, family=binomial)
PredClustTrain5 = predict(clglm5,type="response")
PredClustTest5= predict(clglm5, newdata = test5,type="response")


levels(train5$NewsDesk)
levels(test5$NewsDesk)

table(train5$NewsDesk)
table(test5$NewsDesk)







####gbm##############
library(caret)
library(gbm)

set.seed(42)

gbmGrid <- expand.grid(.n.trees = 100, 
                       .interaction.depth = 1:4, 
                       .shrinkage = 0.05)


trainedGBM <- train(Popular ~ ., method = "gbm", distribution='bernoulli',
                    data = hForestTrain, tuneGrid = gbmGrid, 
                    trControl = trainControl(method = "repeatedcv", number = 10, 
                                             repeats = 3, verboseIter = FALSE, 
                                             returnResamp = "all"))
print(trainedGBM)        

ntrees <-  gbm.perf(trainedGBM$finalModel, method="OOB")
PredTraingbm=predict(trainedGBM$finalModel, n.trees=ntrees,type='response')
PredTestgbm=predict(trainedGBM$finalModel, newdata=hForestTest,
                    n.trees=ntrees,type='response')

writesubmission(NewsTest$UniqueID,PredTestgbm, "9.gbm.csv" )
getAcc(PredTraingbm,pop)

##################
library(ROCR)
pop=NewsTrain$Popular
methods<-c();trainAccs<-c();rocrs<-c()
methods<-c(methods,"logWordCount","HeadlineWordsLog","HeadDesk","Hhrday","Snippet","HTree","STree","Forest")
trainAccs<-c(trainAccs, calcTrainAcc(pop,PredTrainWordCount,0.5))
trainAccs<-c(trainAccs, calcTrainAcc(pop,PredHeadlineTrainLog,0.5))
trainAccs<-c(trainAccs, calcTrainAcc(pop,PredHeadDeskTrain,0.5))
trainAccs<-c(trainAccs, calcTrainAcc(pop,predHhrdayTrain,0.5))
trainAccs<-c(trainAccs, calcTrainAcc(pop,PredSnippetTrain,0.5))
trainAccs<-c(trainAccs, calcTrainAcc(pop,PredHTreeTrain[,2],0.5))
trainAccs<-c(trainAccs, calcTrainAcc(pop,PredSTreeTrain[,2],0.5))
trainAccs<-c(trainAccs, calcTrainAcc(pop,PredHForestTrain[,2],0.5))
rocrs<-c(rocrs,getAcc(PredTrainWordCount,pop))
rocrs<-c(rocrs,getAcc(PredHeadlineTrainLog,pop))
rocrs<-c(rocrs,getAcc(PredHeadDeskTrain,pop))
rocrs<-c(rocrs,getAcc(predHhrdayTrain,pop))
rocrs<-c(rocrs,getAcc(PredSnippetTrain,pop))
rocrs<-c(rocrs,getAcc(PredHTreeTrain[,2],pop))
rocrs<-c(rocrs,getAcc(PredSTreeTrain[,2],pop))
rocrs<-c(rocrs,getAcc(PredHForestTrain[,2]))
showAccs()

#################



