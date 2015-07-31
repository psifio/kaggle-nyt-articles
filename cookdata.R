library(ROCR); library(rpart); library(rpart.plot); library(caret);library(e1071);library(tm);library(SnowballC);
library(randomForest);library(caret);library(gbm)
calcTrainAcc <-function(myvalues,mypredictions,mypercentage){
  tempTable<-table(myvalues,mypredictions>mypercentage)
  t11<-tempTable[1,1];t22<-tempTable[2,2];t12<-tempTable[1,2];t21<-tempTable[2,1]
  return( (t11 +t22)/(t11+t22+t21+t12) )
}
getAcc<-function(mypred,myvals){
  return(as.numeric(performance(prediction(mypred,pop),"auc")@y.values))
}
writesubmission <- function (myunique, mypredictions ,myfilename){
  MySubmission = data.frame(UniqueID = myunique, Probability1 = mypredictions)
  write.csv(MySubmission, myfilename, row.names=FALSE)
}
########

setwd('Y:/Mooc/0 took/01 Data Science n math/edx-15.071x The Analytics Edge/kaggle')
NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)
#Dates (0 = Sunday, 1 = Monday, etc.) ?POSIXlt
NewsTrain$PubDate = strptime(NewsTrain$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTest$PubDate = strptime(NewsTest$PubDate, "%Y-%m-%d %H:%M:%S")

NewsTrain$Weekday = NewsTrain$PubDate$wday
NewsTest$Weekday = NewsTest$PubDate$wday
#Empty values
NewsTrain$NewsDesk[NewsTrain$NewsDesk == ''] = 'nonewsdesk'
NewsTest$NewsDesk[NewsTest$NewsDesk == ''] = 'nonewsdesk'
NewsTrain$SectionName[NewsTrain$SectionName == ''] = 'nosection'
NewsTest$SectionName[NewsTest$SectionName == ''] = 'nosection'
NewsTrain$SubsectionName[NewsTrain$SubsectionName == ''] = 'nosubsection'
NewsTest$SubsectionName[NewsTest$SubsectionName == ''] = 'nosubsection'
NewsTrain$Headline[NewsTrain$Headline == ''] = 'noheadline'
NewsTest$Headline[NewsTest$Headline == ''] = 'noheadline'
NewsTrain$Snippet[NewsTrain$Snippet == ''] = 'nosnippet'
NewsTest$Snippet[NewsTest$Snippet == ''] = 'nosnippet'
NewsTrain$Abstract[NewsTrain$Abstract == ''] = 'noabstract'
NewsTest$Abstract[NewsTest$Abstract == ''] = 'noabstract'
pop=NewsTrain$Popular;
#@@@@@@@@@@@@@@@@@@@@  Headline
CorpusHeadline = Corpus(VectorSource(c(NewsTrain$Headline, NewsTest$Headline)))
CorpusHeadline = tm_map(CorpusHeadline, tolower)
CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)
CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)
CorpusHeadline = tm_map(CorpusHeadline, removeWords, stopwords("english"))
CorpusHeadline = tm_map(CorpusHeadline, stemDocument)
dtm = DocumentTermMatrix(CorpusHeadline)
sparse = removeSparseTerms(dtm, 0.99)
HeadlineWords = as.data.frame(as.matrix(sparse))
# Let's make sure our variable names are okay for R:
colnames(HeadlineWords) = make.names(colnames(HeadlineWords))
colnames(HeadlineWords) = paste("head.", colnames(HeadlineWords),sep="")
HeadlineWordsTrain = head(HeadlineWords, nrow(NewsTrain))
HeadlineWordsTest = tail(HeadlineWords, nrow(NewsTest))
#0@@@@@@@@@@@@@@@@@@@@ Abstract
CorpusAbstract = Corpus(VectorSource(c(NewsTrain$Abstract, NewsTest$Abstract)))
CorpusAbstract = tm_map(CorpusAbstract, PlainTextDocument)
CorpusAbstract = tm_map(CorpusAbstract, removePunctuation)
CorpusAbstract = tm_map(CorpusAbstract, removeWords, stopwords("english"))
CorpusAbstract = tm_map(CorpusAbstract, stemDocument)
dtmAbstract = DocumentTermMatrix(CorpusAbstract)
sparseAbstract = removeSparseTerms(dtmAbstract, 0.99)
AbstractWords = as.data.frame(as.matrix(sparseAbstract))
# Let's make sure our variable names are okay for R:
colnames(AbstractWords) = make.names(colnames(AbstractWords))
colnames(AbstractWords) = paste("abstract.", colnames(AbstractWords),sep="")
AbstractWordsTrain = head(AbstractWords, nrow(NewsTrain))
AbstractWordsTest = tail(AbstractWords, nrow(NewsTest))

#####################################################################################################
myTrain=HeadlineWordsTrain
myTest=HeadlineWordsTest
#myTrain=cbind(HeadlineWordsTrain,AbstractWordsTrain)
#myTest=cbind(HeadlineWordsTest,AbstractWordsTest)

#myTrain$Popular = NewsTrain$Popular
myTrain$Popular = as.factor(NewsTrain$Popular)
myTrain$WordCount = log(NewsTrain$WordCount+1)
myTest$WordCount = log(NewsTest$WordCount+1)

myTrain$NewsDesk=as.factor(NewsTrain$NewsDesk)
myTest$NewsDesk=as.factor(NewsTest$NewsDesk)
levels(myTest$NewsDesk) <- levels(myTrain$NewsDesk)
myTrain$SectionName=as.factor(NewsTrain$SectionName)
myTest$SectionName=as.factor(NewsTest$SectionName)
levels(myTest$SectionName) <- levels(myTrain$SectionName)
myTrain$SubsectionName=as.factor(NewsTrain$SubsectionName)
myTest$SubsectionName=as.factor(NewsTest$SubsectionName)
levels(myTest$SubsectionName) <- levels(myTrain$SubsectionName)
###########DATES#########################################################
#myTrain$Day = NewsTrain$PubDate$wday
#myTest$Day= NewsTest$PubDate$wday
#myTrain$Hour = NewsTrain$PubDate$hour
#myTest$Hour= NewsTest$PubDate$hour
#myTrain$Month = NewsTrain$PubDate$mon
#myTest$Month= NewsTest$PubDate$mon
#as.factor
myTrain$Day = as.factor(NewsTrain$PubDate$wday)
myTest$Day = as.factor(NewsTest$PubDate$wday)
levels(myTest$Day) <- levels(myTrain$Day)
myTrain$Hour = as.factor(NewsTrain$PubDate$hour)
myTest$Hour= as.factor(NewsTest$PubDate$hour)
levels(myTest$Hour) <- levels(myTrain$Hour)
myTrain$Month=as.factor(NewsTrain$PubDate$mon)
myTest$Month=as.factor(NewsTest$PubDate$mon)
levels(myTest$Month) <- levels(myTrain$Month)
#######################
myTrain$Day = NULL
myTest$Day = NULL

myTrain$Weekend = ifelse( (NewsTrain$PubDate$wday==0)|(NewsTrain$PubDate$wday==6), 1, 0) 
myTest$Weekend = ifelse( (NewsTest$PubDate$wday==0)|(NewsTest$PubDate$wday==6), 1, 0)  

names(myTrain)
myTrain$head.X2014=NULL
myTrain$head.X2015=NULL
myTrain$head.day=NULL
myTrain$head.week=NULL
myTrain$head.word=NULL
myTrain$abstract.X2015=NULL
myTrain$abstract.thursday=NULL
myTrain$abstract.three=NULL
myTrain$abstract.two=NULL
myTrain$abstract.what=NULL
myTrain$head.news=NULL

myTest$head.X2014=NULL
myTest$head.X2015=NULL
myTest$head.day=NULL
myTest$head.week=NULL
myTest$head.word=NULL
myTest$abstract.X2015=NULL
myTest$abstract.thursday=NULL
myTest$abstract.three=NULL
myTest$abstract.two=NULL
myTest$abstract.what=NULL
myTest$head.news=NULL


####### 001 glm #######
myglm = glm(Popular ~ ., data=myTrain, family=binomial)
#sos check warning
pred.Train.glm = predict(myglm, type="response")
pred.Test.glm = predict(myglm, newdata=myTest, type="response")
print('glm:');getAcc(pred.Train.glm,pop) #0.95
#writesubmission(NewsTest$UniqueID,pred.Test.glm , "1.new.glm.noabsract.csv" )
####### random forest #####
set.seed(200)
myTrain$Popular=as.factor(NewsTrain$Popular)
myForest = randomForest(Popular ~ ., data = myTrain,method="class")
#prp(myForest)
pred.Train.rForest  = predict(myForest,type="prob")
pred.Test.rForest  = predict(myForest, newdata = myTest,type="prob")
print('forest:');getAcc(pred.Train.rForest[,2],pop) 
#writesubmission(NewsTest$UniqueID,pred.Test.rForest[,2], "2.new.randomForest.noabstract.csv" )


################ cart
# Create a new CART model
mytree = rpart(Popular ~ ., data = myTrain, method="class", cp = 0.00085)
PredHTreeTrain = predict(mytree)
PredHTreeTest = predict(mytree, newdata = myTest)
print('rpart:');getAcc(PredHTreeTrain[,2],pop) 
writesubmission(NewsTest$UniqueID,PredHTreeTest[,2], "3.new.rpart.csv" )

###### cross validation
numFoldsH = trainControl( method = "cv", number = 20 )
cpGridH = expand.grid( .cp = seq(0.0001,0.5,0.0005)) 
res.train=train(Popular ~ ., data = myTrain, method = "rpart", trControl = numFoldsH, tuneGrid = cpGridH )
# Create a new CART model
hTreeCV = rpart(Popular ~ ., data = HhrdayTrain, method="class", cp = 0.001)
#prp(StevensTreeCV)
PredHTreeTrain = predict(hTreeCV)
PredHTreeTest = predict(hTreeCV, newdata = myTest)
print('cv:');getAcc(PredHTreeTrain [,2],pop) 
writesubmission(NewsTest$UniqueID,pred.Test.rForest[,2], "4.new.crossvrf.noabstract.reducedvars.csv" )

#####gbm


fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 7,
  ## repeated ten times
  repeats = 7)

set.seed(825)
gbmFit1 <- train(Popular ~ ., data = myTrain,
                 method = "gbm",
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
mymodel=gbmFit1$finalModel
pred.Train.gbm  = predict(mymodel,n.trees=150,interaction.depth=3,shrinkage=0.1,type="response")
pred.Test.gbm  =predict(mymodel, newdata=myTest,n.trees=150,interaction.depth=3,shrinkage=0.1,type="response")

myTrain$Popular=NewsTrain$Popular
mygbm<-gbm(Popular ~ ., data = myTrain,n.trees=150,interaction.depth=3,shrinkage=0.1)
pred.Train.gbm  = predict(mygbm,n.trees=150,interaction.depth=3,shrinkage=0.1,type="response")
pred.Test.gbm  =predict(mygbm, newdata=myTest,n.trees=150,interaction.depth=3,shrinkage=0.1,type="response")


print('gbm:');getAcc(pred.Train.gbm,pop) 
writesubmission(NewsTest$UniqueID,pred.Test.gbm , "gbm.csv" )

#####################mean
myens2= (pred.Test.glm+pred.Test.rForest[,2]+pred.Test.gbm)/3
writesubmission(NewsTest$UniqueID,myens2, "99.average.with.gbm.csv" )
