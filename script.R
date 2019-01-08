setwd("~/Documents/FEUP/5A/1S/ML/Project/adult/adult/dataset.data")

Adult <- read.table("dataset.data", header = TRUE, quote = "\n", dec = ".", na.strings = "?", check.names=TRUE)
dim(Adult)
summary(Adult)

#missing values
#eliminate the ones with missing values (use Adult.complete)
Adult.complete <- na.omit(Adult)

#ratios

#balance
class2 <- Adult.complete[Adult.complete[,15] == ">50K",]
Adult.complete <- rbind(rbind(Adult.complete,class2),class2)
summary(Adult.complete)

#feature selection
#correlation between vars (including class)
Adult.factors <- data.frame(Adult.complete$age, Adult.complete$workclass, Adult.complete$fnlwgt, Adult.complete$education,
                            Adult.complete$educationnum, Adult.complete$maritalstatus, Adult.complete$ocupation, Adult.complete$relationship, 
                            Adult.complete$race, Adult.complete$sex, Adult.complete$capitalgain, Adult.complete$capitalloss, 
                            Adult.complete$hoursweek, Adult.complete$nativecountry, Adult.complete$class)

Adult.complete$class <- as.numeric(Adult.complete$class)
Adult.complete$workclass <- as.numeric(Adult.complete$workclass)
Adult.complete$education <- as.numeric(Adult.complete$education)
Adult.complete$maritalstatus <- as.numeric(Adult.complete$maritalstatus)
Adult.complete$ocupation <- as.numeric(Adult.complete$ocupation)
Adult.complete$relationship <- as.numeric(Adult.complete$relationship)
Adult.complete$race <- as.numeric(Adult.complete$race)
Adult.complete$sex <- as.numeric(Adult.complete$sex)
Adult.complete$nativecountry <- as.numeric(Adult.complete$nativecountry)

Adult.numeric <- data.frame(Adult.complete$age, Adult.complete$workclass, Adult.complete$fnlwgt, Adult.complete$education,
                       Adult.complete$educationnum, Adult.complete$maritalstatus, Adult.complete$ocupation, Adult.complete$relationship, 
                       Adult.complete$race, Adult.complete$sex, Adult.complete$capitalgain, Adult.complete$capitalloss, 
                       Adult.complete$hoursweek, Adult.complete$nativecountry, Adult.complete$class)

coors <- cor(Adult.numeric)
colnames(coors) <- colnames(Adult)

rownames(coors) <- colnames(Adult)
library(corrplot)

corrplot(coors, type="upper", order="hclust", tl.col="black")
colnames(Adult.factors) <- colnames(Adult.complete)

Adult.numeric <- data.frame(Adult.complete$age, Adult.complete$education,
                       Adult.complete$educationnum, Adult.complete$maritalstatus, Adult.complete$ocupation, Adult.complete$relationship, 
                       Adult.complete$race, Adult.complete$capitalgain, Adult.complete$capitalloss, 
                       Adult.complete$hoursweek, Adult.complete$class)

Adult.factors <- data.frame(Adult.factors$age, Adult.factors$education,
                            Adult.factors$educationnum, Adult.factors$maritalstatus, Adult.factors$ocupation, Adult.factors$relationship, 
                            Adult.factors$race, Adult.factors$capitalgain, Adult.factors$capitalloss, 
                            Adult.factors$hoursweek, Adult.factors$class)

#take out the feature "sex" because it has a strong correlation with the feature "relationship" and has a lower correlation with "class" than the latter


##############################
#### STATISTICAL ANALYSIS ####
##############################

library('psych')

psych::describe(Adult.numeric)


#summary
summary(Adult.factors) 


##############################
####### Training / Test ######
##############################

#TODO: check differences without oversampling the test sample

t <-sample(1:nrow(Adult.numeric),round(0.02*nrow(Adult.numeric),digits=0))
t2 <-sample(1:nrow(Adult.numeric),round(0.002*nrow(Adult.numeric),digits=0))
traindata.numeric <-Adult.numeric[t, ]
testdata.numeric <-Adult.numeric[t2, ]

traindata <-Adult.factors[t, ] #factor
testdata <-Adult.factors[t2, ]


#### Algorithms #####

###### Linear #######

######## SVM ########

library('e1071')

traindata.n <- data.frame(scale(traindata.numeric$Adult.complete.age), scale(traindata.numeric$Adult.complete.education),
                          scale(traindata.numeric$Adult.complete.educationnum), scale(traindata.numeric$Adult.complete.maritalstatus), scale(traindata.numeric$Adult.complete.ocupation),
                          scale(traindata.numeric$Adult.complete.relationship), 
                          scale(traindata.numeric$Adult.complete.race), scale(traindata.numeric$Adult.complete.capitalgain), scale(traindata.numeric$Adult.complete.capitalloss), 
                          scale(traindata.numeric$Adult.complete.hoursweek), traindata.numeric$Adult.complete.class)

testdata.n <- data.frame(scale(testdata.numeric$Adult.complete.age), scale(testdata.numeric$Adult.complete.education),
                          scale(testdata.numeric$Adult.complete.educationnum), scale(testdata.numeric$Adult.complete.maritalstatus), scale(testdata.numeric$Adult.complete.ocupation),
                         scale(testdata.numeric$Adult.complete.relationship), 
                          scale(testdata.numeric$Adult.complete.race), scale(testdata.numeric$Adult.complete.capitalgain), scale(testdata.numeric$Adult.complete.capitalloss), 
                          scale(testdata.numeric$Adult.complete.hoursweek), testdata.numeric$Adult.complete.class)

traindata.n[traindata.n$traindata.numeric.Adult.complete.class == 2, 10] <- -1 

testdata.n[testdata.n$testdata.numeric.Adult.complete.class == 2, 10] <- -1 

colnames(traindata.n) <- c("age", "education", "educationnum", "maritalstatus", "ocupation","relationship", "race", "capitalgain", "capitalloss", "hoursweek", "class" )
colnames(testdata.n) <- c("age", "education", "educationnum", "maritalstatus", "ocupation", "relationship", "race", "capitalgain", "capitalloss", "hoursweek", "class" )


model.train <- as.matrix(traindata.n)
model.test <- as.matrix(testdata.n)

for (C in 10^seq(-2,3))
{
  modelmenor35linear <- svm(model.train[,-11], model.train[,11], 
                            type="C-classification", 
                            cost=C, kernel="linear", 
                            scale = FALSE)
  pred <- predict(modelmenor35linear,model.train[,-11])
  t_true <- model.train[,11]
  table(pred,t_true)
  print(paste("C:",C,"  error:",(sum(pred != t_true)/length(t_true))*100,"%"))
}


pred <- predict(modelmenor35linear,model.test[,-11])
t_true <- model.test[,11]
table(pred,t_true)
print(paste("C:",C,"  error:",(sum(pred != t_true)/length(t_true))*100,"%"))

######## MLP
library(stats)
library(MASS)
library(nnet)
library(caret)
library(rlist)

train.numeric <- traindata.numeric

train.numeric$Adult.complete.class <- as.factor(train.numeric$Adult.complete.class)

sizes <- seq(2,20,by=2)
trc <- trainControl(method="repeatedcv", number=1, repeats=1)
model.10x10CV <- train (Adult.complete.class ~ ., train.numeric, method='nnet', maxit = 300, trace = FALSE,
                        tuneGrid = expand.grid(.size=sizes,.decay=0), trControl=trc)
save(model.10x10CV, file = "cv1.adult")
load ("cv1.adult")

bestsize <- as.integer(model.10x10CV$bestTune[1])

decays <- 10^seq(-3,0,by=0.05)

model.10x10CV.2 <- train (Adult.factors.class ~ ., traindata, method='nnet', maxit = 300, trace = FALSE,
                        tuneGrid = expand.grid(.size=bestsize,.decay=decays), trControl=trc)
save(model.10x10CV.2, file = "cv2.adult")
load("cv2.adult")


####### Naive Bayes #####

model <- naiveBayes(Adult.factors.class ~ ., data = traindata)
pred <- predict(model, traindata[,-11])
t_true <- traindata[,11]
tablet <- table(pred,t_true)
e <- 100*(1-sum(diag(tablet))/nrow(traindata))

####### LogReg #####

library(aod)

logit <- glm(Adult.factors.class ~., data = traindata, family = "binomial")
suppressWarnings(logit.AIC <- step (logit))
summary(logit.AIC)
(prediccio <- predict(logit.AIC, data=testdata))

logit.accs <- function (P=0.5)
{
  ## Compute accuracy in learning data
  
  logit.AICpred <- NULL
  P <- 0.5
  
  logit.AICpred[logit.AIC$fitted.values<P] <- 0
  logit.AICpred[logit.AIC$fitted.values>=P] <- 1
  
  logit.AICpred <- factor(logit.AICpred, labels=c("<=50k",">50k"))
  
  print(M1.TRtable <- table(Truth=traindata$Adult.factors.class,Pred=logit.AICpred))
  nlearn <- nrow(traindata)
  print(100*(1-sum(diag(M1.TRtable))/nlearn))
  
  ## Compute accuracy in test data
  
  gl1t <- predict(logit.AIC, newdata=testdata,type="response")
  gl1predt <- NULL
  gl1predt[gl1t<P] <- 0
  gl1predt[gl1t>=P] <- 1
  
  gl1predt <- factor(gl1predt, labels=c("nonspam","spam"))
  
  print(M1.TEtable <- table(Truth=testdata$Adult.factors.class,Pred=gl1predt))
  ntest <- nrow(testdata)
  print(100*(1-sum(diag(M1.TEtable))/ntest))
}

logit.accs()


###### Non-linear ######

####### Random-Forest 

#install.packages("TunePareto")
#install.packages("tree")
#install.packages('randomForest')

library("TunePareto")
library("tree")
library('randomForest')

(ntrees <- round(10^seq(1,3.5, by=0.4)))
rf.results <- matrix (rep(0,2*length(ntrees)),
                      nrow=length(ntrees))
colnames (rf.results) <- c("ntrees", "OOB")
rf.results[,"ntrees"] <- ntrees
rf.results[,"OOB"] <- 0

ii <- 1

for (nt in ntrees)
{ 
  print(nt)
  
  model.rf <- randomForest(Adult.factors.class ~ ., data =  traindata , ntree=nt, proximity=FALSE)
  
  # get the OOB
  rf.results[ii,"OOB"] <- model.rf$err.rate[nt,1]
  
  ii <- ii+1
}


lowest.OOB.error <- as.integer(which.min(rf.results[,"OOB"]))
(ntrees.best <- rf.results[lowest.OOB.error,"ntrees"])

model.rf <- randomForest(Adult.factors.class ~ ., data =  traindata , ntree=ntrees.best, proximity=FALSE)


pred.rf.final <- predict (model.rf, 
                          testdata, 
                          type="class")

(ct <- table(Truth=testdata$Adult.factors.class, 
             Pred=pred.rf.final))


round(100*(1-sum(diag(ct))/sum(ct)),2)

####### SVM Radial ####
gammas = c(0,0.01,0.02,0.03,0.04,0.05)

tuned_parameters.radial <- tune.svm(Adult.factors.class ~ ., data =  traindata, gamma = gammas, cost = 10^(-2:1), kernel = 'radial')

pred.svmr.final <- predict (tuned_parameters.radial$best.model, 
                            traindata, 
                          type="class")

(ct <- table(Truth=traindata$Adult.factors.class, 
             Pred=pred.svmr.final))


round(100*(1-sum(diag(ct))/sum(ct)),2)
