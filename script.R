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
                       Adult.complete$educationnum, Adult.complete$maritalstatus, Adult.complete$relationship, 
                       Adult.complete$race, Adult.complete$capitalgain, Adult.complete$capitalloss, 
                       Adult.complete$hoursweek, Adult.complete$class)

Adult.factors <- data.frame(Adult.factors$age, Adult.factors$education,
                            Adult.factors$educationnum, Adult.factors$maritalstatus, Adult.factors$relationship, 
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

model.train <- as.matrix(traindata.numeric)

for (C in 10^seq(-2,3))
{
  modelmenor35linear <- svm(model.train[,-10], model.train[,10], 
                            type="C-classification", 
                            cost=0.1, kernel="linear", 
                            scale = FALSE)
  pred <- predict(modelmenor35linear,model.train[,-10])
  t_true <- model.train[,10]
  table(pred,t_true)
  print(paste("C:",C,"  error:",(sum(pred != t_true)/length(t_true))*100,"%"))
}

######## MLP

###### Non-linear ######
