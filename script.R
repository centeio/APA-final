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
                       Adult.complete$race, Adult.complete$sex, Adult.complete$capitalgain, Adult.complete$capitalloss, 
                       Adult.complete$hoursweek, Adult.complete$class)

Adult.factors <- data.frame(Adult.factors$age, Adult.factors$education,
                            Adult.factors$educationnum, Adult.factors$maritalstatus, Adult.factors$relationship, 
                            Adult.factors$race, Adult.factors$sex, Adult.factors$capitalgain, Adult.factors$capitalloss, 
                            Adult.factors$hoursweek, Adult.factors$class)

summary(Adult.factors)



