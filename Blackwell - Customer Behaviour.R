## set repository and load data
setwd("C:/Users/Gebruiker/Downloads")
complete <- read.csv("CompleteResponses.csv")
incomplete <- read.csv("SurveyIncomplete.csv")

## get yo packages
library(ggplot2)

str(complete)
str(incomplete)

summary(incomplete)

## set car type as  factor 
complete$car <- factor(complete$car)
incomplete$car <- as.factor(incomplete$car)

## set elevel as !! ordinal !! factor
complete$elevel <- factor(complete$elevel,
                             order = TRUE,
                             levels = c("0", "1", "2", "3", "4"))
incomplete$elevel <- as.ordered(incomplete$elevel)

## set zipcode as factor
complete$zipcode <- as.factor(complete$zipcode)
incomplete$zipcode <- as.factor(incomplete$zipcode)

## set brand as factor 
complete$brand <- as.factor(complete$brand)
incomplete$brand <- as.factor(incomplete$brand)

# library(plyr)
complete$brand <- revalue(complete$brand, c("0"="Acer", "1"="Sony"))


### bin salary into 13 groups of 10 000
# install.packages("mltools")
# library(mltools)
#
#complete$salary <- bin_data(complete$salary, 
 #                           bins = 13, 
  #                          binType = "explicit",
   #                         boundaryType = "lcro]",
    #                        returnDT = FALSE,)


#incomplete$salary <- bin_data(incomplete$salary, 
 #                           bins = 13, 
  #                          binType = "explicit",
   #                         boundaryType = "lcro]",
    #                        returnDT = FALSE,)


   ### AGE ###
  # SET as NUM for BOXPLOT

# complete$age <- as.integer(complete$age)
# str(complete$age)

## bin age into 6/3 groups of 10/20
complete$age <- bin_data(complete$age, 
                            bins = 3, 
                            binType = "explicit",
                            boundaryType = "lcro]",
                            returnDT = FALSE,)


incomplete$age <- bin_data(incomplete$age, 
                              bins = 3, 
                              binType = "explicit",
                              boundaryType = "lcro]",
                              returnDT = FALSE,)
str(complete$age)

  #########################
 ### plot some shhhiii ###
#########################

 ### BOXPLOT ###
# SALARY per ZIP #
ggplot(complete, aes(zipcode, salary))+ 
  geom_boxplot()

#  AGE per ZIP   #
ggplot(complete, aes(zipcode, age))+ 
  geom_boxplot()

#  SALARY per AGE   #
ggplot(complete, aes(age, salary))+ 
  geom_boxplot()



#  SALARY per BRAND   #
ggplot(complete, aes(salary, brand))+ 
  geom_boxplot()


ggplot(complete, aes(brand))+ 
  geom_bar()


###   HISTO   ###
# not working anymore, prop because of bins (prev: working but ugly)
par(mfrow=c(1, 2))

ggplot(complete, aes(x = brand)) +
  geom_bar(color="darkblue", fill="lightblue")+
  xlab("Actual Customer Brand Preference")+
  ylab(NULL)

?ylab

ggplot(tuned.prediction, aes(x = predict.new.brands)) +
  geom_bar(color="darkblue", fill="lightblue")+
  xlab("Predicted Customer Brand Preference")+
  ylab(NULL)
    
  
str(tuned.prediction)


 ###   scatter ###
## scatterplot : age - salary - brand   ###   WU-TANG   ###
ggplot(complete, aes(age,
                     salary,
                     color = brand)) +
  geom_jitter()

install.packages("data.table")
library(data.table)


###### TRY THIS FCKN COLORiNG FOR CAR #5 PLS ####
ggplot(complete, aes(age,
                     salary,
                     color = (car = "5"))) +
  geom_jitter()


help(fifelse)


### doesnt add much ###
ggplot(complete, aes(brand, 
                     salary,
                     color = zipcode))+ 
  geom_jitter()


## neither does this ##
ggplot(complete, aes(elevel, 
                    salary, 
                    color = brand))+
  geom_jitter()


       ########################
      ## RUN MODELS 'CARET' ##
     ########################

### get yo packs ###
install.packages("caret")
library(caret)
install.packages("mlbench")
library(mlbench)

### set seed ###
set.seed(123)


####    TRAIN SET    #### 

### Adjusted Set ###
adjusted <- subset( complete,
                    select = -c(elevel, car, zipcode, credit))

head(adjusted)

# trainset, y=outcome data, p = percentage of data in trainingset,don't list result 
inTrain <- createDataPartition(y = complete$brand, p = .75, list = FALSE)

inTrain.a <- createDataPartition(y = adjusted$brand, p = .75, list = FALSE)


## The format of the results
## The output is a set of integers for the rows of complete
## that belong in the training set.
str(inTrain)

training <- complete[inTrain, ]
testing <- complete[-inTrain, ]

training.a <- adjusted[inTrain.a, ]
testing.a <- adjusted[-inTrain.a, ]

##  install pls package to make function below work
install.packages(pls)
library(pls)

##modify resampling method : repeatedcv = K-fold Cross Val
ctrl <- trainControl(method = "repeatedcv",
                     verboseIter = TRUE,
                     repeats = 3)

#mtry <- 2
tunegrid <- expand.grid(.mtry = 5)

## use train function to tune algorithm/model
model.c5.a <- train(brand ~ .,
                data = training.a,
                method = "C5.0",
                tuneLength = 2,
                #tuneGrid = tunegrid,
                trControl = ctrl,
                preProc = c("center", "scale"))


model.c5.a
plot(model.c5.a)

varImp(model.rf.a)
plot(varImp(model.c5.a))

help(labels)


## predict new classes ##

predict.brands.rf.a <- predict(model.rf.a, newdata = testing.a)
str(predict.brands.rf.a)


## predict probability new samples ##

probs.brands.a.rf <- predict(model.rf.a,
                              newdata = testing.a,
                              type = "prob")
head(probs.brands.a)


### confusion matrix ####

confusionMatrix(data = predict.brands.rf.a, testing.a$brand)


#### Jitterplot that those prediction values : age  - salary - pred values

test.and.pred.rf.a <-cbind(testing.a, predict.brands.rf.a)

ggplot(test.and.pred.rf.a, aes(age,
                     salary,
                     color = interaction(brand,
                                         predict.brands.rf.a,
                                         sep = " - "),
                     size = 2)) +
  geom_jitter()+
  labs(color = "Brand: Actual - Predicted")+
  guides(size = FALSE)


  ##############################
 #### RUN THEM PREDICTIONS ####
##############################


###   prep for nice xls file   ###
predict.new.brands <- predict(model.c5.a, newdata = incomplete)
pred.incomplete <- cbind(incomplete, predict.new.brands) 

str(pred.incomplete)
tuned.prediction <- subset( pred.incomplete,
                    select = c(salary, age, predict.new.brands))



####    scatterplot of prediction    ####
ggplot(pred.incomplete, aes(age,
                               salary,
                               color = predict.new.brands)) +
  geom_jitter()+
  labs(color = "Brand")


###  postResample   ###
postResample(test.and.pred.a$predict.brands.c5.a, testing.a$brand)



#### how many people want Sony/Acer?

summary(predict.brands.c5.a)
summary(complete$brand)


#####    t.test    #####

num.compl.brand <- revalue(complete$brand, c("Acer"="0", "Sony"="1"))
num.compl.brand <- as.numeric(num.compl.brand)

num.pred.brand <- revalue(predict.brands.c5.a, c("Acer"="0", "Sony"="1"))
num.pred.brand <- as.numeric(num.pred.brand)


t.test(x = num.compl.brand,
       y = num.pred.brand)

(1.621742  + 1.615198) / 2

t.test(x = num.pred.brand,
       mu = 1.62)




1522 + 952
2474/100
1522/24.74
952/24.74


3744 + 6154 
9898/100
6154/98.98

?t.test
## more functions
library(e1071)
## more
library(klaR)

