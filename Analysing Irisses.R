library("readr")

#set repository
setwd ("C:/Users/Gebruiker/Downloads/R Tutorial Data/R Tutorial Data Sets")

#read data set
IrisDataset <- read.csv("iris.csv")

attributes(IrisDataset)

summary(IrisDataset)

str(IrisDataset)


names(IrisDataset)

hist(IrisDataset$Species)
class(IrisDataset$Species)
barplot(table(IrisDataset$Species))

#restore split screen
par(mfrow=c(1,1))

#alternative for historgram 
barplot(table(IrisDataset$Species))

plot(IrisDataset$Sepal.Length)

qqnorm(IrisDataset$Sepal.Width)
help("qqnorm")

IrisDataset$Species<- as.numeric(IrisDataset$Species)
set.seed(123)
trainSize <- round(nrow(IrisDataset) * 0.8)
testSize <- nrow(IrisDataset) - trainSize

#warning: In Ops.factor(left, right) : ‘-’ not meaningful for factors ??
#species is no longer a factor
#take warning into account for later class changes
summary(IrisDataset$Species)
class(IrisDataset$Species)
levels(IrisDataset$Species)

trainSize
testSize

training_indices<-sample(seq_len(nrow(IrisDataset)),size=trainSize)

trainSet <- IrisDataset[training_indices, ]
testSet <- IrisDataset[-training_indices, ]

set.seed(123)

trainSet <- IrisDataset[training_indices, ]
testSet <- IrisDataset[-training_indices, ]

LinearModel<- lm(Petal.Width ~ Petal.Length, trainSet)
help(lm)

summary(LinearModel)

prediction<-predict(LinearModel, testSet)
prediction


#split plotting screen
par(mfrow=c(1, 2))

# plot prediction speed next to actual speed 
scatter.smooth(x=testSet$Petal.Width, y=testSet$Petal.Length, 
               main="Actual Width")
scatter.smooth(x=testSet$Petal.Width, y=prediction, 
               main="Predicted Widht")

#split plotting screen
par(mfrow=c(1, 1))
scatter.smooth(prediction ~testSet$Petal.Length,
               main="Prediction to actual length",
               ylab = "predicted length of petal",
               xlab = "actual length of Petal (in testset)")

#perfect correlation
cor(prediction, testSet$Petal.Length)

