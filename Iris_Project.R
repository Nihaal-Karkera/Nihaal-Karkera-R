getwd()

#Load the data
library(datasets)
data(iris)
View (iris)

#Head & Tail
head(iris, 4)
tail(iris, 4)

#Display the Summary Statistics for the data
summary(iris)
summary(iris$Petal.Length)

#Check for missing Values
sum(is.na(iris))

#Detailed Summary Statistics
library(skimr)
skim(iris)

#Group data by species
iris %>%
  dplyr::group_by(Species) %>%
  skim()

#Data vizualization

#1. Panel Plot
plot(iris, col = "red")

#2. Scatter Plot
plot(iris$Sepal.Width, iris$Sepal.Length, col="red",
     xlab="sepal width",ylab = "sepal length")

#3. Histogram
hist(iris$Sepal.Width, col = "red", main = "Histogram")

#4. Feature Plot
library(caret)
featurePlot (x = iris[,1:4],
             y = iris$Species,
             plot="box",
             strip = strip.custom (par.strip.text = list(cex=.7)),
             scales = list(x = list(relation = "free"),
                           y = list(relation = "free")))

#Making a Classification Model 
set.seed(1997)

#split the data into training and test
TrainingIndex <- createDataPartition(iris$Species, p=0.8, list = FALSE)
TrainingSet <- iris[TrainingIndex,]
TestingSet <- iris[-TrainingIndex,]

plot(TrainingSet$Sepal.Width, TrainingSet$Sepal.Length, col="blue",
     xlab="sepal width",ylab = "sepal length")

plot(TestingSet$Sepal.Width, TestingSet$Sepal.Length, col="red",
     xlab="sepal width",ylab = "sepal length")

#I am using an SVM Model
# Build Training Model
Model <- train(Species ~.,data = TrainingSet,
               method = 'svmPoly',
               na.action = na.omit,
               preProcess=c("scale","center"),
               trControl = trainControl(method="none"),
               tuneGrid = data.frame(degree=1, scale=1,C=1))

#Build CV model
Model.cv <- train(Species ~.,data = TrainingSet,
                  method = 'svmPoly',
                  na.action = na.omit,
                  preProcess=c("scale","center"),
                  trControl = trainControl(method="cv",number=10),
                  tuneGrid = data.frame(degree=1, scale=1,C=1))
                  
#Apply Model for Prediction

Model.training <- predict(Model, TrainingSet)
Model.testing <- predict(Model, TestingSet)
Model.cv <- predict(Model.cv, TrainingSet)

# Display Model Performance

Model.training.confusion <- confusionMatrix(Model.training, TrainingSet$Species)
Model.testing.confusion <- confusionMatrix(Model.testing, TestingSet$Species)
Model.cv.confustion <- confusionMatrix(Model.cv, TrainingSet$Species)

print(Model.training.confusion)
print(Model.testing.confusion)
print(Model.cv.confustion)

#Importance of Variables for predictions

Importance <- varImp(Model)
plot(Importance)

