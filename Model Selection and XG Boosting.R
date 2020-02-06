
############# Data Preprocessing ############################
#Checking the missing values
sum(is.na(stats))
stats$Age = ifelse(is.na(stats$Age),
                   ave(stats$Age, FUN = function(x) mean(x, na.rm = TRUE)),
                   stats$Age)


#Encoding/changing categorical values as factors

stats$Sex = factor(stats$Sex,
                       levels = c('male','female'),
                       labels = c(0 , 1))

stats$Sex <- as.numeric(factor(stats$Sex))
############# k-Fold Cross Validation #######################

# Importing the dataset
stats = read.csv('Titanictest.csv')

# Encoding the target feature as factor
stats$Survived = factor(stats$Survived, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(stats$Survived, SplitRatio = 0.75)
training_set = subset(stats, split == TRUE)
test_set = subset(stats, split == FALSE)
as.numeric(Sex)
# Feature Scaling
training_set[-5] = scale(training_set[-5])
test_set[-5] = scale(test_set[-5])

# Fitting Kernel SVM to the Training set
# install.packages('e1071')
library(e1071)
classifier = svm(formula = Survived ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'radial')
summary(classifier)
# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-5])
y_pred
# Making the Confusion Matrix
cm = table(test_set[, 5], y_pred)
cm
Accuracy=(cm[1]+cm[4])/(cm[1]+cm[4]+cm[3]+cm[2])
Accuracy
#Accuracy for svm = 76 %

# Applying k-Fold Cross Validation
# install.packages('caret')
library(caret)
folds = createFolds(training_set$Survived, k = 10)
#Mentioning k = 10 as we are doing 10 folds in this dataset
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  classifier = svm(formula = Survived ~ .,
                   data = training_fold,
                   type = 'C-classification',
                   kernel = 'radial')
  y_pred = predict(classifier, newdata = test_fold[-5])
  cm = table(test_fold[, 5], y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})
accuracy = mean(as.numeric(cv))
accuracy
#Accuracy for K-fold cross validation = 81 %

# Applying Grid Search to find the best parameters
#Here we are choosing kernel svm model.
# install.packages('caret')
library(caret)
classifier = train(form = Survived ~ ., data = training_set, method = 'svmRadial')
classifier
classifier$bestTune

################ XG Boost #######################
stats = read.csv('Titanictest.csv')
sum(is.na(stats))
stats$Age = ifelse(is.na(stats$Age),
                   ave(stats$Age, FUN = function(x) mean(x, na.rm = TRUE)),
                   stats$Age)


#Encoding/changing categorical values as factors

stats$Sex = as.numeric(factor(stats$Sex,
                   levels = c('male','female'),
                   labels = c(0 , 1)))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(stats$Survived, SplitRatio = 0.8)
training_set = subset(stats, split == TRUE)
test_set = subset(stats, split == FALSE)

# Fitting XGBoost to the Training set
# install.packages('xgboost')
library(xgboost)
classifier = xgboost(data = as.matrix(training_set[-5]), label = training_set$Survived, nrounds = 10)
#n rounds=10, number of iterations. 
# Applying k-Fold Cross Validation
# install.packages('caret')
library(caret)
folds = createFolds(training_set$Survived, k = 10)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  classifier = xgboost(data = as.matrix(training_set[-5]), label = training_set$Survived, nrounds = 10)
  y_pred = predict(classifier, newdata = as.matrix(test_fold[-5]))
  y_pred = (y_pred >= 0.5)
  cm = table(test_fold[, 5], y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})
accuracy = mean(as.numeric(cv))
accuracy











