install.packages("ggplot2")
rm(list = ls())
library(ggplot2)
library(class)


#import csv
heartdisease=read.csv(file.choose(),header=T, stringsAsFactors=TRUE)

#change target variable 'num' to binomial for logistic regression. 0-4 to 0-1
heartdisease$num <- ifelse(heartdisease$num > 0, 1, heartdisease$num)

# exploratory data analysis ---------------------------------------------------------------------

#look at first 6 rows of dataset
head(heartdisease)

#check rows and columns of dataset
dim(heartdisease) 

#show summary of the data of each column
str(heartdisease)

#show statistical summary of each column.
summary(heartdisease)

#show distribution of patients with heart diseases 
heartdisease_table = table(heartdisease$num)
heartBarplot = barplot(heartdisease_table, main="Distribution of Heart Disease 'num'", xlab="Heart Disease Category", ylab="Count", col=c("green", "red"))
text(x = heartBarplot, y = heartdisease_table, labels = heartdisease_table, pos = 1)
legend("topleft", legend = c("No Heart Disease", "Heart Disease"), fill = c("green", "red")) 

#distribution of gender
barplot(table(heartdisease$sex), main="Distribution of Gender", ylab="Count", xlab="Gender", col=c("pink", "blue"))

#distribution of age
plot(hist(heartdisease$age), main="Distribution of Age", xlab="Age", ylab="Count", col="green")

 #scatter plot to see correlation between age and max heart rate 
plot(heartdisease$age, heartdisease$thalch, main = "Age vs. Max Heart Rate Achieved", xlab = "Age", ylab = "Max Heart Rate", col = heartdisease$num + 1)
  legend("topright", legend = c("No Heart Disease", "Heart Disease"), fill = c("black", "red"), cex = 0.7, title = "Heart Disease Status")

#scatter plot of age and resting blood pressure
colors <- ifelse(heartdisease$num == 1, "red", "blue")
plot(heartdisease$age, heartdisease$trestbps, main="Scatter plot of Age vs. Resting Blood Pressure", xlab="Age", ylab="Resting Blood Pressure (trestbps)", pch=1, col=colors)
legend("topright", legend=c("No Heart Disease", "Heart Disease"), pch=1, col=c("blue", "red"), cex = 0.7)








#data pre processing ----------------------------------------------------------------------------------------------------------

#1. Remove ca , thal , slope column b/c too much missing data  results --> 16 to 12 columns
#   Also remove id column as its unncessary.
heartdisease$ca <- NULL
heartdisease$thal <- NULL
heartdisease$slope <- NULL
heartdisease$id <- NULL

dim(heartdisease) 
#2. Remove rows with missing values in columns where there is missing values

#2a) create list of columns with missing data
columns_to_check <- c("trestbps" , "chol" , "fbs" , "restecg" , "thalch" , "exang" , "oldpeak")

#2b) remove rows where there is missing value from list of columns  Result--> 920 to 741 rows
heartdisease <- heartdisease[!rowSums(is.na(heartdisease[, columns_to_check])) > 0, ]

dim(heartdisease) 
#3. one-hot encoding to create binary values for categorical columns-----------------------------------------------------------------

#3a) install library to help with the process of one-hot encoding
install.packages("fastDummies")
library(fastDummies)

#3b) create variable to hold columns to do one-hot encoding for
columns_to_encode <- c("dataset", "cp", "restecg")

#3c) apply one-hot encoding to dataset
heartdisease <- dummy_cols(heartdisease, select_columns = columns_to_encode)

#3d) drop original columns
heartdisease <- heartdisease[, !(names(heartdisease) %in% columns_to_encode )]
#3d) drop  columns to fix multicollinearity
heartdisease$restecg_ <- NULL
heartdisease$restecg_normal <- NULL

heartdisease$`dataset_VA Long Beach` <- NULL
heartdisease$`cp_typical angina` <- NULL


dim(heartdisease) 
#3e) encode these columns : sex , fbs , exang

  #Sex becomes Male = 1 , Female = 0
heartdisease$sex <- as.integer(heartdisease$sex == "Male")

  #fbs becomes TRUE = 1 , FALSE = 0
heartdisease$fbs <- as.integer(heartdisease$fbs == "TRUE")

  #exang becomes TRUE = 1 , FALSE = 0
heartdisease$exang <- as.integer(heartdisease$exang == "TRUE")



str(heartdisease)


#3. Use Scaling to standardize numerical columns--------------------------------------------------------------------

#3a) create list to store columns to scale
numerical_columns <- c("age", "trestbps" , "chol" , "thalch", "oldpeak")

#3b ) apply scaling to the columns
heartdisease[numerical_columns] <- scale(heartdisease[numerical_columns])

#created csv just to confirm everything looks good.
write.csv(heartdisease, "heartdisease_clean_data_preprocessed.csv", row.names = FALSE)




#-------------------------------*-*-*-*-*-*-*-* Model Building *-*-*-*-*-*-*-*-------------------------------------------------------------------

# First Model : Logistic Regression notes5--------------------------------------------------------------------------------------

#1. building the model 

logistic_regression_model_fit1 = glm(num ~ . , data=heartdisease, family = "binomial")


summary(logistic_regression_model_fit1)
exp(coef(logistic_regression_model_fit1))


#2. See model performance

    # 5-fold cross-validation
set.seed(1)
k=5
folds = sample(1:k, nrow(heartdisease), replace=TRUE)

accuracy = rep(0,k)

for (i in 1:k) {
  train_data <- heartdisease[folds != i,]
  test.truevalue <- heartdisease[folds == i,]
  
  logistic_regression_model_fit2 <- glm(num ~ . , data = train_data, family = "binomial")
  
  logistic_regression_model_probs <- predict(logistic_regression_model_fit2, newdata = test.truevalue, type = "response")
  logistic_regression_model_preds <- ifelse(logistic_regression_model_probs > 0.5, 1, 0)
  
  actual_value <- test.truevalue$num
  
  confusion_matrix_log_reg <- table(Predicted = logistic_regression_model_preds, Actual = actual_value)
  accuracy[i] <- sum(diag(confusion_matrix_log_reg)) / sum(confusion_matrix_log_reg)
}

  # 2a) Calculate the average cross-validation accuracy 
mean(accuracy)

  # 2b) show the confusion matrix to calculate Recall, Precision, Accuracy of the last fold,
confusion_matrix_log_reg


# Second  Model : KNN (K-Nearest Neighbors) notes5---------------------------------------------------------------------

#1. building the model 
set.seed(1)
accuracy=matrix(0,10,5)

folds = sample(1:5, nrow(heartdisease), replace=TRUE)

for (k in 1:10) {
  for (i in 1:5) {
    train_set <- heartdisease[folds != i,]
    test_set <- heartdisease[folds == i,]
    train_labels <- heartdisease$num[folds != i]
    test_labels <- heartdisease$num[folds == i]
    knn_pred <- knn(train = train_set, test = test_set, cl = train_labels, k = k)
    accuracy[k, i] <- mean(knn_pred == test_labels)
  }
}

#2. See model performance
# 2a) Calculate the average cross-validation accuracy 
cv.accuracy=apply(accuracy,1,mean)

mean(cv.accuracy)

# 2b) show the confusion matrix to calculate Recall, Precision, Accuracy of the last fold,
confusion_matrix_knn <- table(Predicted=knn_pred , Actual=test_labels)

confusion_matrix_knn


# Third  Model : Naive Bayes  Notes6---------------------------------------------------------------------
#load package for naive bayes
install.packages("e1071")
library(e1071)

#1. building the model 
set.seed(1)
accuracy <- rep(0, 5)
folds = sample(1:5, nrow(heartdisease), replace = TRUE)

for (i in 1:5) {
  # Create training and testing sets
  train_data <- heartdisease[folds != i, ]
  test_data <- heartdisease[folds == i, ]
  train_labels <- heartdisease$num[folds != i]
  test_labels <- heartdisease$num[folds == i]
  
  naive_bayes_model <- naiveBayes(num ~ ., data = train_data)
  
  naive_pred <- predict(naive_bayes_model, newdata = test_data)
  
  confusion_matrix <- table(Predicted = naive_pred, Actual = test_labels)
  
  accuracy[i] <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
}

#2. See model performance

# 5-fold cross-validation
mean_accuracy <- mean(accuracy)
mean_accuracy

# show the confusion matrix to calculate Recall, Precision, Accuracy,
confusion_matrix_nb <- table(Predicted = naive_pred, Actual = test_labels)
confusion_matrix_nb




# use the coefficients of variables in logistic regression to identify important features

summary(logistic_regression_model_fit1)
#summary shows there are 4 variables that are highly statistically significant with positive 
#coefficients --> sex, exang, oldpeak, and cp_asymptomatic
