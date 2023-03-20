install.packages("corrplot")
library(ggplot2)
library(corrplot)


# Load the heart_failure.csv dataset
data <- read.csv("C:/Users/74054/Desktop/heart_failure.csv")

#Get cor coef
cor_matrix <- cor(data)
corrplot(cor_matrix, type = "upper", order = "hclust", tl.col = "black", 
         tl.srt = 45, tl.cex = 0.8, method = "circle", col = colorRampPalette(c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#ffffff", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061"))(200))


# Summary statistics for numeric variables
summary(data[, c("age", "creatinine_phosphokinase", "ejection_fraction", "platelets", "serum_creatinine", "serum_sodium", "time")])

# Distribution of age variable
hist(data$age, main = "Distribution of Age", xlab = "Age")

# Scatterplot matrix of numeric variables
pairs(data[, c("age", "creatinine_phosphokinase", "ejection_fraction", "platelets", "serum_creatinine", "serum_sodium", "time")])

# Bar chart of categorical variables
barplot(table(data$anaemia), main = "Anaemia Diagnosis", xlab = "Anaemia", ylab = "Frequency")
barplot(table(data$diabetes), main = "Diabetes Diagnosis", xlab = "Diabetes", ylab = "Frequency")
barplot(table(data$high_blood_pressure), main = "High Blood Pressure Diagnosis", xlab = "High Blood Pressure", ylab = "Frequency")
barplot(table(data$sex), main = "Gender Distribution", xlab = "Sex", ylab = "Frequency")
barplot(table(data$smoking), main = "Smoking Habits", xlab = "Smoker", ylab = "Frequency")

# Boxplots of numeric variables by fatal_mi status
boxplot(age ~ fatal_mi, data = data, main = "Age by Fatal MI Status", xlab = "Fatal MI", ylab = "Age")
boxplot(creatinine_phosphokinase ~ fatal_mi, data = data, main = "CPK by Fatal MI Status", xlab = "Fatal MI", ylab = "CPK")
boxplot(ejection_fraction ~ fatal_mi, data = data, main = "Ejection Fraction by Fatal MI Status", xlab = "Fatal MI", ylab = "Ejection Fraction")
boxplot(platelets ~ fatal_mi, data = data, main = "Platelets by Fatal MI Status", xlab = "Fatal MI", ylab = "Platelets")
boxplot(serum_creatinine ~ fatal_mi, data = data, main = "Serum Creatinine by Fatal MI Status", xlab = "Fatal MI", ylab = "Serum Creatinine")
boxplot(serum_sodium ~ fatal_mi, data = data, main = "Serum Sodium by Fatal MI Status", xlab = "Fatal MI", ylab = "Serum Sodium")
boxplot(time ~ fatal_mi, data = data, main = "Follow-up Time by Fatal MI Status", xlab = "Fatal MI", ylab = "Follow-up Time")







library(readxl)
heart_failure <- read.csv("C:/Users/74054/Desktop/heart_failure.csv")
View(heart_failure)
heart_failure

library(ggplot2)
library(tidyverse) # for data manipulation and visualization
library(gridExtra) # for grid layout of plots


# Explore the structure of the dataset
str(heart_failure)

# Summary of the dataset
summary(heart_failure)


# Distribution of the target variable
ggplot(heart_failure, aes(x = factor(fatal_mi))) + 
  geom_bar() + 
  labs(x = "Fatal Myocardial Infarction", y = "Count")

# Histograms of the numerical features
hist1 <- ggplot(heart_failure, aes(x = age)) + 
  geom_histogram(binwidth = 5, fill = "dodgerblue", color = "black") + 
  labs(x = "Age", y = "Count")

hist2 <- ggplot(heart_failure, aes(x = creatinine_phosphokinase)) + 
  geom_histogram(binwidth = 500, fill = "dodgerblue", color = "black") + 
  labs(x = "CPK", y = "Count")

hist3 <- ggplot(heart_failure, aes(x = ejection_fraction)) + 
  geom_histogram(binwidth = 5, fill = "dodgerblue", color = "black") + 
  labs(x = "Ejection Fraction", y = "Count")

hist4 <- ggplot(heart_failure, aes(x = platelets)) + 
  geom_histogram(binwidth = 50, fill = "dodgerblue", color = "black") + 
  labs(x = "Platelets", y = "Count")

hist5 <- ggplot(heart_failure, aes(x = serum_creatinine)) + 
  geom_histogram(binwidth = 0.5, fill = "dodgerblue", color = "black") + 
  labs(x = "Serum Creatinine", y = "Count")

hist6 <- ggplot(heart_failure, aes(x = serum_sodium)) + 
  geom_histogram(binwidth = 1, fill = "dodgerblue", color = "black") + 
  labs(x = "Serum Sodium", y = "Count")

grid.arrange(hist1, hist2, hist3, hist4, hist5, hist6, ncol = 2)

# Box plots of the numerical features
box1 <- ggplot(heart_failure, aes(x = factor(fatal_mi), y = age)) + 
  geom_boxplot(fill = "dodgerblue", color = "black") + 
  labs(x = "Fatal Myocardial Infarction", y = "Age")

box2 <- ggplot(heart_failure, aes(x = factor(fatal_mi), y = creatinine_phosphokinase)) + 
  geom_boxplot(fill = "dodgerblue", color = "black") + 
  labs(x = "Fatal Myocardial Infarction", y = "CPK")

box3 <- ggplot(heart_failure, aes(x = factor(fatal_mi), y = ejection_fraction)) + 
  geom_boxplot(fill = "dodgerblue", color = "black") + 
  labs(x = "Fatal Myocardial Infarction", y = "Ejection Fraction")


box4 <- ggplot(heart_failure, aes(x = factor(fatal_mi), y =platelets)) +
  geom_boxplot(fill = "dodgerblue", color = "black") +
  labs(x = "Fatal Myocardial Infarction", y = "Platelets")

box5 <- ggplot(heart_failure, aes(x = factor(fatal_mi), y = serum_creatinine)) +
  geom_boxplot(fill = "dodgerblue", color = "black") +
  labs(x = "Fatal Myocardial Infarction", y = "Serum Creatinine")

box6 <- ggplot(heart_failure, aes(x = factor(fatal_mi), y = serum_sodium)) +
  geom_boxplot(fill = "dodgerblue", color = "black") +
  labs(x = "Fatal Myocardial Infarction", y = "Serum Sodium")

grid.arrange(box1, box2, box3, box4, box5, box6, ncol = 2)

bar1 <- ggplot(heart_failure, aes(x = factor(fatal_mi), fill = factor(anaemia))) +
  geom_bar(position = "fill") +
  labs(x = "Fatal Myocardial Infarction", y = "Proportion", fill = "Anaemia")

bar2 <- ggplot(heart_failure, aes(x = factor(fatal_mi), fill = factor(diabetes))) +
  geom_bar(position = "fill") +
  labs(x = "Fatal Myocardial Infarction", y = "Proportion", fill = "Diabetes")

bar3 <- ggplot(heart_failure, aes(x = factor(fatal_mi), fill = factor(high_blood_pressure))) +
  geom_bar(position = "fill") +
  labs(x = "Fatal Myocardial Infarction", y = "Proportion", fill = "High Blood Pressure")

bar4 <- ggplot(heart_failure, aes(x = factor(fatal_mi), fill = factor(sex))) +
  geom_bar(position = "fill") +
  labs(x = "Fatal Myocardial Infarction", y = "Proportion", fill = "Sex")

bar5 <- ggplot(heart_failure, aes(x = factor(fatal_mi), fill = factor(smoking))) +
geom_bar(position = "fill") +
labs(x = "Fatal Myocardial Infarction", y = "Proportion", fill = "Smoking")

grid.arrange(bar1, bar2, bar3, bar4, bar5, ncol = 2)

library(caret)
library(ranger)

library(glmnet)
library(pROC)


# Split the dataset into training and testing sets

heart_failure <- read.csv("C:/Users/74054/Desktop/heart_failure.csv")

# Split the dataset into training and testing sets
set.seed(666)
train_index <- createDataPartition(heart_failure$fatal_mi, p = 0.7, list = FALSE)
train_data <- heart_failure[train_index, ]
test_data <- heart_failure[-train_index, ]


# Train a random forest classifier on the training data
rf_model <- ranger(factor(fatal_mi) ~ ., data = train_data, num.trees = 500)

# Predict the target variable on the test data
test_pred <- predict(rf_model, data = test_data)$predictions

# Evaluate the model performance on the test data
conf_mat <- confusionMatrix(test_pred, factor(test_data$fatal_mi))
conf_mat



data<-heart_failure

train_index <- sample(1:nrow(data), 0.7 * nrow(data))
train <- data[train_index, ]
test <- data[-train_index, ]

# Fit a logistic regression model with cross-validation
fit <- cv.glmnet(as.matrix(train[, -13]), train$fatal_mi, family = "binomial")

# Plot the ROC curve and calculate the AUC on the test set
prob <- predict(fit, newx = as.matrix(test[, -13]), s = "lambda.min", type = "response")
roc <- roc(test$fatal_mi, prob)
plot(roc, print.auc = TRUE)

# Calculate the cross-validated log-loss on the training set
cv_fit <- cv.glmnet(as.matrix(data[, -13]), data$fatal_mi, family = "binomial")
cv_loss <- cv_fit$cvm[cv_fit$lambda == cv_fit$lambda.min]
cv_loss


predictions <- predict(fit, as.matrix(test[, -13]))
pred_class <- ifelse(predictions >= 0.5, 1, 0)
confusionMatrix(as.factor(pred_class), as.factor(test$fatal_mi))



# Split the dataset into training and testing sets
set.seed(666)
train_index <- createDataPartition(heart_failure$fatal_mi, p = 0.7, list = FALSE)
train_data <- heart_failure[train_index, ]
test_data <- heart_failure[-train_index, ]
# Train a logistic regression model on the training data
glm_model <- glm(factor(fatal_mi) ~ ., data = train_data, family = binomial)

# Predict the target variable on the test data
test_pred <- predict(glm_model, newdata = test_data, type = "response")
test_pred <- ifelse(test_pred > 0.5, 1, 0)

# Evaluate the model performance on the test data
conf_mat <- confusionMatrix(factor(test_pred), factor(test_data$fatal_mi))
conf_mat



# Define the parameter grid to search over
param_grid <- expand.grid(mtry = seq(1, ncol(heart_failure) - 1, 2),
                          splitrule = c("gini", "extratrees"),
                          min.node.size = c(1, 3, 5,7,9))

# Split the dataset into training and testing sets
set.seed(777)
train_index <- createDataPartition(heart_failure$fatal_mi, p = 0.7, list = FALSE)
train_data <- heart_failure[train_index, ]
test_data <- heart_failure[-train_index, ]

# Train a random forest model with grid search and cross-validation
ctrl <- trainControl(method = "cv", number = 5)
rf_model <- train(factor(fatal_mi) ~ ., data = train_data, method = "ranger",
                  num.trees = 500, importance = "permutation",
                  trControl = ctrl, tuneGrid = param_grid)

rf_model
# Print the best hyperparameters
rf_model$bestTune

# Predict the target variable on the test data
test_pred <- predict(rf_model, newdata = test_data)

# Evaluate the model performance on the test data
conf_mat <- confusionMatrix(factor(test_pred), factor(test_data$fatal_mi))
conf_mat







