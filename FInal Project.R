# Load libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(GGally)
library(patchwork)
library(caret)
library(class)
library(e1071)
library(randomForest)
library(ggcorrplot)
library(corrplot)
library(tree)

# Load dataset into environment and review DataFrame structure
cancer_data <- read.csv(file = "dataset/breast-cancer-wisconsin.data", sep = ",", header = FALSE)
str(cancer_data)

#Providing feature names
colnames(cancer_data) <- c("sample_code_number",
                           "clump_thickness",
                           "uniformity_of_cell_size",
                           "uniformity_of_cell_shape",
                           "marginal_adhesion",
                           "single_epithelial_cell_size",
                           "bare_nuclei",
                           "bland_chromatin",
                           "normal_nucleoli",
                           "mitoses",
                           "class")

# Reconfirm that feature names were updated and preview data
colnames(cancer_data)

# Drop "sample_code_number" as it is not noteworthy
cancer_data <- select(cancer_data, -c("sample_code_number"))

# Replace "?" `bare_nuclei` values with NA in the dataset;
# This is missing data
cancer_data$bare_nuclei <- na_if(cancer_data$bare_nuclei, "?")
cancer_data$bare_nuclei <- as.integer(cancer_data$bare_nuclei)

#Checking for missing variables
sapply(cancer_data, function(x) sum(is.na(x)))

# Impute NA values with median (for now)
cancer_data$bare_nuclei[is.na(cancer_data$bare_nuclei)] <- median(cancer_data$bare_nuclei, na.rm = TRUE)

# Data Exploration.
# TODO: Generate Histograms for each feature, plus boxplots vs. the target classes

# Transform class encoding from 2 and 4 to 0 and 1
cancer_data$class <- ifelse(cancer_data$class == 2, 0, 1)
cancer_data$class <- as.integer((cancer_data$class))

# Boxplots
ggplot(cancer_data) +
  geom_boxplot(aes(x = factor(class, labels = c("Benign", "Malignant")),
                   y = clump_thickness,
                   fill = factor(class, labels = c("Benign", "Malignant")))) +
  scale_x_discrete(labels = c('Benign', 'Malignant')) +
  labs(x = "Tumour type",
       y = "Clump thickness",
       fill = "Legend",
       title = "Boxplot of Clump thickness vs. classification",
       caption = "Notice the clear differentiation in the range of clump thickness values") +
  scale_fill_manual(values = c('Benign' = '#006989', 'Malignant' = '#CFDBD5'))

# Histograms
ggplot(cancer_data) +
  geom_histogram(aes(x = clump_thickness),
                 binwidth = 1,
                 fill = "#006989",
                 colour = "#1E1E1E") +
  labs(x = "Clump thickness",
       y = "Frequency",
       title = "Histogram of clump thickness values")


# hist(df2$clump_thickness)
# hist(df2$uniformity_of_cell_size)
# hist(df2$uniformity_of_cell_shape)
# hist(df2$marginal_adhesion)
# hist(df2$single_epithelial_cell_size)
# hist(df2$bare_nuclei)
# hist(df2$bland_chromatin)
# hist(df2$normal_nucleoli)
# hist(df2$mitoses)

# Splitting the dataset into a 70/30
# train/test split;
set.seed(5302)
train_index <- createDataPartition(y = cancer_data$class,
                                   p = 0.7,
                                   list = TRUE,
                                   times = 1)
training_data <- cancer_data[train_index$Resample1,]
test_data <- cancer_data[-train_index$Resample1,]

# Defining base models
base_models <- list(
  model_lr = glm(training_data$class ~ ., data = training_data, family = "binomial"),
  model_dt = tree(training_data$class ~ ., data = training_data),
  model_svm = svm(training_data$class ~ ., data = training_data),
  # KNN "models" directly give us the labels that they assign for each instance
  model_knn = knn(select(training_data, -c("class")), select(test_data, -c("class")), cl = training_data$class, k = 3)
)


# Extract predictions from base models
# Step 1: Get predictions from base models
predictions_lr <- predict(base_models$model_lr, newdata = test_data, type = "response")
# Assuming 'class' is a binary variable (0 or 1)
predictions_dt <- predict(base_models$model_dt, newdata = test_data)
predictions_svm <- predict(base_models$model_svm, newdata = test_data, probability = TRUE)
predictions_knn <- base_models$model_knn

# Combine base model predictions into a data frame
base_model_predictions <- data.frame(
  LR = predictions_lr,
  DT = predictions_dt,
  SVM = predictions_svm,  # Use decision values for SVM
  KNN = predictions_knn,  # Convert KNN predictions to numeric,
  class = test_data$class
)

# Fit the meta-model (Logistic Regression)
meta_model <- glm(class ~ ., data = base_model_predictions, family = "binomial")

# Now, let's make predictions on the test data using the base models and the meta-model
# First, get base model predictions on the test data
test_predictions_lr <- predict(base_models$model_lr, newdata = test_data, type = "response")
test_predictions_dt <- predict(base_models$model_dt, newdata = test_data)
test_predictions_svm <- predict(base_models$model_svm, newdata = test_data, probability = TRUE)
test_predictions_knn <- base_models$model_knn

# Combine base model predictions into a data frame for the test data
test_base_model_predictions <- data.frame(
  LR = test_predictions_lr,
  DT = test_predictions_dt,
  SVM = test_predictions_svm,
  KNN = test_predictions_knn
)

# Make predictions using the meta-model
ensemble_predictions <- predict(meta_model, newdata = test_base_model_predictions, type = "response")

# Evaluate the ensemble model performance
conf_matrix <- table(Actual = test_data$class, Predicted = ifelse(ensemble_predictions > 0.5, 1, 0))
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

summary(conf_matrix)
accuracy