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
library(corrplot)

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

# Transform class encoding from 2 and 4 to 0 and 1;
# then cast chr to int
cancer_data$class <- ifelse(cancer_data$class == 2, 0, 1)
cancer_data$class <- as.integer((cancer_data$class))

# Boxplots
# Example: Identify outliers using boxplot
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


ggplot(cancer_data) +
  geom_boxplot(aes(x = factor(class, labels = c("Benign", "Malignant")),
                   y = uniformity_of_cell_size,
                   fill = factor(class, labels = c("Benign", "Malignant")))) +
  scale_x_discrete(labels = c('Benign', 'Malignant')) +
  labs(x = "Tumour type",
       y = "Uniformity of cell size",
       fill = "Legend",
       title = "Boxplot of uniformity of cell size by classification",
       caption = "") +
  scale_fill_manual(values = c('Benign' = '#006989', 'Malignant' = '#CFDBD5'))

ggplot(cancer_data) +
  geom_boxplot(aes(x = factor(class, labels = c("Benign", "Malignant")),
                   y = uniformity_of_cell_shape,
                   fill = factor(class, labels = c("Benign", "Malignant")))) +
  scale_x_discrete(labels = c('Benign', 'Malignant')) +
  labs(x = "Tumour type",
       y = "Uniformity of cell size",
       fill = "Legend",
       title = "Boxplot of uniformity of cell shape by classification",
       caption = "") +
  scale_fill_manual(values = c('Benign' = '#006989', 'Malignant' = '#CFDBD5'))

ggplot(cancer_data) +
  geom_boxplot(aes(x = factor(class, labels = c("Benign", "Malignant")),
                   y = marginal_adhesion,
                   fill = factor(class, labels = c("Benign", "Malignant")))) +
  scale_x_discrete(labels = c('Benign', 'Malignant')) +
  labs(x = "Tumour type",
       y = "Marginal adhesion",
       fill = "Legend",
       title = "Boxplot of marginal adhesion by classification",
       caption = "") +
  scale_fill_manual(values = c('Benign' = '#006989', 'Malignant' = '#CFDBD5'))

ggplot(cancer_data) +
  geom_boxplot(aes(x = factor(class, labels = c("Benign", "Malignant")),
                   y = single_epithelial_cell_size,
                   fill = factor(class, labels = c("Benign", "Malignant")))) +
  scale_x_discrete(labels = c('Benign', 'Malignant')) +
  labs(x = "Tumour type",
       y = "Single epithelial cell size",
       fill = "Legend",
       title = "Boxplot of single epithelial cell size by classification",
       caption = "") +
  scale_fill_manual(values = c('Benign' = '#006989', 'Malignant' = '#CFDBD5'))

ggplot(cancer_data) +
  geom_boxplot(aes(x = factor(class, labels = c("Benign", "Malignant")),
                   y = bare_nuclei,
                   fill = factor(class, labels = c("Benign", "Malignant")))) +
  scale_x_discrete(labels = c('Benign', 'Malignant')) +
  labs(x = "Tumour type",
       y = "Bare nuclei",
       fill = "Legend",
       title = "Boxplot of bare nuclei by classification",
       caption = "") +
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

ggplot(cancer_data) +
  geom_boxplot(aes(x = factor(class, labels = c("Benign", "Malignant")),
                   y = bland_chromatin,
                   fill = factor(class, labels = c("Benign", "Malignant")))) +
  scale_x_discrete(labels = c('Benign', 'Malignant')) +
  labs(x = "Tumour type",
       y = "Bland chromatin",
       fill = "Legend",
       title = "Boxplot of bland chromatin by classification",
       caption = "") +
  scale_fill_manual(values = c('Benign' = '#006989', 'Malignant' = '#CFDBD5'))

ggplot(cancer_data) +
  geom_boxplot(aes(x = factor(class, labels = c("Benign", "Malignant")),
                   y = normal_nucleoli,
                   fill = factor(class, labels = c("Benign", "Malignant")))) +
  scale_x_discrete(labels = c('Benign', 'Malignant')) +
  labs(x = "Tumour type",
       y = "Normal nucleoli",
       fill = "Legend",
       title = "Boxplot of normal nucleoli by classification",
       caption = "") +
  scale_fill_manual(values = c('Benign' = '#006989', 'Malignant' = '#CFDBD5'))

ggplot(cancer_data) +
  geom_boxplot(aes(x = factor(class, labels = c("Benign", "Malignant")),
                   y = mitoses,
                   fill = factor(class, labels = c("Benign", "Malignant")))) +
  scale_x_discrete(labels = c('Benign', 'Malignant')) +
  labs(x = "Tumour type",
       y = "Mitoses",
       fill = "Legend",
       title = "Boxplot of mitoses by classification",
       caption = "") +
  scale_fill_manual(values = c('Benign' = '#006989', 'Malignant' = '#CFDBD5'))


#Data Exploration
ggpairs(cancer_data)

boxplot(cancer_data$clump_thickness, col = "skyblue", main = "Boxplot")
boxplot(cancer_data$uniformity_of_cell_size, col = "skyblue", main = "Boxplot")
boxplot(cancer_data$uniformity_of_cell_shape, col = "skyblue", main = "Boxplot")
boxplot(cancer_data$marginal_adhesion, col = "skyblue", main = "Boxplot")
boxplot(cancer_data$single_epithelial_cell_size, col = "skyblue", main = "Boxplot")
boxplot(cancer_data$bare_nuclei, col = "skyblue", main = "Boxplot")
boxplot(cancer_data$bland_chromatin, col = "skyblue", main = "Boxplot")
boxplot(cancer_data$normal_nucleoli, col = "skyblue", main = "Boxplot")
boxplot(cancer_data$mitoses, col = "skyblue", main = "Boxplot")

#Correlation matrix
correlation_matrix <- cor(cancer_data)

# TODO: Convert to ggcorrplot
ggcorrplot(correlation_matrix) +
  labs(title = "Correlation between all features")

ggplot(cancer_data) +
  geom_histogram(aes(x = mitoses,
                     fill = factor(class, labels = c("Benign", "Malignant"))),
                 binwidth = 1,
                 colour = "#1e1e1e") +
  labs(x = "Mitoses by classification",
       y = "Frequency",
       fill = "Classification",
       title = "Distribution of mitoses by classification") +
  scale_fill_manual(values = c('Benign' = 'orange', 'Malignant' = 'darkblue'))

ggplot(cancer_data) +
  geom_histogram(aes(x = clump_thickness,
                     fill = factor(class, labels = c("Benign", "Malignant"))),
                 binwidth = 1,
                 colour = "#1e1e1e") +
  labs(x = "Clump thickness by classification",
       y = "Frequency",
       fill = "Classification",
       title = "Distribution of clump thickness by classification") +
  scale_fill_manual(values = c('Benign' = 'orange', 'Malignant' = 'darkblue'))

ggplot(cancer_data) +
  geom_histogram(aes(x = uniformity_of_cell_size,
                     fill = factor(class, labels = c("Benign", "Malignant"))),
                 binwidth = 1,
                 colour = "#1e1e1e") +
  labs(x = "Uniformity of cell size by classification",
       y = "Frequency",
       fill = "Classification",
       title = "Distribution of uniformity of cell size by classification") +
  scale_fill_manual(values = c('Benign' = 'orange', 'Malignant' = 'darkblue'))

ggplot(cancer_data) +
  geom_histogram(aes(x = uniformity_of_cell_shape,
                     fill = factor(class, labels = c("Benign", "Malignant"))),
                 binwidth = 1,
                 colour = "#1e1e1e") +
  labs(x = "Uniformity of cell shape by classification",
       y = "Frequency",
       fill = "Classification",
       title = "Distribution of uniformity of cell shape by classification") +
  scale_fill_manual(values = c('Benign' = 'orange', 'Malignant' = 'darkblue'))

ggplot(cancer_data) +
  geom_histogram(aes(x = marginal_adhesion,
                     fill = factor(class, labels = c("Benign", "Malignant"))),
                 binwidth = 1,
                 colour = "#1e1e1e") +
  labs(x = "Marginal adhesion by classification",
       y = "Frequency",
       fill = "Classification",
       title = "Distribution of marginal adhesion by classification") +
  scale_fill_manual(values = c('Benign' = 'orange', 'Malignant' = 'darkblue'))

ggplot(cancer_data) +
  geom_histogram(aes(x = single_epithelial_cell_size,
                     fill = factor(class, labels = c("Benign", "Malignant"))),
                 binwidth = 1,
                 colour = "#1e1e1e") +
  labs(x = "Single cell epithelial size by classification",
       y = "Frequency",
       fill = "Classification",
       title = "Distribution of single cell epithelial size by classification") +
  scale_fill_manual(values = c('Benign' = 'orange', 'Malignant' = 'darkblue'))

ggplot(cancer_data) +
  geom_histogram(aes(x = bare_nuclei,
                     fill = factor(class, labels = c("Benign", "Malignant"))),
                 binwidth = 1,
                 colour = "#1e1e1e") +
  labs(x = "Bare nuclei by classification",
       y = "Frequency",
       fill = "Classification",
       title = "Distribution of bare_nuclei by classification") +
  scale_fill_manual(values = c('Benign' = 'orange', 'Malignant' = 'darkblue'))

ggplot(cancer_data) +
  geom_histogram(aes(x = bland_chromatin,
                     fill = factor(class, labels = c("Benign", "Malignant"))),
                 binwidth = 1,
                 colour = "1") +
  labs(x = "Bland chromatin by classification",
       y = "Frequency",
       fill = "Classification",
       title = "Distribution of bland chromatin by classification") +
  scale_fill_manual(values = c('Benign' = 'orange', 'Malignant' = 'darkblue'))

ggplot(cancer_data) +
  geom_histogram(aes(x = normal_nucleoli,
                     fill = factor(class, labels = c("Benign", "Malignant"))),
                 binwidth = 1,
                 colour = "#1e1e1e") +
  labs(x = "Normal nucleoli by classification",
       y = "Frequency",
       fill = "Classification",
       title = "Distribution of normal nucleoli by classification") +
  scale_fill_manual(values = c('Benign' = 'orange', 'Malignant' = 'darkblue'))

ggplot(cancer_data) +
  geom_bar(aes(x = factor(class, labels = c("Benign", "Malignant")),
               fill = factor(class, labels = c("Benign", "Malignant"))),
           binwidth = 1) +
  labs(x = "Occurrences of each class",
       y = "Frequency",
       fill = "Classification",
       title = "Distribution of classification") +
  scale_fill_manual(values = c('Benign' = 'orange', 'Malignant' = 'darkblue'))


# These plots were translated into ggplot; could make a function to plot them
# hist(cancer_data$clump_thickness)
# hist(cancer_data$uniformity_of_cell_size)
# hist(cancer_data$uniformity_of_cell_shape)
# hist(cancer_data$marginal_adhesion)
# hist(cancer_data$single_epithelial_cell_size)
# hist(cancer_data$bare_nuclei)
# hist(cancer_data$bland_chromatin)
# hist(cancer_data$normal_nucleoli)
# hist(cancer_data$mitoses)

#EDA
highly_correlated_features <- colnames(cancer_data)[findCorrelation(correlation_matrix, cutoff = 0.9, verbose = TRUE)]
highly_correlated_features

cancer_data <- select(cancer_data, -c(highly_correlated_features[1]))

# Data preparation
# PCA
# Step 1: Select features; exclude target
features <- select(cancer_data, -c("class"))

# Step 2: Scale and center features
scaled_features <- scale(features)

# Step 3: Run PCA
pca <- prcomp(scaled_features, center = FALSE, scale = FALSE)
summary(pca)

prop_var <- pca$sdev^2 / sum(pca$sdev^2)
prop_var

# Going by PCA results and 0.95 variance threshold, remove `mitoses`
cancer_data <- select(cancer_data, -c("mitoses"))

# Plot a Scree Plot to visualise PCA results
qplot(seq_along(prop_var), prop_var) +
  geom_line() +
  geom_point() +
  labs(x = "Principal components",
       y = "Variance explained",
       title = "Scree plot",
       caption = "Notice that PC1 explains most of the variance in the dataset")

# Training the model
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

# Make predictions using the meta-model
ensemble_predictions <- predict(meta_model, newdata = base_model_predictions, type = "response")

# Evaluate the ensemble model performance
conf_matrix <- table(Actual = test_data$class, Predicted = ifelse(ensemble_predictions > 0.5, 1, 0))
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

summary(conf_matrix)
accuracy

# Create correlation plot with custom styling
ggcorrplot(correlation_matrix,
           hc.order = TRUE, # Hierarchical clustering
           type = "upper",  # Display lower triangle of the correlation matrix
           outline.color = "#1e1e1e",  # Color of the outline
           lab_size = 8,  # Size of the correlation coefficient labels
           method = "circle",  # Type of correlation display (you can also use "number", "ellipse", etc.)
           colors = c("darkblue", "purple", "yellow"),  # Color range for correlation values
           title = "Correlation between all features",
           lab = FALSE,
           tl.cex = 10.5,
           tl.srt = 45,
           as.is = FALSE,
           # ggtheme = theme_minimal(),  # Choose a theme (e.g., theme_minimal(), theme_classic(), etc.)
           legend.title = "Correlation")