# Load libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(GGally)
library(patchwork)
library(caret)

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
str(cancer_data)
head(cancer_data)

#Checking for missing variables
sapply(cancer_data, function(x) sum(is.na(x)))

# Drop "sample_code_number" as it is not noteworthy
cancer_data <- select(cancer_data, -c("sample_code_number"))

str(cancer_data)
head(cancer_data)

#Checking for missing variables
sapply(cancer_data, function(x) sum(is.na(x)))

# Replace "?" `bare_nuclei` values with NA in the dataset;
# This is missing data
na_if(cancer_data$bare_nuclei, "?")

# Impute NA values with median (for now)
cancer_data$bare_nuclei[is.na(cancer_data$bare_nuclei)] <- median(cancer_data$bare_nuclei, na.rm = TRUE)

# Data Exploration.
# TODO: Generate Histograms for each feature, plus boxplots vs. the target classes

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