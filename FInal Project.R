# Load dataset into environment and review DataFrame structure
cancer_data <- read.csv(file = "dataset/breast-cancer-wisconsin.data", sep=",", header = FALSE)
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







