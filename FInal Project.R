setwd("C:/Users/soham/Documents/Github rep/5302-project")
df <- read.csv(file = "./dataset/breast-cancer-wisconsin.data", sep=",", header = FALSE)
df

str(df)

#Providing feature names
colnames(df) <- c("sample_code_number",
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
str(df)
head(df)

#Checking for missing variables
sapply(df, function(x) sum(is.na(x)))












