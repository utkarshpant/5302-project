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


df$bare_nuclei

head(df$bare_nuclei)

summary(df)

#Visualization

df2 <- df
df2

df2$bare_nuclei[ddf$bare_nuclei == "?"] <- NA
df2$bare_neuclei

# Impute missing values with mean (you can choose a different imputation method)
df2$bare_nuclei <- as.integer(ifelse(is.na(df2$bare_nuclei), median(df2$bare_nuclei, na.rm = TRUE), df2$bare_nuclei))
df2$bare_nuclei


#Data Exploration
plot(df)

hist(df2$clump_thickness)
hist(df2$uniformity_of_cell_size)
hist(df2$uniformity_of_cell_shape)
hist(df2$marginal_adhesion)
hist(df2$single_epithelial_cell_size)
hist(df2$bare_nuclei)
hist(df2$bland_chromatin)
hist(df2$normal_nucleoli)
hist(df2$mitoses)

#code start
hist(df2$class, breaks = seq(min(df2$class), max(df2$class), length.out = 1), axes = FALSE)
axis(side=1, at = c(2.5,3.5), c("Benign", "Malignant"))
box()
axis(side = 2)
#code end


     