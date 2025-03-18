# Read the dataset from the specified path
# dataset <- read.csv("D:/Documents/master/mldm/m1/data-mining/data/Sudden-Cardiac-Death-Holter-Database.csv")
dataset <- read.csv("D:/Documents/master/mldm/m1/semester-2/data-mining/data/Sudden-Cardiac-Death-Holter-Database.csv")

# Remove the 'record' column
dataset <- subset(dataset, select = -record)

# Display only the first two rows of the dataset
print(head(dataset, n = 2))

# Check the variable types in the dataset
print(str(dataset))

# Check for class imbalance in the 'type' column
type_counts <- table(dataset$type)
print(type_counts)

# Check for missing values in the dataset
missing_values <- colSums(is.na(dataset))
print(missing_values)

# Transform 'type' column into binary classification
dataset$type <- ifelse(dataset$type %in% c('VEB', 'SVEB', 'F', 'Q'), 'arrhythmia', 'normal')

# Convert 'arrhythmia' to 0 and 'normal' to 1
dataset$type <- ifelse(dataset$type == 'arrhythmia', 0, 1)

# Display the transformed 'type' column
print(table(dataset$type))

install.packages("e1071")
# Load the e1071 package for skewness calculation
library(e1071)
if (!require(e1071)) install.packages("e1071")
library(e1071)

# Check skewness of numerical columns
cat("\nSkewness of numerical columns:\n")
numerical_columns <- sapply(dataset, is.numeric)
skewness_values <- sapply(dataset[, numerical_columns], skewness)
print(skewness_values)

# Check kurtosis of numerical columns
cat("\nKurtosis of numerical columns:\n")
kurtosis_values <- sapply(dataset[, numerical_columns], kurtosis)
print(kurtosis_values)

# Create boxplots for numerical features excluding 'type' column
cat("\nBoxplots of numerical features excluding 'type' column:\n")
numerical_columns <- names(dataset)[sapply(dataset, is.numeric) & names(dataset) != "type"]
boxplot(dataset[, numerical_columns], main = "Boxplots of Numerical Features", las = 2)

# Check correlation matrix using Spearman method
cat("\nCorrelation matrix (Spearman method):\n")
correlation_matrix <- cor(dataset[, numerical_columns], method = "spearman", use = "complete.obs")
print(correlation_matrix)

mean_values <- sapply(dataset[, numerical_columns], mean, na.rm = TRUE)
median_values <- sapply(dataset[, numerical_columns], median, na.rm = TRUE)
sd_values <- sapply(dataset[, numerical_columns], sd, na.rm = TRUE)

mean_df <- data.frame(Feature = names(mean_values), Mean = mean_values)
median_df <- data.frame(Feature = names(median_values), Median = median_values)
sd_df <- data.frame(Feature = names(sd_values), SD = sd_values)

mean_df