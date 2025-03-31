install.packages("RPostgres")  # Or install.packages("RPostgreSQL")
library(RPostgres)            # Or library(RPostgreSQL)

con <- dbConnect(
  RPostgres::Postgres(),      # Or RPostgreSQL::PostgreSQL()
  dbname = "testdb",
  host = "localhost",    # e.g., "localhost" or an IP address
  port = 5432,                # Default PostgreSQL port
  user = "postgres",
  password = "postgre"
)

df <- dbGetQuery(con, "SELECT * FROM scdhd")
print(df)

df <- dbFetch(query)
dbClearResult(query)  # Clear the result after fetching

# dbDisconnect(con) -- disconnect with database

# Check the shape of the dataframe
shape <- dim(df)
print(shape)

# Remove column record
df <- subset(df, select = -record)
print(df)

# List all column names
columns <- colnames(df)
print(columns)

# Display the structure and data types
str(df)

# Identify duplicate rows
duplicates <- duplicated(df)
print(duplicates)

# View only duplicate rows
duplicate_rows <- df[duplicates, ]
print(duplicate_rows)

# Count total missing values in df
total_missing <- sum(is.na(df))
print(paste("Total missing values:", total_missing))

# Check proportion of missing values per column
missing_per_column <- colMeans(is.na(df))
print("Proportion of missing values per column:")
print(missing_per_column)

# Check proportion of missing values per row
missing_per_row <- rowMeans(is.na(df))
print("Proportion of missing values per row:")
print(missing_per_row)

# Remove rows with more than 50% missing values
df <- df[rowMeans(is.na(df)) < 0.5, ]

# Remove the 'type' column
df_numeric <- df[, !(names(df) %in% c("type"))]

non_numeric_columns <- sapply(df, function(x) !is.numeric(x))
print(names(df)[non_numeric_columns])

# install.packages("DMwR2")  # Install the package
# library(DMwR2)             # Load the package
# 
# # Perform KNN imputation (k = 3)
# df_imputed <- knnImputation(df_numeric, k = 3)

# install.packages("missRanger")
# library(missRanger)
# df_imputed <- missRanger(df_numeric, num.trees = 100, seed = 123)

install.packages("Hmisc")
library(Hmisc)

df_numeric[] <- lapply(df_numeric, function(col) {
  if (is.numeric(col)) impute(col, fun = median) else col
})

# Reintegrate Non-Numeric Columns
# df <- cbind(df_numeric, df[, !sapply(df, is.numeric)])
df <- cbind(df_numeric, type = df$type)

# Display summary statistics
summary(df)

# Count the occurrences of each class in the 'type' column
type_counts <- table(df$type)

# Plot the class distribution
barplot(type_counts, main = "Class Distribution of 'type'", col = "skyblue",
        xlab = "Type", ylab = "Frequency")

install.packages("moments")  # Install the package
library(moments)             # Load the package

variable_names <- colnames(df_numeric)

# Calculate skewness for each numeric column, ignoring missing values
skewness_values <- sapply(df_numeric, function(x) {
  if (length(unique(x)) > 1) {  # Check if the column has more than one unique value
    skewness(x, na.rm = TRUE)
  } else {
    NA  # Assign NA for constant columns
  }
})

# Remove NA or non-finite skewness values
finite_skewness <- skewness_values[is.finite(skewness_values)]
variable_names <- names(finite_skewness)

# Bar plot for skewness with improved visualization
barplot(skewness_values,
        names.arg = variable_names,
        col = rainbow(length(skewness_values)),  # Use a rainbow color palette
        main = "Skewness of Features",
        # xlab = "Variables",
        ylab = "Skewness",
        ylim = c(min(skewness_values) - 0.5, max(skewness_values) + 0.5),
        cex.names = 0.7,  # Reduce the size of the variable names
        las = 2)          # Rotate the labels to vertical

# Calculate kurtosis for each numeric column, ignoring missing values
kurtosis_values <- sapply(df_numeric, function(x) {
  if (length(unique(x)) > 1) {  # Check if the column has more than one unique value
    kurtosis(x, na.rm = TRUE)
  } else {
    NA  # Assign NA for constant columns
  }
})

# Remove NA or non-finite kurtosis values
finite_kurtosis <- kurtosis_values[is.finite(kurtosis_values)]
variable_names_kurtosis <- names(finite_kurtosis)

# Bar plot for kurtosis with improved visualization
barplot(finite_kurtosis,
        names.arg = variable_names_kurtosis,
        col = rainbow(length(finite_kurtosis)),  # Use a rainbow color palette
        main = "Kurtosis of Features",
        # xlab = "Variables",  # Optional: Remove or customize x-axis label
        ylab = "Kurtosis",
        ylim = c(min(finite_kurtosis) - 0.5, max(finite_kurtosis) + 0.5),
        cex.names = 0.7,  # Reduce the size of the variable names
        las = 2)          # Rotate the labels to vertical

### 
  
# Loop through each numerical column in df_numeric
par(mfrow = c(2, 2))  # Set layout for multiple plots (2 rows, 2 columns)

for (col in colnames(df_numeric)) {
  # Create histogram for each column
  hist(df_numeric[[col]], 
       main = paste("Distribution of", col), 
       xlab = col, 
       col = "skyblue", 
       border = "white",
       freq = FALSE)  # Set freq = FALSE to overlay density plot
  
  # Overlay density plot
  lines(density(df_numeric[[col]]), col = "red", lwd = 2)
}
par(mfrow = c(1, 1))  # Reset layout to default

# Load required libraries
install.packages("reshape2")  # Or install.packages("RPostgreSQL")
library(reshape2)
library(ggplot2)

# Step 1: Calculate the correlation matrix
cor_matrix <- round(cor(df_numeric), 2)  # Compute correlations and round to 2 decimals

# Step 2: Melt the correlation matrix into long format
melted_cor <- melt(cor_matrix)

# Step 3: Create the heatmap using ggplot2
ggplot(data = melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +  # Add white borders around tiles
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                       limit = c(-1, 1), name = "Correlation") +
  labs(title = "Correlation Heatmap", x = "Features", y = "Features") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

## check rules/patterns 
# Install and load required packages
install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)

# Convert the dataframe to transactions
transactions <- as(df_numeric, "transactions")

# Apply the Apriori algorithm
rules <- apriori(transactions, 
                 parameter = list(supp = 0.01, conf = 0.8))

# Filter and sort rules
filtered_rules <- subset(rules, lift > 1)
sorted_rules <- sort(filtered_rules, by = "confidence", decreasing = TRUE)

# View the rules
inspect(sorted_rules)

# Visualize the rules
plot(sorted_rules, method = "graph", engine = "htmlwidget")

# DBSCAN for Hidden Clusters
install.packages('dbscan')
library(dbscan)
# Apply DBSCAN
# Perform DBSCAN clustering
clustering <- dbscan(df_numeric, eps = 0.3, minPts = 10)
# Add cluster labels to the dataset
df$cluster <- clustering$cluster
# Reduce dimensionality using PCA for visualization
pca_result <- prcomp(df_numeric, scale. = TRUE)  # Perform PCA
pca_data <- as.data.frame(pca_result$x[, 1:2])   # Take the first two principal components
pca_data$cluster <- factor(df$cluster)          # Add cluster labels
# Plot results
ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point() +
  labs(title = "DBSCAN Clustering Results (PCA Projection)",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal()



## modeling
# Feature Importance (Most Valuable Factors)
install.packages('xgboost')
library(xgboost)

# Replace 'N' with 0 and all other values with 1 in the 'type' column
df$type <- ifelse(df$type == "N", 0, 1)

# Convert df_numeric to a matrix for X
X <- as.matrix(df_numeric)
# Use the 'type' column from df for y
y <- df$type
y <- as.numeric(as.factor(y)) - 1  # Convert factor to numeric (0 and 1 for binary classification)

valid_indices <- !is.na(y) & !is.nan(y) & !is.infinite(y)
X <- X[valid_indices, ]
y <- y[valid_indices]

# Verify the changes
print(table(df$type))  # Check the distribution of 0s and 1s

# Train XGBoost Model
model <- xgboost(data = X, label = y, nrounds = 50, objective = "binary:logistic")

# Get Feature Importance
importance <- xgb.importance(model = model)
print(importance)

# Plot Importance
# xgb.plot.importance(importance)
if (!is.null(importance) && nrow(importance) > 0) {
  xgb.plot.importance(importance)
} else {
  print("Feature importance is empty or invalid.")
}

# Handle imbalance dataset
install.packages("smotefamily")
library(smotefamily)

# Ensure 'type' is the target column and convert it to a factor
df$type <- as.factor(df$type)

# Remove the 'type' column to create df_numeric (features only)
df_numeric <- df[, !(names(df) %in% c("type"))]

# Apply SMOTE to balance the dataset
smote_result <- SMOTE(df_numeric, df$type, K = 5, dup_size = 2)

# Extract the balanced dataset
df_balanced <- smote_result$data
df_balanced$type <- as.factor(df_balanced$class)  # Add the balanced 'type' column back
df_balanced$class <- NULL  # Remove the temporary 'class' column

# Check the class distribution after SMOTE
print(table(df_balanced$type))

# Display the first few rows of the balanced dataset
print(head(df_balanced))

# Sample data 
# Ensure the dataset has at least 5,000 rows
if (nrow(df) >= 5000) {
  # Randomly sample 5,000 rows
  df_sampled <- df[sample(nrow(df), 5000), ]
} else {
  # If the dataset has fewer than 5,000 rows, use the entire dataset
  df_sampled <- df
}

# Check the dimensions of the sampled dataset
print(dim(df_sampled))

# Display the first few rows of the sampled dataset
print(head(df_sampled))

### rule/pattern
transactions <- as(df_sampled[, !(names(df_sampled) %in% c("type", "id", "Cluster"))], "transactions")

# Apply the Apriori algorithm
rules <- apriori(transactions, 
                 parameter = list(supp = 0.01, conf = 0.8))

# Filter and sort rules
filtered_rules <- subset(rules, lift > 1)
sorted_rules <- sort(filtered_rules, by = "confidence", decreasing = TRUE)

# View the rules
inspect(sorted_rules)

# Visualize the rules
plot(sorted_rules, method = "graph", engine = "htmlwidget")

## knn vs kd-tree

install.packages("class")       # For kNN
install.packages("caret")       # For data splitting and evaluation
install.packages("e1071")       # For confusion matrix
install.packages("ggplot2")     # For visualization (optional)

library(caret)

# Remove non-numeric columns and separate features and target
df_numeric <- df[, !(names(df) %in% c("type"))]
target <- df$type

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_index <- createDataPartition(target, p = 0.8, list = FALSE)
X_train <- df_numeric[train_index, ]
X_test <- df_numeric[-train_index, ]
y_train <- target[train_index]
y_test <- target[-train_index]

library(FNN)

# Train and predict using brute force kNN
k <- 5  # Number of neighbors
y_pred_brute <- knn(train = X_train, test = X_test, cl = y_train, k = k, algorithm = "brute")

# Evaluate accuracy
conf_matrix_brute <- confusionMatrix(as.factor(y_pred_brute), as.factor(y_test))
print(conf_matrix_brute)

# Train and predict using KD-Tree kNN
y_pred_kdtree <- knn(train = X_train, test = X_test, cl = y_train, k = k, algorithm = "kd_tree")

# Evaluate accuracy
conf_matrix_kdtree <- confusionMatrix(as.factor(y_pred_kdtree), as.factor(y_test))
print(conf_matrix_kdtree)

# Perform PCA on training data
pca_result <- prcomp(X_train, scale. = TRUE)

# Transform both training and testing data
X_train_pca <- predict(pca_result, X_train)[, 1:2]  # Use the first 2 principal components
X_test_pca <- predict(pca_result, X_test)[, 1:2]

# Check the variance explained by the first 2 components
print(summary(pca_result))

# Train and predict using brute force kNN on PCA-reduced data
y_pred_brute_pca <- knn(train = X_train_pca, test = X_test_pca, cl = y_train, k = k, algorithm = "brute")

# Evaluate accuracy
conf_matrix_brute_pca <- confusionMatrix(as.factor(y_pred_brute_pca), as.factor(y_test))
print(conf_matrix_brute_pca)

# Print accuracy for all models
cat("Accuracy (Brute Force, Original Data):", conf_matrix_brute$overall["Accuracy"], "\n")
cat("Accuracy (KD-Tree, Original Data):", conf_matrix_kdtree$overall["Accuracy"], "\n")
cat("Accuracy (Brute Force, PCA-Reduced Data):", conf_matrix_brute_pca$overall["Accuracy"], "\n")
cat("Accuracy (KD-Tree, PCA-Reduced Data):", conf_matrix_kdtree_pca$overall["Accuracy"], "\n")

# Load necessary libraries
library(caret)         # For data splitting and model training
library(e1071)         # For SVM and Naive Bayes
library(rpart)         # For Decision Tree
library(mlr)           # For Stacking Classifier
library(adabag)        # For AdaBoost

# Set seed for reproducibility
set.seed(123)

# Combine X and y into a single data frame for caret
data <- as.data.frame(X)
data$y <- as.factor(y)  # Ensure y is a factor for classification

# Split the data into training (70%) and testing (30%) sets
trainIndex <- createDataPartition(data$y, p = 0.7, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Initialize a vector to store accuracies
accuracies <- numeric()

# 1. Logistic Regression
logit_model <- glm(y ~ ., data = trainData, family = binomial)
logit_pred <- predict(logit_model, newdata = testData, type = "response")
logit_pred_class <- ifelse(logit_pred > 0.5, levels(trainData$y)[2], levels(trainData$y)[1])
logit_accuracy <- mean(logit_pred_class == testData$y)
accuracies <- c(accuracies, logit_accuracy)

# 2. Decision Tree
dt_model <- rpart(y ~ ., data = trainData, method = "class")
dt_pred <- predict(dt_model, newdata = testData, type = "class")
dt_accuracy <- mean(dt_pred == testData$y)
accuracies <- c(accuracies, dt_accuracy)

# 3. SVM
svm_model <- svm(y ~ ., data = trainData, kernel = "radial", probability = TRUE)
svm_pred <- predict(svm_model, newdata = testData)
svm_accuracy <- mean(svm_pred == testData$y)
accuracies <- c(accuracies, svm_accuracy)

# 4. Naive Bayes
nb_model <- naiveBayes(y ~ ., data = trainData)
nb_pred <- predict(nb_model, newdata = testData)
nb_accuracy <- mean(nb_pred == testData$y)
accuracies <- c(accuracies, nb_accuracy)

# 5. Stacking Classifier (using mlr for stacking)
# Define base learners
base_learners <- c("classif.logreg", "classif.rpart", "classif.svm")
# Create a stacking learner
stacking_learner <- makeStackedLearner(base.learners = base_learners,
                                       predict.type = "prob",
                                       method = "classif.logreg")
# Train the stacking model
stacking_task <- makeClassifTask(data = trainData, target = "y")
stacking_model <- train(stacking_learner, stacking_task)
# Predict on test data
test_task <- makeClassifTask(data = testData, target = "y")
stacking_pred <- predict(stacking_model, task = test_task)$data$response
stacking_accuracy <- mean(stacking_pred == testData$y)
accuracies <- c(accuracies, stacking_accuracy)

# 6. AdaBoost
adaboost_model <- boosting(y ~ ., data = trainData, mfinal = 100)
adaboost_pred <- predict(adaboost_model, newdata = testData)$class
adaboost_accuracy <- mean(adaboost_pred == testData$y)
accuracies <- c(accuracies, adaboost_accuracy)

# Create the results table
model_names <- c("Logistic Regression", "Decision Tree", "SVM", "Naive Bayes", "Stacking Classifier", "AdaBoost")
results <- data.frame(Model = model_names, Accuracy = accuracies)

# Round the accuracies to 4 decimal places to match the table
results$Accuracy <- round(results$Accuracy, 4)

# Print the table
print(results)
