# install package
install.packages(c("forecast",
                   "leaps",
                   "gplots",
                   "reshape",
                   "GGally",
                   "MASS",
                   "cowplot"))
install.packages('cowplot')
install.packages(c("FNN",
                   "caret"))
install.packages('pROC')

library(pROC)
library(forecast)
library(leaps)
library(gplots)
library(reshape)
library(GGally)
library(MASS)
library(ggplot2)
library(dplyr)
library(cowplot)
library(e1071)
library(caret)
library(FNN)
#1. Data Exploration 
# Load data set 
telco.df <- read.csv("Telco-Customer-Churn.csv")

# Drop customerID
telco.df <- telco.df[, -c(1)]
summary(telco.df)

# Identify missing row 
colSums(is.na(telco.df))
which(is.na(telco.df))


# the data shows there are 11 missing value in the TotalCharges col 
# explore the distribution of column TotalCharges 
ggplot(telco.df, aes(TotalCharges)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  ggtitle("TotalCharges Distribution") + 
  theme_classic() + 
  theme(plot.title = element_text(size = 18))

#perform zero, mean, median imputation on TotalCharges
value_imputed <- data.frame(
  original = telco.df$TotalCharges,
  imputed_zero = replace(telco.df$TotalCharges, is.na(telco.df$TotalCharges), 0),
  imputed_mean = replace(telco.df$TotalCharges, is.na(telco.df$TotalCharges), mean(telco.df$TotalCharges, na.rm = TRUE)),
  imputed_median = replace(telco.df$TotalCharges, is.na(telco.df$TotalCharges), median(telco.df$TotalCharges, na.rm = TRUE))
)
value_imputed
h1 <- ggplot(value_imputed, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution") +
  theme_classic()
h2 <- ggplot(value_imputed, aes(x = imputed_zero)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("Zero-imputed distribution") +
  theme_classic()
h3 <- ggplot(value_imputed, aes(x = imputed_mean)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("Mean-imputed distribution") +
  theme_classic()
h4 <- ggplot(value_imputed, aes(x = imputed_median)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("Median-imputed distribution") +
  theme_classic()

plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

# Replace missing value with mean imputation to TotalCharge column 
telco.df$TotalCharges = replace(telco.df$TotalCharges, is.na(telco.df$TotalCharges),
                       mean(telco.df$TotalCharges, na.rm = TRUE))

# Declare categorical variable as factor 
telco.df$gender <- factor(telco.df$gender)
telco.df$SeniorCitizen <- factor(telco.df$SeniorCitizen)
telco.df$Partner <- factor(telco.df$Partner)
telco.df$Dependents <- factor(telco.df$Dependents)
telco.df$PhoneService <- factor(telco.df$PhoneService)
telco.df$PaperlessBilling <- factor(telco.df$PaperlessBilling)
telco.df$Contract <- factor(telco.df$Contract, levels = c("Month-to-month", "One year", "Two year"))
telco.df$PaymentMethod <- factor(telco.df$PaymentMethod, levels = c("Electronic check", "Mailed check", "Bank transfer (automatic)", "Credit card (automatic)"))
telco.df$MultipleLines <- factor(telco.df$MultipleLines, levels = c("Yes", "No", "No phone service"))
telco.df$InternetService <- factor(telco.df$InternetService, levels = c("DSL", "Fiber optic", "No"))
telco.df$OnlineSecurity <- factor(telco.df$OnlineSecurity, levels = c("Yes", "No", "No internet service"))
telco.df$OnlineBackup <- factor(telco.df$OnlineBackup, levels = c("Yes", "No", "No internet service"))
telco.df$DeviceProtection <- factor(telco.df$DeviceProtection, levels = c("Yes", "No", "No internet service"))
telco.df$TechSupport <- factor(telco.df$TechSupport, levels = c("Yes", "No", "No internet service"))
telco.df$StreamingTV <- factor(telco.df$StreamingTV, levels = c("Yes", "No", "No internet service"))
telco.df$StreamingMovies <- factor(telco.df$StreamingMovies, levels = c("Yes", "No", "No internet service"))
telco.df$Churn <- factor(telco.df$Churn)

# Categorical columns visualization 
# Gender 
barplot(table(telco.df$gender, telco.df$Churn), beside = TRUE, legend = TRUE,
        main = "Churn Comparison to Gender", 
        xlab = "Gender", ylab = "Count",
        col = c("blue", "red"))
legend("topright", legend = c("Female", "Male"), fill = c("blue", "red"))

# Senior Citizen 
barplot(table(telco.df$SeniorCitizen, telco.df$Churn), beside = TRUE,
        main = "Churn Comparison to SeniorCitizen", 
        xlab = "Senior Citizen", ylab = "Count",
        col = c("blue", "red"))
legend("topright", legend = c("Is Senior", "Not Senior"), fill = c("blue", "red"))

# Partner 
barplot(table(telco.df$Partner, telco.df$Churn), beside = TRUE,
        main = "Churn Comparison to Partner", 
        xlab = "Partner", ylab = "Count",
        col = c("blue", "red"))
legend("topright", legend = c("Has a partner", "Not"), fill = c("blue", "red"))

# Dependents 
barplot(table(telco.df$Dependents, telco.df$Churn), beside = TRUE,
        main = "Churn Comparison to Dependents", 
        xlab = "Dependents", ylab = "Count",
        col = c("blue", "red"))
legend("topright", legend = c("Customer has dependents", "Not"), fill = c("blue", "red"))

#Contract 
barplot(table(telco.df$Contract, telco.df$Churn), beside = TRUE,
        main = "Churn Comparison to Contract", 
        xlab = "Contract", ylab = "Count",
        col = c("blue", "red", "green"))
legend("topright", legend = c("Month-to-month", "One Year", "Two Year"), fill = c("blue", "red", "green"))

#PaperlessBilling 
barplot(table(telco.df$PaperlessBilling, telco.df$Churn), beside = TRUE,
        main = "Churn Comparison to PaperlessBilling", 
        xlab = "PaperlessBilling", ylab = "Count",
        col = c("blue", "red"))
legend("topright", legend = c("Customer's payment method", "Not"), fill = c("blue", "red"))

#PaymentMethod 
barplot(table(telco.df$PaymentMethod, telco.df$Churn), beside = TRUE,
        main = "Churn Comparison to PaymentMethod", 
        xlab = "PaymentMethod", ylab = "Count",
        col = c("blue", "red", "green", "yellow"))
legend("topright", legend = c("Electronic check", "Mailed check", "Bank transfer", "Credit card"), fill = c("blue", "red", "green", "yellow"))

#PhoneService 
barplot(table(telco.df$PhoneService, telco.df$Churn), beside = TRUE,
        main = "Churn Comparison to PhoneService", 
        xlab = "PhoneService", ylab = "Count",
        col = c("blue", "red"))
legend("topright", legend = c("Yes", "No"), fill = c("blue", "red"))

#MultipleLines 
barplot(table(telco.df$MultipleLines, telco.df$Churn), beside = TRUE,
        main = "Churn Comparison to MultipleLines", 
        xlab = "MultipleLines", ylab = "Count",
        col = c("blue", "red", "green"))
legend("topright", legend = c("Yes", "No", "No phone service"), fill = c("blue", "red", "green"))

#InternetService 
barplot(table(telco.df$InternetService, telco.df$Churn), beside = TRUE,
        main = "Churn Comparison to InternetService", 
        xlab = "InternetService", ylab = "Count",
        col = c("blue", "red", "green"))
legend("topright", legend = c("DSL", "Fiber optic", "No"), fill = c("blue", "red", "green"))

#OnlineSecurity 
barplot(table(telco.df$OnlineSecurity, telco.df$Churn), beside = TRUE,
        main = "Churn Comparison to OnlineSecurity", 
        xlab = "OnlineSecurity", ylab = "Count",
        col = c("blue", "red", "green"))
legend("topright", legend = c("Yes", "No", "No internet service"), fill = c("blue", "red", "green"))

#OnlineBackup 
barplot(table(telco.df$OnlineBackup, telco.df$Churn), beside = TRUE,
        main = "Churn Comparison to OnlineBackup", 
        xlab = "OnlineBackup", ylab = "Count",
        col = c("blue", "red", "green"))
legend("topright", legend = c("Yes", "No", "No internet service"), fill = c("blue", "red", "green"))

#DeviceProtection 
barplot(table(telco.df$DeviceProtection, telco.df$Churn), beside = TRUE,
        main = "Churn Comparison to DeviceProtection", 
        xlab = "DeviceProtection", ylab = "Count",
        col = c("blue", "red", "green"))
legend("topright", legend = c("Yes", "No", "No internet service"), fill = c("blue", "red", "green"))

#TechSupport 
barplot(table(telco.df$TechSupport, telco.df$Churn), beside = TRUE,
        main = "Churn Comparison to TechSupport", 
        xlab = "TechSupport", ylab = "Count",
        col = c("blue", "red", "green"))
legend("topright", legend = c("Yes", "No", "No internet service"), fill = c("blue", "red", "green"))

# Continuous columns visualization
#tenure 
hist(telco.df$tenure[telco.df$Churn == "Yes"], col = "red", 
     main = "Churn Comparison tenure (Churn == Yes)", 
     xlab = "Tenure", ylab = "Frequency")

hist(telco.df$tenure[telco.df$Churn == "No"], col = "blue", 
     main = "Churn Comparison tenure (Churn == No)", 
     xlab = "Tenure", ylab = "Frequency", add = TRUE)

#MonthlyCharges 
# Plot for Churn == Yes
hist(telco.df$MonthlyCharges[telco.df$Churn == "Yes"], col = "red", 
     main = "Churn Comparison MonthlyCharge (Churn == Yes)", 
     xlab = "Monthly Charges", ylab = "Frequency")

# Plot for Churn == No
hist(telco.df$MonthlyCharges[telco.df$Churn == "No"], col = "blue", 
     main = "Churn Comparison MonthlyCharge (Churn == No)", 
     xlab = "Monthly Charges", ylab = "Frequency")

#TotalCharges 
# Plot histogram for Churn == Yes
hist(telco.df$TotalCharges[telco.df$Churn == "Yes"], col = "red", 
     main = "Churn Comparison TotalCharge (Churn == Yes)", 
     xlab = "TotalCharge", ylab = "Frequency")

# Plot histogram for Churn == No
hist(telco.df$TotalCharges[telco.df$Churn == "No"], col = "blue", 
     main = "Churn Comparison TotalCharge (Churn == No)", 
     xlab = "TotalCharge", ylab = "Frequency")
#List of categorical variables to apply hot encoding 
categorical_vars <- c("Contract", 
                      "PaymentMethod", 
                      "MultipleLines", 
                      "InternetService", 
                      "OnlineSecurity",
                      "OnlineBackup",
                      "DeviceProtection",
                      "TechSupport",
                      "StreamingTV",
                      "StreamingMovies")

# Perform one-hot encoding for each categorical variable 
encoded_Contract <- model.matrix(~ Contract - 1, data = telco.df)
encoded_PaymentMethod <- model.matrix(~ PaymentMethod - 1, data = telco.df)
encoded_MultipleLines <- model.matrix(~ MultipleLines - 1, data = telco.df)
encoded_InternetService <- model.matrix(~ InternetService - 1, data = telco.df)
encoded_OnlineSecurity <- model.matrix(~ OnlineSecurity - 1, data = telco.df)
encoded_OnlineBackup <- model.matrix(~ OnlineBackup - 1, data = telco.df)
encoded_DeviceProtection <- model.matrix(~ DeviceProtection - 1, data = telco.df)
encoded_TechSupport <- model.matrix(~ TechSupport - 1, data = telco.df)
encoded_StreamingTV <- model.matrix(~ StreamingTV - 1, data = telco.df)
encoded_StreamingMovies <- model.matrix(~ StreamingMovies - 1, data = telco.df)

# Combine encoded variables with original dataset 
telco.df <- cbind(telco.df, encoded_Contract)
telco.df <- cbind(telco.df, encoded_PaymentMethod)
telco.df <- cbind(telco.df, encoded_MultipleLines)
telco.df <- cbind(telco.df, encoded_InternetService)
telco.df <- cbind(telco.df, encoded_OnlineSecurity)
telco.df <- cbind(telco.df, encoded_OnlineBackup)
telco.df <- cbind(telco.df, encoded_DeviceProtection)
telco.df <- cbind(telco.df, encoded_TechSupport)
telco.df <- cbind(telco.df, encoded_StreamingTV)
telco.df <- cbind(telco.df, encoded_StreamingMovies)

# Remove original categorical variables 
telco.df <- telco.df[, !(names(telco.df) %in% c("Contract", 
                                                "PaymentMethod", 
                                                "MultipleLines", 
                                                "InternetService", 
                                                "OnlineSecurity",
                                                "OnlineBackup",
                                                "DeviceProtection",
                                                "TechSupport",
                                                "StreamingTV",
                                                "StreamingMovies"))]

# transform into factor for the dummy variable 
telco.df$`ContractTwo year` <- factor(telco.df$`ContractTwo year`)
telco.df$`MultipleLinesNo phone service` <- factor(telco.df$`MultipleLinesNo phone service`)
telco.df$InternetServiceNo <- factor(telco.df$InternetServiceNo)
telco.df$OnlineSecurityNo <- factor(telco.df$OnlineSecurityNo)
telco.df$`OnlineSecurityNo internet service` <- factor(telco.df$`OnlineSecurityNo internet service`)
telco.df$OnlineBackupNo <- factor(telco.df$OnlineBackupNo)
telco.df$`OnlineBackupNo internet service` <- factor(telco.df$`OnlineBackupNo internet service`)
telco.df$DeviceProtectionNo <- factor(telco.df$DeviceProtectionNo)
telco.df$`DeviceProtectionNo internet service` <- factor(telco.df$`DeviceProtectionNo internet service`)
telco.df$TechSupportNo <- factor(telco.df$TechSupportNo)
telco.df$`TechSupportNo internet service` <- factor(telco.df$`TechSupportNo internet service`)
telco.df$StreamingTVNo <- factor(telco.df$StreamingTVNo)
telco.df$`StreamingTVNo internet service` <- factor(telco.df$`StreamingTVNo internet service`)

#Variable Selection 
selected.var <- c(1, 2, 4, 5, 8, 9, 10, 11, 12, 13, 14,
                  17, 18, 19, 20, 21, 22, 23, 24,
                  25, 26, 27, 28, 29, 30, 31, 32, 33,
                  34, 35, 36, 37, 38)
# List selected variables 
names(telco.df)[selected.var]
# Create data frame with selected var 
telco.sdf <- telco.df[selected.var]

#2. Data Preprocessing (optional)
#3. Data and dimension reduction (optional)
#4. Partition the data & #5 Model Building 
# Calculate the number of observations per subset and the remainder
observations_per_subset <- floor(nrow(telco.sdf) / 10)
remainder <- nrow(telco.sdf) %% 10

# Initialize the list to hold subsets 
telco.sdf.list <- list()

# Generate random indices 
random.indices <- sample(nrow(telco.sdf))

# Partition the data into 10 subsets
set.seed(100)
start_index <- 1 
for (i in 1:10) {
  end_index <- start_index + observations_per_subset - 1
  if (i <= remainder) {
    end_index <- end_index + 1
  }
  telco.sdf.list[[i]] <- telco.sdf[random.indices[start_index:end_index], ]
  start_index <- end_index + 1
}

# Initialize lists to store confusion matrices for each fold 
nb.train.confM <- NULL
nb.valid.confM <- NULL
logit.train.confM <- NULL
logit.valid.confM <- NULL
# Initialize empty vectors to store predictions 
all.train.pred.nb <- c()
all.valid.pred.nb <- c()
all.train.pred.logit <- c()
all.valid.pred.logit <- c()


# Perform k-fold cross-validation 
for (i in 1:10) {
  # Get the current fold's validation set 
  valid.set <- telco.sdf.list[[i]]
  
  # Combine the remaining data to form the training set
  train_indices <- setdiff(seq_len(nrow(telco.sdf)), as.integer(names(telco.sdf.list[[i]])))
  train.set <- telco.sdf[train_indices, ]
  
  # Naive Bayes Model 
  nb.model <- naiveBayes(Churn ~ ., data = train.set)
  train.pred.nb <- predict(nb.model, newdata = train.set, type = "class")
  valid.pred.nb <- predict(nb.model, newdata = valid.set, type = "class")
  
  # Aggregate predictions 
  all.train.pred.nb <- c(all.train.pred.nb, train.pred.nb)
  all.valid.pred.nb <- c(all.valid.pred.nb, valid.pred.nb)
  
  # Logistic Regression Model (Classification)
  logit.model <- glm(Churn ~ ., data = train.set, family = binomial)
  train.pred.logit <- predict(logit.model, train.set, type = 'response')
  train.pred.logit.class <- ifelse(train.pred.logit > 0.5, "Yes", "No")
  valid.pred.logit <- predict(logit.model, valid.set, type = 'response')
  valid.pred.logit.class <- ifelse(valid.pred.logit > 0.5, "Yes", "No")
  
  # Aggregate predictions
  all.train.pred.logit <- c(all.train.pred.logit, train.pred.logit.class)
  all.valid.pred.logit <- c(all.valid.pred.logit, valid.pred.logit.class)
  
}

# System produces warning message of NAs introduced by coercion 
# Checking for missing values after the loop 
missing_values <- sum(is.na(logit.model))
if (missing_values > 0) {
  cat("There are", missing_values, "missing values in the dataset.")
} else {
  cat("There are no missing values in the dataset.")
}
missing_values <- sum(is.na(telco.sdf.list))
if (missing_values > 0) {
  cat("There are", missing_values, "missing values in the dataset.")
} else {
  cat("There are no missing values in the dataset.")
}

# Calculate confusion matrices for NAive Bayes 
nb.train.confM <- confusionMatrix(factor(train.pred.nb, levels = c("Yes", "No")),
                                       factor(train.set$Churn, levels = c("Yes", "No")))
nb.valid.confM <- confusionMatrix(factor(valid.pred.nb, levels = c("Yes", "No")),
                                       factor(valid.set$Churn, levels = c("Yes", "No")))

# Calculate confusion matrices for Logistic Regression 
logit.train.confM <- confusionMatrix(factor(train.pred.logit.class, levels = c("Yes", "No")),
                                          factor(train.set$Churn, levels = c("Yes", "No")))
logit.valid.confM <- confusionMatrix(factor(valid.pred.logit.class, levels = c("Yes", "No")),
                                          factor(valid.set$Churn, levels = c("Yes", "No")))
nb.train.confM
nb.valid.confM
logit.train.confM
logit.valid.confM
#6.Performance Evaluation
# Naive Bayes train 
nb.train.confM

print("Naive Bayes train:")
cat("Accuracy:", 0.7373)
cat("Sensitivity:", 0.7919)
cat("Specificity:", 0.7176)
cat("Precision:", 0.5032)
cat("FDR:",  0.4968)
cat("FOR:", 0.095)

# Naive Bayes valid 
nb.valid.confM

print("Naive Bayes valid:")
cat("Accuracy:", 0.7543)
cat("Sensitivity:", 0.7889)
cat("Specificity:", 0.7424)
cat("Precision:", 0.5126)
cat("FDR:", 0.4874)
cat("FOR:", 0.0889)
# Lift Chart 
nb_probs <- predict(nb.model, newdata = valid.set, type = "raw")[, "Yes"]

# Sort instances by predicted probabilities in descending order
sorted_indices <- order(nb_probs, decreasing = TRUE)
sorted_probs <- nb_probs[sorted_indices]

# Calculate cumulative gains
cumulative_gains <- cumsum(valid.set$Churn[sorted_indices] == "Yes")

# Calculate ideal cumulative gains
total_positives <- sum(valid.set$Churn == "Yes")
ideal_cumulative_gains <- cumsum(rep(1, length(sorted_probs)))

# Plot the lift chart
plot(100 * cumulative_gains / total_positives, type = "l", xlab = "Percentage of Instances", ylab = "Cumulative Gains", ylim = c(0, 100), col = "blue", lwd = 2)
# Limiting the y values to fall within the plot range (0 to 100)
y_values <- pmin(pmax(100 * ideal_cumulative_gains / total_positives, 0), 100)

# Plot the line
lines(y_values, col = "red", lty = 2, lwd = 2)
legend("bottomright", legend = c("Naive Bayes Model", "Ideal"), col = c("blue", "red"), lty = c(1, 2), lwd = 2)

# ROC chart 
# Predict probabilities of the positive class
nb_probs <- predict(nb.model, newdata = valid.set, type = "raw")[, "Yes"]

# Sort probabilities and true labels by descending order of probabilities
sorted_data <- data.frame(prob = nb_probs, label = valid.set$Churn)[order(-nb_probs), ]

# Calculate total positive and negative instances
total_positives <- sum(sorted_data$label == "Yes")
total_negatives <- sum(sorted_data$label == "No")

# Calculate cumulative True Positive Rate (Sensitivity) and False Positive Rate (1 - Specificity)
cumulative_TPR <- cumsum(sorted_data$label == "Yes") / total_positives
cumulative_FPR <- cumsum(sorted_data$label == "No") / total_negatives

# Plot ROC chart
plot(cumulative_FPR, cumulative_TPR, type = "l", col = "blue", lwd = 2, xlim = c(0, 1), ylim = c(0, 1),
     xlab = "False Positive Rate", ylab = "True Positive Rate", main = "ROC Chart for Naive Bayes Model")

# Get the maximum value of FPR and TPR
max_value <- max(c(max(cumulative_FPR), max(cumulative_TPR)))

# Add a diagonal reference line (random classifier)
abline(a = 0, b = 1, lty = 2, col = "red", xlim = c(0, max_value), ylim = c(0, max_value))
# Add legend
legend("bottomright", legend = c("Naive Bayes Model", "Random"), col = c("blue", "red"), lty = c(1, 2), lwd = 2)

# Compute AUC
auc_value <- auc(cumulative_FPR, cumulative_TPR)
# Print AUC
print(auc_value)

# Logistic Regression train  
logit.train.confM
print("Logistic Regression train:")
cat("Accuracy:", 0.8004)
cat("Sensitivity:", 0.5431)
cat("Specificity:", 0.8933)
cat("Precision:", 0.6477)
cat("FDR:", 0.3522)
cat("FOR:", 0.1559)

# Logistic Regression valid 
logit.valid.confM

print("Logistic Regression valid:")
cat("Accuracy:", 0.8068)
cat("Sensitivity:", 0.5444)
cat("Specificity:", 0.8969)
cat("Precision:", 0.6447)
cat("FDR:", 0.3553)
cat("FOR:", 0.1486)

# Lift Chart 
logit_probs <- predict(logit.model, newdata = valid.set, type = "response")

# Sort instances by predicted probabilities in descending order
sorted_indices <- order(logit_probs, decreasing = TRUE)
sorted_probs <- logit_probs[sorted_indices]

# Calculate cumulative gains
cumulative_gains <- cumsum(valid.set$Churn[sorted_indices] == "Yes")

# Calculate ideal cumulative gains
total_positives <- sum(valid.set$Churn == "Yes")
ideal_cumulative_gains <- cumsum(rep(1, length(sorted_probs)))

# Plot the lift chart
plot(100 * cumulative_gains / total_positives, type = "l",main = "Lift Chart for Logistic Regression", xlab = "Percentage of Instances", ylab = "Cumulative Gains", ylim = c(0, 100), col = "blue", lwd = 2)
# Limiting the y values to fall within the plot range (0 to 100)
y_values <- pmin(pmax(100 * ideal_cumulative_gains / total_positives, 0), 100)

# Plot the line
lines(y_values, col = "red", lty = 2, lwd = 2)
legend("bottomright", legend = c("Logistic Regression Model", "Ideal"), col = c("blue", "red"), lty = 1:2, lwd = 2)

# ROC Chart
# Predict probabilities of the positive class
logit_probs <- predict(logit.model, newdata = valid.set, type = "response")

# Sort probabilities and true labels by descending order of probabilities
sorted_data <- data.frame(prob = logit_probs, label = valid.set$Churn)[order(-logit_probs), ]

# Calculate total positive and negative instances
total_positives <- sum(sorted_data$label == "Yes")
total_negatives <- sum(sorted_data$label == "No")

# Calculate cumulative True Positive Rate (Sensitivity) and False Positive Rate (1 - Specificity)
cumulative_TPR <- cumsum(sorted_data$label == "Yes") / total_positives
cumulative_FPR <- cumsum(sorted_data$label == "No") / total_negatives

# Plot ROC chart
plot(cumulative_FPR, cumulative_TPR, type = "l", col = "blue", lwd = 2, xlim = c(0, 1), ylim = c(0, 1),
     xlab = "False Positive Rate", ylab = "True Positive Rate", main = "ROC Chart for Logistic Regression Model")

# Add a diagonal reference line (random classifier)
abline(a = 0, b = 1, lty = 2, col = "red")
# Add legend
legend("bottomright", legend = c("Logistic Regression Model", "Random"), col = c("blue", "red"), lty = c(1, 2), lwd = 2)
# Compute AUC
auc_value <- auc(cumulative_FPR, cumulative_TPR)

# Print AUC
print(auc_value)

#7 Model Deployment
# Selected Logistic Regression as the model deployment 
# Get the coefficients 
coefficients <- coef(logit.model)
# Print the coefficients 
print(coefficients)



