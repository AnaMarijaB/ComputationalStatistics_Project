#################################################################
# EDA
#################################################################


library(ggplot2)
library(dplyr)
library(rpart)


column_names <- c("ID", "CHOL", "SGLU", "HDL", "GHB", "LOCATION", "AGE", "GENDER", "HHT", "WHT", "FRAME", "SBP", "DSP", "W", "H")
data <- read.table("Dataset5.txt", header = FALSE, sep = "\t", col.names = column_names)

#################################
# Explore
#################################

head(data)
summary(data)
str(data)
dim(data)


#################################
# NA
#################################


col_na <- colnames(data)[colSums(is.na(data)) > 0]
# only numerical variables has NA values 

input_na <- function(data, method){
  if (method == "delete") {
    data <- na.omit(data)
  } else if (method == "mean") {
    data <- data %>% mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
  } else if (method == "medain"){
    data <- data %>% mutate(across(everything(), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))
  } else if (method == "model"){
    cols_na <- colnames(data)[colSums(is.na(data)) > 0]
    for (col in cols_na) {
      df_without_missing <- data[complete.cases(data), ]
      predictors <- setdiff(names(data), df_without_missing)
      
      # Train a decision tree model
      decision_tree_model <- rpart(
        paste(col, "~ ."),
        data = df_without_missing,
        method = "class"
      )
      # Impute missing values using the decision tree model
      missing_indices <- is.na(data[[col]])
      predicted_values <- predict(decision_tree_model, newdata = data[missing_indices, ], type = "class")
      data[missing_indices, col] <- predicted_values
    }
  } else {
    print("wrong method")
  }
  return(data)
}

data <- input_na(data, "model")

write.csv(data, "Data/ful_data.csv", row.names = FALSE)




#################################
# Numerical variables
#################################

num_vars <- sapply(data, is.numeric)
numeric_data <- data[, num_vars]

# corr matrix
correlation_matrix <- cor(numeric_data)
correlation_with_target <- correlation_matrix["GHB", ]
# potrebno preveriti kjer se pojavi vrednost 1 ali -1


# Example using the 'ggplot2' package
ggplot(numeric_data, aes(x = CHOL, y = SGLU)) +
  geom_point() +
  labs(title = "Scatter Plot of CHOL & SGLU")
# se ne vidi nic pametnega (mogoce samo za kasno posebno kombinacijo)



ggplot(numeric_data, aes(x = HDL, y = GHB)) +
  geom_boxplot() +
  labs(title = "Box Plot of HDL &  GHB")



ggplot(numeric_data, aes(x = HDL)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(title = "Histogram of HDL", x = "HDL", y = "Frequency")


# Outliers
ggplot(data, aes(x = 1, y = HDL)) +
  geom_boxplot() +
  labs(title = "Outlier Detection for HDL")



# Pairwise scatterplot matrix
pairs(numeric_data)

shape <- dim(data)
n <- shape[1]
m <- shape[2]


par(mfrow=c(2,2))
for (col in colnames(numeric_data)) {
  hist(data[[col]], main=col, xlab="Value", col="lightblue", border="black")
}





low_threshold <- 0.3
large_threshold <- 0.7


low_correlations <- which(abs(correlation_matrix) < low_threshold & abs(correlation_matrix) > 0, arr.ind = TRUE)

# Identify large correlations
large_correlations <- which(abs(correlation_matrix) > large_threshold & abs(correlation_matrix) < 1, arr.ind = TRUE)

# Print the results
print("Low Correlations:")
print(low_correlations)
print("Large Correlations:")
print(large_correlations)







#################################
# Chategorical variables
#################################

#....








