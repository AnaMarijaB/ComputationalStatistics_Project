
library(ggplot2)
library(dplyr)
library(rpart)
library(imputeTS)
library(knitr)
library(kableExtra)

column_names <- c("ID", "CHOL", "SGLU", "HDL", "GHB", "LOCATION", "AGE", "GENDER", "HHT", "WHT", "FRAME", "SBP", "DSP", "W", "H")
data <- read.table("Dataset5.txt", header = FALSE, sep = "\t", col.names = column_names)

#################################
# Explore
#################################

head(data)
summary(data)
dim(data)

#################################
# NA
#################################

num_na <- data %>% summarise(across(everything(), ~ sum(is.na(.))))
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

data <- input_na(data, "mean")
data <- data %>% select(-{'ID'})

###################################
num_vars <- sapply(data, is.numeric)
numeric_data <- data[, num_vars]


#################################
# HISTOGRAMS
#################################

# Numerical variables

par(mfrow = c(ceiling(sqrt(ncol(numeric_data))), ceiling(sqrt(ncol(numeric_data)))))

for (col in names(numeric_data)) {
  hist(numeric_data[[col]], main = paste("Histogram of", col), xlab = col, col = "lightblue", border = "black")
}

# Categorical variables

gender_summary <- data %>% group_by(GENDER) %>% summarise(count = n())
location_summary <- data %>% group_by(LOCATION) %>% summarise(count = n())
frame_summary <- data %>% group_by(FRAME) %>% summarise(count = n()) %>% mutate_all(~ifelse(. == "" | is.na(.), 'No value', .))

separator_code <- "<hr style='border-top: 1px solid #000000;'>"

# Combine the tables with the separator
combined_table <- cbind(
  kable(gender_summary) %>% kable_styling(),
  as.character(separator_code),
  kable(location_summary) %>% kable_styling(),
  as.character(separator_code),
  kable(frame_summary) %>% kable_styling()
)

# Print the combined table with styling
kable(combined_table, escape = FALSE, format = "html") %>%
  kable_styling(full_width = FALSE)
















