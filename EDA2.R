
library(ggplot2)
library(dplyr)
library(rpart)
library(imputeTS)
library(knitr)
library(kableExtra)
library(corrplot)
library(psych)
library(tidyverse)
library(fastDummies)

column_names <- c("ID", "CHOL", "SGLU", "HDL", "GHB", "LOCATION", "AGE", "GENDER", "HHT", "WHT", "FRAME", "SBP", "DSP", "W", "H")
data <- read.table("Data/Dataset5.txt", header = FALSE, sep = "\t", col.names = column_names)
data <- data %>% select(-{'ID'})

#################################
# Explore
#################################

head(data)
summary(data)
dim(data)

#################################
# CATEGORICAL VARIABLES
#################################

gender_summary <- data %>% group_by(GENDER) %>% summarise(count = n())
location_summary <- data %>% group_by(LOCATION) %>% summarise(count = n())
frame_summary <- data %>% group_by(FRAME) %>% summarise(count = n()) %>% mutate_all(~ifelse(. == "" | is.na(.), 'No value', .))



separator_code <- "<hr style='border-top: 1px solid #000000;'>"

combined_table <- cbind(
  kable(gender_summary) %>% kable_styling(),
  as.character(separator_code),
  kable(location_summary) %>% kable_styling(),
  as.character(separator_code),
  kable(frame_summary) %>% kable_styling()
)

kable(combined_table, escape = FALSE, format = "html") %>%
  kable_styling(full_width = FALSE)

#################################
# NUMERICAL VARIABLES
#################################

num_vars <- sapply(data, is.numeric)
numeric_data <- data[, num_vars]

# histograms

par(mfrow = c(ceiling(sqrt(ncol(numeric_data))), ceiling(sqrt(ncol(numeric_data)))))

par(mfrow = c(4,3))

for (col in names(numeric_data)) {
  hist(numeric_data[[col]], probability = TRUE, main = paste("Histogram of", col), xlab = col, col = "lightblue", border = "black")
  lines(density(numeric_data[[col]], na.rm = TRUE), col = "darkorange", lwd = 2)
}

# boxplots

my_data_long <- tidyr::gather(numeric_data)

ggplot(my_data_long, aes(x = "", y = value, fill = key)) +
  geom_boxplot() +
  facet_wrap(~key, scales = "free_y", labeller = label_parsed, ncol = length(unique(my_data_long$key))) +
  theme_minimal() +
  labs(y = "Value") +
  theme(strip.placement = "outside", strip.background = element_blank()) +
  guides(fill = FALSE)

################################
# outliers
###############################

outliers <- c('SBP', 'DSP', 'CHOL', 'HDL')

data_out <- data %>%
  mutate_at(
    vars(outliers),
    funs(if_else(
      . < quantile(., 0.25, na.rm = TRUE) - 1.5 * IQR(., na.rm = TRUE) |
        . > quantile(., 0.75, na.rm = TRUE) + 1.5 * IQR(., na.rm = TRUE),
      quantile(., 0.5, na.rm = TRUE),
      .
    ))
  )

#################################
# NA
#################################

col_na <- colnames(data)[colSums(is.na(data)) > 0]

na_sum <- colSums(is.na(numeric_data)) 
kable(na_sum, format = "html", longtable = F) %>%
  kable_styling(full_width = FALSE)

# insertamo mean namesto NA vrednosti

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

data_mvi <- input_na(data_out, "mean")

################################
# DATA ENCODING
###############################

data_encoded <- data_mvi %>% mutate(GENDER_num = if_else(GENDER == "female", 1, 0), 
                                LOCATION_num = if_else(LOCATION == "Buckingham", 1, 0),
                                FRAME = if_else(FRAME == "", 'medium', FRAME))
data_encoded <- dummy_cols(data_encoded, select_columns = "FRAME", remove_selected_columns = TRUE)
data_encoded <- data_encoded %>% select(-c('FRAME_small', 'GENDER', 'LOCATION'))

data_encoded <- data_encoded %>% mutate(BMI = (W / (H ^2))*703)

#################################
# CORRELATION ANALYSIS
#################################

# correlation matrix

corrplot(cor(data_encoded), type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# scatterplots + correlation coef

pairs.panels(numeric_data, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

###############################
# SAVING PREPARED DATA
##############################

write.csv(data_encoded, "Data/data_encoded.csv", row.names = TRUE)




