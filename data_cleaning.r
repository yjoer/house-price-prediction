# import necessary library
library(dplyr)
library(readr)
library(ggplot2)
library(psych)
library(gridExtra)
library(DescTools)
library(corrplot)
library(caret)
library(mlr)

# check current working directory
getwd()

# change working directory
# setwd("C:/Users/peili/Downloads/Data Cleaning")

# Import Dataset
dataset <- read.csv("train.csv")
head(dataset)

# Dataset Overview
## There are 1460 records (rows) and 81 attributes (columns). 
## Dataset are mostly made up of numerical value (int64 & float64) and categorical value (object). Statistics are provided for numerical variables only. 
cat("Total rows & columns: ", dim(dataset), '\n\n')
str(dataset)
summary(dataset)



# Data Preprocessing
## check for sum of duplicates between columns
## no duplication inside the dataset
duplicates <- sum(duplicated(dataset))
cat(paste("Number of duplicates:", duplicates)) 

## Remove MeaningLess Column
## At this stage, we have removed irrelevant columns from the dataset.
## 'Id' column is removed, as it is a unique identifier for each customer. 
## 'GarageCars' column is removed, as it provides redundant information already captured by the 'GarageArea' column. 
## We also removed the 'Exterior2nd' column, as it represents a limited set of additional choices for exterior covering. 
## Furthermore, we removed the 'GarageYrBlt' column, as the year of construction is typically the same as the 'YearBuilt' column. 
## Finally, we removed the 'TotRmsAbvGrd' column, as it provides a similar meaning as the 'First Floor' square footage.
dataset <- subset(dataset, select = -c(Id, GarageCars, Exterior2nd, GarageYrBlt, TotRmsAbvGrd))



## Check for Missing Values
## There are total of 19 columns existing with missing values
## 5 of them having the missing vales higher than 20%
## Columns which having the missing values higher than 20%: PoolQC, MiscFeature, Alley, Fence, FireplaceQu 
## Column with missing values higher than 20% will be remove in this stage

# Calculate missing percentages
missing_percentages <- colSums(is.na(dataset)) / nrow(dataset) * 100

# create dataframe with column names and missing percentages
missing_dataset <- data.frame(
  column_name = names(missing_percentages),
  missing_percentage = missing_percentages
) %>%
  # sort the dataframe by missing percentages in descending order
  arrange(desc(missing_percentage)) %>%
  # filter out columns with 0 missing percentages
  filter(missing_percentage > 0)

# Print the missing dataset
print(missing_dataset)

# Remove column with missing value more than 20%
dataset <- subset(dataset, select = -c(PoolQC, MiscFeature, Alley, Fence, FireplaceQu))



## Impute numerical missing data using mean & Impute categorical missing data using mode
## Median is choose to fill in missing values, rather than the mean.
## The mean is sensitive to extreme values and can be heavily influenced by outliers in the data, which can lead to biased estimates. In contrast, the median is a robust measure of central tendency that is less sensitive to extreme values and is more appropriate for skewed data.

# identify numerical columns & replace missing numerical values with mean
num_cols <- sapply(dataset, is.numeric) 
for (col in names(dataset[,num_cols])) {
  col_mean <- mean(dataset[,col], na.rm=TRUE)
  dataset[,col][is.na(dataset[,col])] <- rep(col_mean, sum(is.na(dataset[,col])))
}

# identify categorical columns & replace missing categorical values with mode
cat_cols <- sapply(dataset, is.factor) | sapply(dataset, is.character) # identify categorical or character columns
for (col in names(dataset[,cat_cols])) {
  dataset[,col][is.na(dataset[,col])] <- as.character(Mode(dataset[,col], na.rm=TRUE))
}





#### Explore Data - Explaratory Data Analysis (EDA)
### Plot Distribution of Numeric Data
## Majority of numerical column's distribution are right skewed (31 attribute), however 3 of them are left skewed

# Select numerical columns using the "is.numeric" function
# Select categorical columns using the "is.factor" or "is.character" function
numerical_cols <- names(dataset)[sapply(dataset, is.numeric)]
categorical_cols <- names(dataset)[sapply(dataset, is.factor) | sapply(dataset, is.character)]

# Create new data frames with only numerical and categorical columns
numerical_data <- dataset[numerical_cols]
categorical_data <- dataset[categorical_cols]


#Create a function to plot probability density plot and skewness
plot_density <- function(column, col_name) {
  # Compute the skewness of the data
  skewness <- skew(column)
  
  # Set the plot color based on the skewness
  if (skewness < 0) {
    plot_color <- "blue"
  } else {
    plot_color <- "red"
  }
  
  # Create the density plot
  plot_title <- paste("Skewness:", round(skewness, 2), "| Column:", col_name)
  density_plot <- ggplot(data = data.frame(column), aes(x = column)) +
    geom_density(fill = plot_color, alpha = 0.3) +
    ggtitle(plot_title)
  
  return(density_plot)
}

# Apply the plot_density function to each column of numerical data in numerical_data
plots <- lapply(names(numerical_data), function(col_name) {
  plot_density(numerical_data[[col_name]], col_name)
})

# Arrange the resulting plots in a grid with 5 columns
grid.arrange(grobs = plots, ncol = 5)


## Check Outlier by using boxplot
# create a list of boxplots for first 20 attributes
# Outlier found in most of the columns- 27 attributes
boxplot_numerical_first20attributes <- lapply(1:20, function(i) {
  ggplot(numerical_data, aes(x = 1, y = numerical_data[, i])) + 
    geom_boxplot() + 
    ggtitle(paste0("Boxplot of ", colnames(numerical_data)[i])) + 
    xlab("") + 
    ylab(colnames(numerical_data)[i])
})

# create a list of boxplots for rest 14 attributes
boxplot_numerical_rest14attributes <- lapply(21:34, function(i) {
  ggplot(numerical_data, aes(x = 1, y = numerical_data[, i])) + 
    geom_boxplot() + 
    ggtitle(paste0("Boxplot of ", colnames(numerical_data)[i])) + 
    xlab("") + 
    ylab(colnames(numerical_data)[i])
})

grid.arrange(grobs = boxplot_numerical_first20attributes, ncol = 5)
grid.arrange(grobs = boxplot_numerical_rest14attributes, ncol = 5)

# Perform Correlation Matrix
correlation_matrix <- cor(numerical_data, method = "pearson")
print(correlation_matrix)

# Create heatmap of correlation matrix
corrplot(correlation_matrix, method = "color", type = "upper", 
         order = "hclust", tl.col = "black", tl.cex=0.3,tl.srt = 90)

print(correlation_matrix)
high_corr <- which(abs((correlation_matrix) > 0.8 | correlation_matrix < -0.8)& correlation_matrix!= 1, arr.ind=TRUE)
high_corr_values <- correlation_matrix[high_corr]
high_corr_df <- data.frame(row=high_corr[,1], col=high_corr[,2], corr=high_corr_values)
high_corr_df



# Categorical Data
# Display first six rows of the categorical data frame
head(categorical_data)

# print bar plot for categorical data for first 20 attributes
plots_first20attribute <- lapply(colnames(categorical_data)[1:20], function(col) {
  ggplot(categorical_data, aes_string(x = col)) +
    geom_bar() +
    xlab(col) +
    ylab("Count") +
    ggtitle(paste0("Distribution of ", col))+
    scale_x_discrete(guide = guide_axis(check.overlap = TRUE))
})

# print bar plot for categorical data for rest 17 attributes
plots_rest23attribute <- lapply(colnames(categorical_data)[21:37], function(col) {
  ggplot(categorical_data, aes_string(x = col)) +
    geom_bar() +
    xlab(col) +
    ylab("Count") +
    ggtitle(paste0("Distribution of ", col))+
    scale_x_discrete(guide = guide_axis(check.overlap = TRUE))
})

# print the plots in a grid
grid.arrange(grobs = plots_first20attribute, ncol = 5)
grid.arrange(grobs = plots_rest23attribute, ncol = 5)




# Encode Categorical Data
# Create a copy of the dataset
# dataset_encode <- dataset

# for (col in colnames(dataset_encode)) {
#   if (is.factor(dataset_encode[[col]]) | is.character(dataset_encode[[col]])) {
#     encoded_col <- predict(dummyVars(formula = as.formula(paste0("~", col)), data = dataset_encode), newdata = dataset_encode)
#     dataset_encode[[col]] <- encoded_col
#   }
# }

