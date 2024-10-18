
#setwd("C:\\Users\\rutep\\Desktop\\Faculdade de Ciências\\Mestrado em Ciência de Dados\\2º Semestre\\Regressão e Análise da Variância\\Problem to Delivery\\Problem to Delivery 2")
# 2.1.
# Load necessary libraries
library(tidyverse)
library(caret)
library(car)

# Load the data
houses_train<-read.csv("train.csv",header=TRUE)
head(houses_train)
houses_train <- subset(houses_train, select = c("LotArea", "YearBuilt", "YearRemodAdd", "MasVnrArea", "BsmtFinSF1", 
                                                "BsmtFinSF2", "BsmtUnfSF", "X1stFlrSF", "X2ndFlrSF", "LowQualFinSF", 
                                                "BsmtFullBath", "BsmtHalfBath", "FullBath", "HalfBath", "BedroomAbvGr", 
                                                "KitchenAbvGr", "TotRmsAbvGrd", "Fireplaces", "GarageCars", "GarageArea", 
                                                "WoodDeckSF", "OpenPorchSF", "EnclosedPorch", "X3SsnPorch", "ScreenPorch", 
                                                "PoolArea", "MiscVal", "MoSold", "YrSold", "MSSubClass", "MSZoning", 
                                                "Street", "LotShape", "LandContour", "Utilities", "LotConfig", "LandSlope", 
                                                "Neighborhood", "Condition1", "Condition2", "BldgType", "HouseStyle", 
                                                "OverallQual", "OverallCond", "RoofStyle", "RoofMatl", "Exterior1st", 
                                                "Exterior2nd", "MasVnrType", "ExterQual", "ExterCond", "Foundation", 
                                                "Heating", "HeatingQC", "CentralAir", "Electrical", "KitchenQual", 
                                                "Functional", "PavedDrive", "SaleType", "SaleCondition","SalePrice"))
head(houses_train)
missing_count <- sum(is.na(houses_train))
missing_count
# Total number of rows with at least a missing value
rows_with_missing <- sum(apply(houses_train, 1, function(x) any(is.na(x))))
rows_with_missing
# Removing the rows with at least one missing value
houses_train<-houses_train[complete.cases(houses_train), ]
# Checking if it worked
missing_count <- sum(is.na(houses_train))
missing_count
# Inspect the data
glimpse(houses_train)
summary(houses_train)

# 2.2.
houses_train$SalePrice <- houses_train$SalePrice / 10000
houses_train_full_model <- houses_train

# 2.3.
# Continuous variables
continuous_vars <- c("LotArea", "YearBuilt", "YearRemodAdd", "MasVnrArea", "BsmtFinSF1", "BsmtFinSF2",
                     "BsmtUnfSF", "X1stFlrSF", "X2ndFlrSF", "LowQualFinSF", "BsmtFullBath", "BsmtHalfBath",
                     "FullBath", "HalfBath", "BedroomAbvGr", "KitchenAbvGr", "TotRmsAbvGrd", "Fireplaces",
                     "GarageCars", "GarageArea", "WoodDeckSF", "OpenPorchSF", "EnclosedPorch", "X3SsnPorch",
                     "ScreenPorch", "PoolArea", "MiscVal", "MoSold", "YrSold")

# Check VIF
model_continuous <- lm(SalePrice ~ ., data = houses_train[, c(continuous_vars, "SalePrice")])
vif(model_continuous)>10
# No problem of multicollinearity, because there are no values above 10.

summary(model_continuous)

# Function to determine significance values (stars)
get_significance_stars <- function(p_value) {
  if (p_value < 0.001) {
    return("***")
  } else if (p_value < 0.01) {
    return("**")
  } else if (p_value < 0.05) {
    return("*")
  } else {
    return("")
  }
}

# Initialize a data frame to store results
results <- data.frame(Variable = character(), Beta = numeric(), P_Value = numeric(), Significance = character(), stringsAsFactors = FALSE)

# Loop over each continuous variable to fit linear regression models
for (var in continuous_vars) {
  # Create the formula for the linear regression model
  formula <- as.formula(paste("SalePrice ~", var))
  
  # Fit the linear regression model
  model <- lm(formula, data = houses_train)
  
  # Extract the summary of the model
  summary_model <- summary(model)
  
  # Extract the beta coefficient and p-value for the variable
  beta <- summary_model$coefficients[2, "Estimate"]
  p_value <- summary_model$coefficients[2, "Pr(>|t|)"]
  
  # Determine significance stars
  significance <- get_significance_stars(p_value)
  
  # Append the results to the data frame
  results <- rbind(results, data.frame(Variable = var, Beta = beta, P_Value = p_value, Significance = significance, stringsAsFactors = FALSE))
}

# Display the results
print(results)

# Remove not important variables
houses_train <- houses_train[, !(names(houses_train) %in% c("BsmtFinSF2", "LowQualFinSF", "BsmtHalfBath", "X3SsnPorch", "MiscVal", "MoSold", "YrSold"))]
head(houses_train)

# Continuous variables important
continuous_vars_reduced <- c("LotArea", "YearBuilt", "YearRemodAdd", "MasVnrArea", "BsmtFinSF1",
                     "BsmtUnfSF", "X1stFlrSF", "X2ndFlrSF", "BsmtFullBath",
                     "FullBath", "HalfBath", "BedroomAbvGr", "KitchenAbvGr", "TotRmsAbvGrd", "Fireplaces",
                     "GarageCars", "GarageArea", "WoodDeckSF", "OpenPorchSF", "EnclosedPorch",
                     "ScreenPorch", "PoolArea")

par(mfrow = c(2, 2))
plot(SalePrice~LotArea,houses_train,
     main = "Sale Price vs. LotArea",  
     pch = 16,                            
     col = "#43CD80")
# beta: 0.0002100613
plot(SalePrice~YearBuilt,houses_train,
     main = "Sale Price vs. YearBuilt",  
     pch = 16,                            
     col = "#FF8C69")
# beta: 0.1374680132
plot(SalePrice~YearRemodAdd,houses_train,
     main = "Sale Price vs. YearRemodAdd",  
     pch = 16,                            
     col = "#AB82FF")
# beta: 0.1949024539
plot(SalePrice~MasVnrArea,houses_train,
     main = "Sale Price vs. MasVnrArea",  
     pch = 16,                            
     col = "#97FFFF")
# beta: 0.0209104053
plot(SalePrice~BsmtFinSF1,houses_train,
     main = "Sale Price vs. BsmtFinSF1",  
     pch = 16,                            
     col = "#43CD80")
# beta: 0.0066880730
plot(SalePrice~BsmtUnfSF,houses_train,
     main = "Sale Price vs. BsmtUnfSF",  
     pch = 16,                            
     col = "#FF8C69")
# beta: 0.0038688031
plot(SalePrice~X1stFlrSF,houses_train,
     main = "Sale Price vs. X1stFlrSF",  
     pch = 16,                            
     col = "#AB82FF")
# beta: 0.0125038078
plot(SalePrice~X2ndFlrSF,houses_train,
     main = "Sale Price vs. X2ndFlrSF",  
     pch = 16,                            
     col = "#97FFFF")
# beta: 0.0058666299
plot(SalePrice~BsmtFullBath,houses_train,
     main = "Sale Price vs. BsmtFullBath",  
     pch = 16,                            
     col = "#43CD80")
# beta: 3.4392568654
plot(SalePrice~FullBath,houses_train,
     main = "Sale Price vs. FullBath",  
     pch = 16,                            
     col = "#FF8C69")
# beta: 8.1100531122
plot(SalePrice~HalfBath,houses_train,
     main = "Sale Price vs. HalfBath",  
     pch = 16,                            
     col = "#AB82FF")
# beta: 4.4555070824
plot(SalePrice~BedroomAbvGr,houses_train,
     main = "Sale Price vs. BedroomAbvGr",  
     pch = 16,                            
     col = "#97FFFF")
# beta: 1.6732224459
plot(SalePrice~KitchenAbvGr,houses_train,
     main = "Sale Price vs. KitchenAbvGr",  
     pch = 16,                            
     col = "#43CD80")
# beta: -4.9647746298
plot(SalePrice~TotRmsAbvGrd,houses_train,
     main = "Sale Price vs. TotRmsAbvGrd",  
     pch = 16,                            
     col = "#FF8C69")
# beta: 2.6153305169
plot(SalePrice~Fireplaces,houses_train,
     main = "Sale Price vs. Fireplaces",  
     pch = 16,                            
     col = "#AB82FF")
# beta: 5.7806577626
plot(SalePrice~GarageCars,houses_train,
     main = "Sale Price vs. GarageCars",  
     pch = 16,                            
     col = "#97FFFF")
# beta: 6.7768074249
plot(SalePrice~GarageArea,houses_train,
     main = "Sale Price vs. GarageArea",  
     pch = 16,                            
     col = "#43CD80")
# beta:0.0230518245
plot(SalePrice~WoodDeckSF,houses_train,
     main = "Sale Price vs. WoodDeckSF",  
     pch = 16,                            
     col = "#FF8C69")
# beta: 0.0205277439
plot(SalePrice~OpenPorchSF,houses_train,
     main = "Sale Price vs. OpenPorchSF",  
     pch = 16,                            
     col = "#AB82FF")
# beta: 0.0373680517
plot(SalePrice~EnclosedPorch,houses_train,
     main = "Sale Price vs. EnclosedPorch",  
     pch = 16,                            
     col = "#97FFFF")
# beta: -0.0167202768
plot(SalePrice~ScreenPorch,houses_train,
     main = "Sale Price vs. ScreenPorch",  
     pch = 16,                            
     col = "#43CD80")
# beta: 0.0160300598
plot(SalePrice~PoolArea,houses_train,
     main = "Sale Price vs. PoolArea",  
     pch = 16,                            
     col = "#FF8C69")
# beta: 0.0183222823

# 2.4.
# Categorical variables
categorical_vars <- c("MSSubClass", "MSZoning", "Street", "LotShape", "LandContour", "Utilities", "LotConfig",
                      "LandSlope", "Neighborhood", "Condition1", "Condition2", "BldgType", "HouseStyle",
                      "OverallQual", "OverallCond", "RoofStyle", "RoofMatl", "Exterior1st", "Exterior2nd",
                      "MasVnrType", "ExterQual", "ExterCond", "Foundation", "Heating", "HeatingQC", "CentralAir",
                      "Electrical", "KitchenQual", "Functional", "PavedDrive", "SaleType", "SaleCondition")

# Convert to factors
houses_train[categorical_vars] <- lapply(houses_train[categorical_vars], as.factor)

# Function to calculate Cramér's V
library(vcd)
calculate_cramers_v <- function(var1, var2) {
  tbl <- table(var1, var2)
  chi2 <- chisq.test(tbl, correct = FALSE)$statistic
  n <- sum(tbl)
  min_dim <- min(nrow(tbl) - 1, ncol(tbl) - 1)
  cramers_v <- sqrt(chi2 / (n * min_dim))
  return(cramers_v)
}

# Initialize an empty matrix to store Cramér's V values
n <- length(categorical_vars)
cramers_v_matrix <- matrix(NA, n, n)
rownames(cramers_v_matrix) <- categorical_vars
colnames(cramers_v_matrix) <- categorical_vars

# Loop through each pair of categorical variables
for (i in 1:(n-1)) {
  for (j in (i+1):n) {
    var1 <- categorical_vars[i]
    var2 <- categorical_vars[j]
    cramers_v <- calculate_cramers_v(houses_train[[var1]], houses_train[[var2]])
    cramers_v_matrix[i, j] <- cramers_v
    cramers_v_matrix[j, i] <- cramers_v
    
    # Print variable names and Cramér's V if the value is above 0.3
    if (cramers_v > 0.3) {
      cat("High correlation between", var1, "and", var2, ": Cramér's V =", cramers_v, "\n")
    }
  }
}
# Print the correlation matrix
print(cramers_v_matrix)

# Loop of the ANOVA for each categorical variable
# Initialize a data frame to store results
anova_results <- data.frame(Variable = character(), F_Statistic = numeric(), P_Value = numeric(), stringsAsFactors = FALSE)

# Loop over each categorical variable to fit ANOVA models
for (var in categorical_vars) {
  # Create the formula for the ANOVA model
  formula <- as.formula(paste("SalePrice ~", var))
  
  # Fit the ANOVA model
  model <- aov(formula, data = houses_train)
  
  # Extract the summary of the model
  summary_model <- summary(model)
  
  # Extract the F-statistic and p-value for the variable
  f_statistic <- summary_model[[1]][["F value"]][1]
  p_value <- summary_model[[1]][["Pr(>F)"]][1]
  
  # Determine significance stars
  significance <- get_significance_stars(p_value)
  
  # Append the results to the data frame
  anova_results <- rbind(anova_results, data.frame(Variable = var, F_Statistic = f_statistic, P_Value = p_value, Significance = significance, stringsAsFactors = FALSE))
}

# Display the results
print(anova_results)

# Remove not important variables
houses_train <- houses_train[, !(names(houses_train) %in% c("Street", "Utilities", "LandSlope"))]
head(houses_train)

# Categorical variables important
categorical_vars_reduced <- c("MSSubClass", "MSZoning", "LotShape", "LandContour", "LotConfig",
                              "Neighborhood", "Condition1", "Condition2", "BldgType", "HouseStyle",
                              "OverallQual", "OverallCond", "RoofStyle", "RoofMatl", "Exterior1st", "Exterior2nd",
                              "MasVnrType", "ExterQual", "ExterCond", "Foundation", "Heating", "HeatingQC", "CentralAir",
                              "Electrical", "KitchenQual", "Functional", "PavedDrive", "SaleType", "SaleCondition")
# Convert to factors
houses_train[categorical_vars_reduced] <- lapply(houses_train[categorical_vars_reduced], as.factor)

# 2.5.
model_full <- lm(SalePrice ~ ., data = houses_train_full_model)
summary(model_full)
reduced_model <- lm(SalePrice ~ MSSubClass + MSZoning + LotShape + LandContour + LotConfig + Neighborhood + Condition1 +
                      Condition2 + BldgType + HouseStyle + OverallQual + OverallCond + RoofStyle + RoofMatl + Exterior1st +
                      Exterior2nd + MasVnrType + ExterQual + ExterCond + Foundation + Heating + HeatingQC + CentralAir + Electrical +
                      KitchenQual + Functional + PavedDrive + SaleType + SaleCondition + LotArea + YearBuilt + YearRemodAdd + MasVnrArea + BsmtFinSF1 +
                      BsmtUnfSF + X1stFlrSF + X2ndFlrSF + BsmtFullBath + FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr + TotRmsAbvGrd + Fireplaces +
                      GarageCars + GarageArea + WoodDeckSF + OpenPorchSF + EnclosedPorch + ScreenPorch + PoolArea, data = houses_train)
summary(reduced_model)

# H0: beta_MSSubClass = beta_MSZoning = beta_LotShape = ... = 0
# vs
# H1: beta_MSSubClass != beta_MSZoning != beta_LotShape != ... != 0
anova(reduced_model, model_full, test = "F")
summary(reduced_model)$r.squared
# 93% of target variable (price) is explained by the reduced_model.

# 2.6.
library(leaps)
library(MASS)
null_model<-lm(SalePrice~1,houses_train)
m_sback<-stepAIC(reduced_model,scope=list(lower=null_model,upper=reduced_model),direction = "backward",trace=T)
summary(m_sback)
m_sforward<-stepAIC(null_model,scope=list(lower=null_model,upper=reduced_model),direction = "forward",trace=T)
summary(m_sforward)
# Stepwise model selection
m_sboth <- stepAIC(reduced_model, direction = "both")
summary(m_sboth)
# Choose the one with the lowest AIC.
AIC(m_sback)
AIC(m_sforward)
AIC(m_sboth)
# Since backward and both have the lowest AIC, we choose the both.
AIC(reduced_model, m_sboth)
# The both had a lower AIC compared to the reduced model, so we choose the both as the best model.

# 2.7.
# Residual diagnostics
par(mfrow = c(2, 2))
plot(m_sboth)

# 2.8.
# Load the test data
houses_test <- read.csv('test.csv')
houses_test <- subset(houses_test, select = c("LotArea", "YearBuilt", "YearRemodAdd", "MasVnrArea", "BsmtFinSF1", 
                                              "BsmtUnfSF", "X1stFlrSF", "X2ndFlrSF", "BsmtFullBath", "FullBath", "HalfBath", "BedroomAbvGr", 
                                              "KitchenAbvGr", "TotRmsAbvGrd", "Fireplaces", "GarageCars", "GarageArea", 
                                              "WoodDeckSF", "OpenPorchSF", "EnclosedPorch", "ScreenPorch", 
                                              "PoolArea", "MSSubClass", "MSZoning", "LotShape", "LandContour", "LotConfig", 
                                              "Neighborhood", "Condition1", "Condition2", "BldgType", "HouseStyle", 
                                              "OverallQual", "OverallCond", "RoofStyle", "RoofMatl", "Exterior1st", 
                                              "Exterior2nd", "MasVnrType", "ExterQual", "ExterCond", "Foundation", 
                                              "Heating", "HeatingQC", "CentralAir", "Electrical", "KitchenQual", 
                                              "Functional", "PavedDrive", "SaleType", "SaleCondition"))

head(houses_test)
sample_submission <- read.csv('sample_submission.csv')
sample_submission$SalePrice <- sample_submission$SalePrice / 10000
missing_count <- sum(is.na(houses_test))
missing_count
# Rows with at least a missing value
missing_rows <- !complete.cases(houses_test)

# Print or subset your dataset based on missing rows
options(max.print = 99999)
print(houses_test[missing_rows, ])
# Removing the rows with at least one missing value
houses_test<-houses_test[complete.cases(houses_test), ]
# Checking if it worked
missing_count <- sum(is.na(houses_test))
missing_count
# Clean sample_submission
# Create a vector of row numbers to remove
rows_to_remove <- c(96, 232, 247, 423, 456, 486, 533, 545, 582, 
                    661, 692, 729, 757, 791, 852, 866, 881, 890, 
                    909, 1014, 1030, 1117, 1133, 1151, 1198, 1227, 
                    1403, 1445)

# Remove the specified rows from our dataset
sample_submission <- sample_submission[-rows_to_remove, ]

categorical_vars_reduced <- c("MSSubClass", "MSZoning", "LotShape", "LandContour", "LotConfig",
                              "Neighborhood", "Condition1", "Condition2", "BldgType", "HouseStyle",
                              "OverallQual", "OverallCond", "RoofStyle", "RoofMatl", "Exterior1st", "Exterior2nd",
                              "MasVnrType", "ExterQual", "ExterCond", "Foundation", "Heating", "HeatingQC", "CentralAir",
                              "Electrical", "KitchenQual", "Functional", "PavedDrive", "SaleType", "SaleCondition")

# Convert to factors
houses_test[categorical_vars_reduced] <- lapply(houses_test[categorical_vars_reduced], as.factor)
ncol(houses_test)
ncol(houses_train)
# Predict using the stepwise model
predicted_SalePrice <- predict(m_sboth, newdata = houses_test)
predicted_SalePrice

# Add the predicted SalePrice as a new column in houses_test
houses_test$Predicted_SalePrice <- predicted_SalePrice
houses_test$Predicted_SalePrice
# R-squared on test data
SSR <- sum((houses_test$Predicted_SalePrice - mean(sample_submission$SalePrice))^2)
SSE <- sum((houses_test$Predicted_SalePrice - sample_submission$SalePrice)^2)
# SST <- sum((sample_submission$SalePrice - mean(sample_submission$SalePrice))^2)
SST <- SSR + SSE
R2 <- SSR/SST
R2

# 2.9.
# Prediction intervals
pred_interval <- predict(m_sboth, houses_test, interval = "prediction")
pred_interval

# Initialize a counter for the number of values within the predicted interval
count_within_interval <- 0

# Iterate through each value in sample_submission$SalePrice
for (i in 1:length(sample_submission$SalePrice)) {
  # Print the current index, corresponding SalePrice value, and interval values
  cat("Index:", i, ", SalePrice:", sample_submission$SalePrice[i], ", Lower Bound:", pred_interval[i, "lwr"], ", Upper Bound:", pred_interval[i, "upr"], "\n")
  
  # Check if the value is within the corresponding prediction interval
  if (sample_submission$SalePrice[i] >= pred_interval[i, "lwr"] && 
      sample_submission$SalePrice[i] <= pred_interval[i, "upr"]) {
    # Increment the counter if the value is within the interval
    count_within_interval <- count_within_interval + 1
    cat("Incremented\n")
  }
}

# Print the final count
cat("Count within interval:", count_within_interval, "\n")

# Calculate the ratio of values within the predicted interval to the total number of predicted values
ratio_within_interval <- count_within_interval / nrow(pred_interval)

# Display the ratio
print(ratio_within_interval*100)

# 2.10.
# Mean absolute error
mean_abs_error <- mean(abs(houses_test$Predicted_SalePrice - sample_submission$SalePrice))
mean_abs_error
