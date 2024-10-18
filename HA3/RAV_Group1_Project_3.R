# 3.1.
# Load necessary libraries
library(readxl)

# Load the data
heart_failure <- read_excel("Problem 3 - heart_failure_clinical_records_dataset.xlsx", col_names=TRUE)

# View the original column names
print(colnames(heart_failure))

# Change the column names
colnames(heart_failure) <- c("Age", "Anaemia", "Creatinine_Phosphokinase", "Diabetes", 
                             "Ejection_Fraction", "High_Blood_Pressure", "Platelets", 
                             "Serum_Creatinine", "Serum_Sodium", "Sex", "Smoking", 
                             "Time", "Death_Event")

# View the new column names
print(colnames(heart_failure))

head(heart_failure)


# Checking for missing values
missing_count <- sum(is.na(heart_failure))
missing_count

# Defining the categorical variables as factors
binary_variables <- c("Anaemia", "High_Blood_Pressure", "Diabetes", "Sex", "Smoking")
heart_failure[binary_variables] <- lapply(heart_failure[binary_variables], as.factor)

# Defining the continuous variables in the dataset
continuous_vars <- c("Age", "Creatinine_Phosphokinase", "Ejection_Fraction", "Platelets", "Serum_Creatinine", "Serum_Sodium", "Time")

# Function to determine significance (stars)
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
results <- data.frame(Variable = character(), Beta = numeric(), P_Value = numeric(), Significance = character(), R_squared = numeric(), stringsAsFactors = FALSE)

# Loop over each continuous variable to fit linear regression models
for (var in continuous_vars) {
  # Create the formula for the linear regression model
  formula <- as.formula(paste("Death_Event ~", var))
  
  # Fit the linear regression model
  model <- lm(formula, data = heart_failure)
  
  # Extract the summary of the model
  summary_model <- summary(model)
  
  # Extract the beta coefficient and p-value for the variable
  beta <- summary_model$coefficients[2, "Estimate"]
  p_value <- summary_model$coefficients[2, "Pr(>|t|)"]
  
  # R squared value for each model
  r_squared<-summary_model$r.squared
  
  # Determine significance stars
  significance <- get_significance_stars(p_value)
  
  # Append the results to the data frame
  results <- rbind(results, data.frame(Variable = var, Beta = beta, P_Value = p_value, Significance = significance, R_squared = r_squared, stringsAsFactors = FALSE))
}

# Display the results
print(results)
# The variable with significant p-values are Age, Ejection_Fraction, Serum_Creatinine, Serum_Sodium and Time.
# Since "Time" has the highest R_squared value, it is the best characteristic that describes the target variable "Death_Event".

# 3.2.
# Boxplot of Time
library(ggplot2)
ggplot(heart_failure, aes(x = Time, y = as.factor(Death_Event), fill = as.factor(Death_Event))) +
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "#B0E2FF", "1" = "#0000CD")) +  labs(x = "Follow-up Period", y = "Death", fill = "Death Event")+
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white"),
    legend.background = element_rect(fill = "white")
  )

# 3.3.
# To access the values in the Time variable
summary(heart_failure$Time)

# To see the percentage of values in each quartile
with(heart_failure, quantile(Time, probs = seq(0, 1, 0.2)))

# Spliting the time variable with the obtained values
breaks_time <- c(4, 60, 95, 147, 210, 285) # Cuts or breaks the variable Time

heart_failure$time_category <- with(heart_failure, cut(Time, breaks = breaks_time, include.lowest = T)) # Break the Time by categories between the values from breaks_time
summary(heart_failure$time_category)          

# Mean of Death_Event in each time interval bin
values_proportion <- with(heart_failure, tapply(Death_Event, time_category, mean)) # Proportion in each class
values_categories <- levels(heart_failure$time_category)

values <- data.frame(values_categories, values_proportion)
values$values_categories_ordered <- factor(values$values_categories, levels = values$values_categories)
ggplot(values, aes(x = values_categories_ordered, y = values_proportion)) +
  geom_point(size = 3, color = "#0000CD") +
  labs(x = "Time Intervals", y = "Mean of Death_Event") +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white"),
    legend.background = element_rect(fill = "white")
  )
# The point from the first time bin is the highest since more people die at the beginning of the follow-up days.
# However, as the days go by we see a decrease in the proportion of people dying. Thus, implying that people tend to live more as time goes by.

# 3.4.
# Automatically
logistic_function <- Death_Event ~ Time
logistic_model <- glm(logistic_function, binomial, heart_failure)
# A interceção representa o log das probabilidades quando todas as variáveis independentes são iguais a zero.
# Relativamente aos coeficientes, os parâmetros estimados do modelo de regressão logística mostram como as variáveis independentes e dependentes se relacionam entre si.
p_logistic2 <- fitted(logistic_model)
summary(logistic_model)

# Plot the Logistic Funtion
# Fit logistic regression model
logistic_model <- glm(Death_Event ~ Time, family = binomial, data = heart_failure)

# Get coefficients
beta_0 <- coef(logistic_model)[1]
beta_1 <- coef(logistic_model)[2]

# Define logistic function
logistic_function <- function(x) {
  p <- exp(beta_0 + beta_1 * x) / (1 + exp(beta_0 + beta_1 * x))
  return(p)
}

# Create sequence of x values for plotting
x_values <- seq(min(heart_failure$Time), max(heart_failure$Time), length.out = 100)

# Calculate corresponding y values using logistic function
y_values <- logistic_function(x_values)

# Plot logistic function
plot(x_values, y_values, type = "l", 
     xlab = "Time", ylab = "Probability of Death", 
     main = "Logistic Function for Death Probability vs. Time")

# 3.5.
# 3.5.1.
null_model <- glm(Death_Event ~ 1, binomial, heart_failure)
anova(null_model, logistic_model, test = "Chisq")
# H0: the null model is similar to the logistic model
# vs
# H1: the logistic model is better than the null model
# We reject H0, so the logistic model is better than the null model.

# 3.5.2.
library(ROCR)
prediction_model <- prediction(fitted(logistic_model, type = "response"), heart_failure$Death_Event)
# ROC (Receiver Operating Characteristic) Curve (x = TPR vs y = FPR) --> Tentar que esteja longe da linha reta (random classifier)
# Performance - FPR vs TPR - ROC Curve
performance_roc <- performance(prediction_model, x.measure = "fpr", measure = "tpr")
plot(performance_roc)
# From 0.6, we have a good discrimination and from 0.8, we have an excellent discrimination.

# AUC (Area Under the Curve)
performance_auc <- performance(prediction_model, "auc")
round(100*performance_auc@y.values[[1]])
# Com %
paste(round(100*performance_auc@y.values[[1]]),"%")

# Accuracy
performance_accuracy <- performance(prediction_model, "acc")
plot(performance_accuracy)

# 3.5.3.
# Define the costs
cost_fp <- 1000  # Cost of a False Positive
cost_fn <- 1500  # Cost of a False Negative

# Get the prediction probabilities from the logistic model
predicted_probs <- predict(logistic_model, type = "response")

# Create a sequence of threshold values
thresholds <- seq(0, 1, by = 0.01)

# Initialize a vector to store the total costs for each threshold
total_costs <- numeric(length(thresholds))

# Calculate the total cost for each threshold
for (i in seq_along(thresholds)) {
  threshold <- thresholds[i]
  predictions <- ifelse(predicted_probs > threshold, 1, 0)
  
  # Calculate false positives and false negatives
  fp <- sum(predictions == 1 & heart_failure$Death_Event == 0)
  fn <- sum(predictions == 0 & heart_failure$Death_Event == 1)
  
  # Calculate the total cost
  total_costs[i] <- fp * cost_fp + fn * cost_fn
}

# Find the threshold with the minimum cost
optimal_threshold_index <- which.min(total_costs)
optimal_threshold <- thresholds[optimal_threshold_index]
optimal_cost <- total_costs[optimal_threshold_index]

# Print the optimal threshold and the corresponding cost
cat("Optimal Threshold:", optimal_threshold, "\n")
cat("Minimum Cost:", optimal_cost, "\n")

# Plot the total cost against the thresholds
plot(thresholds, total_costs, type = "l", xlab = "Threshold", ylab = "Total Cost", main = "Total Cost vs. Threshold")
abline(v = optimal_threshold, col = "red", lty = 2)


