
# Import packages 
if (!require(haven)) install.packages("haven")
if (!require(dplyr)) install.packages("dplyr")
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(labelled)) install.packages("labelled")
if (!require(tidyr)) install.packages("tidyr")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(skimr)) install.packages("skimr")
if (!require(knitr)) install.packages("knitr")
if (!require(stargazer)) install.packages("stargazer")
if (!require(stargazer)) install.packages("KableExtra")
if (!require(car)) install.packages("car")
library(kableExtra)
library(haven)
library(tidyverse)
library(labelled)
library(tidyr)
library(ggplot2)
library(dplyr)
library(skimr)
library(knitr)
library(stargazer)
library(car)

# Load data 
root_dir =  "/Users/rfernex/Documents/Education/SciencesPo/Courses"
Sub_dir = "/S2/Econometrics/TD/Psets/Pset1/Data/ee2002ext.dta"
fullpath = paste0(root_dir,Sub_dir)
employment_data = read_dta(fullpath)

skim(employment_data)

# Question 2 & 3

employment_data <- employment_data %>% na.omit() %>%
  select(salfr, ddipl1, s, agd) %>%
  mutate(log_salfr = log(salfr),
         agd_sqd = agd^2) %>%
  filter(log_salfr > quantile(log_salfr, probs = 0.005), 
         log_salfr < quantile(log_salfr, probs = 0.995), 
         ddipl1!= 7)

# Question 4 

# turns the education and sex variable into categorical variables
employment_data$ddipl1 <- as.factor(employment_data$ddipl1)
employment_data$s <- as.factor(employment_data$s)

# runs model
model <- lm(log_salfr ~ s + agd + agd_sqd + ddipl1, data = employment_data)

stargazer(model, type = "text", title = "Regression Results", 
          dep.var.labels = "Log(salfr)", 
          omit.stat = c("f", "ser"))

# Question 5 

#All independent variables are significant at the 1% significance level. 
#We notice that age squared is negatively related to log wages when age is 
#positively related. 
#Since we include a squared term we are in fact modeling a quadratic relationship
#and this indicates that log wage increase age but at a decreasing rate over time
#and that it eventually reaches a maximum before declining. 
#Indeed the derivative of Log(wages) as a function of Age is negative (since \beta_2 is negative)
#This makes sense since, past a certain age salaries can't be expected to increase further. 

# Question 6 
SSR <- sum(residuals(model)^2)
N <- length(employment_data$log_salfr)
K <- length(coef(model))

cat("Sum of squared residuals : ", sprintf("%.2f", SSR))
cat("Number of independent variables (+intercept) : ", K)
cat("Number of observations : ", N)

# Question 7 

## Method 1 
restricted_model <- lm(log_salfr ~ agd + agd_sqd, data = employment_data)
anova(restricted_model, model)

# After computing the F test statistic and the corresponding p-value, we observe
# the latter is low enough to reject the null hypothesis. This supports the 
# fact that the level of education is indeed signficantly related to log wages. 

# Question 8 

# Run the test 

## Method 1
linearHypothesis(model, "ddipl12 = ddipl13")

## Method 2
beta <- coef(model)
vcov_mat <- vcov(model)

# Compute the difference and its standard error
estimate <- beta["ddipl12"] - beta["ddipl13"]

# Standard error of the difference:
SE <- sqrt(vcov_mat["ddipl12", "ddipl12"] + vcov_mat["ddipl13", "ddipl13"] -
                  2 * vcov_mat["ddipl12", "ddipl13"])

# Compute the t-statistic
t_stat <- estimate / SE

# Degrees of freedom: often taken as residual degrees of freedom from the model
df <- df.residual(model)

# Calculate the two-sided p-value
p_value <- 2 * pt(-abs(t_stat), df)

# Print the results
cat("Estimate of the parameter :", estimate, "\n")
cat("Standard Error:", SE, "\n")
cat("t-statistic:", t_stat, "\n")
cat("p-value:", p_value, "\n")

# 


# Based on the P-value, we find that the null hypothesis can be rejected at the 
# 5% significance level. This implies that having only the BEPC or having the CAP, BEP ..
# does not have the same impact on the dependent variable. This makes sense as we would expect a higher level of education to have a more significant impact on the independent variable

# Question 9 

# Create constrained version where levels 2 and 3 are combined
employment_data_reduced <- employment_data %>%
  mutate(
    # Combine Education levels 2 and 3
    ddipl1_reduced = case_when(
      ddipl1 %in% c(2, 3) ~ "2_3",
      ddipl1 %in% c(4, 5, 6) ~ as.character(ddipl1),
      TRUE ~ "1"  # Reference level
    ),
    ddipl1_reduced = factor(ddipl1_reduced),
    agd_sqd = agd^2
  )

# Run the model with the reduced education variable
model2 <- lm(log_salfr~ s + agd + agd_sqd + ddipl1_reduced, 
                      data = employment_data_reduced)

stargazer(model2, type = "text", title = "Regression Results", 
          dep.var.labels = "Log(salfr)", 
          omit.stat = c("f", "ser"))

# Apply the transformation: exp(coef) - 1
original_coef <- coef(model2)
transformed_coef <- exp(original_coef) - 1

coef_matrix <- rbind(Original = original_coef, `Exp(Coef)-1` = transformed_coef)
coef_matrix_rounded <- round(coef_matrix, 4)

# Print a header and the formatted table using knitr::kable
cat("\n\nCoefficients Comparison:\n\n")
kable(coef_matrix_rounded, caption = "Comparison of Original vs. Transformed Coefficients")

# We notice grouping both categories under category 2 does not change the impact of category 2 which remains significant at the 1% significance level

# Question 10 
model2_restricted <- lm(log_salfr ~ s + agd + agd_sqd, data = employment_data)

stargazer(model2_restricted, type = "text", title = "Regression Results", 
          dep.var.labels = "Log(salfr)", 
          omit.stat = c("f", "ser"))
# We observe that all independent variables are significiant at the 1% significance leveL. 

# Question 11
SSR2 <- sum(residuals(model2_restricted)^2)
cat("Sum of squared residuals : ", sprintf("%.2f", SSR2))

# Question 12 
test_result <- anova(model2_restricted, model2)

# method 1 : retrieves the F-statistic from the ANOVA table
F_statistic <- test_result$F[2]

# method 2 : manually computes the F-statistic 
F_statistic_manual <- ((SSR2-SSR)/4)/(SSR/(N-5))

cat("F-statistic for the education test (manual) :", sprintf("%.2f", F_statistic))
cat("F-statistic for the education test (manual) :", sprintf("%.2f", F_statistic_manual))

# Question 13 
df1 <- test_result$Df[2]
df2 <- length(employment_data$log_salfr)
critical_value <- qf(0.95, df1, df2)
cat("95th percentile of the Fisher Distribution :", sprintf("%.2f", critical_value))

# The critical value is vastly under the value of the Fisher statistic we computed earlier, thus we can safely reject the null hypothesis.
# This is in accordance with what we found earlier and seems to indicate that adding education greatly improves the model. 

# Question 14

## a)

model_interact <- lm(log_salfr ~ agd + agd_sqd + ddipl1 + s + ddipl1:s, data = employment_data)

# computes the estimate for \hat{T}
beta <- coef(model_interact)
estimate <- beta["ddipl15:s2"] + beta["ddipl15"] - beta["ddipl13"]
cat("we get the following estimate for the parameter : ", estimate, "\n")

## b) 

# Get the variance-covariance matrix 
vcov_matrix <- vcov(model_interact)
var_cov_subset <- vcov_matrix[c("ddipl15", "ddipl15:s2", "ddipl13"),
                              c("ddipl15", "ddipl15:s2", "ddipl13")]

# Define the gradient vector
gradient <- c(1, 1, -1)

# Computes Var(\hat{})
var_delta <- t(gradient) %*% var_cov_subset %*% gradient

# Compute the standard error
se_delta <- sqrt(var_delta)
cat("We find the following standard error :", sprintf("%.2f", se_delta),"\n")

## c)

# Computes the test statistic
test_statistic <- estimate/se_delta

# Computes the p-value for the test 
df_resid <- df.residual(model_interact)
p_value <- 2 * (1 - pt(abs(test_statistic), df = df_resid))

# Compute the two-sided p-value
cat("test statistic : ", test_statistic, "\n")
cat("p-value:", sprintf("%.2f", p_value), "\n")
