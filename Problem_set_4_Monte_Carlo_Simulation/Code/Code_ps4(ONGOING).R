
if (!require(haven)) install.packages("haven")
if (!require(dplyr)) install.packages("dplyr")
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(labelled)) install.packages("labelled")
if (!require(tidyr)) install.packages("tidyr")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(skimr)) install.packages("skimr")
if (!require(knitr)) install.packages("knitr")
if (!require(stargazer)) install.packages("stargazer")
if (!require(KableExtra)) install.packages("KableExtra")
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

# Question 1 : Generate dataset and plot
set.seed(123)
x1  <- runif(1000, min = 0, max = 1)
u <- rchisq(1000, df = 1) - 1
data <- data.frame(x1 = x1, u = u)

hist(data$x1, main = 'Histogram of x1 \
 Uniform(0,1)', xlab = 'x1', col = 'skyblue', border = 'black')

hist(data$u, main = 'Histogram of u \
 (u+1 ~ Chi-square(1))', xlab = 'u', col = 'salmon', border = 'black')

# Question 2 : 

## a)
chi_mean_std <- function(N) {
  u <- rchisq(N, df = 1) - 1
  mean_u <- mean(u)
  sd_u <- sd(u)
  return(c(mean_u, sd_u))
}

result <- chi_mean_std(5)

## b)
results_df <- data.frame(sample_mean=numeric(),sample_sd=numeric())  |>  setNames(c("sample_mean", "sample_sd"))
for (i in 1:10000) {
  res <- chi_mean_std(5)
  results_df <- rbind(results_df, data.frame(sample_mean = res[1], sample_sd = res[2]) )
}

## c)
hist_simulation <- results_df %>% 
  pull(sample_mean) %>% 
  hist(main = 'Histogram of means of u', xlab = 'u', col = 'red', border = 'black')

# Question 3
sample_mean_stats <- data.frame(sample_size = integer(),   
                                mean_of_means = numeric(),   
                                sd_of_means = numeric())  

num_iterations <- 10000  # fixed number of simulation replications for convergence  

for (N in c(10, 100, 1000)) {  
  results_df <- data.frame(sample_mean = numeric(), sample_sd = numeric())  
  for (j in 1:num_iterations) {  
    res <- chi_mean_std(N)  
    results_df <- rbind(results_df,   
                        data.frame(sample_mean = res[1], sample_sd = res[2]))  
  }  
  sample_mean_stats <- rbind(sample_mean_stats,   
                             data.frame(sample_size = N,   
                                        mean_of_means = mean(results_df$sample_mean),   
                                        sd_of_means = sd(results_df$sample_mean)))  
}  

# Question 4
modified_sd <- sample_mean_stats %>%  
  pull(sd_of_means) %>% 
  first() %>%
  {list(  
      m100 = . * (sqrt(10) / sqrt(100)),  
      m1000 = . * (sqrt(10) / sqrt(1000))  
    )  
  }  
print(paste("m100:", modified_sd$m100))  
print(paste("m1000:", modified_sd$m1000))  

# we clearly see that the standard deviation goes closer to 0 as we increase the denominator (which makes sense).
# By applying these transformations we get the predicted standard error of the mean of u for sample sizes 100 or 1000 respectively
# We notice that these predicted values are very close from those we computed in 3 for samples of size 100 and 10 000 respectively
# We solve the following equation to find the right N : sd_n = sd_10 * \sqrt{\frac{10}{N}} with sd_10 = 0.44.. and sd_n = 0.001

# Question 5

num_iterations <- 10000  # fixed number of simulation replications for convergence      

par(mfrow = c(1, 3))     

for (N in c(10, 100, 1000)) {      
  results_df <- data.frame(sample_mean = numeric(), sample_sd = numeric())      
  for (j in 1:num_iterations) {      
    res <- chi_mean_std(N)      
    results_df <- rbind(results_df,       
                        data.frame(sample_mean = res[1], sample_sd = res[2]))      
  }    
  mean_of_means <- mean(results_df$sample_mean)      
  sd_of_means <- sd(results_df$sample_mean)      
  
  # Plot histogram with density scale (this is the key change)  
  hist(results_df$sample_mean,      
       main = paste("N =", N),      
       xlab = "Sample Mean",       
       col = "skyblue",       
       breaks = 30,  
       freq = FALSE)  # Use density scale instead of counts  
  
  # Get the x-axis limits from the current plot  
  x_range <- par("usr")[1:2]  
  
  # Overlay normal curve with the calculated mean and sd  
  curve(dnorm(x, mean = 0, sd = sd_of_means),     
        add = TRUE,     
        col = "red",     
        lwd = 2,  
        from = x_range[1],  
        to = x_range[2])  
}  

# Reset plotting layout  
par(mfrow = c(1, 1))  

# in accordance with the central limit theorem, we can see that the distribution of the sample mean converges to a standard normal distribution

# Question 6 

reg_function <- function(N) {  
  x1 <- runif(N, min = 0, max = 1)  
  x2 <- rbinom(N, size = 1, prob = 0.3)  
  u  <- rchisq(N, df = 1) - 1  
  y  <- 1 + 2*x1 + 10*x2 + u  
  data_df <- data.frame(x1 = x1, x2 = x2, y = y)    
  model <- lm(y ~ x1 + x2, data = data_df)  
  return(list(model=model, data_df=data_df))  
}  

simulation <- function(N){
  results_df <- as.data.frame(matrix(numeric(0), ncol = 6))  
  colnames(results_df) <- c("Intercept_coef", "Intercept_se", "x1_coef", "x1_se", "x2_coef", "x2_se")  
  for (i in 1:10000) {  
    outcome <- reg_function(N)  
    model <- outcome$model  
    data_df <- outcome$data_df  
    coefs <- summary(model)$coefficients  
    # Check if x2 has no variation in data  
    if(var(data_df$x2) == 0) {  
      # We assign x2_coef = 0 and x2_se = 0 because x2 is essentially constant  
      x2_coef_val <- 0  
      x2_se_val <- 0  
    } else {  
      x2_coef_val <- coefs["x2", "Estimate"]  
      x2_se_val <- coefs["x2", "Std. Error"]  
    }  
    results_df <- rbind(results_df, data.frame(   
      Intercept_coef = coefs["(Intercept)", "Estimate"],    
      Intercept_se   = coefs["(Intercept)", "Std. Error"],    
      x1_coef        = coefs["x1", "Estimate"],    
      x1_se          = coefs["x1", "Std. Error"],    
      x2_coef        = x2_coef_val,    
      x2_se          = x2_se_val  
    )) 
    } 
  return(results_df)
}

simulation_res10 <-simulation(10)
print(head(simulation_res10))
skim(simulation_results_10)

# question 7 : 

# case N = 10

par(mfrow = c(3, 1))  
cols <- c("Intercept_coef", "x1_coef", "x2_coef")  

for(cl in cols){  
  vec <- simulation_results_10[[cl]]  
  hist(vec,   
       main = paste("N =", 10),   
       xlab = cl,   
       col = "skyblue",   
       breaks = 30,   
       freq = FALSE)  
  mean_p <- mean(vec, na.rm = TRUE)  
  sd_p <- sd(vec, na.rm = TRUE)  
  # Use the range of the data with some padding  
  x_range <- range(vec, na.rm = TRUE) + c(-0.1, 0.1) * sd_p  
  curve(dnorm(x, mean = mean_p, sd = sd_p),   
        add = TRUE,   
        col = "red",   
        lwd = 2,  
        from = x_range[1],  
        to = x_range[2])  
}  



## case N = 1000

simulation_res1000 <-simulation(1000)
skim(simulation_results_10)

par(mrow = c(3, 1))  
cols <- c("Intercept_coef", "x1_coef", "x2_coef")  

for(col in cols){  
  vec <- simulation_results_1000[[col]]  
  hist(vec,   
       main = paste("N =", 1000),   
       xlab = col,   
       col = "skyblue",   
       breaks = 30,   
       freq = FALSE)  
  mean_p <- mean(vec)  
  sd_p   <- sd(vec)  
  
  # Define x-range based on data (with a little margin)  
  x_range <- range(vec, na.rm = TRUE) + c(-1, 1)*sd_p*0.1  
  curve(dnorm(x, mean = mean_p, sd = sd_p),   
        add = TRUE, col = "red", lwd = 2,   
        from = x_range[1], to = x_range[2])  
}  



# the more we increase N the more the distribution of the sample mean tends towards a normal distribution which is conform to the central limit theorem. Thus the coefficients converge to the population value. 
# the mean and stdev of the standard error of coefficients tends towards zero as sample size (N) increases, thus we can safely assert that this estimator is consistent (variance is asymptotically null)


# Question 8

simulation2 <- function(N){ 
  test_results <- data.frame(F_statistic = numeric(0))  
  for (i in 1:10000) {  
    result <- tryCatch({  
      outcome <- reg_function(N)  
      model <- outcome$model  
      data_df <- outcome$data_df  
      # Check for aliased coefficients  
      if(any(is.na(coef(model)))) {  
        next  
      }  
      joint_test <- linearHypothesis(model, c("(Intercept) = 1", "x1 = 2"))  
      f_stat <- joint_test$F[2]  
    }, error = function(e) {  
      NA  
    })  
    if(!is.na(result)) {  
      test_results <- rbind(test_results, data.frame(F_statistic = result))  
    }  
  }  
  return(test_results)  
} 

# Test : Sample size N = 10
F_stat_vec_10 <- simulation2(10)$F_statistic  

# Define an appropriate x-range  
sd_p_10 <- sd(F_stat_vec_10, na.rm = TRUE)  
x_range <- range(F_stat_vec_10, na.rm = TRUE)  
x_range[1] <- max(0, x_range[1] - 0.1 * sd_p_10)  
x_range[2] <- x_range[2] + 0.1 * sd_p_10  

# Plot histogram  
hist(F_stat_vec_10,   
     main = "F Statistic Distribution (N = 10)",   
     xlab = "F Statistic",   
     col = "skyblue",   
     breaks = 30,   
     freq = FALSE,   
     xlim = x_range)  

# Test : Sample size N = 1000
 F_stat_vec_1000 <- simulation2(1000)$F_statistic    
  
 # Define an appropriate x-range   
 sd_p_1000 <- sd(F_stat_vec_1000, na.rm = TRUE)  
 x_range <- range(F_stat_vec_1000, na.rm = TRUE)  
 x_range[1] <- max(0, x_range[1] - 0.1 * sd_p_1000)  
 x_range[2] <- x_range[2] + 0.1 * sd_p_1000  
   
 # Plot histogram for N = 1000 F statistics  
 hist(F_stat_vec_1000,     
      main = "F Statistic Distribution (N = 1000)",     
      xlab = "F Statistic",     
      col = "skyblue",     
      breaks = 30,     
      freq = FALSE,     
      xlim = x_range)  
 
 # Question 9 
 
 ## a)
 
critval_95_n10 <- qf(0.95, df1 = 2, df2 = 7)
critval_99_n10 <- qf(0.99, df1 = 2, df2 = 7)
critval_95_n1000 <- qf(0.95, df1 = 2, df2 = 997)
critval_99_n1000 <- qf(0.99, df1 = 2, df2 = 997)
 
## b) 

nb_reject_95_n10 <- sum(F_stat_vec_10 > critval_95_n10)
nb_reject_99_n10 <- sum(F_stat_vec_10 > critval_99_n10)
nb_reject_95_n1000 <- sum(F_stat_vec_1000 > critval_95_n1000)
nb_reject_99_n1000 <- sum(F_stat_vec_1000 > critval_99_n1000)

pct_reject_95_n10 <- (nb_reject_95_n10 / length(F_stat_vec_10)) * 100
pct_reject_99_n10 <- (nb_reject_99_n10 / length(F_stat_vec_10)) * 100
pct_reject_95_n1000 <- (nb_reject_95_n1000 / length(F_stat_vec_1000)) * 100
pct_reject_99_n1000 <- (nb_reject_99_n1000 / length(F_stat_vec_1000)) * 100

results_df <- data.frame(
  Distribution = c("F(2,10)", "F(2,10)", "F(2,1000)", "F(2,1000)"),
  Significance_Level = c("95%", "99%", "95%", "99%"),
  Critical_Value = c(critval_95_n10, critval_99_n10, critval_95_n1000, critval_99_n1000),
  Rejections_Count = c(nb_reject_95_n10, nb_reject_99_n10, nb_reject_95_n1000, nb_reject_99_n1000),
  Rejections_Percentage = c(pct_reject_95_n10, pct_reject_99_n10, pct_reject_95_n1000, pct_reject_99_n1000)
)
str(results_df)
results_df$Critical_Value <- round(results_df$Critical_Value, 4)  
results_df$Rejections_Percentage <- round(results_df$Rejections_Percentage, 2)
kable(results_df, format = "html",   
      col.names = c("Distribution", "Significance Level",  
                    "Critical Value", "Rejections Count", "Rejections (%)"),  
      caption = "F-Test Rejection Results"  
) %>%  
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),   
                full_width = FALSE,   
                position = "center")  

## c)
# if the distribution of the error terms had been normal, we would expect that :
# at the 95% (99%) we would reject H_0 for 5%(1%) of the samples tested.
# We notice that, based on our simulation, the observed rejection rates were much higher when the sample size is 10 (around 11% of rejection at the 95% confidence level and around 5% for the 99% confidence level)
# When we look at samples of larger sizes (1000), however, we notice observed results are much closer to the expected proportions ! 
# This is a direct implication of the central limit theorem : a larger sample size improves the approximation to normality

