
if(!require("skedastic")) install.packages("skedastic")
if(!require("ggplot2")) install.packages("ggplot2")
if(!require("sandwich")) install.packages("sandwhich")
if(!require("lmtest")) install.packages("lmtest")
if(!require("dplyr")) install.packages("dplyr")
if(!require("nlme")) install.packages("nlme")
if(!require("plm")) install.packages("plm")
library(skedastic)
library(ggplot2)
library(sandwich)
library(lmtest)
library(dplyr)
library(nlme)
library(plm)

# Q1
set.seed(123)
sample_size <- 10000
x <- rnorm(sample_size, mean = 2, sd=3)
aux <- rnorm(sample_size, mean = 0, sd = 1)
u <- 2*x*aux
y <- 2 + 3*x + u

# Q2
reg <- lm(y ~ x)
summary(reg)
data<-data.frame(x,y)
ggplot(data=data, aes(x = x, y = y)) +  
  geom_point(color = "black", size = 0.5, alpha = 0.3) +       # Smaller points  
  geom_abline(slope = 0, intercept = 0, linetype = "dashed") +  # Optional: dashed abline for emphasis  
  theme_bw() + theme(aspect.ratio = 0.8) + labs(title = "Residuals vs x_1", x="x_1", y="Residuals") +
  theme(plot.title = element_text(face = "bold", size = 12, family = "Helvetica", hjust=0),  
axis.title = element_text(size = 10, family = "Helvetica"))

# Q3
## a)
y_hat <- predict(reg)
sqrd_y_hat <- y_hat^2
u_hat <- residuals(reg)
sqrd_u_hat <- u_hat^2

## b)
reg_SR <- lm(sqrd_u_hat ~ y_hat + sqrd_y_hat)
summary(reg_SR)
R_sqrd_eps <- summary(reg_SR)$r.squared
print(R_sqrd_eps)

## c)
threshold <- qchisq(.95, df=2)
print(threshold)
t_stat <- sample_size*R_sqrd_eps
print(t_stat)

## d)
white_test <- white(mainlm = reg, interactions = TRUE)
white_test$statistic
white_test

# Q5 
summary(reg)
## Robust (White)
coeftest(reg,vcov = vcovHC(reg, "HC1"))
## Alternative way : GLS and WLS (since var(u|x)=4x^2var(aux) = 4x^2)
weights <- 1/(4*x^2)
model_wls <- lm(y ~ x, weights = weights)  
summary(model_wls)
