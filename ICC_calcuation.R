library(geepack)
library(dplyr)
library(readxl)
library(lme4)
library(performance)

data <- read_excel("./dataset/icc_cal_post_data.xlsx")

data$class <- as.factor(data$class)

# Fit random intercept logistic regression model
model <- glmer(
 outcome ~ 1 + (1 | class),
 data = data,
 family = binomial(link = "logit")
)

# Calculate ICC
var_between <- as.numeric(VarCorr(model)$class[1])
ICC <- var_between / (var_between + (pi^2 / 3))
print(ICC)
icc_value <- icc(model)
print(icc_value)

# Calculate Design Effect
n_total <- nrow(data)
n_cluster <- length(unique(data$class))
m_bar <- n_total / n_cluster
DE <- 1 + (m_bar - 1) * ICC
print(DE)

# Power calculation
n_total <- 2671
n_class <- 180
ICC <- 0.113
m <- n_total/n_class
DE <- 1 + (m - 1) * ICC
P1 <- 0.018
P2 <- 0.071
delta <- abs(P2 - P1)
Z_alpha_over_2 <- 1.96
m1 <- n_total/2

# Z_beta
Z_beta <- sqrt((m1 * delta^2)/(DE * (P1*(1-P1) + P2*(1-P2)))) - Z_alpha_over_2
print(paste("Z_beta:", Z_beta))

# power
power <- 1 - pnorm(-Z_beta)
print(paste("Power:", round(power, 4)))