# Table 3 | Association between chatbot engagement characteristics and HPV vaccine receipt 
# or scheduled appointment in the intervention group (n=1,051)

library(dplyr)
library(readxl)
library(stats)
library(sandwich)

# Load data
data <- read_excel("./data/...")

# Function to calculate risk ratio
calculate_log_binomial_rr <- function(data, exposure_var, outcome_var, adjust_vars) {
  formula_str <- paste(outcome_var, "~", exposure_var, "+", 
                       paste(adjust_vars, collapse = " + "))
  formula <- as.formula(formula_str)
  
  model <- glm(formula, 
               family = binomial(link = "log"), 
               data = data,
               control = glm.control(maxit = 2000))
  
  se <- sqrt(diag(vcovHC(model, type = "HC0")))
  
  coef <- coef(model)[exposure_var]
  ci <- coef + c(-1, 1) * 1.96 * se[exposure_var]
  
  rr <- exp(coef)
  ci_lower <- exp(ci[1])
  ci_upper <- exp(ci[2])
  p_value <- summary(model)$coefficients[exposure_var, "Pr(>|z|)"]
  
  crude_rates <- tapply(data[[outcome_var]], data[[exposure_var]], mean)
  crude_rr <- crude_rates[2] / crude_rates[1]
  
  return(list(
    adjusted_rr = rr,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    p_value = p_value,
    crude_rr = crude_rr
  ))
}

# Define adjustment variables
adjust_vars <- c(
  "citytype", "relationship", "m_vaccinated", "education",
  "income", "occupation", "newage", "girls_age", "grade_x",
  "only_child", "Left_behind", "influenza_vaccination",
  "sexual_education"
)

# Define engagement variables with descriptive names
engagement_vars <- c(
  "total_engagement",
  "engagement_interaction_count", 
  "engagement_duration",
  "engagement_question_count",
  "engagement_category_diversity"
)

variable_names <- c(
  "Total Engagement",
  "Interaction Count",
  "Usage Duration", 
  "Question Count",
  "Category Diversity"
)

# A. Chatbot engagement metrics
create_adjusted_rr_table <- function(data, engagement_vars, adjust_vars) {
  results <- data.frame(
    Variable = character(),
    `Low_Engagement` = character(),
    `High_Engagement` = character(),
    `P_Value` = character(),
    `Adjusted_RR(95%_CI)` = character(),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(engagement_vars)) {
    exposure_var <- engagement_vars[i]
    
    low_group <- data[data[[exposure_var]] == 0, ]
    high_group <- data[data[[exposure_var]] == 1, ]
    
    low_rate <- sprintf("%d/%d (%.1f%%)", 
                        sum(low_group$vac_bahaviour), 
                        nrow(low_group),
                        100 * mean(low_group$vac_bahaviour))
    
    high_rate <- sprintf("%d/%d (%.1f%%)", 
                         sum(high_group$vac_bahaviour), 
                         nrow(high_group),
                         100 * mean(high_group$vac_bahaviour))
    
    results_model <- calculate_log_binomial_rr(
      data = data,
      exposure_var = exposure_var,
      outcome_var = "vac_bahaviour",
      adjust_vars = adjust_vars
    )
    
    results[i, ] <- c(
      variable_names[i],
      low_rate,
      high_rate,
      sprintf("%.3f", results_model$p_value),
      sprintf("%.2f (%.2f-%.2f)", 
              results_model$adjusted_rr,
              results_model$ci_lower,
              results_model$ci_upper)
    )
  }
  return(results)
}

# Run analysis and create table for engagement metrics
engagement_results <- create_adjusted_rr_table(
  data = data,
  engagement_vars = engagement_vars,
  adjust_vars = adjust_vars
)

# B. Chatbot persona comparison (expert only vs Nurse or combined)
calculate_bot_persona_metrics <- function(data, outcome_var = "vac_bahaviour", adjust_vars) {
  # Get statistics for each group (0 = nurse, 1 = expert)
  nurse_group <- filter(data, bot_usage_type == 0)
  expert_group <- filter(data, bot_usage_type == 1)
  
  nurse_stats <- list(
    vac = sum(nurse_group$vac_bahaviour),
    total = nrow(nurse_group),
    rate = mean(nurse_group$vac_bahaviour)
  )
  
  expert_stats <- list(
    vac = sum(expert_group$vac_bahaviour),
    total = nrow(expert_group),
    rate = mean(expert_group$vac_bahaviour)
  )
  
  # Calculate crude RR and p-value
  cont_table <- table(data$bot_usage_type, data[[outcome_var]])
  crude_rr <- expert_stats$rate / nurse_stats$rate
  
  # Calculate standard error for crude RR
  se_ln_rr <- sqrt(
    (1 - nurse_stats$rate) / (nurse_stats$total * nurse_stats$rate) +
    (1 - expert_stats$rate) / (expert_stats$total * expert_stats$rate)
  )
  
  # Calculate crude CI
  crude_ci_lower <- exp(log(crude_rr) - 1.96 * se_ln_rr)
  crude_ci_upper <- exp(log(crude_rr) + 1.96 * se_ln_rr)
  
  # Calculate crude p-value
  crude_p_value <- chisq.test(cont_table)$p.value
  
  # Calculate adjusted RR
  formula_str <- paste(outcome_var, "~ bot_usage_type +", 
                     paste(adjust_vars, collapse = " + "))
  
  model <- glm(as.formula(formula_str), 
             family = binomial(link = "log"), 
             data = data,
             control = glm.control(maxit = 2000))
  
  se <- sqrt(diag(vcovHC(model, type = "HC0")))
  
  coef <- coef(model)["bot_usage_type"]
  ci <- coef + c(-1, 1) * 1.96 * se["bot_usage_type"]
  
  adj_rr <- exp(coef)
  adj_ci_lower <- exp(ci[1])
  adj_ci_upper <- exp(ci[2])
  adj_p_value <- summary(model)$coefficients["bot_usage_type", "Pr(>|z|)"]
  
  # Create results dataframe
  results_df <- data.frame(
    Analysis = c("Unadjusted", "Adjusted"),
    `Nurse_Persona` = c(
      sprintf("%d/%d (%.1f%%)", nurse_stats$vac, nurse_stats$total, nurse_stats$rate * 100),
      ""
    ),
    `Expert_Persona` = c(
      sprintf("%d/%d (%.1f%%)", expert_stats$vac, expert_stats$total, expert_stats$rate * 100),
      ""
    ),
    P_value = sprintf("%.3f", c(crude_p_value, adj_p_value)),
    `RR(95%CI)` = c(
      sprintf("%.2f (%.2f-%.2f)", crude_rr, crude_ci_lower, crude_ci_upper),
      sprintf("%.2f (%.2f-%.2f)", adj_rr, adj_ci_lower, adj_ci_upper)
    ),
    stringsAsFactors = FALSE
  )
  
  return(results_df)
}

# Run analysis for chatbot persona comparison
persona_results <- calculate_bot_persona_metrics(data, adjust_vars = adjust_vars)

# Print results
cat("\nA. Chatbot engagement metrics\n")
print(engagement_results)

cat("\nB. Chatbot persona comparison (expert vs nurse)\n")
print(persona_results)