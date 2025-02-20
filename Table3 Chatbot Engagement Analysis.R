library(dplyr)
library(readxl)
library(stats)
library(openxlsx)

# Table 3 | Association between chatbot engagement characteristics and HPV vaccine uptake or scheduled appointment in the intervention group
# Load data
data <- read_excel("")

# Function to calculate risk ratio with fallback options
calculate_log_binomial_rr <- function(data, exposure_var, outcome_var, adjust_vars) {
  # Create formula
  formula_str <- paste(outcome_var, "~", exposure_var, "+", 
                       paste(adjust_vars, collapse = " + "))
  formula <- as.formula(formula_str)
  
  # Try log-binomial first, if fails use Poisson
  model_results <- tryCatch({
    # Attempt log-binomial model
    model <- glm(formula, 
                 family = binomial(link = "log"), 
                 data = data,
                 control = glm.control(maxit = 2000))
    
    list(model = model, type = "log-binomial")
    
  }, error = function(e) {
    # If log-binomial fails, try Poisson
    message("Log-binomial failed, switching to Poisson regression")
    model <- glm(formula, 
                 family = poisson(link = "log"), 
                 data = data)
    list(model = model, type = "poisson")
  })
  
  # Extract model and type from results
  model <- model_results$model
  model_type <- model_results$type
  
  # Get robust standard errors
  se <- sqrt(diag(vcovHC(model, type = "HC0")))
  
  # Calculate RR and CI
  coef <- coef(model)[exposure_var]
  ci <- coef + c(-1, 1) * 1.96 * se[exposure_var]
  
  rr <- exp(coef)
  ci_lower <- exp(ci[1])
  ci_upper <- exp(ci[2])
  p_value <- summary(model)$coefficients[exposure_var, "Pr(>|z|)"]
  
  # Calculate crude RR
  crude_rates <- tapply(data[[outcome_var]], data[[exposure_var]], mean)
  crude_rr <- crude_rates[2] / crude_rates[1]
  
  return(list(
    adjusted_rr = rr,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    p_value = p_value,
    crude_rr = crude_rr,
    model_type = model_type
  ))
}

# Define adjustment variables
adjust_vars <- c(
  "citytype", "relationship", "m_vaccinated", "education",
  "income", "occupation", "newage", "girls_age", "grade_x",
  "only_child", "Left_behind", "influenza_vaccination",
  "sexual_education"
)

# Define engagement variables
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

# Function to create results table
create_adjusted_rr_table <- function(data, engagement_vars, adjust_vars) {
  results <- data.frame(
    Variable = character(),
    `Low_Engagement(Vaccination)` = character(),
    `High_Engagement(Vaccination)` = character(),
    `P_Value` = character(),
    `Adjusted_RR(95%_CI)` = character(),
    `Model_Type` = character(),
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
      sprintf("%.4f", results_model$p_value),
      sprintf("%.2f (%.2f-%.2f)", 
              results_model$adjusted_rr,
              results_model$ci_lower,
              results_model$ci_upper),
      results_model$model_type
    )
  }
  return(results)
}

# Run analysis and create table
adjusted_results <- create_adjusted_rr_table(
  data = data,
  engagement_vars = engagement_vars,
  adjust_vars = adjust_vars
)

# A. Chatbot engagement metrics#
# Print results
print(adjusted_results)

# Define adjustment variables
adjust_vars <- c(
  "citytype", "relationship", "m_vaccinated", "education",
  "income", "occupation", "newage", "girls_age", "grade_x",
  "only_child", "Left_behind", "influenza_vaccination",
  "sexual_education"
)

# Function to calculate adjusted and unadjusted metrics
calculate_bot_usage_metrics <- function(data, outcome_var = "vac_bahaviour", adjust_vars) {
  require(survival)
  
  cont_table <- table(data$bot_usage_type, data[[outcome_var]])
  
  get_group_stats <- function(group_data) {
    vac_count <- sum(group_data[[outcome_var]] == 1)
    total <- nrow(group_data)
    rate <- vac_count / total
    return(list(vac = vac_count, total = total, rate = rate))
  }
  
  groups <- list(
    A = get_group_stats(filter(data, bot_usage_type == 0)),
    B = get_group_stats(filter(data, bot_usage_type == 1))
  )
  
  # Unadjusted analysis
  calculate_unadj_rr_ci <- function(data1, data2) {
    if (data1$rate == 0 || data2$rate == 0) return(c(Inf, Inf, Inf))
    
    rr <- data1$rate / data2$rate
    se_ln_rr <- sqrt(
      (1/data1$vac - 1/data1$total) +
        (1/data2$vac - 1/data2$total)
    )
    ci_lower <- exp(log(rr) - 1.96 * se_ln_rr)
    ci_upper <- exp(log(rr) + 1.96 * se_ln_rr)
    
    return(c(rr, ci_lower, ci_upper))
  }
  
  # Calculate unadjusted results
  unadj_rr_results <- calculate_unadj_rr_ci(groups$B, groups$A)
  unadj_p_value <- chisq.test(cont_table)$p.value
  
  # Adjusted analysis using logistic regression
  # Create formula with adjustment variables
  formula_str <- paste("vac_bahaviour ~ bot_usage_type +", 
                       paste(adjust_vars, collapse = " + "))
  
  # Fit logistic regression model
  model <- glm(as.formula(formula_str), 
               family = binomial(link = "logit"), 
               data = data)
  
  # Extract coefficient and CI for bot_usage_type
  coef_bot <- coef(model)["bot_usage_type"]
  ci <- confint(model)["bot_usage_type", ]
  
  # Convert log odds to RR
  adj_rr <- exp(coef_bot)
  adj_ci_lower <- exp(ci[1])
  adj_ci_upper <- exp(ci[2])
  
  # Get p-value for bot_usage_type
  adj_p_value <- summary(model)$coefficients["bot_usage_type", "Pr(>|z|)"]
  
  # Calculate unadjusted results
  unadj_rr_results <- calculate_unadj_rr_ci(groups$B, groups$A)
  unadj_p_value <- chisq.test(cont_table)$p.value
  
  # Create results dataframes
  results_df <- data.frame(
    Analysis = c("Unadjusted", "Adjusted"),
    Comparison = "B vs A",
    P_value = sprintf("%.4f", c(unadj_p_value, adj_p_value)),
    `RR(95%CI)` = c(
      sprintf("%.2f (%.2f-%.2f)", unadj_rr_results[1], unadj_rr_results[2], unadj_rr_results[3]),
      sprintf("%.2f (%.2f-%.2f)", adj_rr, adj_ci_lower, adj_ci_upper)
    ),
    stringsAsFactors = FALSE
  )
  
  # Create overall statistics
  overall_stats <- data.frame(
    Variable = "Bot Usage",
    `Type_A` = sprintf("%d/%d (%.1f%%)", 
                         groups$A$vac, groups$A$total, groups$A$rate * 100),
    `Type_B` = sprintf("%d/%d (%.1f%%)", 
                         groups$B$vac, groups$B$total, groups$B$rate * 100),
    Unadj_P = sprintf("%.4f", unadj_p_value),
    Adj_P = sprintf("%.4f", adj_p_value),
    stringsAsFactors = FALSE
  )
  
  # Get model summary for adjustment variables
  adj_vars_summary <- summary(model)$coefficients[adjust_vars, ]
  adj_vars_results <- data.frame(
    Variable = rownames(adj_vars_summary),
    Coefficient = adj_vars_summary[, "Estimate"],
    Std_Error = adj_vars_summary[, "Std. Error"],
    P_value = adj_vars_summary[, "Pr(>|z|)"],
    stringsAsFactors = FALSE
  )
  
  return(list(
    overall_stats = overall_stats,
    results = results_df,
    adjustment_vars = adj_vars_results
  ))
}

# B. Chatbot persona comparison
# Run analysis
results <- calculate_bot_usage_metrics(data, adjust_vars = adjust_vars)

# Print results
print(results$overall_stats)
print(results$results)