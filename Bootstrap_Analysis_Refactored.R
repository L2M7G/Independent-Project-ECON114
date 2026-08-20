##### For ECON 114 Final Project #####
### Louisa Gallagher

# Simplified bootstrap procedure using custom functions


set.seed(8545)
B <- 10000
n <- length(unique(gini_complete$country))


results_fe_1 <- matrix(data = NA, nrow = B, ncol = 1)
results_fe_controls_1 <- matrix(data = NA, nrow = B, ncol = 1)
results_fd_1 <- matrix(data = NA, nrow = B, ncol = 1)
results_fd_controls_1 <- matrix(data = NA, nrow = B, ncol = 1)

results_fe_5 <- matrix(data = NA, nrow = B, ncol = 1)
results_fe_controls_5 <- matrix(data = NA, nrow = B, ncol = 1)
results_fd_5 <- matrix(data = NA, nrow = B, ncol = 1)
results_fd_controls_5 <- matrix(data = NA, nrow = B, ncol = 1)


# Bootstrap functions

bootstrap_function <- function(regression, data, model_type) {
  
  model <- plm(regression, data = data, model = model_type, effect = "twoways")
  return(model)
}


# Regression models with and without controls (to reduce repetition)

regression_1 <- gini ~ trade_lag_1

regression_5 <- gini ~ trade_lag_5

regression_controls_1 <- gini ~ trade_lag_1 + unemploy_lag_1 + gdp_pc_lag_1 + inflation_lag_1 + tax_lag_1

regression_controls_5 <- gini ~ trade_lag_5 + unemploy_lag_5 + gdp_pc_lag_5 + inflation_lag_5 + tax_lag_5


# Loop to execute function

for(b in 1:B){
  
  sampled_countries <- sample(unique(gini_complete$country), size = n, replace = TRUE)
  
  temp <- do.call(rbind, lapply(sampled_countries, function(c) 
    gini_complete[gini_complete$country == as.character(c), ])) 
  
  temp <- pdata.frame(temp, index = c("country", "year"))
  
  
  # 1 Year Lag
  
  results_fe_1[b, ] <- coef(
    bootstrap_function(regression_1, 
                     temp, 
                     "within"))["trade_lag_1"]
  
  results_fe_controls_1[b, ] <- coef(
    bootstrap_function(regression_controls_1, 
                     temp, 
                     "within"))["trade_lag_1"]
  
  results_fd_1[b, ] <- coef(
    bootstrap_function(regression_1, 
                     temp, 
                     "fd"))["trade_lag_1"]
  
  results_fd_controls_1[b, ] <- coef(
    bootstrap_function(regression_controls_1, 
                     temp, 
                     "fd"))["trade_lag_1"]

  
  # 5 year lag
  
  results_fe_5[b, ] <- coef(
    bootstrap_function(regression_5, 
                     temp, 
                     "within"))["trade_lag_5"]
  
  results_fe_controls_5[b, ] <- coef(
    bootstrap_function(regression_controls_5, 
                     temp, 
                     "within"))["trade_lag_5"]
  
  results_fd_5[b, ] <- coef(
    bootstrap_function(regression_5, 
                     temp, 
                     "fd"))["trade_lag_5"]
  
  results_fd_controls_5[b, ] <- coef(
    bootstrap_function(regression_controls_5, 
                     temp, 
                     "fd"))["trade_lag_5"]
}
 

results_bootstrap_1 <- data.frame(
  
  fe_1 = results_fe_1,
  fe_controls_1 = results_fe_controls_1,
  
  fd_1 = results_fd_1,
  fd_controls_1 = results_fd_controls_1
  
)

results_bootstrap_5 <- data.frame(
  
  fe_5 = results_fe_5,
  fe_controls_5 = results_fe_controls_5,
  
  fd_5 = results_fd_5,
  fd_controls_5 = results_fd_controls_5
  
)



# Figure 2 & 3

results_bootstrap_long_1 <- gather(results_bootstrap_1, key = "model", value = "coefficient")
results_bootstrap_long_5 <- gather(results_bootstrap_5, key = "model", value = "coefficient")

original_estimates_1 <- data.frame(
  model = c("fe_1", "fe_controls_1", "fd_1", "fd_controls_1"),
  original_value = c(beta_gini_fe_trade_1, beta_gini_fe_c_trade_1, 
                     beta_gini_fd_trade_1, beta_gini_fd_c_trade_1)
)

original_estimates_5 <- data.frame(
  model = c("fe_5", "fe_controls_5", "fd_5", "fd_controls_5"),
  original_value = c(beta_gini_fe_trade_5, beta_gini_fe_c_trade_5, 
                     beta_gini_fd_trade_5, beta_gini_fd_c_trade_5)
)


# Set labels

labels_1 <- c(
  "fd_1" = "First Difference", 
  "fd_controls_1" = "First Difference With Controls",
  "fe_1" = "Fixed Effects",
  "fe_controls_1" = "Fixed Effects With Controls"
)

labels_5 <- c(
  "fd_5" = "First Difference", 
  "fd_controls_5" = "First Difference With Controls",
  "fe_5" = "Fixed Effects",
  "fe_controls_5" = "Fixed Effects With Controls"
)


# Function to plot the bootstrap data

bootstrap_plot <- function(bootstrap_data, original_data, labels, title) {
  
  ggplot(bootstrap_data, aes(x = coefficient)
    ) +
    geom_histogram(
      binwidth = 0.005,
      fill = "lightblue",
      color = 'black',
      alpha = 0.7
      ) +
    geom_vline(
      data = original_data, 
      aes(xintercept = original_value),
      color = 'black',
      linetype = 'dashed',
      size = 0.5
      ) +
    facet_wrap(~ model, 
               nrow = 2,
               ncol = 2,
               labeller = labeller(model = labels)
               ) +
    labs(title = title,
         x = "Coefficient Value", y = "Frequency") +
        theme_minimal() +
        xlim(-0.15, 0.15)
}

boot_lag_1 <- bootstrap_plot(results_bootstrap_long_1, 
                             original_estimates_1, 
                             labels_1, 
                             "Figure 2: Bootstrap Distribution of Estimates (1 Year Lag)")

boot_lag_5 <- bootstrap_plot(results_bootstrap_long_5,
                            original_estimates_5,
                            labels_5,
                            "Figure 3: Bootstrap Distribution of Estimates (5 Year Lag)")
       

ggsave("plot_boot_lag_1.png", plot = boot_lag_1)

ggsave("plot_boot_lag_5.png", plot = boot_lag_5)



# Bootstrap CI

# Function to calculate upper and lower bounds

bootstrap_ci <- function(results, alpha = 0.05) {
  
  lower <- quantile(x = results, probs = alpha/2)
  upper <- quantile(x = results, probs = 1 - alpha/2)
  
  return(c(
    Lower_Bound = lower,
    Upper_Bound = upper
  ))
  
}

# 1 Year Lag

fe_ci_bootstrap_1 <- bootstrap_ci(results_fe_1)

fe_controls_ci_bootstrap_1 <- bootstrap_ci(results_fe_controls_1)

fd_ci_bootstrap_1 <- bootstrap_ci(results_fd_1)

fd_controls_ci_bootstrap_1 <- bootstrap_ci(results_fd_controls_1)


# 5 Year Lag

fe_ci_bootstrap_5 <- bootstrap_ci(results_fe_5)

fe_controls_ci_bootstrap_5 <- bootstrap_ci(results_fe_controls_5)

fd_ci_bootstrap_5 <- bootstrap_ci(results_fd_5)

fd_controls_ci_bootstrap_5 <- bootstrap_ci(results_fd_controls_5)


# Create tables with the CI vectors

ci_bootstrap_lag_1 <- data.frame(
  Model = c(
    "Fixed Effects", 
    "Fixed Effects With Controls", 
    "First Difference", 
    "First Difference With Controls"
    ),
  rbind(
    fe_ci_bootstrap_1,
    fe_controls_ci_bootstrap_1,
    fd_ci_bootstrap_1,
    fd_controls_ci_bootstrap_1
  )
)

ci_bootstrap_lag_5 <- data.frame(
  Model = c(
    "Fixed Effects", 
    "Fixed Effects With Controls", 
    "First Difference", 
    "First Difference With Controls"
  ),
  rbind(
    fe_ci_bootstrap_5,
    fe_controls_ci_bootstrap_5,
    fd_ci_bootstrap_5,
    fd_controls_ci_bootstrap_5
  )
)



# Table 6

stargazer(ci_bootstrap_lag_1, 
          type = "text", 
          summary = FALSE, 
          rownames = FALSE, 
          column.labels = c("Lower Bound", "Upper Bound"),
          title = "Table 6: Bootstrap Confidence Intervals (1 Year Lag)", 
          out = "table_6.html")


# Table 7

stargazer(ci_bootstrap_lag_5, 
          type = "text", 
          summary = FALSE, 
          rownames = FALSE, 
          column.labels = c("Lower Bound", "Upper Bound"),
          title = "Table 7: Bootstrap Confidence Intervals (5 Year Lag)", 
          out = "table_7.html")

##### END #####