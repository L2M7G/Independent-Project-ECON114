##### ECON 114 Final Project #####
### Louisa Gallagher

# Exploring the Role of Economic Integration in Inequality: A Fixed Effects and First Difference Analysis of Trade Openness and the Gini Index 


rm(list = ls())  ; cat("\014")

# setwd("/Users/louisagallagher/ECON114")

# install.packages(c("tidyverse", "dplyr", "plm", "readxl", "stargazer", "ggplot2", "tidyr", "RColorBrewer", "boot", "gridExtra"))

library(tidyverse) 
library(dplyr)
library(plm)
library(readxl)
library(stargazer)
library(ggplot2)
library(tidyr)
library(RColorBrewer)
library(boot)
library(gridExtra)


### Data 


## Gini

gini_raw <- read_excel("gini_raw.xlsx", na = "..")


# Pivot to wide 

gini_raw_clean <- gini_raw %>%
  filter(!is.na(`Country Name`) & 
           !is.na(`Country Code`) & 
           !is.na(`Time`) & 
           !is.na(`Series Name`))

gini_raw_wide <- gini_raw_clean %>%
  select(`Country Name`, `Country Code`, `Time`, `Series Name`, `Value`) %>%
  pivot_wider(names_from = `Series Name`, values_from = `Value`)


# Rename Variables

gini_raw_wide <- gini_raw_wide %>%
  rename(
    year = Time,
    country = `Country Name`,
    code = `Country Code`,
    trade = `Trade (% of GDP)`,
    gini = `Gini index`,
    unemploy = `Unemployment, total (% of total labor force) (modeled ILO estimate)`,
    inflation = `Inflation, consumer prices (annual %)`,
    tax = `Tax revenue (% of GDP)`,
    gdp_pc = `GDP per capita (constant 2015 US$)`
  )

length(unique(gini_raw_wide$country)) 


# Get set of Countries with Complete Data

gini_complete <- gini_raw_wide %>%
  group_by(country) %>%
  filter (
  all(!is.na(trade)) & 
  all(!is.na(gini)) & 
  all(!is.na(unemploy)) & 
  all(!is.na(gdp_pc)) & 
  all(!is.na(inflation)) & 
  all(!is.na(tax))
  ) %>%
  ungroup()

length(unique(gini_complete$country))  

# 8 countries have complete data for both trade openness and gini coefficient 


gini_complete <- pdata.frame(gini_complete, index = c("country", "year"))


# Summary Statistics and Plots

# Table 1

sumstat <- gini_complete[c("trade", "gini", "unemploy", "gdp_pc", "inflation", "tax")]
sumstat <- as.data.frame(sumstat)

stargazer(sumstat, 
          type = "text", 
          title = "Table 1: Descriptive Statistics", 
          digits = 2,
          summary.stat = c("n", "mean", "sd", "min", "max"),
          covariate.labels = c("Trade (% GDP)", 
                               "Gini Index", 
                               "Unemployment Rate", 
                               "GDP per Capita", 
                               "Inflation Rate", 
                               "Tax Revenue (% GDP)"),
          column.labels = c("Observations", "Mean", "Std. Dev.", "Min", "Max"), 
          out = "table_1.html" 
)

# Figure 1

long_gini <- gini_complete %>%
  pivot_longer(cols = c("trade", "gini", "unemploy", "gdp_pc", "inflation", "tax"),
               names_to = "variable",  
               values_to = "value")    

long_gini$variable <- factor(long_gini$variable, 
                             levels = c("trade", "gini", "unemploy", "gdp_pc", "inflation", "tax"))
long_gini$year <- as.numeric(as.character(long_gini$year))

gini <- ggplot(long_gini, aes(x = year, y = value, color = country, group = country)) +
  geom_line() +  
  facet_wrap(~variable, scales = "free_y", ncol = 2, 
             labeller = labeller(variable = c("trade" = "Trade (% GDP)", 
                                              "gini" = "Gini Index", 
                                              "unemploy" = "Unemployment Rate", 
                                              "gdp_pc" = "GDP per Capita", 
                                              "inflation" = "Inflation Rate", 
                                              "tax" = "Tax Revenue (% GDP)"))) +
  labs(title = "Figure 1: Variables Over Time",
       x = "Year",
       y = NULL) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = seq(min(long_gini$year), max(long_gini$year), by = 4)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", family = "Times New Roman"),
    axis.title.x = element_text(size = 12, family = "Times New Roman"),  
    axis.text = element_text(size = 10),  
    strip.text = element_text(size = 12)  
  ) 

ggsave("figure_1.png", plot = gini)
  
  
# Lag Trade and Controls by 1 & 5 years
  
gini_complete$year <- as.numeric(as.character(gini_complete$year))

gini_complete <- gini_complete %>%
  arrange(country, year)


gini_complete$trade_lag_1 <- lag(gini_complete$trade, k = 1)
gini_complete$unemploy_lag_1 <- lag(gini_complete$unemploy, k = 1)
gini_complete$gdp_pc_lag_1 <- lag(gini_complete$gdp_pc, k = 1)
gini_complete$inflation_lag_1 <- lag(gini_complete$inflation, k = 1)
gini_complete$tax_lag_1 <- lag(gini_complete$tax, k = 1)

gini_complete$trade_lag_5 <- lag(gini_complete$trade, k = 5)
gini_complete$unemploy_lag_5 <- lag(gini_complete$unemploy, k = 5)
gini_complete$gdp_pc_lag_5 <- lag(gini_complete$gdp_pc, k = 5)
gini_complete$inflation_lag_5 <- lag(gini_complete$inflation, k = 5)
gini_complete$tax_lag_5 <- lag(gini_complete$tax, k = 5)



### Empirical Methods and Results 
  
  
## Fixed Effects (Year and Country)

fe_gini_1 <- plm(gini ~ trade_lag_1, 
               data = gini_complete, 
               model = "within", 
               effect = "twoways")

fe_gini_controls_1 <- plm(gini ~ trade_lag_1 + unemploy_lag_1 + gdp_pc_lag_1 + inflation_lag_1 + tax_lag_1, 
                        data = gini_complete, 
                        model = "within", 
                        effect = "twoways")

fe_gini_5 <- plm(gini ~ trade_lag_5, 
                 data = gini_complete, 
                 model = "within", 
                 effect = "twoways")

fe_gini_controls_5 <- plm(gini ~ trade_lag_5 + unemploy_lag_5 + gdp_pc_lag_5 + inflation_lag_5 + tax_lag_5, 
                          data = gini_complete, 
                          model = "within", 
                          effect = "twoways")

# Table 2 & 3

stargazer(fe_gini_1, fe_gini_controls_1, type = "text",
          title = "Table 2: Fixed Effects Regression Results (1 Year Lag)", 
          column.labels = c("Gini", "Gini with Controls"),
          covariate.labels = c("Trade (% GDP)", 
                               "Gini Index", 
                               "Unemployment Rate", 
                               "GDP per Capita", 
                               "Inflation Rate", 
                               "Tax Revenue (% GDP)"),
          notes = "Naive standard errors in parentheses. *** p<0.01, ** p<0.05, * p<0.1",
          out = "table_2.html")


stargazer(fe_gini_5, fe_gini_controls_5, type = "text",
          title = "Table 3: Fixed Effects Regression Results (5 Year Lag)",
          column.labels = c("Gini", "Gini with Controls"),
          covariate.labels = c("Trade (% GDP)", 
                               "Gini Index", 
                               "Unemployment Rate", 
                               "GDP per Capita", 
                               "Inflation Rate", 
                               "Tax Revenue (% GDP)"),
          notes = "Naive standard errors in parentheses. *** p<0.01, ** p<0.05, * p<0.1",
          out = "table_3.html")


beta_gini_fe_trade_1 = coefficients(fe_gini_1)[1]
beta_gini_fe_c_trade_1 = coefficients(fe_gini_controls_1)[1]

beta_gini_fe_trade_5 = coefficients(fe_gini_5)[1]
beta_gini_fe_c_trade_5 = coefficients(fe_gini_controls_5)[1]



## First Difference

fd_gini_1 <- plm(gini ~ trade_lag_1, 
               data = gini_complete, 
               model = "fd")

fd_gini_controls_1 <- plm(gini ~ trade_lag_1 + unemploy_lag_1 + gdp_pc_lag_1 + inflation_lag_1 + tax_lag_1, 
                        data = gini_complete, 
                        model = "fd")

fd_gini_5 <- plm(gini ~ trade_lag_5, 
                 data = gini_complete, 
                 model = "fd")

fd_gini_controls_5 <- plm(gini ~ trade_lag_5 + unemploy_lag_5 + gdp_pc_lag_5 + inflation_lag_5 + tax_lag_5, 
                          data = gini_complete, 
                          model = "fd")


# Table 4 & 5 

stargazer(fd_gini_1, fd_gini_controls_1, type = "text",
          title = "Table 4: First Difference Regression Results (1 Year Lag)", 
          column.labels = c("Gini", "Gini with Controls"),
          covariate.labels = c("Trade (% GDP)", 
                               "Gini Index", 
                               "Unemployment Rate", 
                               "GDP per Capita", 
                               "Inflation Rate", 
                               "Tax Revenue (% GDP)"),
          notes = "Naive standard errors in parentheses. *** p<0.01, ** p<0.05, * p<0.1",
          out = "table_4.html")


stargazer(fd_gini_5, fd_gini_controls_5, type = "text",
          title = "Table 5: First Difference Regression Results (5 Year Lag)",
          column.labels = c("Gini", "Gini with Controls"),
          covariate.labels = c("Trade (% GDP)", 
                               "Gini Index", 
                               "Unemployment Rate", 
                               "GDP per Capita", 
                               "Inflation Rate", 
                               "Tax Revenue (% GDP)"),
          notes = "Naive standard errors in parentheses. *** p<0.01, ** p<0.05, * p<0.1",
          out = "table_5.html")


beta_gini_fd_trade_1 = coefficients(fd_gini_1)[2]
beta_gini_fd_c_trade_1 = coefficients(fd_gini_controls_1)[2]

beta_gini_fd_trade_5 = coefficients(fd_gini_5)[2]
beta_gini_fd_c_trade_5 = coefficients(fd_gini_controls_5)[2]




### Inference


## Bootstrap

set.seed(8545)
B = 10000
n <- length(unique(gini_complete$country))

results_fe_1 = matrix(data = NA, nrow = B, ncol = 1)
results_fe_controls_1 = matrix(data = NA, nrow = B, ncol = 1)
results_fd_1 = matrix(data = NA, nrow = B, ncol = 1)
results_fd_controls_1 = matrix(data = NA, nrow = B, ncol = 1)

results_fe_5 = matrix(data = NA, nrow = B, ncol = 1)
results_fe_controls_5 = matrix(data = NA, nrow = B, ncol = 1)
results_fd_5 = matrix(data = NA, nrow = B, ncol = 1)
results_fd_controls_5 = matrix(data = NA, nrow = B, ncol = 1)


for(b in 1:B){
  
  sampled_countries <- sample(unique(gini_complete$country), size = n, replace = TRUE)
  temp <- do.call(rbind, lapply(sampled_countries, function(c) 
    gini_complete[gini_complete$country == as.character(c), ])) 
  temp <- pdata.frame(temp, index = c("country", "year"))
  
  
  # 1 Year Lag
  
    temp_fe_1 <- plm(gini ~ trade_lag_1, data = temp, model = "within", effect = "twoways")
    results_fe_1[b, ] = coef(temp_fe_1)["trade_lag_1"]
    
    temp_fe_controls_1 <- plm(gini ~ trade_lag_1 + unemploy_lag_1 + gdp_pc_lag_1 + inflation_lag_1 + tax_lag_1, data = temp, model = "within", effect = "twoways")
    results_fe_controls_1[b, ] = coef(temp_fe_controls_1)["trade_lag_1"]
  
    temp_fd_1 <- plm(gini ~ trade_lag_1, data = temp, model = "fd")
    results_fd_1[b, ] = coef(temp_fd_1)["trade_lag_1"]
    
    temp_fd_controls_1 <- plm(gini ~ trade_lag_1 + unemploy_lag_1 + gdp_pc_lag_1 + inflation_lag_1 + tax_lag_1, data = temp, model = "fd")
    results_fd_controls_1[b, ] = coef(temp_fd_controls_1)["trade_lag_1"]
  
    
  # 5 Year Lag
    
    temp_fe_5 <- plm(gini ~ trade_lag_5, data = temp, model = "within", effect = "twoways")
    results_fe_5[b, ] = coef(temp_fe_5)["trade_lag_5"]
    
    temp_fe_controls_5 <- plm(gini ~ trade_lag_5 + unemploy_lag_5 + gdp_pc_lag_5 + inflation_lag_5 + tax_lag_5, data = temp, model = "within", effect = "twoways")
    results_fe_controls_5[b, ] = coef(temp_fe_controls_5)["trade_lag_5"]
    
    temp_fd_5 <- plm(gini ~ trade_lag_5, data = temp, model = "fd")
    results_fd_5[b, ] = coef(temp_fd_5)["trade_lag_5"]
    
    temp_fd_controls_5 <- plm(gini ~ trade_lag_5 + unemploy_lag_5 + gdp_pc_lag_5 + inflation_lag_5 + tax_lag_5, data = temp, model = "fd")
    results_fd_controls_5[b, ] = coef(temp_fd_controls_5)["trade_lag_5"]
    
  
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

boot_lag_1 <- ggplot(results_bootstrap_long_1, aes(x = coefficient)) +
  geom_histogram(binwidth = 0.005, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_vline(data = original_estimates_1, aes(xintercept = original_value), 
             color = "black", linetype = "dashed", size = 0.5) +
  facet_wrap(~ model, nrow = 2, ncol = 2, 
             labeller = labeller(model = c("fd_1" = "First Difference", 
                                                             "fd_controls_1" = "First Difference With Controls",
                                                             "fe_1" = "Fixed Effects",
                                                             "fe_controls_1" = "Fixed Effects With Controls"))) +
  labs(title = "Figure 2: Bootstrap Distribution of Estimates (1 Year Lag)", 
       x = "Coefficient Value", y = "Frequency") +
  theme_minimal()+
  xlim(-0.15, 0.15)

ggsave("plot_boot_lag_1.png", plot = boot_lag_1)


boot_lag_5 <- ggplot(results_bootstrap_long_5, aes(x = coefficient)) +
  geom_histogram(binwidth = 0.005, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_vline(data = original_estimates_5, aes(xintercept = original_value), 
             color = "black", linetype = "dashed", size = 0.5) +
  facet_wrap(~ model, nrow = 2, ncol = 2, 
             labeller = labeller(model = c("fd_5" = "First Difference", 
                                           "fd_controls_5" = "First Difference With Controls",
                                           "fe_5" = "Fixed Effects",
                                           "fe_controls_5" = "Fixed Effects With Controls"))) +
  labs(title = "Figure 3: Bootstrap Distribution of Estimates (5 Year Lag)", 
       x = "Coefficient Value", y = "Frequency") +
  theme_minimal()+
  xlim(-0.15, 0.15)

ggsave("plot_boot_lag_5.png", plot = boot_lag_5)



# Bootstrap CI

alpha <- 0.05


# 1 Year Lag

fe_ci_upper_boostrap_1 <- quantile(x = results_fe_1, probs = 1-alpha/2)
fe_ci_lower_boostrap_1 <- quantile(x = results_fe_1, probs = alpha/2)

fe_controls_ci_upper_boostrap_1 <- quantile(x = results_fe_controls_1, probs = 1-alpha/2)
fe_controls_ci_lower_boostrap_1 <- quantile(x = results_fe_controls_1, probs = alpha/2)

fd_ci_upper_boostrap_1 <- quantile(x = results_fd_1, probs = 1-alpha/2)
fd_ci_lower_boostrap_1 <- quantile(x = results_fd_1, probs = alpha/2)

fd_controls_ci_upper_boostrap_1 <- quantile(x = results_fd_controls_1, probs = 1-alpha/2)
fd_controls_ci_lower_boostrap_1 <- quantile(x = results_fd_controls_1, probs = alpha/2)


# 5 Year Lag

fe_ci_upper_boostrap_5 <- quantile(x = results_fe_5, probs = 1-alpha/2)
fe_ci_lower_boostrap_5 <- quantile(x = results_fe_5, probs = alpha/2)

fe_controls_ci_upper_boostrap_5 <- quantile(x = results_fe_controls_5, probs = 1-alpha/2)
fe_controls_ci_lower_boostrap_5 <- quantile(x = results_fe_controls_5, probs = alpha/2)

fd_ci_upper_boostrap_5 <- quantile(x = results_fd_5, probs = 1-alpha/2)
fd_ci_lower_boostrap_5 <- quantile(x = results_fd_5, probs = alpha/2)

fd_controls_ci_upper_boostrap_5 <- quantile(x = results_fd_controls_5, probs = 1-alpha/2)
fd_controls_ci_lower_boostrap_5 <- quantile(x = results_fd_controls_5, probs = alpha/2)


ci_bootstrap_lag_1 <- data.frame(
  Model = c("Fixed Effects", "Fixed Effects With Controls", "First Difference", "First Difference With Controls"),
  Lower_Bound = c(fe_ci_lower_boostrap_1, fe_controls_ci_lower_boostrap_1, fd_ci_lower_boostrap_1, fd_controls_ci_lower_boostrap_1),
  Upper_Bound = c(fe_ci_upper_boostrap_1, fe_controls_ci_upper_boostrap_1, fd_ci_upper_boostrap_1, fd_controls_ci_upper_boostrap_1)
)


ci_bootstrap_lag_5 <- data.frame(
  Model = c("Fixed Effects", "Fixed Effects With Controls", "First Difference", "First Difference With Controls"),
  Lower_Bound = c(fe_ci_lower_boostrap_5, fe_controls_ci_lower_boostrap_5, fd_ci_lower_boostrap_5, fd_controls_ci_lower_boostrap_5),
  Upper_Bound = c(fe_ci_upper_boostrap_5, fe_controls_ci_upper_boostrap_5, fd_ci_upper_boostrap_5, fd_controls_ci_upper_boostrap_5)
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



## Clustered Standard Errors

# 1 Year Lag

se_cluster_gini_fe_1 <- sqrt(vcovHC(fe_gini_1, cluster = "group", type = "HC1")[1])
se_cluster_gini_fe_c_1 <- sqrt(vcovHC(fe_gini_controls_1, cluster = "group", type = "HC1")[1])
se_cluster_gini_fd_1 <- sqrt(vcovHC(fd_gini_1, cluster = "group", type = "HC1")[2,2])
se_cluster_gini_fd_c_1 <- sqrt(vcovHC(fd_gini_controls_1, cluster = "group", type = "HC1")[2,2])

se_cluster_gini_fe_5 <- sqrt(vcovHC(fe_gini_5, cluster = "group", type = "HC1")[1])
se_cluster_gini_fe_c_5 <- sqrt(vcovHC(fe_gini_controls_5, cluster = "group", type = "HC1")[1])
se_cluster_gini_fd_5 <- sqrt(vcovHC(fd_gini_5, cluster = "group", type = "HC1")[2,2])
se_cluster_gini_fd_c_5 <- sqrt(vcovHC(fd_gini_controls_5, cluster = "group", type = "HC1")[2,2])

cluster_se_1 <- data.frame(
  Model = c("Fixed Effects", "Fixed Effects With Controls", "First Difference", "First Difference With Controls"),
  Standard_Error = c(se_cluster_gini_fe_1, se_cluster_gini_fe_c_1, se_cluster_gini_fd_1, se_cluster_gini_fd_c_1)
)

cluster_se_5 <- data.frame(
  Model = c("Fixed Effects", "Fixed Effects With Controls", "First Difference", "First Difference With Controls"),
  Standard_Error = c(se_cluster_gini_fe_5, se_cluster_gini_fe_c_5, se_cluster_gini_fd_5, se_cluster_gini_fd_c_5)
)



# Table 8 

stargazer(cluster_se_1, 
          type = "text", 
          summary = FALSE, 
          rownames = FALSE, 
          column.labels = c("Trade Coefficient Standard Error"),
          title = "Table 8: Clustered Standard Errors (1 Year Lag)", 
          out = "table_8.html")


# Table 9 

stargazer(cluster_se_5, 
          type = "text", 
          summary = FALSE, 
          rownames = FALSE, 
          column.labels = c("Trade Coefficient Standard Error"),
          title = "Table 9: Clustered Standard Errors (5 Year Lag)", 
          out = "table_9.html")


# Confidence Intervals

# 1 Year Lag

CIlower_cluster_fe_1 <- beta_gini_fe_trade_1 - 1.96*se_cluster_gini_fe_1
CIupper_cluster_fe_1 <- beta_gini_fe_trade_1 + 1.96*se_cluster_gini_fe_1

CIlower_cluster_fe_c_1 <- beta_gini_fe_c_trade_1 - 1.96*se_cluster_gini_fe_c_1
CIupper_cluster_fe_c_1 <- beta_gini_fe_c_trade_1 + 1.96*se_cluster_gini_fe_c_1

CIlower_cluster_fd_1 <- beta_gini_fd_trade_1 - 1.96*se_cluster_gini_fd_1
CIupper_cluster_fd_1 <- beta_gini_fd_trade_1 + 1.96*se_cluster_gini_fd_1

CIlower_cluster_fd_c_1 <- beta_gini_fd_c_trade_1 - 1.96*se_cluster_gini_fd_c_1
CIupper_cluster_fd_c_1 <- beta_gini_fd_c_trade_1 + 1.96*se_cluster_gini_fd_c_1


# 5 Year Lag

CIlower_cluster_fe_5 <- beta_gini_fe_trade_5 - 1.96*se_cluster_gini_fe_5
CIupper_cluster_fe_5 <- beta_gini_fe_trade_5 + 1.96*se_cluster_gini_fe_5

CIlower_cluster_fe_c_5 <- beta_gini_fe_c_trade_5 - 1.96*se_cluster_gini_fe_c_5
CIupper_cluster_fe_c_5 <- beta_gini_fe_c_trade_5 + 1.96*se_cluster_gini_fe_c_5

CIlower_cluster_fd_5 <- beta_gini_fd_trade_5 - 1.96*se_cluster_gini_fd_5
CIupper_cluster_fd_5 <- beta_gini_fd_trade_5 + 1.96*se_cluster_gini_fd_5

CIlower_cluster_fd_c_5 <- beta_gini_fd_c_trade_5 - 1.96*se_cluster_gini_fd_c_5
CIupper_cluster_fd_c_5 <- beta_gini_fd_c_trade_5 + 1.96*se_cluster_gini_fd_c_5


ci_cluster_lag_1 <- data.frame(
  Model = c("Fixed Effects", "Fixed Effects With Controls", "First Difference", "First Difference With Controls"),
  Lower_Bound = c(CIlower_cluster_fe_1, CIlower_cluster_fe_c_1, CIlower_cluster_fd_1, CIlower_cluster_fd_c_1),
  Upper_Bound = c(CIupper_cluster_fe_1, CIupper_cluster_fe_c_1, CIupper_cluster_fd_1, CIupper_cluster_fd_c_1)
)

ci_cluster_lag_5 <- data.frame(
  Model = c("Fixed Effects", "Fixed Effects With Controls", "First Difference", "First Difference With Controls"),
  Lower_Bound = c(CIlower_cluster_fe_5, CIlower_cluster_fe_c_5, CIlower_cluster_fd_5, CIlower_cluster_fd_c_5),
  Upper_Bound = c(CIupper_cluster_fe_5, CIupper_cluster_fe_c_5, CIupper_cluster_fd_5, CIupper_cluster_fd_c_5)
)


# Table 10

stargazer(ci_cluster_lag_1, 
          type = "text", 
          summary = FALSE, 
          rownames = FALSE, 
          column.labels = c("Lower Bound", "Upper Bound"),
          title = "Table 10: Clustered Confidence Intervals (1 Year Lag)", 
          out = "table_10.html")


# Table 11

stargazer(ci_cluster_lag_5, 
          type = "text", 
          summary = FALSE, 
          rownames = FALSE, 
          column.labels = c("Lower Bound", "Upper Bound"),
          title = "Table 11: Clustered Confidence Intervals (5 Year Lag)", 
          out = "table_11.html")


# Table 12 created manually



##### END #####