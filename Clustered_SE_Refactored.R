##### For ECON 114 Final Project #####
### Louisa Gallagher

# Simplified clustering procedure using custom functions

## Clustered Standard Errors

# Define custom function to cluster and calculate SE (separate for FE and FD due to different indexing)

fe_cluster_se <- function(model_name) {
  
  sqrt(vcovHC(model_name, cluster = "group", type = "HC1")[1])
  
}

fd_cluster_se <- function(model_name) {
  
  sqrt(vcovHC(model_name, cluster = "group", type = "HC1")[2, 2])
  
}

# 1 Year Lag

se_cluster_gini_fe_1 <- fe_cluster_se(fe_gini_1)
se_cluster_gini_fe_c_1 <- fe_cluster_se(fe_gini_controls_1)
se_cluster_gini_fd_1 <- fd_cluster_se(fd_gini_1)
se_cluster_gini_fd_c_1 <- fd_cluster_se(fd_gini_controls_1)

# 5 Year Lag

se_cluster_gini_fe_5 <- fe_cluster_se(fe_gini_5)
se_cluster_gini_fe_c_5 <- fe_cluster_se(fe_gini_controls_5)
se_cluster_gini_fd_5 <- fd_cluster_se(fd_gini_5)
se_cluster_gini_fd_c_5 <- fd_cluster_se(fd_gini_controls_5)

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

# Define CI function

cluster_CI <- function(beta, se, alpha = 0.05) {
  
  lower <- beta - qnorm(1 - alpha/2) * se
  upper <- beta + qnorm(1 - alpha/2) * se
  
  return(c(
    Lower_Bound = lower,
    Upper_Bound = upper
  ))
  
}


# 1 Year Lag

fe_ci_cluster_1 <- cluster_CI(beta_gini_fe_trade_1, se_cluster_gini_fe_1)

fe_controls_ci_cluster_1 <- cluster_CI(beta_gini_fe_c_trade_1, se_cluster_gini_fe_c_1)

fd_ci_cluster_1 <- cluster_CI(beta_gini_fd_trade_1, se_cluster_gini_fd_1)

fd_controls_ci_cluster_1 <- cluster_CI(beta_gini_fd_c_trade_1, se_cluster_gini_fd_c_1)

# 5 Year Lag

fe_ci_cluster_5 <- cluster_CI(beta_gini_fe_trade_5, se_cluster_gini_fe_5)

fe_controls_ci_cluster_5 <- cluster_CI(beta_gini_fe_c_trade_5, se_cluster_gini_fe_c_5)

fd_ci_cluster_5 <- cluster_CI(beta_gini_fd_trade_5, se_cluster_gini_fd_5)

fd_controls_ci_cluster_5 <- cluster_CI(beta_gini_fd_c_trade_5, se_cluster_gini_fd_c_5)


#Create tables with the CI vectors

ci_cluster_lag_1 <- data.frame(
  Model = c(
    "Fixed Effects", 
    "Fixed Effects With Controls", 
    "First Difference", 
    "First Difference With Controls"
  ),
  rbind(
    fe_ci_cluster_1,
    fe_controls_ci_cluster_1,
    fd_ci_cluster_1,
    fd_controls_ci_cluster_1
  )
)

ci_cluster_lag_5 <- data.frame(
  Model = c(
    "Fixed Effects", 
    "Fixed Effects With Controls", 
    "First Difference", 
    "First Difference With Controls"
  ),
  rbind(
    fe_ci_cluster_5,
    fe_controls_ci_cluster_5,
    fd_ci_cluster_5,
    fd_controls_ci_cluster_5
  )
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


##### END #####