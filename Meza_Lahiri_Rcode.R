
library(dplyr)
library(ggplot2)

set.seed(123)# set a seed for reproducibility x1,x2,x3,x4 will be the same for every run.
x1 = runif(10* 5)
x2 = runif(50, 1, 2)
x3 = rpois(50 , lambda = 3)
x4 = rexp(50, rate = 0.5)

generate_data_1 <- function(sigma_e_sq, sigma_v_sq, n_groups, n_obs,beta) {
  beta0 <- beta[1]
  beta1 <- beta[2]
  beta2 <- beta[3]
  beta3 <- beta[4]
  beta4 <- beta[5]
  
  # Generate data
  data <- data.frame(
    group = rep(1:n_groups, each = n_obs),
    x1,
    x2,
    x3,
    x4,
    v_ii <- rnorm(n_groups, 0, sqrt(sigma_v_sq)),
    v_i = rep(v_ii, each = n_obs),
    e_ij = rnorm(n_groups * n_obs, sd = sqrt(sigma_e_sq))
  )
  # print(head(data,10)) # to support on validation of simulation x1,x2,x3,x4 are same for every run but v_ii, v_i, e_ij change in every run
  # Generate response variable y_star which satisfy the assumption based on Li paper
  
  for (i in 1:n_groups) {
    data$y_prime[data$group == i] <- beta0 * 1 + 
      beta1 * data$x1[data$group == i] + 
      beta2 * data$x2[data$group == i] + 
      beta3 * data$x3[data$group == i] + 
      beta4 * data$x4[data$group == i] +
      data$v_i[data$group == i] + 
      data$e_ij[data$group == i]
  }
  
  # Transform to y using Eq. (3)
  
  lambda=0.3 # fixed based on Li simulation results and suggestions
  inv_lambda<- 1/lambda
  data$y<- sign(data$y_prime) * abs(data$y_prime) ^ inv_lambda  # these are the original values of y 
  # the above code is for generating data y and x only is not used for var selection or estimation
  
  
  # Given the data x and y only we now do var selection
  # the above data y and x are generated according to model 2 of Li et al, 
  # Meza and Lahiri method does not apply to model 2
  # If y'=h(y;lambda) is known then we can apply Meza and Lahiri on y'=x*beta+v_k +eps_kj, (y' may be used as response variable in Meza and Lahiri (M&L) )
  
  # Only y is observed, y' is not observed. To apply M&L we predict y_star using y by estimating lambda
  #         lambda_hat= - n/(sum(log(abs(data$y)))) # estimator of lambda 
  
  lambda_hat= -n_obs*n_groups/(sum(log(abs(data$y)))) # estimator of lambda 
  
  #         
  # 
  #Suppose lambda is estimated by lambda_hat then y' can be predicted as 
  #        y'_hat=h(y;lambda_hat)= sign(data$y) * abs(data$y) ^ lambda_hat
  
  data$y_prime_hat= sign(data$y) * abs(data$y) ^ lambda_hat
  
  # To select variables, we can apply M&L to y'_hat=x*beta+v_k +eps_kj (y'_hat may be used as response variable in Meza and Lahiri (M&L) )
  # use y'_hat and x as y and x in M&L
  
  
  
  #This function will help organize each group in a new dataframe to calculate easily the information of mean and also the new values calculated. It is used into the transform _data function below.
  split_data<-function(data) {
    # Create a list to store the split data frames
    split_data_list <- list()
    
    # Loop over the groups and split the data
    for (k in 1:n_groups) {
      # Select data for the i-th group and remove group column
      data_new <- data %>% dplyr::filter(group == k) %>% dplyr::select(-group)
      # Append the data frame to the list
      split_data_list[[k]] <- data_new
    }
    # Return the list of split data frames
    return(split_data_list)
  }
  
  transform_data <- function(data, n_groups, n_obs) {
    sigma_sq <- sigma_v_sq + sigma_e_sq
    rho <- sigma_v_sq / sigma_sq
    # Calculate alpha_i for all groups
    n_i<-n_groups*n_obs
    alpha_i <- sqrt((1 - rho) / (1 + (n_i- 1) * rho))
    
    # Split the data into groups
    split_data_list <- split_data(data)
    
    # Loop over the groups
    for (i in 1:n_groups) {
      # Select data for the i-th group and remove group column
      data_new <- split_data_list[[i]]
      
      # Calculate mean values for the i-th group
      mean_y <- mean(data_new$y_prime_hat)
      mean_x_1 <- mean(data_new$x1)
      mean_x_2 <- mean(data_new$x2)
      mean_x_3 <- mean(data_new$x3)
      mean_x_4 <- mean(data_new$x4)
      
      # Transform the data for the i-th group
      data_new$y_prime_hat <- data_new$y_prime_hat - alpha_i[i] * mean_y
      data_new$x1 <- data_new$x1 - alpha_i[i] * mean_x_1
      data_new$x2 <- data_new$x2 - alpha_i[i] * mean_x_2
      data_new$x3 <- data_new$x3 - alpha_i[i] * mean_x_3
      data_new$x4 <- data_new$x4 - alpha_i[i] * mean_x_4
      
      # Replace the data for the i-th group in the list
      split_data_list[[i]] <- data_new
    }
    # Combine the transformed data frames into a single data frame
    transformed_data <- bind_rows(split_data_list)
    return(transformed_data)
  }
  # 
  
  data<-transform_data(data, n_groups, n_obs*n_groups)
  
  # 
  # apply the above model transf y and x based on M&L and then follow the procedure of AIC
  
  
  
  
  # Create a vector of variable names
  vars <- c("x1", "x2", "x3", "x4")
  
  # Create all possible combinations of 1, 2, 3, and 4 variables
  combs_1 <- combn(vars, 1, simplify = FALSE)
  combs_2 <- combn(vars, 2, simplify = FALSE)
  combs_3 <- combn(vars, 3, simplify = FALSE)
  combs_4 <- combn(vars, 4, simplify = FALSE)
  
  # Combine all the combinations into a single vector
  combs <- c(combs_1,combs_2, combs_3, combs_4)
  
  # Create a list to store the AIC and BIC values and variables used
  aic_bic_list <- list()
  
  # Fit linear models with each combination of variables and store AIC, BIC, and variables used
  for (i in 1:length(combs)) {
    formula <- as.formula(paste("y_prime_hat ~", paste(combs[[i]], collapse = "+")))
    model <- lm(formula, data = data)
    aic_bic_list[[i]] <- list(aic = AIC(model), bic = BIC(model), variables = paste(combs[[i]], collapse = "+"))
  }
  
  # Convert the AIC and BIC values and variables to a data frame
  aic_bic_df <- data.frame(Model = 1:length(aic_bic_list), 
                           Variables = sapply(aic_bic_list, function(x) x$variables),
                           AIC = unlist(lapply(aic_bic_list, function(x) x$aic)),
                           BIC = unlist(lapply(aic_bic_list, function(x) x$bic)))
  #aic_bic_df
  
  # sort the data frame by AIC and print the top 3 models with minimum AIC and show information on variable used
  #head(aic_bic_df[order(aic_bic_df$AIC), ], 3)
  
  # sort the data frame by BIC and print the top 3 models with minimum BIC and show information on variable used
  #head(aic_bic_df[order(aic_bic_df$BIC), ], 3)
  
  # Create a vector of labels
  labels <- rep("", length(aic_bic_df))
  # Replace the corresponding labels with the combination names
  labels[1:length(combs)] <- lapply(combs, paste, collapse = "+")
  
  
  # sort the data frame by AIC and print the top 3 models with minimum AIC and show information on variable used
  sorted_DF_aic <- aic_bic_df[order(aic_bic_df$AIC), ]
  
  # sort the data frame by BIC and print the top 3 models with minimum BIC and show information on variable used
  sorted_DF_bic <- aic_bic_df[order(aic_bic_df$BIC), ]
  
  # print the tables with allmodels and top 3 models with minimum values of AIC and BIC
  return(list(All_Models = aic_bic_df, Top_Models_AIC= head(sorted_DF_aic, 3),Top_Models_BIC= head(sorted_DF_bic, 3)))
  
  
}

# organize the output forthe shiny app to make possible visualize the data generated
generate_data_1_1 <- function(sigma_e_sq, sigma_v_sq, n_groups, n_obs,beta) {
  
  set.seed(123)# set a seed for reproducibility x1,x2,x3,x4 will be the same for every run.
  x1 = runif(10*5)
  x2 = runif(50, 1, 2)
  x3 = rpois(50, lambda = 3)
  x4 = rexp(50, rate = 0.5)
  
  beta0 <- beta[1]
  beta1 <- beta[2]
  beta2 <- beta[3]
  beta3 <- beta[4]
  beta4 <- beta[5]
  
  # Generate data
  data <- data.frame(
    group = rep(1:5, each = 10),
    x1,
    x2,
    x3,
    x4,
    v_ii <- rnorm(n_groups, 0, sqrt(sigma_v_sq)),
    v_i = rep(v_ii, each = n_obs),
    e_ij = rnorm(n_groups * n_obs, sd = sqrt(sigma_e_sq))
  )
  # print(head(data,10)) # to support on validation of simulation x1,x2,x3,x4 are same for every run but v_ii, v_i, e_ij change in every run
  # Generate response variable y_star which satisfy the assumption based on Li paper
  
  for (i in 1:5) {
    data$y_prime[data$group == i] <- beta0 * 1 + 
      beta1 * data$x1[data$group == i] + 
      beta2 * data$x2[data$group == i] + 
      beta3 * data$x3[data$group == i] + 
      beta4 * data$x4[data$group == i] +
      data$v_i[data$group == i] + 
      data$e_ij[data$group == i]
  }
  
  # Transform to y using Eq. (3)
  
  lambda=0.3 # fixed based on Li simulation results and suggestions
  inv_lambda<- 1/lambda
  data$y<- sign(data$y_prime) * abs(data$y_prime) ^ inv_lambda  # these are the original values of y 
  # the above code is for generating data y and x only is not used for var selection or estimation
  
  
  # Given the data x and y only we now do var selection
  # the above data y and x are generated according to model 2 of Li et al, 
  # Meza and Lahiri method does not apply to model 2
  # If y'=h(y;lambda) is known then we can apply Meza and Lahiri on y'=x*beta+v_k +eps_kj, (y' may be used as response variable in Meza and Lahiri (M&L) )
  
  # Only y is observed, y' is not observed. To apply M&L we predict y_star using y by estimating lambda
  #         lambda_hat= - n/(sum(log(abs(data$y)))) # estimator of lambda 
  
  lambda_hat= -n_obs*n_groups/(sum(log(abs(data$y)))) # estimator of lambda 
  
  #         
  # 
  #Suppose lambda is estimated by lambda_hat then y' can be predicted as 
  #        y'_hat=h(y;lambda_hat)= sign(data$y) * abs(data$y) ^ lambda_hat
  
  data$y_prime_hat= sign(data$y) * abs(data$y) ^ lambda_hat
  
  # To select variables, we can apply M&L to y'_hat=x*beta+v_k +eps_kj (y'_hat may be used as response variable in Meza and Lahiri (M&L) )
  # use y'_hat and x as y and x in M&L
  
  
  
  #This function will help organize each group in a new dataframe to calculate easily the information of mean and also the new values calculated. It is used into the transform _data function below.
  split_data<-function(data) {
    # Create a list to store the split data frames
    split_data_list <- list()
    
    # Loop over the groups and split the data
    for (k in 1:n_groups) {
      # Select data for the i-th group and remove group column
      data_new <- data %>% dplyr::filter(group == k) %>% dplyr::select(-group)
      # Append the data frame to the list
      split_data_list[[k]] <- data_new
    }
    # Return the list of split data frames
    return(split_data_list)
  }
  
  transform_data <- function(data, n_groups, n_obs) {
    sigma_sq <- sigma_v_sq + sigma_e_sq
    rho <- sigma_v_sq / sigma_sq
    # Calculate alpha_i for all groups
    n_i<-n_groups*n_obs
    alpha_i <- sqrt((1 - rho) / (1 + (n_i- 1) * rho))
    
    # Split the data into groups
    split_data_list <- split_data(data)
    
    # Loop over the groups
    for (i in 1:n_groups) {
      # Select data for the i-th group and remove group column
      data_new <- split_data_list[[i]]
      
      # Calculate mean values for the i-th group
      mean_y <- mean(data_new$y_prime_hat)
      mean_x_1 <- mean(data_new$x1)
      mean_x_2 <- mean(data_new$x2)
      mean_x_3 <- mean(data_new$x3)
      mean_x_4 <- mean(data_new$x4)
      
      # Transform the data for the i-th group
      data_new$y_prime_hat <- data_new$y_prime_hat - alpha_i[i] * mean_y
      data_new$x1 <- data_new$x1 - alpha_i[i] * mean_x_1
      data_new$x2 <- data_new$x2 - alpha_i[i] * mean_x_2
      data_new$x3 <- data_new$x3 - alpha_i[i] * mean_x_3
      data_new$x4 <- data_new$x4 - alpha_i[i] * mean_x_4
      
      # Replace the data for the i-th group in the list
      split_data_list[[i]] <- data_new
    }
    # Combine the transformed data frames into a single data frame
    transformed_data <- bind_rows(split_data_list)
    return(transformed_data)
  }
  # 
  
  data<-transform_data(data, n_groups, n_obs*n_groups)
  
  print(data)
  # apply the above model transf y and x based on M&L and then follow the procedure of AIC
}


run_var_selection_1 <- function(n_runs, sigma_e_sq, sigma_v_sq, n_groups, n_obs,beta) {
  # create empty dataframes to store the results
  results_AIC <- data.frame(Variables = character(), AIC = numeric(), BIC = numeric())
  results_BIC <- data.frame(Variables = character(), AIC = numeric(), BIC = numeric())
  
  # run the function n times and store the first row of Top_Models_AIC and Top_Models_BIC in separate dataframes
  for (i in 1:n_runs) {
    output <- generate_data_1(sigma_e_sq, sigma_v_sq, n_groups, n_obs,beta)
    print(head(output))
    results_AIC <- rbind(results_AIC, output$Top_Models_AIC[1, c("Variables", "AIC", "BIC")])
    results_BIC <- rbind(results_BIC, output$Top_Models_BIC[1, c("Variables", "AIC", "BIC")])
  }
  
  # print the results dataframes
  print(results_AIC)
  print(results_BIC)
  
  # print the frequency of the top model selected , show variables and frequency in percentage  
  freq_AIC <- round(prop.table(table(results_AIC$Variables)) * 100, 2)
  freq_BIC <- round(prop.table(table(results_BIC$Variables)) * 100, 2)
  
  print(data.frame(Variables = names(freq_AIC), Frequency = as.numeric(freq_AIC)))
  print(data.frame(Variables = names(freq_BIC), Frequency = as.numeric(freq_BIC)))
  
  # create a dataframe from the frequency tables
  df_AIC <- data.frame(Variables = names(freq_AIC), Frequency = as.numeric(freq_AIC))
  df_BIC <- data.frame(Variables = names(freq_BIC), Frequency = as.numeric(freq_BIC))
  
  # create a bar chart using ggplot for the frequency table
  # create a bar chart using ggplot for the frequency table
  plot_AIC <- ggplot(df_AIC, aes(x = Variables, y = Frequency)) +
    geom_bar(stat = "identity", fill = "lightblue") +
    geom_text(aes(label = paste0(Frequency, "%"), y = Frequency + 0.5), 
              size = 3, color = "black") + 
    xlab("Top Models AIC") + ylab("Frequency (%)") +
    ggtitle("Frequency of Top Model AIC Selected") +
    theme(plot.title = element_text(hjust = 0.5))
  
  plot_BIC <- ggplot(df_BIC, aes(x = Variables, y = Frequency)) +
    geom_bar(stat = "identity", fill = "lightblue") +
    geom_text(aes(label = paste0(Frequency, "%"), y = Frequency + 0.5), 
              size = 3, color = "black") + 
    xlab("Top Models BIC") + ylab("Frequency (%)") +
    ggtitle("Frequency of Top Model BIC Selected") +
    theme(plot.title = element_text(hjust = 0.5))
  
  
  # print the bar chart
  print(plot_AIC)
  print(plot_BIC)
  
  # generate scatterplot
  generate_scatterplot <- function(results_AIC, results_BIC) {
    
    # create a dataframe with the frequency of each model combination in AIC
    freq_AIC <- round(prop.table(table(results_AIC$Variables)) * 100, 2)
    df_AIC <- data.frame(Variables = names(freq_AIC), Frequency = as.numeric(freq_AIC))
    
    # create a dataframe with the frequency of each model combination in BIC
    freq_BIC <- round(prop.table(table(results_BIC$Variables)) * 100, 2)
    df_BIC <- data.frame(Variables = names(freq_BIC), Frequency = as.numeric(freq_BIC))
    
   
    
    # Merge df_AIC and df_BIC by Variables
    df <- merge(df_AIC, df_BIC, by = "Variables")
    print(df)
    
    
    # create a scatterplot with different colors for the different number of variables in the model combinations
    plot <- ggplot(df, aes(x = Frequency.x, y = Frequency.y)) +
      geom_point(aes(color = factor(nchar(Variables) - 1)), size = 6) +
      geom_text(aes(label = Variables), hjust = 0, vjust = 0, check_overlap = TRUE, size = 3) +
      scale_color_manual(values = c("#DD70CC", "#009E73", "#56B4E9", "#FF7F50"), 
                         labels = c("One Variable", "Two Variables", "Three Variables", "Four Variables")) +
      xlab("Frequency in AIC") + ylab("Frequency in BIC") +
      ggtitle("Frequency of Variables in AIC and BIC") +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 12),
            legend.title = element_blank(),
            legend.position = "top")+
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "darkblue") # this line shows when AIC performs better than BIC 
    
    # print the scatterplot
    print(plot + xlim(-1, NA) + ylim(-1, NA))
    
    # print the frequency tables for AIC and BIC
    cat("\nFrequency of Model Combinations in AIC:\n")
    print(df_AIC)
    cat("\nFrequency of Model Combinations in BIC:\n")
    print(df_BIC)
    
  }
  
  
  generate_scatterplot(results_AIC, results_BIC)
  # return list of outputs
}
# function for shiny app use

run_var_selection_1_1 <- function(n_runs, sigma_e_sq, sigma_v_sq, n_groups, n_obs,beta) {
  # create empty dataframes to store the results
  results_AIC <- data.frame(Variables = character(), AIC = numeric(), BIC = numeric())
  results_BIC <- data.frame(Variables = character(), AIC = numeric(), BIC = numeric())
  
  # run the function n times and store the first row of Top_Models_AIC and Top_Models_BIC in separate dataframes
  for (i in 1:n_runs) {
    output <- generate_data_1(sigma_e_sq, sigma_v_sq, n_groups, n_obs,beta)
    #print(head(output))
    results_AIC <- rbind(results_AIC, output$Top_Models_AIC[1, c("Variables", "AIC", "BIC")])
    results_BIC <- rbind(results_BIC, output$Top_Models_BIC[1, c("Variables", "AIC", "BIC")])
  }
  
  # print the results dataframes
 # print(results_AIC)
 # print(results_BIC)
  
  # print the frequency of the top model selected , show variables and frequency in percentage  
  freq_AIC <- round(prop.table(table(results_AIC$Variables)) * 100, 2)
  freq_BIC <- round(prop.table(table(results_BIC$Variables)) * 100, 2)
  
  #print(data.frame(Variables = names(freq_AIC), Frequency = as.numeric(freq_AIC)))
 # print(data.frame(Variables = names(freq_BIC), Frequency = as.numeric(freq_BIC)))
  
  # create a dataframe from the frequency tables
  df_AIC <- data.frame(Variables = names(freq_AIC), Frequency_AIC_perc = as.numeric(freq_AIC))
 # df_BIC <- data.frame(Variables = names(freq_BIC), Frequency = as.numeric(freq_BIC))
  return(arrange(df_AIC, desc(Frequency_AIC_perc)))
}


run_var_selection_1_2 <- function(n_runs, sigma_e_sq, sigma_v_sq, n_groups, n_obs,beta) {
  # create empty dataframes to store the results
  results_AIC <- data.frame(Variables = character(), AIC = numeric(), BIC = numeric())
  results_BIC <- data.frame(Variables = character(), AIC = numeric(), BIC = numeric())
  
  # run the function n times and store the first row of Top_Models_AIC and Top_Models_BIC in separate dataframes
  for (i in 1:n_runs) {
    output <- generate_data_1(sigma_e_sq, sigma_v_sq, n_groups, n_obs,beta)
    #print(head(output))
    results_AIC <- rbind(results_AIC, output$Top_Models_AIC[1, c("Variables", "AIC", "BIC")])
    results_BIC <- rbind(results_BIC, output$Top_Models_BIC[1, c("Variables", "AIC", "BIC")])
  }
  
  # print the results dataframes
  # print(results_AIC)
  # print(results_BIC)
  
  # print the frequency of the top model selected , show variables and frequency in percentage  
  freq_AIC <- round(prop.table(table(results_AIC$Variables)) * 100, 2)
  freq_BIC <- round(prop.table(table(results_BIC$Variables)) * 100, 2)
  
  #print(data.frame(Variables = names(freq_AIC), Frequency = as.numeric(freq_AIC)))
  # print(data.frame(Variables = names(freq_BIC), Frequency = as.numeric(freq_BIC)))
  
  # create a dataframe from the frequency tables
  #df_AIC <- data.frame(Variables = names(freq_AIC), Frequency_AIC_perc = as.numeric(freq_AIC))
   df_BIC <- data.frame(Variables = names(freq_BIC), Frequency_BIC_perc = as.numeric(freq_BIC))
   return(arrange(df_BIC, desc(Frequency_BIC_perc)))
}




# new function for shiny app use fro AIC and BIC barcharts

run_var_selection_2 <- function(n_runs, sigma_e_sq, sigma_v_sq, n_groups, n_obs,beta) {
  # create empty dataframes to store the results
  results_AIC <- data.frame(Variables = character(), AIC = numeric(), BIC = numeric())
  results_BIC <- data.frame(Variables = character(), AIC = numeric(), BIC = numeric())
  
  # run the function n times and store the first row of Top_Models_AIC and Top_Models_BIC in separate dataframes
  for (i in 1:n_runs) {
    output <- generate_data_1(sigma_e_sq, sigma_v_sq, n_groups, n_obs,beta)
   # print(head(output))
    results_AIC <- rbind(results_AIC, output$Top_Models_AIC[1, c("Variables", "AIC", "BIC")])
    results_BIC <- rbind(results_BIC, output$Top_Models_BIC[1, c("Variables", "AIC", "BIC")])
  }
  
  # print the results dataframes
 # print(results_AIC)
 # print(results_BIC)
  
  # print the frequency of the top model selected , show variables and frequency in percentage  
  freq_AIC <- round(prop.table(table(results_AIC$Variables)) * 100, 2)
  freq_BIC <- round(prop.table(table(results_BIC$Variables)) * 100, 2)
  
 # print(data.frame(Variables = names(freq_AIC), Frequency = as.numeric(freq_AIC)))
 # print(data.frame(Variables = names(freq_BIC), Frequency = as.numeric(freq_BIC)))
  
  # create a dataframe from the frequency tables
  df_AIC <- data.frame(Variables = names(freq_AIC), Frequency = as.numeric(freq_AIC))
  df_BIC <- data.frame(Variables = names(freq_BIC), Frequency = as.numeric(freq_BIC))
  
  # create a bar chart using ggplot for the frequency table
  # create a bar chart using ggplot for the frequency table
  plot_AIC <- ggplot(df_AIC, aes(x = Variables, y = Frequency)) +
    geom_bar(stat = "identity", fill = "lightblue") +
    geom_text(aes(label = paste0(Frequency, "%"), y = Frequency + 0.5), 
              size = 5, color = "black") + 
    xlab("Top Models AIC") + ylab("Frequency (%)") +
    ggtitle("Frequency of Top Model AIC Selected") +
    theme(plot.title = element_text(hjust = 0.5))
  
  plot_BIC <- ggplot(df_BIC, aes(x = Variables, y = Frequency)) +
    geom_bar(stat = "identity", fill = "lightblue") +
    geom_text(aes(label = paste0(Frequency, "%"), y = Frequency + 0.5), 
              size = 5, color = "black") + 
    xlab("Top Models BIC") + ylab("Frequency (%)") +
    ggtitle("Frequency of Top Model BIC Selected") +
    theme(plot.title = element_text(hjust = 0.5))
  
  
  # print the bar chart
  # Save the plots with the desired dimensions
  ggsave("plot_AIC.png", plot = plot_AIC, width = 6, height = 12, units = "in")
  ggsave("plot_BIC.png", plot = plot_BIC, width = 6, height = 12, units = "in")
  
  # Return the plots arranged vertically
  return(grid.arrange(plot_AIC, plot_BIC, ncol = 1))
  }


# shiny app for scatterplot
run_var_selection_3 <- function(n_runs, sigma_e_sq, sigma_v_sq, n_groups, n_obs,beta) {
  # create empty dataframes to store the results
  results_AIC <- data.frame(Variables = character(), AIC = numeric(), BIC = numeric())
  results_BIC <- data.frame(Variables = character(), AIC = numeric(), BIC = numeric())
  
  # run the function n times and store the first row of Top_Models_AIC and Top_Models_BIC in separate dataframes
  for (i in 1:n_runs) {
    output <- generate_data_1(sigma_e_sq, sigma_v_sq, n_groups, n_obs,beta)
    #print(head(output))
    results_AIC <- rbind(results_AIC, output$Top_Models_AIC[1, c("Variables", "AIC", "BIC")])
    results_BIC <- rbind(results_BIC, output$Top_Models_BIC[1, c("Variables", "AIC", "BIC")])
  }
  
  # print the results dataframes
  #print(results_AIC)
  #print(results_BIC)
  
  # print the frequency of the top model selected , show variables and frequency in percentage  
  freq_AIC <- round(prop.table(table(results_AIC$Variables)) * 100, 2)
  freq_BIC <- round(prop.table(table(results_BIC$Variables)) * 100, 2)
  
  #print(data.frame(Variables = names(freq_AIC), Frequency = as.numeric(freq_AIC)))
  #print(data.frame(Variables = names(freq_BIC), Frequency = as.numeric(freq_BIC)))
  
  # create a dataframe from the frequency tables
  df_AIC <- data.frame(Variables = names(freq_AIC), Frequency = as.numeric(freq_AIC))
  df_BIC <- data.frame(Variables = names(freq_BIC), Frequency = as.numeric(freq_BIC))
  
  # create a bar chart using ggplot for the frequency table
  # create a bar chart using ggplot for the frequency table
  
  
  # generate scatterplot
  generate_scatterplot <- function(results_AIC, results_BIC) {
    
    # create a dataframe with the frequency of each model combination in AIC
    freq_AIC <- round(prop.table(table(results_AIC$Variables)) * 100, 2)
    df_AIC <- data.frame(Variables = names(freq_AIC), Frequency = as.numeric(freq_AIC))
    
    # create a dataframe with the frequency of each model combination in BIC
    freq_BIC <- round(prop.table(table(results_BIC$Variables)) * 100, 2)
    df_BIC <- data.frame(Variables = names(freq_BIC), Frequency = as.numeric(freq_BIC))
    
    
    
    # Merge df_AIC and df_BIC by Variables
    df <- merge(df_AIC, df_BIC, by = "Variables")
    #print(df)
    
    
    # create a scatterplot with different colors for the different number of variables in the model combinations
    plot <- ggplot(df, aes(x = Frequency.x, y = Frequency.y)) +
      geom_point(aes(color = factor(nchar(Variables) - 1)), size = 6) +
      geom_text(aes(label = Variables), hjust = 0, vjust = 0, check_overlap = TRUE, size = 5) +
      scale_color_manual(values = c("#DD70CC", "#009E73", "#56B4E9", "#FF7F50"), 
                         labels = c("One Variable", "Two Variables", "Three Variables", "Four Variables")) +
      xlab("Frequency in AIC") + ylab("Frequency in BIC") +
      ggtitle("Frequency of Variables in AIC and BIC") +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 12),
            legend.title = element_blank(),
            legend.position = "top")+
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "darkblue") # this line shows when AIC performs better than BIC 
    
    # print the scatterplot
    print(plot + xlim(-1, NA) + ylim(-1, NA))
    
    # print the frequency tables for AIC and BIC
   # cat("\nFrequency of Model Combinations in AIC:\n")
    #print(df_AIC)
    #cat("\nFrequency of Model Combinations in BIC:\n")
    #print(df_BIC)
    
  }
  
  
  generate_scatterplot(results_AIC, results_BIC)
  # return list of outputs
}

# running
run_var_selection_1(n_runs = 100, sigma_e_sq=1, sigma_v_sq=5, n_groups=5, n_obs=10,beta = c(1, 2,0,5,2))
