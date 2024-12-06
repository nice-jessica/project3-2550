
library(ggplot2)
library(plotly)
library(gt)


# Set parameters
alpha <- 0.5   # True intercept
beta <- 1.5    # True treatment effect
gamma_sq <- 0.1  # Variance at the cluster level
sigma_sq <- 0.5  # Variance at the individual level
budget <- 10000  # Total budget for study
c1 <- 100       # Cost of first sample in a cluster
c2 <- 50        # Cost of subsequent samples in a cluster

# Simulate data under normal model
simulate_normal <- function(n_clusters, n_per_cluster, treatment_effect=TRUE) {
  Xi <- rep(0:1, length.out = n_clusters)
  Xi <- sample(Xi)  
  eps_i <- rnorm(n_clusters, 0, sqrt(gamma_sq)) 
  
  Y <- numeric(0)  
  group <- numeric(0) 
  for (i in 1:n_clusters) {
    mu_i <- alpha + beta * Xi[i] + eps_i[i]
    cluster_data <- rnorm(n_per_cluster[i], mu_i, sqrt(sigma_sq))
    Y <- c(Y, cluster_data)
    group <- c(group, rep(Xi[i], n_per_cluster[i]))
  }
  
  data.frame(Outcome = Y, Treatment = group)
}

calculate_cost <- function(n_clusters, n_per_cluster) {
  c1 * n_clusters + c2 * sum(n_per_cluster - 1)
}

# Function for simulation
run_simulation <- function(max_clusters, max_per_cluster) {
  results <- data.frame()
  for (n_clusters in 1:max_clusters) {
    for (n_obs in 1:max_per_cluster) {
      n_per_cluster <- rep(n_obs, n_clusters)
      total_cost <- calculate_cost(n_clusters, n_per_cluster)
      if (total_cost <= budget) {
        sim_data <- simulate_normal(n_clusters, n_per_cluster)
        est_beta <- mean(sim_data$Outcome[sim_data$Treatment == 1]) - mean(sim_data$Outcome[sim_data$Treatment == 0])
        mse <- (beta - est_beta)^2
        results <- rbind(results, data.frame(n_clusters, n_obs, est_beta, mse, total_cost))
      }
    }
  }
  results
}

simulation_results <- run_simulation(50, 20)
optimal_design <- simulation_results[which.min(simulation_results$mse), ]
print(optimal_design)

ggplot(simulation_results, aes(x = n_clusters, y = mse)) +
  geom_point(aes(color = factor(n_obs))) +
  labs(title = "MSE by Number of Clusters and Observations per Cluster",
       x = "Number of Clusters",
       y = "Mean Squared Error",
       color = "Observations per Cluster")


# Extending to Poisson Distribution
simulate_poisson <- function(n_clusters, n_per_cluster) {
  Xi <- rep(0:1, length.out = n_clusters)
  Xi <- sample(Xi)  
  lambda_i <- exp(alpha + beta * Xi + rnorm(n_clusters, 0, sqrt(gamma_sq)))  
  
  Y <- numeric(0)
  group <- numeric(0)
  for (i in 1:n_clusters) {
    cluster_data <- rpois(n_per_cluster[i], lambda_i[i])
    Y <- c(Y, cluster_data)
    group <- c(group, rep(Xi[i], n_per_cluster[i]))
  }
  
  data.frame(Outcome = Y, Treatment = group)
}

# Run simulation for Poisson distribution
run_simulation_poisson <- function(max_clusters, max_per_cluster) {
  results <- data.frame()
  for (n_clusters in 1:max_clusters) {
    for (n_obs in 1:max_per_cluster) {
      n_per_cluster <- rep(n_obs, n_clusters)
      if (calculate_cost(n_clusters, n_per_cluster) <= budget) {
        sim_data <- simulate_poisson(n_clusters, n_per_cluster)
        est_beta <- mean(sim_data$Outcome[sim_data$Treatment == 1]) - mean(sim_data$Outcome[sim_data$Treatment == 0])
        mse <- (beta - est_beta)^2
        results <- rbind(results, data.frame(n_clusters, n_obs, est_beta, mse))
      }
    }
  }
  results
}

poisson_results <- run_simulation_poisson(50, 20)
optimal_poisson_design <- poisson_results[which.min(poisson_results$mse), ]
print(optimal_poisson_design)

# Poisson results
ggplot(poisson_results, aes(x = n_clusters, y = mse)) +
  geom_point(aes(color = factor(n_obs))) +
  labs(title = "Poisson Distribution MSE by Number of Clusters and Observations",
       x = "Number of Clusters",
       y = "Mean Squared Error",
       color = "Observations per Cluster")

# Normal Distribution Results
p <- ggplot(simulation_results, aes(x = n_clusters, y = mse, color = factor(n_obs))) +
  geom_point() +
  labs(title = "MSE by Number of Clusters and Observations per Cluster",
       x = "Number of Clusters",
       y = "Mean Squared Error",
       color = "Observations per Cluster")

p_plotly <- ggplotly(p)
p_plotly

# Poisson Distribution Results
p_poisson <- ggplot(poisson_results, aes(x = n_clusters, y = mse, color = factor(n_obs))) +
  geom_point() +
  labs(title = "Poisson Distribution MSE by Number of Clusters and Observations",
       x = "Number of Clusters",
       y = "Mean Squared Error",
       color = "Observations per Cluster")

p_poisson_plotly <- ggplotly(p_poisson)
p_poisson_plotly

optimal_design_table <- rbind(optimal_design, optimal_poisson_design)
optimal_design_table$Distribution <- c("Normal", "Poisson")

gt_table <- gt(optimal_design_table) %>%
  tab_header(
    title = "Optimal Design Summary for Normal and Poisson Distributions"
  ) %>%
  cols_label(
    n_clusters = "Number of Clusters",
    n_obs = "Observations per Cluster",
    est_beta = "Estimated Beta",
    mse = "Mean Squared Error",
    Distribution = "Distribution"
  ) %>%
  fmt_number(
    columns = vars(mse, est_beta),
    decimals = 2
  ) %>%
  tab_options(
    table.font.size = px(12),
    heading.background.color = "gray",
    data_row.padding = px(5)
  )


library(gt)
library(dplyr)

enhanced_gt_table <- simulation_results %>%
  mutate(Distribution = ifelse(sim_data == "simulate_normal", "Normal", "Poisson")) %>%
  gt() %>%
  tab_header(title = "Detailed Simulation Results") %>%
  data_color(columns = vars(mse), colors = scales::col_numeric(palette = c("red", "green"), domain = NULL)) %>%
  fmt_number(columns = vars(mse, est_beta), decimals = 3) %>%
  tab_options(table.font.size = px(10), heading.background.color = "gray")

print(enhanced_gt_table)


cost_ratios <- seq(1, 2, by = 0.2) 
cost_ratio_results <- data.frame()

for (ratio in cost_ratios) {
  c1 <- 100 
  c2 <- c1 / ratio  
  budget <- 10000  
  simulation_results <- run_simulation(10, 20)  
  simulation_results$ratio <- ratio
  cost_ratio_results <- rbind(cost_ratio_results, simulation_results)
}

# Plot result
library(ggplot2)
ggplot(cost_ratio_results, aes(x = ratio, y = mse, color = factor(n_clusters))) +
  geom_point() +
  geom_line(aes(group = factor(n_clusters))) +
  labs(title = "Impact of Cost Ratios (c1/c2) on MSE",
       x = "Cost Ratio (c1/c2)",
       y = "Mean Squared Error",
       color = "Number of Clusters") +
  theme_minimal()




# Power analysis of Aim 1 and 2 ------------------------------------------------------------

library(lme4)      
library(tidyverse)  
library(parallel)   
library(gridExtra)  

# hierarchical normal data
create_hierarchical_data <- function(num_clusters, obs_per_cluster, intercept = 0, slope = 1, var_cluster = 1, var_residual = 1) {
  treatment <- rep(c(0, 1), length.out = num_clusters)
  random_effect <- rnorm(num_clusters, 0, var_cluster)
  cluster_mean <- intercept + slope * treatment + random_effect
  
  data <- data.frame(
    cluster_id = rep(1:num_clusters, each = obs_per_cluster),
    treatment_group = rep(treatment, each = obs_per_cluster),
    cluster_mean = rep(cluster_mean, each = obs_per_cluster)
  )
  
  data$response <- rnorm(num_clusters * obs_per_cluster, data$cluster_mean, var_residual)
  return(data)
}

# compute total cost
compute_total_cost <- function(num_clusters, obs_per_cluster, base_cost, additional_cost) {
  num_clusters * (base_cost + (obs_per_cluster - 1) * additional_cost)
}

# linear mixed model
fit_linear_model <- function(data) {
  model <- lmer(response ~ treatment_group + (1 | cluster_id), data = data)
  fixef(model)["treatment_group"]
}

# Evaluate design performance
evaluate_design_performance <- function(num_clusters, obs_per_cluster, base_cost, additional_cost, budget_limit, simulations = 100) {
  total_cost <- compute_total_cost(num_clusters, obs_per_cluster, base_cost, additional_cost)
  if (total_cost > budget_limit) return(NULL)
  
  simulation_results <- replicate(simulations, {
    simulated_data <- create_hierarchical_data(num_clusters, obs_per_cluster)
    fit_linear_model(simulated_data)
  })
  
  true_slope <- 1
  mse <- mean((simulation_results - true_slope)^2)
  bias <- mean(simulation_results) - true_slope
  power <- mean(abs(simulation_results / sd(simulation_results)) > qnorm(0.975))
  
  data.frame(
    num_clusters = num_clusters,
    obs_per_cluster = obs_per_cluster,
    cost_ratio = base_cost / additional_cost,
    total_cost = total_cost,
    mse = mse,
    bias = bias,
    power = power
  )
}

# Simulation settings
budget_limit <- 2000
cost_ratios <- c(2, 5, 10)
cluster_range <- seq(10, 50, by = 5)
obs_range <- seq(2, 20, by = 2)

simulation_results <- list()

for (cost_ratio in cost_ratios) {
  base_cost <- cost_ratio * 10
  additional_cost <- 10
  
  design_grid <- expand.grid(num_clusters = cluster_range, obs_per_cluster = obs_range)
  
  results <- mcmapply(
    function(cluster, obs) evaluate_design_performance(cluster, obs, base_cost, additional_cost, budget_limit),
    design_grid$num_clusters, design_grid$obs_per_cluster,
    mc.cores = parallel::detectCores() - 1, SIMPLIFY = FALSE
  )
  
  combined_results <- do.call(rbind, results[!sapply(results, is.null)])
  combined_results$cost_ratio <- cost_ratio
  simulation_results[[as.character(cost_ratio)]] <- combined_results
}

all_results <- bind_rows(simulation_results)

power_plot <- ggplot(all_results, aes(x = num_clusters, y = obs_per_cluster, fill = power)) +
  geom_tile() +
  facet_wrap(~cost_ratio) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Power by Design and Cost Ratio",
       x = "Number of Clusters", y = "Observations per Cluster")

print(power_plot)

mse_plot <- ggplot(all_results, aes(x = num_clusters, y = obs_per_cluster, fill = mse)) +
  geom_tile() +
  facet_wrap(~cost_ratio) +
  scale_fill_gradient(low = "green", high = "yellow") +
  labs(title = "MSE by Design and Cost Ratio",
       x = "Number of Clusters", y = "Observations per Cluster")

optimal_designs <- all_results %>%
  group_by(cost_ratio) %>%
  slice_max(power, n = 1)

print(optimal_designs)

grid.arrange(power_plot, mse_plot, ncol = 1)


#####AIM 3######
# hierarchical Poisson data
create_poisson_data <- function(num_clusters, obs_per_cluster, intercept = 0, slope = 0.5, var_cluster = 0.3) {
  treatment <- rep(c(0, 1), length.out = num_clusters)
  log_mu <- rnorm(num_clusters, intercept + slope * treatment, var_cluster)
  mu <- exp(log_mu)
  
  data <- data.frame(
    cluster_id = 1:num_clusters,
    treatment_group = treatment,
    response = rpois(num_clusters, obs_per_cluster * mu),
    offset_value = log(obs_per_cluster)
  )
  
  return(data)
}

# Poisson mixed model 
fit_poisson_model <- function(data) {
  model <- glmer(response ~ treatment_group + offset(offset_value) + (1 | cluster_id),
                 family = poisson, data = data)
  fixef(model)["treatment_group"]
}

# Evaluate design performance for Poisson
evaluate_poisson_performance <- function(num_clusters, obs_per_cluster, base_cost, additional_cost, budget_limit, simulations = 100) {
  total_cost <- compute_total_cost(num_clusters, obs_per_cluster, base_cost, additional_cost)
  if (total_cost > budget_limit) return(NULL)
  
  simulation_results <- replicate(simulations, {
    tryCatch({
      simulated_data <- create_poisson_data(num_clusters, obs_per_cluster)
      fit_poisson_model(simulated_data)
    }, error = function(e) NA)
  })
  
  simulation_results <- simulation_results[!is.na(simulation_results)]
  if (length(simulation_results) < simulations / 2) return(NULL)
  
  true_slope <- 0.5
  mse <- mean((simulation_results - true_slope)^2)
  bias <- mean(simulation_results) - true_slope
  power <- mean(abs(simulation_results / sd(simulation_results)) > qnorm(0.975))
  
  data.frame(
    num_clusters = num_clusters,
    obs_per_cluster = obs_per_cluster,
    cost_ratio = base_cost / additional_cost,
    total_cost = total_cost,
    mse = mse,
    bias = bias,
    power = power
  )
}

# Simulation settings for Poisson
budget_limit <- 1500
poisson_results <- list()

for (cost_ratio in cost_ratios) {
  base_cost <- cost_ratio * 10
  additional_cost <- 10
  
  design_grid <- expand.grid(num_clusters = cluster_range, obs_per_cluster = obs_range)
  
  results <- mcmapply(
    function(cluster, obs) evaluate_poisson_performance(cluster, obs, base_cost, additional_cost, budget_limit),
    design_grid$num_clusters, design_grid$obs_per_cluster,
    mc.cores = parallel::detectCores() - 1, SIMPLIFY = FALSE
  )
  
  combined_results <- do.call(rbind, results[!sapply(results, is.null)])
  combined_results$cost_ratio <- cost_ratio
  poisson_results[[as.character(cost_ratio)]] <- combined_results
}

all_poisson_results <- bind_rows(poisson_results)

# Create heatmap for Poisson
power_plot_poisson <- ggplot(all_poisson_results, aes(x = num_clusters, y = obs_per_cluster, fill = power)) +
  geom_tile() +
  facet_wrap(~cost_ratio) +
  scale_fill_gradient(low = "green", high = "purple") +
  labs(title = "Power by Design and Cost Ratio (Poisson)",
       x = "Number of Clusters", y = "Observations per Cluster")

print(power_plot_poisson)

combined_plot <- grid.arrange(
  power_plot_poisson, 
  power_plot, 
  ncol = 1,  
  top = "Comparison of Power Across Possion and Normal Distributions"
)

print(combined_plot)

