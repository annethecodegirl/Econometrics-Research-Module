
#Excercise 1

rm(list=ls())

# Set the seed for reproducibility

set.seed(111)

# Initialize vectors to store the simulated means
theta1_hat_val <- numeric(10)
theta2_hat_val <- numeric(10)

# Simulate the process 10 times
for (i in 1:10) {
  # Generate a random sample of size two from a uniform distribution
  U <- runif(2, min = 0, max = 1)
  
  # Calculate theta1_hat and theta2_hat
  theta1_hat <- max(U)
  mean_sample <- mean(U)
  theta2_hat <- 2 * mean_sample
  
  # Store the calculated values in the vectors
  theta1_hat_val[i] <- theta1_hat
  theta2_hat_val[i] <- theta2_hat
}

# Calculate the mean of all theta1_hat and theta2_hat values
mean_theta1_hat <- mean(theta1_hat_val)
mean_theta2_hat <- mean(theta2_hat_val)


# Plot histograms for the means of theta1_hat and theta2_hat
par(mfrow = c(2, 1))  # Set up a 2x1 plot layout
hist(theta1_hat_val, main = "Histogram of theta1_hat Means", xlab = "Mean Values")
hist(theta2_hat_val, main = "Histogram of theta2_hat Means", xlab = "Mean Values")





#Exercise 2

# Function to simulate and calculate MSE for different sample sizes
simulate_mse <- function(n, S) { 
  # Set the seed for reproducibility
  set.seed(111)

  
  # Initialize vectors to store the quadratic loss
  loss_theta1_hat <- numeric(S)
  loss_theta2_hat <- numeric(S)

  
  # True value of theta
  true_theta <- 2

  
  for (i in 1:S) {
    # Generate a random sample of the specified size from a uniform distribution
    U <- runif(n, min = 0, max = 1)

    # Calculate theta1_hat and theta2_hat
    theta1_hat <- max(U)
    mean_sample <- mean(U)
    theta2_hat <- 2 * mean_sample

    # Calculate the quadratic loss function
    loss_theta1_hat[i] <- (theta1_hat - true_theta)^2
    loss_theta2_hat[i] <- (theta2_hat - true_theta)^2
  }
  # Calculate the mean squared error for theta1_hat and theta2_hat
  mse_theta1_hat <- mean(loss_theta1_hat)
  mse_theta2_hat <- mean(loss_theta2_hat)
  
  # Return the mean squared error values
  return(list(mse_theta1_hat, mse_theta2_hat))
}

# Specify the sample sizes and number of simulations
sample_sizes <- c(2, 100, 1000)
S <- 10000

# Create an empty list to store MSE results
mse_results_list <- list()

# Simulate and calculate MSE for each sample size
for (n in sample_sizes) {
  mse_results <- simulate_mse(n, S)
  mse_results_list[[as.character(n)]] <- mse_results
}

# Plot histograms of MSE for each sample size
par(mfrow = c(3, 1))  # Set up a 3x1 plot layout
for (i in 1:length(sample_sizes)) {
  n <- sample_sizes[i]
  mse_results <- mse_results_list[[as.character(n)]]
  
  hist(mse_results[[1]], main = paste("MSE for theta1_hat (Sample Size =", n, ")"), xlab = "MSE Values", col = "lightblue")
  hist(mse_results[[2]], main = paste("MSE for theta2_hat (Sample Size =", n, ")"), xlab = "MSE Values", col = "lightblue")
}

