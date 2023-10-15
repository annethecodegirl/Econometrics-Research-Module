# Coding Exercises-- Estimation
library(tidyverse)
library(Hmisc)

set.seed(111)

# Part 1 - Set-up

mu <- 3
sigma <- 1.5
n <- seq(2,30,2)
R <- 10000

results_x <- matrix(nrow = R, ncol = length(n))
results_x1 <- results_x

# biased 
for (i in 1:R) {
  for(j in n) {
    x <- rnorm(j,mean=mu,sd=sigma)
    results_x[i,j/2] <- sqrt(1/j*sum((x-mean(x))^2))
  }
}

Mse <- function(x) { 1/R*sum((x - sigma)^2) }
Square_Bias<- function(x) { (1/R*sum(x) - sigma)^2 }
Variance <- function(x) { 1/R*sum( (x - 1/R*sum(x))^2 ) }

Mse_results <- apply(results_x,2,Mse)
Square_Bias_results <- apply(results_x,2,Square_Bias)
Variance_results <- apply(results_x,2,Variance)

#unbaised 
for (i in 1:R) {
  for(j in n) {
    x <- rnorm(j,mean=mu,sd=sigma)
    results_x1[i,j/2] <- sqrt(1/(j-1)*sum((x-mean(x))^2))
  }
}
Mse_results_x1 <- apply(results_x1,2,Mse)
Square_Bias_results_x1 <- apply(results_x1,2,Square_Bias)
Variance_results_x1 <- apply(results_x1,2,Variance)

# Presentation of the Simulation-Results

# Plot the results_x
plot(n,Mse_results)
plot(n,Square_Bias_results)
plot(n,Variance_results)

# PLot the results_x1
plot(n,Mse_results_x1)
plot(n,Square_Bias_results_x1)
plot(n,Variance_results_x1)


# Part 2- additional simulation

#  iv - Simulations a simple linear regression model to illistrate the bias ,variance and MSE of the OLS estimator.


# Generating a sample
n <- 50 # Number of observations
beta_1 <- 3 # Intercept
beta_2 <- -1
sigma  <- 2
x1 <- rep(1, n) 
x2 <- rnorm(n, mean=0, sd=sqrt(1.2))
X <- cbind(x1, x2)
eps <- rnorm(n, mean = 0, sd = sigma) # Generate realizations from the heteroscadastic error term
y <- beta_1 + beta_2 * x2 + eps # Linear regression model

# OLS estimate for beta_hat
A <- solve(t(X) %*% X)
beta_hat <- A%*% t(X) %*% y # Computation of the beta_Vector
beta_hat

# Another way for computation of the beta-vector:
myOLSFun <- function(y,x, add.intercept = FALSE){
  n <- length(y) 
  
  # Add an intercept to x
  if(add.intercept){
    Intercept <- rep(1,n)
    x<- cbind(Intercept,x)
  }
  
  # Estimation of the slope- parameter
  beta_hat_vec <- solve(t(x) %*% x) %*% t(x) %*% y
  
  # Return the result
  return(beta_hat_vec)
}

# Run the function
myOLSFun(y=y,x=X)


model <-  lm(y ~ x2)
coef(model)

# MSE for the sample
y_pred <- coef(model)[1] + coef(model)[2]*x2

MSE <- 1/i*sum((y-y_pred)^2)
MSE

plot(y = eps, x= x2,,main="Realization of the \nHeteroscedastic Error Term")
