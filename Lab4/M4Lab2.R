# Module 4 

# Multicollinearity 

library(MASS)
library(ggplot2)

?mvrnorm
beta0 <- 0.5
beta12 <- matrix(c(0.3, 0.7))
n <- 75

# Correlated ============================================================
mu1 <- matrix(c(0,0)) 
sigma1 <- matrix(c(1, 0.9, 0.9, 1), ncol = 2) # Covariance Matrix; Highly correlated

# "for loop" version =================
nsim <- 1000
coef1 <- matrix(nrow = nsim, ncol = 3)
colnames(coef1) <- c("beta1_0", "beta1_1", "beta1_2")
for(i in 1:nsim){
  X1 <- mvrnorm(n = n, mu = mu1, Sigma = sigma1) # MVNormal data
  Y1 <- beta0 + X1 %*% beta12 + rnorm(n, 0, 1) # Response variables
  fit1 <- lm(Y1 ~ X1) # Fit the model
  coef1[i,] <- fit1$coefficients # store coefficients
}

# replicate() version ============

beta0 <- 0.5 # Define beta_0
beta12 <- matrix(c(0.3, 0.7)) # define beta_1, beta_2
mu1 <- matrix(c(0,0)) # Set means
sigma1 <- matrix(c(1, 0.9, 0.9, 1), ncol = 2) # Covariance - correlated

fitmodel1 <- function(n, beta){ # Beta = 1, 2, 3 - specify coefficient
  X1 <- mvrnorm(n = n, mu = mu1, Sigma = sigma1) # Generate data
  Y1 <- beta0 + X1 %*% beta12 + rnorm(n, 0, 1) # Generate/calculate response
  fit1 <- lm(Y1 ~ X1) # Fit the model
  fit1$coefficients[beta] # Return specified coef - "beta" argument
}

replicate(1000, fitmodel1(75, 1)) # simulate


# Uncorrelated ========================================================
mu2 <- matrix(c(0, 0))
sigma2 <- matrix(c(1, 0, 0, 1), ncol = 2) # Uncorrelated

# "for loop" version =================
nsim <- 1000
coef2 <- matrix(nrow = nsim, ncol = 3)
colnames(coef2) <- c("beta2_0", "beta2_1", "beta2_2")
for(i in 1:nsim){
  X2 <- mvrnorm(n = n, mu = mu2, Sigma = sigma2) # MVNormal data
  Y2 <- beta0 + X2 %*% beta12 + rnorm(n, 0, 1) # Response variables
  fit2 <- lm(Y2 ~ X2) # Fit the model
  coef2[i,] <- fit2$coefficients # store coefficients
}

# replicate() version ============

beta0 <- 0.5 # Define beta_0
beta12 <- matrix(c(0.3, 0.7)) # define beta_1, beta_2
mu1 <- matrix(c(0,0)) # Set means
sigma2 <- matrix(c(1, 0, 0, 1), ncol = 2) # Covariance - uncorrelated

fitmodel2 <- function(n, beta){ # Beta = 1, 2, 3 - specify coefficient
  X2 <- mvrnorm(n = n, mu = mu1, Sigma = sigma2) # Generate data
  Y2 <- beta0 + X1 %*% beta12 + rnorm(n, 0, 1) # Generate/calculate response
  fit2 <- lm(Y2 ~ X2) # Fit the model
  fit2$coefficients[beta] # Return specified coef - "beta" argument
}

replicate(1000, fitmodel2(75, 1)) # simulate

# Results - Stacked Histograms ==================
library(reshape)
coefs <- rbind(melt(coef1), melt(coef2))
colnames(coefs) <- c("Index", "Param", "Estimate")
qplot(Estimate, data = coefs) + facet_wrap(~ Param)




