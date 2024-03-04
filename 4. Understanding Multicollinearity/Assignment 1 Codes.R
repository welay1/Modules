rm(list = ls())
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/UofT Admin and TA/2022-23/STA302 S/Assignment 1")
library(MASS)
library(car)

## Simulation for correlated predictors ##
set.seed(1002656486)

nsample <- 50; nsim <- 100
sig2 <- rchisq(1, df = 1) ## The true error variance
bet <- c(rnorm(3, 0, 1), 0) ## 4 values of beta that is beta0, beta1, beta2, beta3 = 0
muvec <- rnorm(3, 0, 1)
sigmat <- diag(rchisq(3, df = 4))
X <- mvrnorm(nsample, mu = muvec, Sigma = sigmat); cor(X[,1], X[,2])
Xmat <- cbind(1, X)

## Simulate the response ##
bets <- matrix(NA, ncol = length(bet), nrow = nsim)
bets.biased <- matrix(NA, ncol = length(bet) - 1, nrow = nsim)
for(i in 1:nsim){
Y <- Xmat%*%bet + rnorm(nsample)
model1 <- lm(Y ~ X[,1])
model2 <- lm(Y ~ X[,2])
model3 <- lm(Y ~ X[,3])
### Store the biased estimates of the betas ##
bets.biased[i,] <- c(coef(model1)[2], coef(model2)[2], coef(model3)[2])
model <- lm(Y ~ X)
bets[i,] <- coef(model)
}

### Calculate the means of the slope estimates ##

## Beta 1
paste0("True Value: ", bet[2])
paste0("Mean of beta1 estimates from MLR: ", colMeans(bets)[2])
paste0("Mean of beta1 estimates from SLR: ", colMeans(bets.biased)[1])

## Beta 2
paste0("True Value: ", bet[3])
paste0("Mean of beta1 estimates from MLR: ", colMeans(bets)[3])
paste0("Mean of beta1 estimates from SLR: ", colMeans(bets.biased)[2])

## Beta 3
paste0("True Value: ", bet[4])
paste0("Mean of beta1 estimates from MLR: ", colMeans(bets)[4])
paste0("Mean of beta1 estimates from SLR: ", colMeans(bets.biased)[3])

#### Change the number of simulations to 1000, 10000 and 100000 to see how the bias changes


## Change sigmat (correlated predictors) ##
## The correlation between X1 and X2 ##
r12 <- 0.8
sigmat[1,2] <- sigmat[2,1] <- r12*sqrt(sigmat[1,1])*sqrt(sigmat[2,2])
## Simulation for Categorical Variables with Interaction ##
set.seed(1002656486)
X <- mvrnorm(nsample, mu = muvec, Sigma = sigmat); cor(X[,1], X[,2])
Xmat <- cbind(1, X)

## Simulate the response ##
bets <- matrix(NA, ncol = length(bet), nrow = nsim)
for(i in 1:nsim){
  Y <- Xmat%*%bet + rnorm(nsample, 0, sig2)
  model11 <- lm(Y ~ X[,1])
  model <- lm(Y ~ X)
  bets[i,] <- coef(model)
}

#### Calculate the means and the variances of the estimates ###
colMeans(bets)[2:4]
apply(bets, 2, var)[2:4]

### True variance
variance.mat <- sig2 * solve(t(Xmat)%*%Xmat)
diag(variance.mat)

### Keep Increasing r12 values to see what eventually happens to the means and variances of the estimates


### Correlation between X1 amd X3
r13 <- 0.8
sigmat <- diag(rchisq(3, df = 4))
sigmat[1,3] <- sigmat[3,1] <- r13*sqrt(sigmat[1,1])*sqrt(sigmat[3,3])
## Simulation for Categorical Variables with Interaction ##
set.seed(1002656486)
X <- mvrnorm(nsample, mu = muvec, Sigma = sigmat); cor(X[,1], X[,3])
Xmat <- cbind(1, X)

## Simulate the response ##
bets <- matrix(NA, ncol = length(bet), nrow = nsim)
bet3 <- numeric(nsim)
for(i in 1:nsim){
  Y <- Xmat%*%bet + rnorm(nsample, 0, sig2)
  ## Fit a simple linear regression model just with X3
  model3 <- lm(Y ~ X[,3])
  model <- lm(Y ~ X)
  bets[i,] <- coef(model)
  bet3[i] <- coef(model3)[2]
}

#### Calculate the means and the variances of the estimates ###
colMeans(bets)[2:4]
apply(bets, 2, var)[2:4]

### True variance
variance.mat <- sig2 * solve(t(Xmat)%*%Xmat)
diag(variance.mat)


### Calculate the means and variances of the beta3 estimates
mean(bet3); var(bet3)