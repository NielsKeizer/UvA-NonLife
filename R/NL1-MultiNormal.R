# Dit script bevat de uitwerkingen van Assignment 1A

# Aanwezige data verwijderen
rm(list=ls(all=TRUE))

# start code
set.seed(1); nor <- qnorm(runif(5))
set.seed(1); nor1 <- rnorm(3)
nor; nor1; nor[c(1,3,5)] - nor1

# Q1
set.seed(1)
sum(duplicated(runif(1e6))) ## = 120
sum(duplicated(rnorm(1e8))) ## = 0

# Check if the outcome is consistent
m <- 2^32;n <- 1e6
f <- 1 - 1/m
num_dup_unif <- n - (1-f^n)/(1-f)
num_dup_unif ## = 116.4 - The outcome is consistent

n_norm <- 1e8
m_norm <- c(1e15,1e16,1e17,1e18)
num_dup_norm <- n_norm^2/(2*m_norm)
num_dup_norm ## = (5, 0.5, 0.05, 0.005)
# The outcome is consistent with a resolution of 10^17 or higher.

#TODO - Derivation for formula when using binomial expansion