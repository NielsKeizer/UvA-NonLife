# Dit script bevat de uitwerkingen van Assignment 1A

# Aanwezige data verwijderen
rm(list=ls(all=TRUE))

# start code
set.seed(1); nor <- qnorm(runif(5))
set.seed(1); nor1 <- rnorm(3)
nor; nor1; nor[c(1,3,5)] - nor1

# ---- Q1 ----

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

#Done - Derivation for formula when using binomial expansion

# ---- Example: Brownian Motion ----
rm(list=ls(all=TRUE))

par(mfrow=c(1,2),lwd=2,bty="n")
set.seed(3); y <- c(0,cumsum(rnorm(800)))/10
plot(exp(y), col = "blue", type="l", ylab="", lwd=2, main="Geometric Brownian Motion")
set.seed(4); y <- c(0,cumsum(rnorm(800)))/10
lines(exp(y), col="forestgreen", lwd="2")
set.seed(5); y <- c(0,cumsum(rnorm(800)))/10
lines(exp(y), col="red")

n <- 50; set.seed(9)
x <- cumsum(c(0,rnorm(n))); y <- cumsum(c(0,rnorm(n)))
plot(x, y, type="n", xlab="", ylab="", main="2-dimensional Brownian Motion")
arrows(x[1:n], y[1:n], x[1+(1:n)], y[1+(1:n)], col=rainbow(n), lwd=2, length=.08)
points(0,0,col="red",cex=1.5,lwd=3)
points(x[n+1], y[n+1], col="red", cex=1.5,lwd=3)

# ---- Q2 ----
# The probability of moving from state i to stat i+1 is equal to p.
# The probability of moving from state i to stat i-1 is equal to 1-p.
# The probability of moving to any other state is equal to 0.
rm(list=ls(all=TRUE))

n <- 200; p <- 0.52
x <- c(0,cumsum(2*rbinom(n,1,p)-1))
plot(x, type="l", lwd=1, ylab="state", xlab="step", main="1-dimensional random walk")

# ---- Q3 ----
rm(list=ls(all=TRUE))

set.seed(2004); options(digits=2)
X <- rnorm(1000); Y <- rnorm(1000)

# Var[Y*] = 1 implies that a^2 + b^2 = 1
# r(X,Y*) = 0.8 implies that a = 0.8
# TODO: Uitwerken in LaTeX

a <- .8; b <- sqrt(1 - a^2); Y <- a*X + b*Y

# ---- Q4 ----
cov_matrix <- matrix(c(1,a,a,1), nrow=2, ncol=2)

# ---- Q5 ----
cov_matrix
t(chol(cov_matrix))

# ---- Q6 ----
c(mean(X), var(X), mean(Y), var(Y), cor(X,Y)) # Gemiddelden liggen bij 0, varianties bij 1, correlatie bij 0.8. Lijkt te kloppen

par(mfrow=c(1,2))
plot(X,Y, pch="*")
d <- -2.2
abline(v=d, col="red")

bad <- (X < d)
plot(X[bad], Y[bad], ylim=range(Y))
abline(v=d, col="red")
cor(X[bad],Y[bad])

# ---- Q7 ----
# DONE: 'Show that' Uitwerken in LaTex. 

chi5 <- sqrt(rchisq(1000, df=5)/5)
X <- X/chi5; Y <- Y/chi5 

# ---- Q8 ----
c(mean(X), var(X), mean(Y), var(Y), cor(X,Y))

# DONE: Show that enz. laten zien in LaTeX

# ---- Q9 ----
par(mfrow=c(1,2))
plot(X,Y, pch="*")
d <- -2.2
abline(v=d, col="red")

bad <- (X < d)
plot(X[bad], Y[bad], ylim=range(Y))
abline(v=d, col="red")
cor(X[bad],Y[bad])

# ---- Q10 ----
rm(list=ls(all=TRUE))
library(MASS)
mu <- c(1,3,5); sig2 <- c(1,2,5)
Corrmat <- rbind(c(1., .3, .3),
                 c(.3, 1., .4),
                 c(.3, .4, 1.))
Varmat <- Corrmat * sqrt(sig2 %*% t(sig2))
Z <- mvrnorm(100, mu, Varmat)
options(digits=7)
colMeans(Z); diag(cov(Z)); cor(Z)

# TODO: Find out what he means byverifying something he calculated himself.

#colMeans(Z) estimates the mean of each column. Therefore colMeans should be close to the mean vector mu.
#diag(cov(Z)) gives the diagonal elements of the covariance matrix. The diagonal elements of the covariance matrix contains the estimated variance of each element of Z. This should be close to sig2.
#cor(z) contains the estimated correlation between the three elements of Z.

# ---- Q11 ----
no_sims = 1e6
VaR <- rep(0,10)
for (i in (1:10)){
  Z <- mvrnorm(no_sims, mu, Varmat)
  VaR[i] <- quantile(rowSums(Z),0.9999)
}
VaR
c(mean(VaR),sd(VaR))
# 22.3 appears to be a good estimate

Var_t <- qnorm(0.9999, sum(mu), sqrt(sum(Varmat)))
Var_t
# ---- Q12 ----
rm(list=ls(all=TRUE))

n <- 1e6
mu <- c(0,0,0)
sigma <- rbind(c(1,1/6,1/6),
               c(1/6,1,1/6),
               c(1/6,1/6,1))
Z <- mvrnorm(n, mu,sigma)

# ---- Q13 ----
V <- rowSums(Z)

# ---- Q14 ----
d <- quantile(V,0.975)
d

# ---- Q15 ----
stoploss_premium <- mean(pmax(V-d,0)) 
stoploss_premium

# ---- Q16 ----
# DONE: In LaTeX uitwerken dat de resulterende verdeling ook normaal is, vervolgens mu en sd bepalen.
mean <- sum(mu); sd <- sqrt(sum(sigma))
c(mean, sd)

# ---- Q17 ----
# Uit vraag 16 blijkt dat mu = 0 en sigma = 2.
d_new <- qnorm(0.975, mean, sd)
stoploss_premium_mart <- sd * dnorm((d_new - mean)/sd) - (d_new - mean)*(1 - pnorm((d_new - mean)/sd))
stoploss_premium_mart

# ---- Q18 ----
rm(list=ls(all=TRUE))

n <- 1e6
k <- 5
mu <- c(0,0,0)
sigma <- rbind(c(1,1/6,1/6),
               c(1/6,1,1/6),
               c(1/6,1/6,1))
Z <- mvrnorm(n, mu,sigma)
chi5 <- sqrt(rchisq(n, df=5)/5)
Z_prime <- Z/chi5
V_prime <- rowSums(Z_prime)
d <- quantile(V_prime,0.975)
d
stoploss_premium <- mean(pmax(V_prime-d,0)) 
stoploss_premium

# ---- Q19 ----
#DONE: Nog helemaal uitwerken.
sd <- sqrt(sum(sigma))
d_new <- sd * qt(0.975,5) 
f <- function(x) {1 - pt(x/sd,5)}
ES <- integrate(f, d_new, Inf)
ES$value
