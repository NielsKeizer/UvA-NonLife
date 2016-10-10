# Aanwezige data verwijderen
rm(list=ls(all=TRUE))

# Poisson GLM
Xij <-       c(232,106,35,16,2,258,115,56,27,221,82,4,359,71,349)
i <- as.factor(c(1  ,1 ,1 ,1,1,  2,  2, 2, 2,  3, 3,3,  4, 4,  5))
j <- as.factor(c(1  ,2 ,3 ,4,5,  1,  2, 3, 4,  1, 2,3,  1, 2,  1))
CL <- glm(Xij ~ i+j, poisson)
coefs <- exp(coef(CL)) ## exponents of parameters estimates
TT <- 5; alpha.CL <- coefs[1] * c(1, coefs[2:TT])
beta.CL <- c(1, coefs[(TT+1):(2*TT-1)])

xtabs(Xij~i+j)

alpha.CL <- alpha.CL* sum(beta.CL)
beta.CL <- beta.CL / sum(beta.CL)

alpha.CL ; beta.CL

# Verbeek's algorithm

Ri <- tapply(Xij, i ,sum); Cj <- tapply(Xij, j, sum)
alpha <- beta <- rep(0,TT)
alpha.sum <- alpha[1] <- Ri[1]
beta.sum <- beta[TT] <- Cj[TT]/Ri[1]
for (k in 2:TT) {
  alpha.sum <- alpha.sum + (alpha[k] <- Ri[k]/(1-beta.sum))
  beta.sum <- beta.sum + (beta[TT-k+1] <- Cj[TT-k+1]/alpha.sum)
}

# Show predictions
# 1.
alpha %o% beta

# 2.
pairs <- expand.grid(i=as.factor(1:TT),j=as.factor(1:TT))
matrix(predict(CL, pairs, type="response"),TT)

# 3.
xtabs(fitted(CL)~i+j) ## upper triangle (past) + zeroes here

# CL on a triangle of claim numbers
rm(list=ls(all=TRUE))
Xij <- scan(n = 36)
156 37  6  5  3  2  1  0
154 42  8  5  6  3  0
178 63 14  5  3  1
198 56 13 11  2
206 49  9  5
250 85 28
252 44
221
TT <- trunc(sqrt(2*length(Xij)))
i <- rep(1:TT, TT:1); j <- sequence(TT:1); k <- i+j-1
fi <- as.factor(i); fj <- as.factor(j); fk <- as.factor(k)

cc <- exp(coef(glm(Xij~fi+fj,quasipoisson())))
alpha <- c(1,cc[2:TT])*cc[1]
beta <- c(1,cc[(TT+1):(2*TT-1)])
alpha <- alpha * sum(beta); beta <- beta/sum(beta)
round(alpha %o% beta, 3)

# Verbeek
Ri <- tapply(Xij, i ,sum); Cj <- tapply(Xij, j, sum)
alpha <- beta <- numeric(TT)
aa <- alpha[1] <- Ri[1]
bb <- beta[TT] <- Cj[TT]/Ri[1]
for (n in 2:TT) {
  aa <- aa + (alpha[n] <- Ri[n]/(1-bb))
  bb <- bb + (beta[TT-n+1] <- Cj[TT-n+1]/aa)
}
round(alpha %*% t(beta), 3)
