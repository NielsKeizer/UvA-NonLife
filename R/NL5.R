rm(list=ls(all=TRUE)) ## Discard old garbage

# Q18
Xij <- scan(n=36)
156 37  6  5 3 2 1 0
154 42  8  5 6 3 0
178 63 14  5 3 1
198 56 13 11 2
206 49  9  5
250 85 28
252 44
221
TT <- 8; i <- rep(1:TT, TT:1); j <- sequence(TT:1); k <- i+j-1
fi <- as.factor(i); fj <- as.factor(j); fk <- as.factor(k)
ee <- c(28950,29754,31141,32443,34700,36268,37032,36637)
Expo <- rep(ee, TT:1)
CL <- glm(Xij~fi+fj, quasipoisson)
EE <- glm(Xij~offset(log(Expo))+fj, quasipoisson)

cc <- exp(coef(CL))
alpha <- cc[1] * c(1,cc[2:8]); names(alpha)[1] <- "fi1"
beta <- c(1,cc[9:15]); names(beta)[1] <- "fj1"
alpha <- alpha * sum(beta); beta <- beta / sum(beta)

i_tot <- rep(1:8, each=8)
j_tot <- rep(1:8,8)
k_tot <- i_tot+j_tot-1
future <- k_tot>8

sum(CL$fitted.values)
sum(alpha[i_tot]*beta[j_tot]*!future)
sum(alpha[i_tot]*beta[j_tot]*future)
sum(alpha %o% beta) - sum(Xij)
# Einde Q18

# Q19

round(tapply(fitted.values(EE)-Xij,j,sum),6)

# Einde Q19

# Q20

reserves <- numeric(); lasts <- c(171,181,191,201,211,271,261,251,241,231,221)
for (last in lasts){
  Xij[36] <- last
  cc <- exp(coef(glm(Xij~fi+fj,quasipoisson)))
  alpha <- c(1,cc[2:TT])*cc[1]; beta <- c(1,cc[(TT+1):(2*TT-1)])
  fits <- (alpha %o% beta)
  reserve <- sum(fits) - sum(Xij) ## the sum of the 'future' fitted values
  reserves <- c(reserves, reserve) 
}
rbind(lasts, reserves=round(reserves))
plot(lasts, reserves); lines(range(lasts),range(reserves))
