
rm(list=ls(all=TRUE)); options(digits=6) ## housekeeping
Xij <- scan(n=55) ## Data Taylor & Ashe (1983)
357848 0766940 0610542 0482940 527326 574398 146342 139950 227229 067948
352118 0884021 0933894 1183289 445745 320996 527804 266172 425046
290507 1001799 0926219 1016654 750816 146923 495992 280405
310608 1108250 0776189 1562400 272482 352053 206286
443160 0693190 0991983 0769488 504851 470639
396132 0937085 0847498 0805037 705960
440832 0847631 1131398 1063269
359480 1061648 1443370
376686 0986608
344014

n <- length(Xij); TT <- trunc(sqrt(2*n))
i <- rep(1:TT, TT:1); j <- sequence(TT:1)
i <- as.factor(i); j <- as.factor(j)

Orig.CL <- glm(Xij~i+j, family=quasipoisson)
coefs <- exp(coef(Orig.CL)); round(coefs,4)
alpha <- c(1, coefs[2:TT]) * coefs[1]
beta <- c(1, coefs[(TT+1):(2*TT-1)])
names(alpha) <- paste0("row",1:10); round(alpha)
names(beta) <- paste0("col",1:10); round(beta, 4)

#Question 1
sum((Xij-fitted(Orig.CL))[i==5])

#Question 2
Orig.fits <- outer(alpha, beta); round(Orig.fits)
future <- row(Orig.fits) + col(Orig.fits) - 1 > TT
(Orig.reserve <- sum(Orig.fits[future])) ## 18680856

row(Orig.fits)
matrix(as.numeric(future),10)

#Question 3
ij <- expand.grid(i=as.factor(1:TT),j=as.factor(1:TT))
ij[c(1,5,10,19,35,67),]
mm <- matrix(predict(Orig.CL, ij, type="response"), TT); round(mm)
sum(Xij); sum(mm[row(mm)+col(mm)-1<=TT])
sum(mm[row(mm)+col(mm)-1<=TT & row(mm)==TT-1])

Prs.resid <- (Xij - fitted(Orig.CL)) / sqrt(fitted(Orig.CL))
p <- 2*TT-1; phi.P <- sum(Prs.resid^2)/(n-p)
Adj.Prs.resid <- Prs.resid * sqrt(n/(n-p))

#Question 4
birthday <- 820911; set.seed(birthday) ## do adjust this line
nBoot <- 1000; payments <- reserves <- n.neg <- numeric(nBoot)
for (boots in 1:nBoot){ ## running this will take 5--10 seconds
  Ps.Xij <- sample(Adj.Prs.resid, n, replace=TRUE) ## 1
  Ps.Xij <- Ps.Xij * sqrt(fitted(Orig.CL)) + fitted(Orig.CL) ## 2
  number.neg <- sum(Ps.Xij<0)
  Ps.Xij <- pmax(Ps.Xij, 0) ## Set obs < 0 to 0
  Ps.CL <- glm(Ps.Xij~i+j, family=quasipoisson) ## 5
  coefs <- exp(as.numeric(coef(Ps.CL)))
  Ps.alpha <- c(1, coefs[2:TT]) * coefs[1]
  Ps.beta <- c(1, coefs[(TT+1):(2*TT-1)])
  Ps.fits <- outer(Ps.alpha, Ps.beta)
  Ps.reserve <- sum(Ps.fits[future])
  Ps.totpayments <- phi.P * rpois(1, Ps.reserve/phi.P) ## 11
  reserves[boots] <- Ps.reserve ## 12
  payments[boots] <- Ps.totpayments; n.neg[boots] <- number.neg}

sum(n.neg)


(PEbs <- sqrt(phi.P*Orig.reserve + sd(reserves)^2)) ## ~ 3000000
sd(reserves)^2 / (phi.P*Orig.reserve) ## ~ 8 for these data
payments <- payments/1e6
round(quantile(payments, c(0.5,0.75,0.9,0.95,0.99)), 1)
## output should resemble:
## 50% 75% 90% 95% 99%
## 19 21 23 24 27
mean(payments) ## ~ 19 million
sd(payments) ## ~ 3 million
100 * sd(payments) / mean(payments) ## c.v. ~ 15%
pp <- (payments-mean(payments))/sd(payments)
sum(pp^3)/(nBoot-1) ## ~ .4 estimates the skewness
sum(pp^4)/(nBoot-1) - 3 ## ~~ .4 estimates the kurtosis
par(mfrow=c(1,2))
hist(payments,breaks=21,prob=T)
lines(density(payments), lty="dashed", col="blue")
curve(dnorm(x, mean(payments), sd(payments)), lty="dotted", add=T, col="red")

#Question 5
mean(payments)
min(payments)
max(payments)
quantile(payments, c(0.25,0.75,0.05,0.95))

#Question 6
Orig.CL.Gamma <- glm(Xij~i+j, family=Gamma(link=log))
Prs.resid <- (Xij - fitted(Orig.CL.Gamma)) / fitted(Orig.CL.Gamma)
p <- 2*TT-1; phi.P <- sum(Prs.resid^2)/(n-p)
Adj.Prs.resid <- Prs.resid * sqrt(n/(n-p))

birthday <- 820911; set.seed(birthday) ## do adjust this line
nBoot <- 1000; payments <- reserves <- n.neg <- numeric(nBoot)
for (boots in 1:nBoot){ ## running this will take 5--10 seconds
  Ps.Xij <- sample(Adj.Prs.resid, n, replace=TRUE) ## 1
  Ps.Xij <- Ps.Xij * fitted(Orig.CL.Gamma) + fitted(Orig.CL.Gamma) ## 2
  number.neg <- sum(Ps.Xij<0.01)
  Ps.Xij <- pmax(Ps.Xij, 0.01) ## Set obs < 0.01 to 0.01
  Ps.CL <- glm(Ps.Xij~i+j, family=Gamma(link=log)) ## 5
  coefs <- exp(as.numeric(coef(Ps.CL)))
  Ps.alpha <- c(1, coefs[2:TT]) * coefs[1]
  Ps.beta <- c(1, coefs[(TT+1):(2*TT-1)])
  Ps.fits <- outer(Ps.alpha, Ps.beta)
  Ps.reserve <- sum(Ps.fits[future])
  vec.Ps.fits <- Ps.fits[future]; h <- length(vec.Ps.fits)
  Ps.totpayments <- sum(rgamma(h,1/phi.P,1/(vec.Ps.fits * phi.P)))
  reserves[boots] <- Ps.reserve ## 12
  payments[boots] <- Ps.totpayments; n.neg[boots] <- number.neg}

payments <- payments/1e6
mean(payments)
quantile(payments, c(0.25,0.75,0.05,0.95))
mean(payments)
sd(payments) 
100 * sd(payments) / mean(payments) 
pp <- (payments-mean(payments))/sd(payments)
sum(pp^3)/(nBoot-1)
sum(pp^4)/(nBoot-1) - 3
hist(payments,breaks=21,prob=T)
lines(density(payments), lty="dashed", col="blue")
curve(dnorm(x, mean(payments), sd(payments)), lty="dotted", add=T, col="red")


#Question 7
coefs <- exp(coef(Orig.CL.Gamma)); round(coefs,4)
alpha <- c(1, coefs[2:TT]) * coefs[1]
beta <- c(1, coefs[(TT+1):(2*TT-1)])
names(alpha) <- paste0("row",1:10); round(alpha)
names(beta) <- paste0("col",1:10); round(beta, 4)
(tapply(Xij/beta[j],i,sum)/tapply(Xij,i,length)-alpha)/alpha*1e6

#Question 8
Xij.1 <- as.vector(t(xtabs(Xij~i+j))) ## stored row-wise as usual
ii <- rep(1:TT, each=TT); jj <- rep(1:TT, TT); future <- ii+jj-1 > TT
ii <- as.factor(ii); jj <- as.factor(jj)
Orig.CL <- glm(Xij~i+j, family=quasipoisson, epsilon = 1e-12)
CL <- glm(Xij.1~ii+jj, fam=quasipoisson, wei=as.numeric(!future))

summary(CL)
summary(Orig.CL)

#Question 9
p <- 2*TT-1
phi.P <- sum((Xij - fitted(Orig.CL))^2 / fitted(Orig.CL))/(n-p) ## 1
phi <- CL$deviance/CL$df.residual ## 2
c(phi, phi.P) ## 52861.5 52601.4
sum(resid(CL)^2)/(n-p) ## 3; "type=devi" is default
sum(resid(CL,type="pear")^2)/(n-p) ## 4
Prs.resid <- (Xij.1 - fitted(CL)) / sqrt(fitted(CL))
sum(as.numeric(!future)*Prs.resid^2)/(n-p) ## 5
dev.resid2 <- 2 * (Xij.1*log(ifelse(Xij.1==0, 1, Xij.1/fitted(CL))) -
                     (Xij.1 - fitted(CL))) ## cf. (9.29)
sum(as.numeric(!future)*dev.resid2)/(n-p) ## 6
summary(CL)$dispersion ## 7 (the warning is reassuring)
summary(Orig.CL)$dispersion ## 8

#Question 10
mu.hat <- fitted(CL)*future
Cov.beta <- vcov(CL)
X <- model.matrix(CL)
Cov.eta <- X %*% Cov.beta %*% t(X)
  MSPE <- phi * sum(mu.hat) + t(mu.hat) %*% Cov.eta %*% mu.hat
  cat("Total reserve =", round(sum(mu.hat)), "p.e. =", round(sqrt(MSPE)), "\n")
  
  
for (r in 2:TT){
  mu.r <- ifelse(ii==r,mu.hat,0) ## replace the elements of mu.hat not having rownr==r by 0
  MSPE <- phi * sum(mu.r) + t(mu.r) %*% Cov.eta %*% mu.r;res <- round(sum(mu.r)); ## see above
  cat("Year =", r, "\treserve =", round(res/1000),
      "\tp.e./res. =", round(100*sqrt(MSPE)/res), "%\n") }
    
#Question 11
rm(list=ls(all=TRUE)); Xij <- scan(n=36)
156 37 6 5 3 2 1 0
154 42 8 5 6 3 0
178 63 14 5 3 1
198 56 13 11 2
206 49 9 5
250 85 28
252 44
221
TT <- 8; i <- rep(1:TT, TT:1); j <- sequence(TT:1); k <- i+j-1
fi <- as.factor(i); fj <- as.factor(j); fk <- as.factor(k)
ee <- c(28950,29754,26315,39442,38423,50268,44762,43541)
Expo <- rep(ee, TT:1)

all(Expo == ee[i])

#Question 12
CL <- glm(Xij~fi+fj, quasipoisson) ## the Chain ladder model
EE <- glm(Xij~fj+offset(log(Expo)),quasipoisson) ## the Exposure model
scale <- CL$deviance / CL$df.residual ## mean-deviance estimate for phi
Delta.Dev.Sc <- (EE$deviance - CL$deviance) / scale ## difference of scaled deviances for CL and EE
Delta.df <- EE$df.residual - CL$df.residual ## difference of degrees of freedom for CL and EE
reject <- Delta.Dev.Sc/scale > qchisq(0.95,Delta.df) ## TRUE if Delta.Dev.Sc > the chi^2 critical value
cat("The exposure model", ifelse(reject, "is", "is not"), "rejected",
    "since the scaled deviance gained by CL is\n",
    round(Delta.Dev.Sc,1), "with", Delta.df, "extra parameters.\n")


#Question 13
xtabs(round(100*(fitted(CL) - fitted(EE))/fitted(CL))~i+j)
round(coef(CL),2); round(coef(EE),2)

coefs.CL <- exp(coef(CL));
alpha <- c(1, coefs.CL[2:TT]) * coefs.CL[1]
beta <- c(1, coefs.CL[(TT+1):(2*TT-1)])
alpha <- alpha*sum(beta); beta <- beta/sum(beta)

coefs.EE <- exp(coef(EE));
M <- ee
delta <- c(1,coefs.EE[2:TT])*coefs.EE[1]
M <- M*sum(delta); delta <- delta/sum(delta)

round(100*(alpha%o%beta-M%o%delta)/(alpha%o%beta))

round(100*(alpha-M)/alpha)
round(100*(beta-delta)/beta)

#Question 14
Three.off <- glm(Xij~offset(log(Expo))+fj+fi+fk, quasipoisson)
anova(Three.off, test="Chisq")

Three.off.without.fk <- glm(Xij~offset(log(Expo))+fj+fi, quasipoisson)
anova(Three.off.without.fk, test="Chisq")

options(digits=7); summary(Three.off) #dispersion is 1.46782
summary(Three.off.without.fk) #dispersion is 1.62435 and deviance equal so lower scaled deviance

#Question 15

exp(coef(Three.off)[8])


#Question 16
Three.off.without.Offset <- glm(Xij~fj+fi+fk, quasipoisson)
exp(Three.off.without.Offset$coefficients[1]) / exp(Three.off$coefficients[1])
# voor model zonder offset is fit op X11, model met offset fit op X11/n1, dus antwoord is n1

exp(Three.off$coefficients[1])*(ee[1])
exp(Three.off.without.Offset$coefficients[1])


#Model zonder offset
intersept.three.off.without.offset <- Three.off.without.Offset$coefficients[1]
alpha2 <- Three.off.without.Offset$coefficients[9]
beta1 <- 0
gamma2 <- Three.off.without.Offset$coefficients[16]

exp(intersept.three.off.without.offset + alpha2 + beta1 + gamma2)

#Model met offset
intersept.three.off <- Three.off$coefficients[1]
alpha2 <- Three.off$coefficients[9]
beta1 <- 0
gamma2 <- Three.off$coefficients[16]

exp(intersept.three.off + alpha2 + beta1 + gamma2)*ee[2]

#fitted values are equal

#Question 17
i.is.3 <- as.numeric(i==3)
Three.off.Dummy3 <- glm(Xij~offset(log(Expo))+fj+i.is.3+fi, quasipoisson)
anova(Three.off.Dummy3, test="Chisq")
options(digits=7); summary(special3)

Expo1 <- c(28950,29754,36315,39442,38423,50268,44762,43541)[i]
Three.off.adjusted <- glm(Xij~offset(log(Expo1))+fj+i.is.3+fi, quasipoisson)
anova(Three.off.adjusted, test="Chisq")


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

# Einde Q20

# Q21

lin_fit <- lm(reserves~lasts)
summary(lin_fit)

lin_fit$coefficients[1]
sum(alpha[1:7] %o% beta)-sum(Xij[i<=7])

# Einde Q21

# Q22

M <- ee / ee[1] * alpha[1]

# Q23
i_tot <- rep(1:8, each=8);j_tot <- rep(1:8,8)
pred.CL <- alpha %*% t(beta); round(pred.CL, 4)
pred.BF <- M %*% t(beta); round(pred.BF, 4)
future <- xtabs(i_tot+j_tot-1>8~i_tot+j_tot)
reserve.CL <- sum(pred.CL*future)
reserve.BF <- sum(pred.BF*future)
reserve.CL;reserve.BF

# Q24

sum(pred.BF*(1-future))
sum(Xij)

# Q25

CLoff <- glm(Xij~offset(log(Expo))+fj, quasipoisson)

# Einde Q25

cc <- exp(coef(CLoff))
betaoff <- cc[1] * c(1,cc[2:TT])
pred.off <- ee %*% t(betaoff); round(pred.off, 4)
round(cbind(rowSums(pred.BF * future),
            rowSums(fits * future),
            rowSums(pred.off * future)), 1)
sum(pred.off * future)

# Q26

sum(pred.off * (1-future)); sum(Xij)

