# Data De Vylder inlezen

rm(list=ls(all=TRUE)) ## Discard old garbage
Xij <- scan(n=60)
     0      0      0     0      0  4627
     0      0      0     0  15140 13343
     0      0      0 43465  19018 12476
     0      0 116531 42390  23505 14371
     0 346807 118035 43784  12750 12284
308580 407117 132247 37086  27744     0
358211 426329 157415 68219      0     0
327996 436744 147154     0      0     0
377369 561699      0     0      0     0
333827      0      0     0      0     0

# Q1 Filling the dots

i <- rep(1:10, each=6) ## the row nrs are (1,1,1,1,1,1,2,2,2,2,2,2,...)
j <- rep(1:6,10)       ## the col nrs are (1,2,3,4,5,6,1,2,3,4,5,6,...)
k <- i+j-1             ## the calendar year of the payments
future <- k>10         ## TRUE for obs with calendar year after now
valid <- ifelse(Xij!=0,1,0) ## 1 for the non-zero obs, 0 for zero obs

# Einde Q1

fi <- as.factor(i); fj <- as.factor(j); fk <- as.factor(k)
xtabs(Xij~i+j)

start <- Xij+0.5
gg <- glm(Xij~fi+fj,gaussian(link=log),weights=valid,mustart=start)

cc <- exp(coef(gg)); round(cc, 3)
alpha <- cc[1] * c(1,cc[2:10]); names(alpha)[1] <- "fi1"
beta <- c(1,cc[11:15]); names(beta)[1] <- "fj1"
alpha <- alpha * sum(beta); beta <- beta / sum(beta)
round(alpha); round(beta, 3)

# Q2

options(digits=10)
beta
alpha

# Einde Q2

# Q3

start <- rep(1,length(Xij))
glm(Xij~fi+fj,gaussian(link=log),weights=valid,mustart=start)$iter
start <- rep(10000,length(Xij))
glm(Xij~fi+fj,gaussian(link=log),weights=valid,mustart=start)$iter
start <- rep(100000,length(Xij))
glm(Xij~fi+fj,gaussian(link=log),weights=valid,mustart=start)$iter
start <- rep(mean(Xij),length(Xij))
glm(Xij~fi+fj,gaussian(link=log),weights=valid,mustart=start)$iter
start <- rep(mean(Xij[Xij>0]), length(Xij))
glm(Xij~fi+fj,gaussian(link=log),weights=valid,mustart=start)$iter
start <- fitted.values(glm(Xij~fi+fj,poisson,weights=valid))
glm(Xij~fi+fj,gaussian(link=log),weights=valid,mustart=start)$iter
start <- Xij+0.5
glm(Xij~fi+fj,gaussian(link=log),weights=valid,mustart=start)$iter
start <- Xij; start[Xij==0] <- 0.01
glm(Xij~fi+fj,gaussian(link=log),weights=valid,mustart=start)$iter
start <- pmax(Xij, 0.01)
glm(Xij~fi+fj,gaussian(link=log),weights=valid,mustart=start)$iter

# Einde Q3

# Q4

gg <- glm(Xij~fi+fj,gaussian(link=log),weights=valid,mustart=start)
ggg <- glm(Xij~fi+fj+k,gaussian(link=log),weights=valid,mustart=fitted(gg))
round(exp(coef(gg)),3); round(exp(coef(ggg)),3)
gg$iter; ggg$iter
(gg$deviance - ggg$deviance)/ggg$deviance

# Einde Q4

# Q5
xtabs(round(fitted(gg))*future~i+j)[6:10,2:6])
# Einde Q5

# Q6

beta <- rep(1, 6)
repeat
{ beta.old <- beta
alpha <- tapply(valid*Xij*beta[j],i,sum)/tapply(valid*beta[j]^2,i,sum)
beta <- tapply(valid*Xij*alpha[i],j,sum)/tapply(valid*alpha[i]^2,j,sum)
if (sum(abs((beta.old-beta)/beta)) < 1e-7) break ## out of the loop
# cat(beta,"\n") ## to monitor the iteration process
}
#round(xtabs(alpha[i]*beta[j]*future~i+j)[6:10,2:6])

# Einde Q6

# Q8

rm(list=ls(all=TRUE)) ## Discard old garbage
TT <- 10; x.top <- 2; d <- .5
gamma <- log(d); delta <- -x.top*gamma
beta <- exp(gamma*(1:TT)+delta*log(1:TT))/exp(gamma)
beta[1]==1

# Einde Q8

beta <- beta * runif(TT,.96,1.04)
plot(beta)
alpha <- 1.03^(1:TT) * c(1,1,1,1.05,.95,1.05,.95,1.05,.95,1)
alpha <- 1000 * alpha / alpha[1] / beta[1]
i <- rep(1:TT,TT:1); j <- sequence(TT:1); fi <- as.factor(i); fj <- as.factor(j)
mu.ij <- alpha[i] * beta[j]
phi <- 2; Xij <- phi * rpois(length(mu.ij), mu.ij/phi)

xtabs(round(mu.ij)~i+j)
round(xtabs(Xij~i+j))

# Q10
CL <- glm(Xij~fi+fj-1, quasipoisson)
exp(coef(CL))

Hoerl <- glm(Xij~fi+I(j-1)+log(j)-1, quasipoisson)

round(coef(CL),3); round(coef(Hoerl),3)
beta.CL <- exp(c(0,coef(CL)[(TT+1):(2*TT-1)]))
beta.Hoerl <- exp(coef(Hoerl)[TT+1]*(0:(TT-1))) * (1:TT)^coef(Hoerl)[TT+2]
round(rbind(beta.CL, beta.Hoerl), 4)
plot(beta.CL); points(beta.Hoerl, col="red")

scale <- CL$deviance/CL$df.residual
Delta.Dev.Sc <- (Hoerl$deviance - CL$deviance)/scale
Delta.df <- Hoerl$df.residual - CL$df.residual
reject <- Delta.Dev.Sc > qchisq(0.95, Delta.df)
cat("The Hoerl model", ifelse(reject, "is", "is not"), "rejected",
    "since the scaled deviance gained by CL is", round(Delta.Dev.Sc,1),
    "\nwith", Delta.df, "extra parameters.\n")
# Q11
anova(Hoerl,CL, test = "Chisq")
# Einde Q11

# Q12
rm(list=ls(all=TRUE)) ## Discard old garbage
Xij <- c(232,106,35,16,2, 258,115,56,27, 221,82,4, 359,71, 349)
  i <- c( 1, 1, 1, 1,1, 2, 2, 2, 2, 3, 3,3, 4, 4, 5)
  j <- c( 1, 2, 3, 4,5, 1, 2, 3, 4, 1, 2,3, 1, 2, 1)
  fi <- as.factor(i); fj <- as.factor(j)
xtabs(Xij~i+j)

# First estimate both methods
CL <- glm(Xij~fi+fj-1, quasipoisson)
Hoerl <- glm(Xij~fi+I(j-1)+log(j)-1, quasipoisson)
# Then we do an anlysis of deviance.
scale <- CL$deviance/CL$df.residual
Delta.Dev.Sc <- (Hoerl$deviance - CL$deviance)/scale
Delta.df <- Hoerl$df.residual - CL$df.residual
reject <- Delta.Dev.Sc > qchisq(0.95, Delta.df)
cat("The Hoerl model", ifelse(reject, "is", "is not"), "rejected",
    "since the scaled deviance gained by CL is", round(Delta.Dev.Sc,1),
    "\nwith", Delta.df, "extra parameters.\n")
# Visual inspection of the beta's
TT <- 5
beta.CL <- exp(c(0,coef(CL)[(TT+1):(2*TT-1)]))
beta.Hoerl <- exp(coef(Hoerl)[TT+1]*(0:(TT-1))) * (1:TT)^coef(Hoerl)[TT+2]
plot(beta.CL); points(beta.Hoerl, col="red")

# Now try with a Hoerlcurve for the portfolio growth.
Hoerl <- glm(Xij~I(i-1)+log(i)+fj-1, quasipoisson)
# Then we do an anlysis of deviance.
scale <- CL$deviance/CL$df.residual
Delta.Dev.Sc <- (Hoerl$deviance - CL$deviance)/scale
Delta.df <- Hoerl$df.residual - CL$df.residual
reject <- Delta.Dev.Sc > qchisq(0.95, Delta.df)
cat("The Hoerl model", ifelse(reject, "is", "is not"), "rejected",
    "since the scaled deviance gained by CL is", round(Delta.Dev.Sc,1),
    "\nwith", Delta.df, "extra parameters.\n")

# Einde Q12

rm(list=ls(all=TRUE)) ## Discard old garbage
Xij <- scan(n=36)
156 37  6  5 3 2 1 0
154 42  8  5 6 3 0
178 63 14  5 3 1
198 56 13 11 2
206 49  9  5
250 85 28
252 44
221
TT <- trunc(sqrt(2*length(Xij)))
i <- rep(1:TT, TT:1); j <- sequence(TT:1); k <- i+j-1
fi <- as.factor(i); fj <- as.factor(j); fk <- as.factor(k)

CL <- glm(Xij~fi+fj, poisson)
Threeway <- glm(Xij~fi+fj+fk, poisson)
anova(CL, Threeway)
round(qchisq(0.95, c(21,15,6)),1)

# Q13

cc <- exp(coef(CL))
alpha <- cc[1] * c(1,cc[2:TT]); names(alpha)[1] <- "fi1"
beta <- c(1,cc[(TT+1):(2*TT-1)]); names(beta)[1] <- "fj1"
alpha <- alpha * sum(beta); beta <- beta / sum(beta)
round(alpha); round(beta, 3)
round(alpha%o%beta,3)

# Einde Q13

# Q14

AS <- glm(Xij~fj+fk, poisson)
exp(coef(AS)["(Intercept)"])

# Einde Q14

cc <- exp(coef(AS))
beta.AS <- c(1,cc[2:8])*cc[1]; gamma.AS <- c(1,cc[9:15])
par(mfrow=c(1,2)); plot(gamma.AS); plot(log(gamma.AS))
ab <- coef(lm(log(gamma.AS)~I(1:8)))

# Q15
gamma.extrapolated <- exp(ab[1]+(1:15)*ab[2])
gammas <- gamma.extrapolated; gammas[1:8] <- gamma.AS
jjj <- rep(1:8,8); kkk <- jjj + rep(1:8,each=8) - 1
mm <- beta.AS[jjj]*gammas[kkk]
round(matrix(mm,8,byrow=TRUE),3)

par(mfrow=c(1,1));plot(log(gammas));lines(log(gamma.extrapolated), col="red")

# Einde Q15

# Q16
cbind("AS"=AIC(AS), "CL"=AIC(CL), "Threeway"=AIC(Threeway))
# Einde Q16

# Q17

rm(list=ls(all=TRUE)) ## Discard old garbage
set.seed(841109) ## replace by your birthday in format yymmdd
top <- 1+1.5*runif(1); decay <- .5 + runif(1)/5
gamma <- log(decay); delta <- -top*gamma
beta <- exp(gamma*(0:(10-1)) + delta*log(1:10))
alpha <- 1.03^(1:10) * (.80+runif(10)/5)
alpha <- 100 * alpha / alpha[1] / beta[1]
i <- rep(1:10,10:1); j <- sequence(10:1)
fi <- as.factor(i); fj <- as.factor(j)
phi <- 1.1+runif(1)/3
Xij <- round(phi * rpois(55, alpha[i] * beta[j]/phi))
rm(phi,alpha,beta,gamma,delta,top,decay)
Xij <- pmax(Xij,1)
xtabs(Xij~i+j)
anova(glm(Xij ~ i+j+log(i)+log(j)+fi+fj, quasipoisson)) ## for Q16
rbind(1:10,round(qchisq(.95,1:10),1))
# Fullest model
CL <- glm(Xij ~ fi+fj, quasipoisson)
phi <- CL$deviance/CL$df.residual
phi

# Hoerl curve in columns
Hoerl_j <- glm(Xij ~ fi+I(j-1)+log(j), quasipoisson)
anova(Hoerl_j,CL, test = "Chisq")
delta.dev.sc <- (Hoerl_j$deviance - CL$deviance)/phi
delta.df <- Hoerl_j$df.residual - CL$df.residual
reject <- delta.dev.sc > qchisq(0.95,delta.df)
reject;delta.dev.sc;qchisq(0.95,delta.df)

# Hoerl curve in rows
Hoerl_i <- glm(Xij ~ I(i-1)+log(i)+fj, quasipoisson)
anova(Hoerl_i,CL, test = "Chisq")
delta.dev.sc <- (Hoerl_i$deviance - CL$deviance)/phi
delta.df <- Hoerl_i$df.residual - CL$df.residual
reject <- delta.dev.sc > qchisq(0.95,delta.df)
reject;delta.dev.sc;qchisq(0.95,delta.df)

# Variate in rows
Variate_i <- glm(Xij ~ i+fj, quasipoisson)
anova(Variate_i,CL, test = "Chisq")
delta.dev.sc <- (Variate_i$deviance - CL$deviance)/phi
delta.df <- Variate_i$df.residual - CL$df.residual
reject <- delta.dev.sc > qchisq(0.95,delta.df)
reject;delta.dev.sc;qchisq(0.95,delta.df)

# Variate in columns
Variate_j <- glm(Xij ~ fi+j, quasipoisson)
anova(Variate_j,CL, test = "Chisq")
delta.dev.sc <- (Variate_j$deviance - CL$deviance)/phi
delta.df <- Variate_j$df.residual - CL$df.residual
reject <- delta.dev.sc > qchisq(0.95,delta.df)
reject;delta.dev.sc;qchisq(0.95,delta.df)

# Einde Q17

# Q19

# Hoerl method
alpha.h <- exp(coef(Hoerl_j))[1]*c(1,exp(coef(Hoerl_j))[2:10])
gamma <- coef(Hoerl_j)[11]; delta <- coef(Hoerl_j)[12]
beta.h <- exp(gamma*(0:9) + delta*log(1:10))
mu.past.h <- alpha.h[i]*beta.h[j]
i.all <- rep(1:10,each=10)
j.all <- rep(1:10,10)
mu.all.h <- alpha.h[i.all]*beta.h[j.all]
reserve.h <- sum(mu.all.h) - sum(mu.past.h)

xtabs(round(mu.past.h,2)~i+j)
xtabs(round(mu.all.h,2)~i.all+j.all)
reserve.h

# CL method
alpha.cl <- exp(coef(CL))[1]*c(1,exp(coef(CL))[2:10])
beta.cl <- c(1,exp(coef(CL))[11:19])
alpha.cl <- alpha.cl*sum(beta.cl)
beta.cl <- beta.cl/sum(beta.cl)
mu.past.cl <- alpha.cl[i]*beta.cl[j]
mu.all.cl <- alpha.cl[i.all]*beta.cl[j.all]
reserve.cl <- sum(mu.all.cl) - sum(mu.past.cl)

xtabs(round(mu.past.cl,2)~i+j)
xtabs(round(mu.all.cl,2)~i.all+j.all)
reserve.cl

reserve.cl - reserve.h

# Q19 einde

# Q21

zero <- glm(Xij~fi+I(j==1)+I(j-2), poisson)
Hoerl <- glm(Xij~fi+I(j-1)+log(j), poisson)
xtabs(round(zero$fitted.values,2)~i+j)

AIC(zero);AIC(Hoerl)
