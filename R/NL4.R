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
xtabs(round(fitted(gg))*future~i+j)[6:10,2:6]
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
round(xtabs(alpha[i]*beta[j]*future~i+j)[6:10,2:6])
