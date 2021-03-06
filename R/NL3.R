# Aanwezige data verwijderen

rm(list=ls(all=TRUE))

#Q3

path <- "http://www1.fee.uva.nl/ke/act/people/kaas/"
nages <- 101; nyears <- 58
Dxt.vec <- scan(paste(path,"deaths.csv",sep=""), sep=";", dec=",")
Ext.vec <- scan(paste(path,"exposures.csv",sep=""), sep=";", dec=",")
Dxt.vec <- round(Dxt.vec)
x <- gl(nages,nyears,nages*nyears); t <- gl(nyears,1,nages*nyears)
lnExt.vec <- log(Ext.vec)


#Q4
Z <- matrix(Dxt.vec, nrow=nages, ncol=nyears, byrow=TRUE)
Z[c(1,2,nages-1,nages), c(1,2,nyears-1,nyears)] # to check if the same
xtabs(Dxt.vec~x+t)[c(1,2,nages-1,nages), c(1,2,nyears-1,nyears)]
Z <- log((Z+.5)/matrix(Ext.vec, nrow=nages, ncol=nyears, byrow=TRUE))
alpha.LC <- rowMeans(Z)
Z <- Z - alpha.LC
s <- svd(Z)
beta.LC <- s$u[,1]/sum(s$u[,1])
kappa.LC <- s$v[,1]*s$d[1]*sum(s$u[,1])

kappa.LC <- kappa.LC * sum(beta.LC)
beta.LC <- beta.LC/sum(beta.LC)
alpha.LC <- alpha.LC + mean(kappa.LC)*beta.LC
kappa.LC <- kappa.LC - mean(kappa.LC)



#Q5
u1 <- eigen(Z%*%t(Z))$vectors[,1]
v1 <- eigen(t(Z)%*%Z)$vectors[,1]
d1 <- sqrt(eigen(t(Z)%*%Z)$values[1])
beta.LC1 <- u1/sum(u1)
kappa.LC1 <- v1*d1*sum(u1)
range(beta.LC-beta.LC1); range(kappa.LC-kappa.LC1) # `identical'

D <- diag(s$d)
dim(s$u); dim(D); dim(t(s$v)) # X = 101 rows x T = 58 columns; 58x58; 58x58
dim(s$u %*% D %*% t(s$v)) # 101x58
dim(t(s$u) %*% Z %*% s$v) # 58x58
range(s$u %*% D %*% t(s$v)-Z) # Z = U D V'
range(t(s$u) %*% Z %*% s$v - D) # D = U' Z V

#Q6


library(gnm) ## install it the first time you use it
set.seed(1)
start <- exp(lnExt.vec + alpha.LC[x] + beta.LC[x]*kappa.LC[t])
system.time(
  gg <- gnm(Dxt.vec ~ 0 + offset(lnExt.vec) + x + Mult(x,t), family=poisson,
            mustart=start, trace=TRUE)
) ## ~ 13 sec
gg$deviance; gg$iter ## 23406.706128 30
gg$coefficients

alpha.gnm <- gg$coefficients[1:101]
beta.gnm <- gg$coefficients[102:202]
kappa.gnm <- gg$coefficients[203:260]

kappa.gnm <- kappa.gnm * sum(beta.gnm)
beta.gnm <- beta.gnm/sum(beta.gnm)
alpha.gnm <- alpha.gnm + mean(kappa.gnm)*beta.gnm
kappa.gnm <- kappa.gnm - mean(kappa.gnm)

#Q7
par(mfrow=c(1,3))
plot(alpha.LC,ylim=range(alpha.LC), ylab="alpha", xlab="x", col="blue", type="l")
lines(alpha.gnm,col="red", type="l")
plot(beta.LC,ylim=range(beta.LC), ylab="beta", xlab="x", col="blue", type="l")
lines(beta.gnm,col="red", type="l")
plot(kappa.LC,ylim=range(kappa.LC), ylab="kappa", xlab="t", col="blue", type="l")
lines(kappa.gnm,col="red", type="l")

kappa.glm <- kappa.LC
g1 <- glm(Dxt.vec ~ x*kappa.glm[t] + offset(lnExt.vec), poisson)
c1 <- coef(g1)
g1$deviance; g1$iter ## 27603.07 4
c1

alpha.glm <- c(c1[1],c1[2:101]+c1[1])
beta.glm <- c(c1[102],c1[103:202]+c1[102])



g2 <- glm(Dxt.vec ~ 0 + x + t:beta.glm[x] + offset(lnExt.vec), poisson,
          mustart=fitted(g1))
c2 <- coef(g2)
g2$deviance; g2$iter ## 23594.62 4
c2[c(1,nages,nages+1,nages+nyears-1,nages+nyears)] ## t58:beta.glm[x] is NA

c2
summary(g2)


alpha.glm <- c2[1:101]
kappa.glm <- c(c2[102:158],0)
kappa.glm <- kappa.glm * sum(beta.glm)

# Q11

abs(exp(log(Ext.vec[532])+alpha.glm[x[532]]+beta.glm[x[532]]*kappa.glm[t[532]])-fitted(g2)[532])<0.001

# Q12
beta.glm <- beta.glm/sum(beta.glm)
alpha.glm <- alpha.glm + mean(kappa.glm)*beta.glm
kappa.glm <- kappa.glm - mean(kappa.glm)
(d1 <- sum(dpois(Dxt.vec,Dxt.vec,log=TRUE))) ## -21643.76
(d2 <- sum(dpois(Dxt.vec,
                 Ext.vec*exp(alpha.glm[x] + beta.glm[x] * kappa.glm[t]),
                 log=TRUE))) ## -33441.07, same as d3
(d3 <- sum(dpois(Dxt.vec,fitted(g2),log=TRUE)))
(d4 <- log(prod(dpois(Dxt.vec,fitted(g2)))))## -Inf
(d5 <- 2*sum(Dxt.vec*log(Dxt.vec/fitted(g2)) - (Dxt.vec-fitted(g2))))
(d1-d2)*2 ## 23594.62, same as d5
  
range(tapply(Dxt.vec-fitted(g2),x,sum)) ## -3e-10 2e-10
range(tapply(Dxt.vec-fitted(g2),t,sum)) ## -3300 2726

#Q14
kappa.glm <- kappa.LC
oldDeviance <- 0; TotnIter <- 0; start=NULL
system.time(
  repeat
  { g1 <- glm(Dxt.vec~x*kappa.glm[t]+offset(lnExt.vec), poisson, mustart=start)
  c1 <- coef(g1)
  alpha.glm <- c(c1[1],c1[2:101]+c1[1])
  beta.glm <- c(c1[102],c1[103:202]+c1[102])
  g2 <- glm(Dxt.vec ~ 0+x + t:beta.glm[x] + offset(lnExt.vec), poisson,
            mustart=fitted(g1))
  8
  c2 <- coef(g2)
  alpha.glm <- c2[1:101]
  kappa.glm <- c(c2[102:158],0)
  kappa.glm <- kappa.glm*sum(beta.glm); beta.glm <- beta.glm/sum(beta.glm);
  alpha.glm <- alpha.glm + mean(kappa.glm)*beta.glm
  kappa.glm <- kappa.glm - mean(kappa.glm)
  TotnIter <- TotnIter + g1$iter + g2$iter
  newDeviance <- g2$deviance;
  done <- abs((oldDeviance-newDeviance)/newDeviance)<1e-6
  cat(g1$deviance, "\t", g2$deviance, "\n")
  oldDeviance <- newDeviance; start <- fitted(g2)
  if (done) break
  }
) ## ~ 6 sec
TotnIter ## 20

AIC(g1); AIC(g2) ## 67098.22 67010.22
logLik(g1); logLik(g2) ## 'log Lik.' -33347.11 with df=202 and df=158

-2*(logLik(g1)-258) 

#Q15
dim(model.matrix(g1))
object.size(g1); object.size(g2);
sort(sapply(g1,object.size))

#Q16
x1 <- as.numeric(x)-1
g3 <- glm(Dxt.vec ~ x1 + offset(lnExt.vec),poisson)
g3$coefficients

(b.Gompertz <- exp(coef(g3)[1]))
(c.Gompertz <- exp(coef(g3)[2]))

alpha.Gompertz <- log(b.Gompertz)
kappa.Gompertz <- log(c.Gompertz)
beta.Gompertz <- (1:nages)-1

kappa.Gompertz <- kappa.Gompertz * sum(beta.Gompertz)
beta.Gompertz <- beta.Gompertz/sum(beta.Gompertz)
alpha.Gompertz <- alpha.Gompertz + mean(kappa.Gompertz)*beta.Gompertz
kappa.Gompertz <- kappa.Gompertz - mean(kappa.Gompertz)

g3.30 <- glm(Dxt.vec ~ x1 + offset(lnExt.vec),poisson,subset = x1>=30)
g3.30$coefficients
  
(b.30.Gompertz <- exp(coef(g3.30)[1]))
(c.30.Gompertz <- exp(coef(g3.30)[2]))

alpha.30.Gompertz <- log(b.30.Gompertz)
kappa.30.Gompertz <- log(c.30.Gompertz)
beta.30.Gompertz <- (1:nages)-1
kappa.30.Gompertz <- kappa.30.Gompertz * sum(beta.30.Gompertz)
beta.30.Gompertz <- beta.30.Gompertz/sum(beta.30.Gompertz)
alpha.30.Gompertz <- alpha.30.Gompertz + mean(kappa.30.Gompertz)*beta.30.Gompertz
kappa.30.Gompertz <- kappa.30.Gompertz - mean(kappa.30.Gompertz)

par(mfrow=c(1,1))
plot(alpha.LC,ylim=range(alpha.LC), ylab="alpha", xlab="x", col="blue", type="l")
lines(alpha.Gompertz,col="black", type="l")
lines(alpha.30.Gompertz,col="red", type="l")


g4 <- glm(Dxt.vec~x1*t-x1-1+offset(lnExt.vec), poisson, subset=x1>=30)
b <- 1e5*exp(head(coef(g4),nyears)); c <- 100*(exp(tail(coef(g4),nyears))-1)
par(mfrow=c(1,2))
plot(b, xlab="t", ylab="b*100000", ylim=c(0,10), type="l", yaxp=c(0,10,2),
     main="Gompertz parameters b;\nages 30+")
plot(c, xlab="t", ylab="c-1 in %", ylim=c(9,12), type="l",
     yaxp=c(9,12,3), main="Gompertz parameters c")

t1 <- 1:nyears
b.lm <- lm(b~t1,subset = t1>20)
b.lm$coefficients

t.intersect <- -b.lm$coefficients[1]/b.lm$coefficients[2]
t.intersect
#Q18
b <- b/1e5; c<- c/100 + 1;
log.mortality <-function(x){ log(b) + 	x * log(c)}

plot(log.mortality(65), xlab="t", ylab="Log-mortality", type="l",col="black",ylim=c(-5,0))
lines(log.mortality(75), col="red",  type="l")
lines(log.mortality(85), col="blue", type="l")

# 1 Analyzing a bonus-malus system using GLM

rm(list=ls(all=TRUE)) ## First remove traces of previous sessions
fn <- "http://www1.fee.uva.nl/ke/act/people/kaas/Cars.txt"
Cars <- read.table(fn, header=TRUE)
Bminus1 <- Cars$B - 1; Bis14 <- as.numeric(Cars$B==14)
Cars$A <- as.factor(Cars$A); Cars$R <- as.factor(Cars$R)
Cars$M <- as.factor(Cars$M); Cars$U <- as.factor(Cars$U)
Cars$B <- as.factor(Cars$B); Cars$WW <- as.factor(Cars$WW)
ActualWt <- c(650,750,825,875,925,975,1025,1075,1175,1375,1600)
W <- log(ActualWt/650)[Cars$WW]

# GLM analysis

g1 <- glm(TotCl/Expo~R+A+U+W+Bminus1+Bis14, quasipoisson, wei=Expo, data=Cars)
g2 <- glm(TotCl/Expo~R+A+U+W+Bminus1+Bis14+M, quasipoisson, wei=Expo, data=Cars)
g3 <- glm(TotCl/Expo~R+A+U+W+B, quasipoisson, wei=Expo, data=Cars)

anova(g1,g2)
anova(g1,g3)

# Multiplicative coefficients
options(digits=7)
exp(coef(g1)); exp(coef(g2)); exp(coef(g3))

# Q1
g1$y[4000]; g1$y["4000"]
g1$y[7000]; g1$y["7000"]
min(which(Cars$Expo==0))

# a) Are the values in table 9.8 correct?
# Upon direct comparison, we can see that the values are equal. Only the values for B3 up to B13 in the g1 and g2 models cannot be directly compared.
# We calculate these through R.
bm_class <- seq(1,13,1)
bm_coef <- exp((bm_class-1)*coef(g1)["Bminus1"])
bm_coef

# Direct comparison show them to be equal.

model.matrix(g2)[4000,]

# Observed value
g1$y[4000]

# Fitted value
fitted(g1)[4000]; fitted(g2)[4000]; fitted(g3)[4000]

# B) 
# The observed value is 326.5, the estimated values are pretty close, 634.1, 636,4 and 644.5.

# Explain this result
g2$family$linkinv(model.matrix(g2)[4000,]%*%coef(g2))

# The inner product of the coefficients and the corresponding model values for cell 4000 is taken. The linkinv function is simply the exponential function, so this line calculates the fitted value for cell 4000. Which is exactly equal to the voutcome of the fitted values from the same model.


# Q1 afnmaken

# Q2
# Determine phi using a rich model
g.rich <- glm(TotCl/Expo~R+A+U+WW+B, quasipoisson, wei=Expo, data=Cars)
anova(g.rich)
phi <- 38521406/7495
# a) Can Bis14 be removed from model g1?
g.test <- glm(TotCl/Expo~R+A+U+W+Bminus1, quasipoisson, wei=Expo, data=Cars)
anova(g.test,g1)

test <- function (Df, Deviance){
  scaled.dev <- Deviance/phi
  test.dev <- qchisq(0.95,Df)
  return(scaled.dev>test.dev)
}
test(1, 138802)

# b) Can B or W be removed from model g3?
g.test <- glm(TotCl/Expo~R+A+U+W, quasipoisson, wei=Expo, data=Cars)
anova(g.test,g3)
test(13,40358385)

g.test <- glm(TotCl/Expo~R+A+U+B, quasipoisson, wei=Expo, data=Cars)
anova(g.test,g3)
test(1,6950616)
# c) In g1, does it help to allow separate coefficients for each weight class?
g.test <- glm(TotCl/Expo~R+A+U+WW+Bminus1+Bis14, quasipoisson, wei=Expo, data=Cars)
anova(g1, g.test)
test(9,23053)

#Q3
test(7515-7491,38616941-38408588)

#Q4
# In this question, we estimate the number of claims and the claimsizes seperately.

g.nCl <- glm(nCl/Expo~R+A+U+W+Bminus1+Bis14, quasipoisson, wei=Expo, data=Cars)
g.sCl <- glm(TotCl/nCl~R+A+U+W+Bminus1+Bis14, Gamma(link="log"), wei=nCl, data=Cars)

g.direct <- glm(TotCl/Expo~R+A+U+W+Bminus1+Bis14, quasipoisson, wei=Expo, data=Cars)
# We can combine the two models by adding their coefficients, because combining the two models would give a product of two exponential functions, which is the same as one exponential with the arguments summed.

mult.coef <- exp(coef(g.nCl)+coef(g.sCl))
direct.coef <- exp(coef(g.direct))
mult.coef; direct.coef
