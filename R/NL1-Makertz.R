gen.Sample <- function(n, a, b, c)
{if (any(a<0,b<0,c<1)) stop("Invalid parameters")
  lifetimes <- log(1+rexp(n)*log(c)/b)/log(c)
  if (a>0) lifetimes <- pmin(lifetimes, rexp(n)/a)
  return(lifetimes)}
set.seed(2525); G <- gen.Sample(2000, 0, 8e-5, 1.08)
set.seed(2525); M <- gen.Sample(2000, 5e-4, 8e-5, 1.08)
all(G>=M) 
mean(M==G) 
rbind(Gompertz=summary(G), Makeham=summary(M))

S <- function(x, a, b, c) exp(-a*x - b/log(c)*(c^x-1))
a <- 5e-4; b <- 8e-5; c <- 1.09
mean.age <- integrate(S, 0, Inf, a, b, c)$value 
mean(gen.Sample(1e6, a, b, c))
integrate(S, 0, Inf, 1.02*a, b, c)$value / mean.age
integrate(S, 0, Inf, a, 1.02*b, c)$value / mean.age
integrate(S, 0, Inf, a, b, 1.02*(c-1)+1)$value / mean.age

log.fx <- function(x, a, b, c) log(a + b*c^x) + (-a*x - b/log(c)*(c^x-1))

exp(-8300)
exp(-10000)
exp(-5000)
exp(-100)
exp(-10)

log.Lik <- function(p) {- sum(log.fx(M, p[1], p[2], p[3]))} ## Makeham on Makeham
a <- 5e-4; b <- 8e-5; c <- 1.08
oMM <- optim(c(a,b,c), log.Lik) 
oMM$value ## 8428.489
oMM$par ## a=7.3e-04 b=6.0e-05 c=1.083

log.Lik <- function(p) {-sum(log.fx(M, 0, p[1], p[2]))} ## Gompertz on Makeham
a <- 0; b <- 8e-5; c <- 1.08
oGM <- optim(c(b,c), log.Lik) ## 13x NaN
oGM$value ## 8456.258
oGM$par ## a=0 b=1.3e-4 c=1.074

log.Lik <- function(p) {-sum(log.fx(G, p[1], p[2], p[3]))} ## Makeham on Gompertz
a <- 5e-4; b <- 8e-5; c <- 1.08
oMG <- optim(c(a,b,c), log.Lik) ## 13x NaN
oMG$value
oMG$par

log.Lik <- function(p) {-sum(log.fx(G,0, p[1], p[2]))} ## Gompertz on Gompertz
a <- 0; b <- 8e-5; c <- 1.08
oGG <- optim(c(b,c), log.Lik) ## 13x NaN
oGG$value
oGG$par

AIC.MM <- 2*oMM$value + 2 * 3;
AIC.GM <- 2*oGM$value + 2 * 2;
AIC.MG <- 2*oMG$value + 2 * 3;
AIC.GG <- 2*oGG$value + 2 * 2;

BIC.MM <- 2*oMM$value + 7.6 * 3;
BIC.GM <- 2*oGM$value + 7.6 * 2;
BIC.MG <- 2*oMG$value + 7.6 * 3;
BIC.GG <- 2*oGG$value + 7.6 * 2;

(AIC.MM<AIC.GM)
(AIC.MG<AIC.GG)
(BIC.MM<BIC.GM)
(BIC.MG<BIC.GG)

S <- function(x, a, b, c) exp(-a*x - b/log(c)*(c^x-1))
x <- 0:110
plot(x, 1-S(x, 0, 8e-5, 1.08), type="h", ylab="F(x; a,b,c)")
lines(x, 1-S(x, 1e-3, 8e-5, 1.08), col="red")
points(x, 1-S(x, 0, 8e-5, 1.09), col="blue")

mu <- function(x, a, b, c) (a + b*c^x)
x <- 0:110
plot(x, mu(x, 0, 8e-5, 1.08), type="h", ylab="mu(x; a,b,c)")
lines(x, mu(x, 1e-3, 8e-5, 1.08), col="red")
points(x, mu(x, 0, 8e-5, 1.09), col="blue")

path <- "http://www1.fee.uva.nl/ke/act/people/kaas/"
D.xt <- round(scan(paste(path,"deaths.csv",sep=""), sep=";", dec=","))
e.xt <- round(scan(paste(path,"exposures.csv",sep=""), sep=";", dec=","))
nages <- 101; nyears <- 58
D.xt <- matrix(D.xt,nages,nyears,byrow=TRUE); D.x <- apply(D.xt,1,sum)
e.xt <- matrix(e.xt,nages,nyears,byrow=TRUE); e.x <- apply(e.xt,1,sum)

?diff
?head

diff(1:10, 2)
diff(1:10, 2, 2)

# Question 9a
S <- function(x, a, b, c) exp(-a*x - b/log(c)*(c^x-1))
agerange <- 1:nages ## denotes the range of ages accounted for when finding ML
LogLik <- function(p)
{ ddfs <- S(0:nages, p[1], p[2], p[3])
q.x <- -diff(ddfs)/head(ddfs,nages)
-sum(dbinom(D.x[agerange], e.x[agerange], q.x[agerange], log=TRUE))}
a <- 1e-4; b <- 8e-5; c <- 1.08
o <- optim(c(a,b,c), LogLik) 
o$value
o$par

ddfs <- S(0:nages, o$par[1], o$par[2], o$par[3])
q.x <- -diff(ddfs)/head(ddfs,nages)

plot(1:nages,D.x[1:nages]/e.x[1:nages],type="p",
     main="Optimally estimated qx & fractions Dx/ex",xlab="age",ylab="q_x")
lines(1:nages, q.x[1:nages], col="blue")

# Question 9b

S <- function(x, a, b, c) exp(-a*x - b/log(c)*(c^x-1))
agerange <- 2:nages ## denotes the range of ages accounted for when finding ML
LogLik <- function(p)
{ ddfs <- S(0:nages, p[1], p[2], p[3])
q.x <- -diff(ddfs)/head(ddfs,nages)
-sum(dbinom(D.x[agerange], e.x[agerange], q.x[agerange], log=TRUE))}
a <- 1e-4; b <- 8e-5; c <- 1.08
o2 <- optim(c(a,b,c), LogLik) 
o2$value
o2$par

ddfs <- S(0:nages, o2$par[1], o2$par[2], o2$par[3])
q2.x <- -diff(ddfs)/head(ddfs,nages)

plot(1:nages,D.x[1:nages]/e.x[1:nages],type="p",
     main="Optimally estimated qx & fractions Dx/ex",xlab="age",ylab="q_x")
lines(1:nages, q.x[1:nages], col="blue")
lines(1:nages, q2.x[1:nages], col="red")

nages <- 60
plot(1:nages,D.x[1:nages]/e.x[1:nages],type="p",
     main="Optimally estimated qx & fractions Dx/ex",xlab="age",ylab="q_x")
lines(1:nages, q.x[1:nages], col="blue")
lines(1:nages, q2.x[1:nages], col="red")


