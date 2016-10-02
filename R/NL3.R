# Aanwezige data verwijderen


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


# TODO Q1 afnmaken

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
