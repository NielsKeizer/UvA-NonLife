rm(list=ls(all=TRUE))

m <- 100; a<- 9; s2 <- 100; w1 <- 100; w2 <- 50; set.seed(2)
Sec <- c(rep(1,w1), rep(2,w2))
Ksi <- rnorm(2,0,sqrt(a)); Xjt <- rnorm(w1+w2, m+Ksi[Sec], sqrt(s2))
## Var[Xjt] = Var[m+Ksi+Theta] = Var[Ksi] + Var[Theta] + 0 = a+s2
x <- tapply(Xjt, Sec, mean) ## 97.2 102.2
anova(lm(Xjt~Sec, test="F")); t.test(Xjt~Sec)