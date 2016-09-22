## Deze file bevat de uitwerkingen voor de opdracht NL2

# Aanwezige data verwijderen
rm(list=ls(all=TRUE))

# 1 Simulating an insurance portfolio—App. A3

# Genereer data
n.obs <- 10000; set.seed(4)
# n.obs <- 10000; set.seed(4) # Gebruik deze regel voor een grotere sample size.
sx <- as.factor(sample(1:2, n.obs, repl=TRUE, prob=c(6,4)))
jb <- as.factor(sample(1:3, n.obs, repl=TRUE, prob=c(3,2,1)))
re.tp <- sample(1:9, n.obs, repl=TRUE, prob=c(.1,.05,.15,.15,.1,.05,.1,.1,.2))
tp <- as.factor(c(1,2,3,1,2,3,1,2,3)[re.tp]) 
re <- as.factor(c(1,1,1,2,2,2,3,3,3)[re.tp])

# Bekijk data in tabel.
table(list(region=re, type=tp))

# Bepaal de hoeveelheid geheugen per element in de array
hh <- 1:2^20; object.size(hh)/length(hh) ## 4 + a bit 
hh <- rep(0,2^20); object.size(hh)/length(hh) ## 8 + a bit 
hh <- as.integer(hh); object.size(hh)/length(hh) ## 4 + a bit

# Verwijder shit
object.size(re.tp); rm(re.tp); rm(hh)

# Q1 ---- How many bytes does it take to store 1,...,10,1000,100000 logical values TRUE/FALSE?
for (n_values in c(1,2,3,4,5,6,7,8,9,10,1000,100000)){
  hh <- rep(TRUE,n_values)
  rr <- sample(c(TRUE,FALSE),n_values,repl=TRUE,prob=c(1,1))
  af <- as.factor(rr)
  print(c(n_values, object.size(hh), object.size(rr), object.size(af)))
}

rm(hh,rr,ah)
# Q1 ---- Einde

# Aantal maanden (mo) in force
mo <- 3 * sample(1:4, n.obs, repl=TRUE, prob=c(1,1,0,8))

# Stel de claim frequentie vast per 'cel'
mu <- 0.05 * c(1,1.2)[sx] *
             c(1,1,1)[jb] * 
             c(1,1.2,1.44)[re] * 
             1.2^(0:2)[tp] * mo/12 
y <- rpois(n.obs, mu)
table(y)

# Q2 ---- Now compare mean(y) and var(y), and comment.
cbind(mean=mean(y),variance=var(y),phi=var(y)/mean(y))

# The overdispersion factor phi is not vary stable and usually larger than 1. This is because the sample is to small for the dispersion factor to converge to its theoretical value.

# Q2 ---- Einde

# Maak tabellen voor inzichtelijkheid data
table(list(nCl=y,gender=sx))
table(list(nCl=y,gender.region=sx:re))

# Aggregeer over de verschillende risico factor.
aggr <- aggregate(list(Expo=mo/12,nCl=y,nPol=1), list(Jb=jb,Tp=tp,Re=re,Sx=sx), sum)

aggr[sort(sample(1:54,10)),]

# Q3 ---- Compute how much memory is gained by using the aggr dataframe, by doing:

object.size(aggr) 
object.size(mo) 
object.size(y) 
object.size(jb) + object.size(tp) + object.size(re) + object.size(sx)

# Q3 ---- Einde

# Q4 ---- What is actually the ML-estimate of \hat{\lambda_{3,3,3,2}}
# Je deelt het aantal claims door de exposure (ipv het aantal polissen).
aggr[54,]
lambda3332 <- aggr$nCl[54]/aggr$Expo[54]
lambda3332
# Q4 ---- Einde

# 2 Exploring the automobile portfolio of Sec. 9.5

rm(list=ls(all=TRUE)) 
n <- scan(n=54) ## read 54 numbers into vector n
 1  8 10  8  5 11 14 12 11 10  5 12 13 12 15 13 12 24
12 11  6  8 16 19 28 11 14  4 12  8 18  3 17  6 11 18
12  3 10 18 10 13 12 31 16 16 13 14  8 19 20  9 23 27
expo <- scan(n=54) ## the number of policies 
10 22 30 11 15 20 25 25 23 28 19 22 19 21 19 16 18 29
25 18 20 13 26 21 27 14 16 11 23 26 29 13 26 13 17 27
20 18 20 29 27 24 23 26 18 25 17 29 11 24 16 11 22 29
expo <- 7 * expo ## each policy is in force during a 7-year period

sex <- gl(2,27); region <- gl(3, 9, 54); type <- gl(3, 3, 54); job <- gl(3, 1, 54)

# Q5 ---- Comment on the diﬀerence between:
str(type)
str(rep(1:3, each=3, len=54))

# The str function compactly displays the structure of an arbitrary R object.
# type contains a factor object, with 3 levels (or categories), every element in the factor must be one of those 3 elements
# rep(1:3, each=3, len=54) creates a vector of integers of three ones, three twos and three threes, repeated to a length of 54.

# Q5 ---- Einde

set.seed(1); subset <- sort(sample(1:54,15))
data.frame(sex, region, type, job, n, expo)[subset,]

# Q6 ---- For the first two cells listed, check if the covariates have the right value.

# The first cell is 3. We check the covariates
cbind(sex=sex[3],region=region[3],type=type[3],job=job[3],n=n[3],expo=expo[3])

# The second cell is 8. We check the covariates
cbind(sex=sex[8],region=region[8],type=type[8],job=job[8],n=n[8],expo=expo[8])

# We conclude that the covariates have the same value.

# Q6 ---- Einde

xt <- xtabs(round(1000 * n/expo) ~ sex+region+type+job)
ftable(xt, row.vars=1:2, col.vars=3:4)

anova(glm(n/expo ~ region*type, quasipoisson, wei=expo))


# Q7 ---- Construct the analysis of deviance table when type is added before region. Are the differ-
#         ences in deviances from adding these terms the same?

anova(glm(n/expo ~ type*region, quasipoisson, wei=expo))
anova(glm(n/expo ~ region*type, quasipoisson, wei=expo))

# The analysis results for 1+region+type is equal to 1+type+region. The same is true for 1+region+type+region*type and 1+type+region+type*region.

# Q7 ---- Einde

# Q8 ---- There are two dierent ways to deal with the `exposure'; see MART p. 250. One is by taking
#         average claim frequencies n/expo and using weights expo, as is done above, the other is by
#         adding the log of the exposure as an `oset' to the linear predictor:

(g.off <- glm(n ~ 1+region+type+region:type+offset(log(expo)),
              family=poisson(link=log)))
(g.wei <- glm(n/expo ~ region*type, poisson, wei=expo))

# The output of g.off and g.wei contains the same coefficients, degrees of freedom, null deviance and residual deviance.
# The AIC for g.off is 290.7, however, for g.wei this is Inf. Also, g.wei throws warnings, on further inspection these arise from having non-integer x values in calls to dpois.

# TODO: Check MART p.250.

# Q8 ---- Einde

summary(g.off)

X <- model.matrix(g.off)
X[subset,] ## print a subset of its rows; all columns

# Q9 ---- Explain why multiplying the 0/1 dummies with region2 and type3 gives the dummy for
#         region2:type3.

# TODO: Maak in LaTeX afbeeldingen van de dummies en laat zien dat het product van de afbeelding gelijk is aan de afbeelding van het product.

# Q9 ---- Einde

# Q10 ---- In this model, what is the estimated annual number of claims for someone with:
#          a) region=1, type=1?
#          b) the worst type/region combination?

g.main <- glm(n/expo ~ region+type, quasipoisson, wei=expo)
coef(g.main)

# Q10 a)
# If region=1 and type=1, then the indicators for region2, region3, type2 and type3 are 0. Thus we only have to calculate:
exp(g.main$coefficients["(Intercept)"])

# The first row of the dataset has region=1 and type=1, so we check against the fitted values from the glm.
g.main$fitted.values[1]
# Which is the same.

# Q10 b)
# Assuming all region/type combinations already exist in the model data:
max(g.main$fitted.value)

# By going through all possible combinations:
exp(g.main$coefficients[1]+max(0,g.main$coefficients[2:3])+max(0,g.main$coefficients[4:5]))

# Showing all possible combinations
exp(g.main$coefficients[1]+matrix(c(0,g.main$coefficients[2:3]),3,3) +t(matrix(c(0,g.main$coefficients[4:5]),3,3)))

# We see that the combination type=3, region=3 gives the highest estimated annual number of claims.

# Q10 ---- Einde

# Q11 ---- Using that the vector of linear predictors equals X.Beta, reconstruct the last vector using the
#          four earlier items.

cbind(g.off$family$linkinv(model.matrix(g.off) %*% coef(g.off) + g.off$offset),fitted.values(g.off))
# The first item contains the list of recalculated values, the second the values, directly from the model.

# Q11 ---- Einde

# Q12 ---- a) Analyze the deviances of g.main and g.
#          b) Why is g. a restriction of g.main?
#          c) Does the restriction in g. actually make sense? Hint: what does it mean for the dierence
#             in premiums for regions 1, 2 and 3, other tari factors being equal?

g. <- glm(n/expo ~ as.numeric(region)+type, quasipoisson, wei=expo)
summary(g.main); summary(g.)
anova(g., g.main)

# It is a restriction, because you imply there is a linear relation between two degrees of freedom for the region types.

# Q12 ---- Einde