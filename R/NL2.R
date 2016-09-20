## Deze file bevat de uitwerkingen voor de opdracht NL2

# Aanwezige data verwijderen
rm(list=ls(all=TRUE))

# Genereer data
n.obs <- 10000; set.seed(4)
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
for (value in c(1,2,3,4,5,6,7,8,9,10,1000,100000)){
  hh <- rep(TRUE,value)
  rr <- sample(c(TRUE,FALSE),value,repl=TRUE,prob=c(1,1))
  af <- as.factor(rr)
  print(c(value, object.size(hh), object.size(rr), object.size(af)))
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
cbind(mean=mean(y),variance=var(y))

phi <- var(y)/mean(y)
phi
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