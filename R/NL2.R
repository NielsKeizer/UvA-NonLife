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
  print(c(value, object.size(hh), object.size(rr)))
}
# Q1 ---- Einde


