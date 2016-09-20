#Eerste deel meegetypt tijdens bekijken college van 15-9-2016

# Aanwezige data verwijderen
rm(list=ls(all=TRUE))
fn <- "http://www1.fee.uva.nl/ke/act/people/kaas/Cars.txt"
Cars <- read.table(fn, header = TRUE)
head(Cars) #Laat de eerste 6 regels van Cars zien.

#Onderzoeken wat het effect is van de bonus malus klasse
Bminus1 <- Cars$B - 1
Bis14 <- as.numeric(Cars$B==14)

#Klassificaties maken van de risico variabelen
Cars$A <- as.factor(Cars$A); Cars$R <- as.factor(Cars$R)
Cars$M <- as.factor(Cars$M); Cars$U <- as.factor(Cars$U)
Cars$B <- as.factor(Cars$B); Cars$WW <- as.factor(Cars$WW)

# In W stoppen we de log van het midden gewicht relatief tot de lichtste auto's.
ActualWt <- c(650,750,825,875,925,1025,1075,1175,1375,1600)
attach(Cars) # Cars toegevoegd aan de namespace, hierdoor is de call WW gelijk aan Cars$WW
W <- log(ActualWt/650)[WW]

#reproduceer tabel 9.3
options(digits=2)
#1-d tabel
100* tapply(nCl, R, sum) / tapply(Expo, R, sum)
#2-d tabel
100* tapply(nCl, list(R=R, A=A), sum) / tapply(Expo, list(R=R, A=A), sum)

#GLM estimations uitvoeren
g2 <- glm(TotCl/Expo~R+A+U+W+Bminus1+Bis14+M, quasipoisson, wei=Expo)
g1 <- glm(TotCl/Expo~R+A+U+W+Bminus1+Bis14, quasipoisson, wei=Expo)
g3 <- glm(TotCl/Expo~R+A+U+W+B, quasipoisson, wei=Expo)
anova(g2,g1,g3) #vereenvoudigt de analysis-of-deviance
# TODO: geeft ander resultaat de Rob laat zien in de slides.

#Scale factor \phi estimated as 35259109/6820 = 5170
#Loss 69287 deviance is in fact a loss of 69287/5170 = 13.4
#95% critical value of \chi^2(11) = qchisq(0.95,11) = 19.7

#Look at two way interaction
g4 <- glm(TotCl/Expo ~ (R+A+U+W+Bminus1+Bis14)^2, quasipoisson, wei=Expo)
summary(g4)

#Loss ratios can for example be produced by
sum(TotCl)/sum(TotPrem)*100 ## the grand total loss ratio
for( rf in list(B,WW,R,M,A,U)){
  print(round(tapply(TotCl,rf,sum)/tapply(TotPrem,rf,sum)*100))
}

#tip for exercise, how to incorporate bonus malus, geometric progression for classes 1 to 13, treat 14 separately
#another tip, take weights with a sleightly less steep function
#beide tips zijn hierboven al uitgewerkt.