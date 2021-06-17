library(car)
library(carData)


# Aufgabe 2

# ?berblick ?ber Datensatz erhalten
# Erkenntnisse: Zu erkl?rende Variable hat die Auspr?gungen 0/1, die Spalten geschlecht und wohnhaft sind kategorial und m?ssen durch Dummyvariablen ersetzt werden
summary(bevoelkerung)
str(bevoelkerung)

#Aufgabe 1

# Anzahl Grundgesamtheit
N1 = 1000000

# 1. Zufallsvariablen generieren:
# V1: Blutdruck als Normalverteilung
set.seed(1000)
systolischer.blutdruck <- rnorm (N1, mean=120, sd=6)

# V2: Alter als Gleichverteilung
alter <- sample(x = 0:90, size = N1, replace = TRUE)

# V3: Rauchverhalten (Anzahl Zigaretten pro Tag)

A <- 0
B <- 1:5
C <- 6:10
D <- 11:18
E <- 19:30
L <- sapply(list(A, B, C, D, E), length)

rauchverhalten <- sample(c(A, B, C, D, E),
                         size = N1,
                         prob = rep(c(0.85, 0.05, 0.08, 0.015, 0.005)/L,L),
                         replace = TRUE)


# V4: Kein Krebs in Familie (Eltern/ Geschwistern)

Krebs <- 0
KeinKrebs <- 1
L <- sapply(list( Krebs, KeinKrebs), length)

kein.krebs.familie<- sample(c(Krebs, KeinKrebs),
                            size = N1,
                            prob = rep(c(0.1, 0.9)/L,L),
                            replace = TRUE)

# V5: Krebs bei weiteren Verwandten (Tante/ Onkel/ Gro?eltern)

Krebs <- 0
KeinKrebs <- 1
L <- sapply(list( Krebs, KeinKrebs), length)

kein.krebs.verwandte <- sample(c(Krebs, KeinKrebs),
                               size = N1,
                               prob = rep(c(0.3, 0.7)/L,L),
                               replace = TRUE)

# V6: Einkommen pro Monat

sehr.niedrig <- 0:500
niedrig <- 500:1000
niedrig.mittel <- 1000:1500
mittel <- 1500:2000
mittel.hoch <- 2000:3000
hoch <- 3000:5000
sehr.hoch <- 5000:10000
L <- sapply(list(sehr.niedrig, niedrig, niedrig.mittel, mittel, mittel.hoch, hoch, sehr.hoch), length)

einkommen <- sample(c(sehr.niedrig, niedrig, niedrig.mittel, mittel, mittel.hoch,hoch, sehr.hoch ),
                    size = N1,
                    prob = rep(c(0.1, 0.2, 0.3, 0.25, 0.1,0.04,0.01 )/L,L),
                    replace = TRUE)

# V7: Wohnhaft (kategorische Variable) als Gleichverteilung, bei der 1 = Dorf, 2 = Kleinstadt, 3 = Gro?stadt

wohnhaft <- sample (c('Grossstadt','Land'), N1, TRUE)

# V8: Fleischkonsum in Gramm pro Woche als Gleichverteilung
fleischkonsum <- round(runif (N1, 0 , 1000))

# V9: Bewegung in Minuten pro Woche
# Funktion verwenden, um keine negativen Werte zu erhalten
# Ungültige samples (kleiner oder größer dem Schwellwert) werden auf NaN gesetzt und in der weiteren Auswertung ignoriert.
# Besser als Grenzen, das Grenzen zu Unstetigkeit führen. Böse Sache.
bewegung <- function(N1, mean, sd, lwr, upr, rounding) {
  bewegung <- round(rnorm(N1, mean, sd), rounding)
  bewegung [bewegung < lwr] <- NaN
  bewegung [bewegung > upr] <- NaN
  bewegung
}

bewegung <- bewegung(N1, mean=120, sd= 60, lwr=0, upr=500, rounding=0)

# V10: Geschlecht

#geschlecht <- sample (c('Weiblich','M?nnlich'), N1, TRUE)
weiblich <- sample (c(0,1),size = N1, replace = TRUE)

# Dataframe der Bev?lkerung mit allen Eigenschaften 
bevoelkerung <- data.frame(alter, rauchverhalten, kein.krebs.verwandte, kein.krebs.familie, einkommen, fleischkonsum, bewegung, wohnhaft, geschlecht)

# Umwandlung der Kategorischen Variablen in Dummyvariablen

# Wohnhaft
Grossstadt <- bevoelkerung$wohnhaft == 'Grossstadt'


# Geschlecht
#weiblich <- bevoelkerung$geschlecht == 'Weiblich'

#Fehlerrate erstellen
#error = rnorm (N1, 0, 2)

# Modell erzeugen, um zu prognostizieren, wie hoch die Wahrscheinlichkeit ist, dass Mensch in den n?chsten Jahren an Krebs erkrankt anhand von beeinflussenden Variablen

set.seed(100)
#y <- 4*rauchverhalten  -  15* kein.krebs.familie  + 1/40 * alter  - 2 *weiblich +fleischkonsum* 1/1500
#y <- 4*rauchverhalten  -  11* kein.krebs.familie  + 0.105 * alter  -1.35 *weiblich + fleischkonsum* 1/1825
y <- rauchverhalten  -  2* kein.krebs.familie  - kein.krebs.verwandte  -3 *weiblich + fleischkonsum* 1/300
wahrscheinlichkeiten = 1/(1+exp(- y)) #wahrscheinlichkeit erzeugen
hist(wahrscheinlichkeiten)
# Monte-Carlo Ansatz wird verwendet, um Entscheidung 0 oder 1 zu treffen, wenn wahrscheinlichkeit >0.5 --> 1
z<- runif(N1,min=0,max=1) #wahrscheinlichkeit erzeugen gleichverteilte
entscheidung <- rep(0,N1)     # Default-Entscheidung = 0
entscheidung[z<=wahrscheinlichkeiten] <- 1
hist(entscheidung)
krebs <- entscheidung

erwarteteAnzahl <- sum(wahrscheinlichkeiten)
erwarteteAnzahl
anzahlSimuliert <- sum(krebs)
anzahlSimuliert


bevoelkerung <- data.frame(krebs, alter, rauchverhalten, kein.krebs.verwandte, kein.krebs.familie, einkommen, fleischkonsum, bewegung, wohnhaft, geschlecht)
str(bevoelkerung)

# Dieser Teil ist zum testen, ob richtige Variablen bei logistischer Regression ermittelt werden  

# Data-Frame bevoelkerung.mod.1 wird erstellt, welche die Kategorischen Variablen geschlecht und wohnhaft durch Dummy-Variablen ersetzt
bevoelkerung.mod.1 <- data.frame(krebs, alter, rauchverhalten, kein.krebs.verwandte, kein.krebs.familie, einkommen, fleischkonsum, bewegung, Grossstadt, weiblich)

# Test, ob mitte logistischer Regression, die Variablen als signifikant gekennzeichnet werden, welche auch die zu beschreibende Variable beeinflussen
# !!Kann am Ende gel?scht werden
#model.test <-  glm(krebs ~ . ,data = bevoelkerung.mod.1,family=binomial("logit"))
#summary(model.test)

#___________________________________________________________________________________________________________________________

#Dummyvariable f?r Wohnhaft
Grossstadt <- bevoelkerung$wohnhaft == 'Grossstadt'
#Kleinstadt <- bevoelkerung$wohnhaft == 'Kleinstadt'
#Dorf <- bevoelkerung$wohnhaft == 'Dorf'

# Dummyvariable f?r Geschlecht
#weiblich <- bevoelkerung$geschlecht == 'Weiblich'

# Data-Frame mit Dummy-Variablen erstellen
bevoelkerung.mod.2 <- data.frame(krebs, alter, rauchverhalten, kein.krebs.verwandte, kein.krebs.familie, einkommen, systolischer.blutdruck, fleischkonsum, bewegung, Grossstadt, weiblich)
bevoelkerung.mod.2

# Zuf?llige Auswahl von 10000 Datens?tzen aus modifizierter Bev?lkerungs-Tabelle

library(caTools)
set.seed(100)
N2 = 10000

split <- sample.split(bevoelkerung.mod.2$krebs,SplitRatio= N2/N1)
bevoelkerung.10000 <- subset (bevoelkerung.mod.2, split== TRUE)


# Logistisches Modell erstellen (unter Anwendung der Konzepte des Statistical Learning)
# Mit allen Variablen beginnen
model.2 <- glm(krebs ~ .,data = bevoelkerung.10000,family=binomial("logit"))
# F-Statistik groesser 1: Daher kann Nullhypothese verworfen werden
summary(model.2)
# Kovarianz im Modell testen, da alle Variablen nahe 1 gibt es keine Kovarianz
vif(model.2)

# Schrittweise Entfernung der gr??ten p-Werte, bis Modell nur signifikante Variablen enth?lt
# !!! Reihenfolge des Entfernens am Ende nochmals pr?fen
model.2 <- update(model.2,~.-einkommen)
summary (model.2)
model.2 <- update(model.2,~.-Grossstadt)
summary (model.2)
model.2 <- update(model.2,~.-bewegung)
summary (model.2)
model.2 <- update(model.2,~.-alter)
summary (model.2)
model.2 <- update(model.2,~.-systolischer.blutdruck)
summary (model.2)
# alle p-Werte sind jetzt signifikant

# Ideales Modell mit logistischer Regression, um Krebs vorherzusagen
# z value hat identische Aussagekraft wie t-statistik, d.h. wenn z-Statistik hoch ist, ist die Wahrscheinlichkeit hoch, dass Nullhypothese falsch ist
# Nullhypothese bedeutet, dass Variable keinen Einfluss auf zu beschreibende Variable hat
# Wenn p-Wert klein wird tats?chlich Nullhypothese verworfen
model.2 <- glm(krebs ~ rauchverhalten + kein.krebs.familie + kein.krebs.verwandte + fleischkonsum + weiblich , data= bevoelkerung.10000, family = binomial("logit"))
model.2 <- glm(krebs ~ 0 + rauchverhalten + kein.krebs.familie + kein.krebs.verwandte + fleischkonsum + weiblich , data= bevoelkerung.10000, family = binomial("logit"))
summary (model.2)

##ROC-Kurve und AUC f?r Trainigsdaten und gesamten Datensatz ermitteln und ROC-Kurve ausgeben
#?? Soll dies bereits hier gemacht werden oder erst in 3d     
# Split des Plot-Screens
par(mfrow= c(1,2))

schaetzwert <- predict(model.2,bevoelkerung.10000, type= "response")#Prognosewerte aus den Trainingsdaten ermitteln
schaetzwertVollstaendig  <- predict(model.2,bevoelkerung.mod.2, type= "response")#Prognosewerte aus alle Daten ermitteln
library(pROC)

roc(bevoelkerung.10000$krebs,schaetzwert,plot=TRUE)
# ROC erstellen mit unabh?ngigem Test-Set, hier kann die G?te des Modells ?berpr?ft werden
# Ergebnis: Fast identisch hoch, wie bei Trainings-Datensatz --> sehr gut!
roc(bevoelkerung.mod.2$krebs,schaetzwertVollstaendig, plot = TRUE)


par(mfrow= c(1,1))

minRange = -3
maxRange = 1
k = 1000 #wieviel Wiederholungen
#3 Farben für drei Versuche werden definiert
colors = c(rgb(1, 0, 0, 1), rgb(0, 1, 0, 1), rgb(0, 0, 1, 1))
sampCnt = c(50000, 10000, 1000) #wieviel Trainingsdaten
firstPlot = TRUE # bist du das Erste Bild?
#svg("img1.svg")
for(VersuchIdx in 1:length(sampCnt)){   # VersuchIdx geht bis die Länge von der SampCnt , also 3
  coeff = rep(0, k)  #Platz für Koeffizienten schaffen, leere Matrix für k Werte erzeugen, um Werte zu schreiben
  for(wiederholungsIdx in 1:k){   #K mal Stichproben auf einen Wert (Schätzung für U1) reduzieren.
    localSampleIndex = sample(1:N1, sampCnt[VersuchIdx]) #Nehmen von Indexen zwischen 1 und 1.000.000 (Index-Zettel)  #es geht von 1 bis 1 Mio Messpunkten für jeweiligen Versuche (1K, 10K, 50K)
    
    if (wiederholungsIdx %% 100 == 0){
      #            cat(sprintf("<set name=\"%s\" value=\"%f\" ></set>\n", df$timeStamp, df$Price))
      cat(sprintf("Run %i of %i; This run is %.1f percent complete\n", VersuchIdx, length(sampCnt), (wiederholungsIdx/k)*100))
    }
    
    #alter, rauchverhalten, kein.krebs.verwandte, kein.krebs.familie, einkommen, fleischkonsum, bewegung, wohnhaft, geschlecht
    #Nehmen von Messpunkten (also abhängige und unabhängige Variablen) entsprechend den Index-Zetteln, z.B. a[2], u1[2], ...,u7[2],...
    locA = krebs[localSampleIndex]; locU1 = alter[localSampleIndex]; locU2 = rauchverhalten[localSampleIndex]; locU3 = kein.krebs.familie[localSampleIndex]; locU4 = fleischkonsum[localSampleIndex]; locU5 = kein.krebs.verwandte[localSampleIndex]
    locU6 = systolischer.blutdruck[localSampleIndex]; locU7 = bewegung[localSampleIndex]; locU8 = einkommen[localSampleIndex]; locU9 = Grossstadt[localSampleIndex]; locU10 = weiblich[localSampleIndex];  # holen wir uns Werte von TraningsDaten
    
    dataMatrix22 = data.frame(locA, locU1, locU2, locU3, locU4, locU5, locU6, locU7, locU8, locU9, locU10)
    
    model22 = glm(locA ~ 0 + locU5 + locU2 + locU3 + locU4 + locU10, data = dataMatrix22, family=binomial(link="logit"))
    
    #es wird Koeffizient von U1 genommen und als Histogramm dargestellt
    coeff[wiederholungsIdx] = model22$coefficients["locU5"]
  }
  if(firstPlot == TRUE) {
    #coeff[coeff < minRange] = NaN
    #coeff[coeff >= maxRange] = NaN
    hist(coeff, xlim=c(minRange,maxRange), ylim=c(0,k), col = colors[VersuchIdx], breaks = seq(minRange, maxRange, by=0.1), main = "Koeffizienten von kein Krebs Verwandte")
    firstPlot = FALSE  #um andere Histogramme dran hängen
  } else {
    hist(coeff, add=T, xlim=c(minRange,maxRange), ylim=c(0,k), col = colors[VersuchIdx], breaks = seq(minRange, maxRange, by=0.1))
  }
  
  print(sd(locU5))  
  #für 50.000 Stk. sd = 0.4594826; Std. Error = 2.864e-02
  #für 10.000 Stk.  sd = 0.4581494; Std. Error = 0.0643104
  #für 1.000 Stk. sd = 0.4535247; Std. Error = 0.1994883
  print(summary(model22)) 
  
}
#dev.off() #spreichert das Bild
#___________________________________________________________________________________

#Standartabweichung
#sd=(model22$coefficients["locU5"])
#print(sd) #-1.181168



#___________________________________________________________________________________
# Grafik auf ganzen Screen plotten: 
par(mfrow= c(1,1))

#Fleischkonsum
LinearesModel_Fleischkonsum <- lm (formula = krebs ~ fleischkonsum, data = bevoelkerung.10000)
summary(LinearesModel_Fleischkonsum)
#plot(aggregierterFleischkonsum$fleischkonsum, aggregierterFleischkonsum$krebs, xlab= 'Fleischkonsum', ylab= 'Krebswahrscheinlichkeit')
#abline (LinearesModel)

schaetzwert <- predict(LinearesModel_Fleischkonsum,bevoelkerung.10000, type= "response")#Prognosewerte aus den Trainingsdaten ermitteln
library(pROC)

roc(bevoelkerung.10000$krebs,schaetzwert,plot=TRUE)
#AUC = 0.5136

#Rauchverhalten
LinearesModel_Rauchverhalten <- lm (formula = krebs ~ rauchverhalten, data = bevoelkerung.10000)
summary(LinearesModel_Rauchverhalten)
#plot(aggregiertesRauchverhalten$rauchverhalten, aggregiertesRauchverhalten$krebs, xlab= 'Rauchverhalten', ylab= 'Krebswahrscheinlichkeit')
#abline (LinearesModel)

schaetzwert <- predict(LinearesModel_Rauchverhalten,bevoelkerung.10000, type= "response")#Prognosewerte aus den Trainingsdaten ermitteln
library(pROC)

roc(bevoelkerung.10000$krebs,schaetzwert,plot=TRUE)
#AUC = 0.8124


#Kein Krebs Familie
LinearesModel_KeinKrebsFamilie <- lm (formula = krebs ~ kein.krebs.familie, data = bevoelkerung.10000)
summary(LinearesModel_KeinKrebsFamilie)
#plot(nicht_aggregiertesKeinKrebsFamilie$kein.krebs.familie, nicht_aggregiertesKeinKrebsFamilie$krebs, xlab= 'Kein Krebs Familie', ylab= 'Krebswahrscheinlichkeit')
#abline (LinearesModel)

schaetzwert <- predict(LinearesModel_KeinKrebsFamilie,bevoelkerung.10000, type= "response")#Prognosewerte aus den Trainingsdaten ermitteln
library(pROC)

roc(bevoelkerung.10000$krebs,schaetzwert,plot=TRUE)
#AUC = 0.6796

#Alter
LinearesModel_Alter <- lm (formula = krebs ~ alter, data = bevoelkerung.10000)
summary(LinearesModel_Alter)
#plot(aggregiertesAlter$alter, aggregiertesAlter$krebs, xlab= 'Alter', ylab= 'Krebswahrscheinlichkeit')
#abline (LinearesModel)

schaetzwert <- predict(LinearesModel_Alter,bevoelkerung.10000, type= "response")#Prognosewerte aus den Trainingsdaten ermitteln
library(pROC)

roc(bevoelkerung.10000$krebs,schaetzwert,plot=TRUE)
#AUC = 0.5551

#Grossstadt
LinearesModel_Grossstadt <- lm (formula = krebs ~ Grossstadt, data = bevoelkerung.10000)
summary(LinearesModel_Grossstadt)
#plot(nicht_aggregierteGrossstadt$bevoelkerung.10000.Grossstadt, nicht_aggregierteGrossstadt$krebs, xlab= 'Grossstadt', ylab= 'Krebswahrscheinlichkeit')
#abline (LinearesModel)
schaetzwert <- predict(LinearesModel_Grossstadt,bevoelkerung.10000, type= "response")#Prognosewerte aus den Trainingsdaten ermitteln
library(pROC)

roc(bevoelkerung.10000$krebs,schaetzwert,plot=TRUE)
#AUC = 0.5138


# Lineares Modell weiblich 
# !! Hier wurde einmal keine Aggregation durchgef?hrt, da weiblich 0/1 Variable ist (macht jedoch auch keinen Sinn!)
LinearesModel_Weiblich <- lm (formula = krebs ~ weiblich, data = bevoelkerung.10000)
summary(LinearesModel_Weiblich)
#plot(bevoelkerung.10000$weiblich, bevoelkerung.10000$krebs, xlab= 'Weiblich', ylab= 'Krebswahrscheinlichkeit')
#abline (LinearesModel)

schaetzwert <- predict(LinearesModel_Weiblich,bevoelkerung.10000, type= "response")#Prognosewerte aus den Trainingsdaten ermitteln
library(pROC)

roc(bevoelkerung.10000$krebs,schaetzwert,plot=TRUE)
#AUC = 0.514


# Bestes Modell lineare Regression (hoher AUC):

LinearesModel_Rauchverhalten <- lm (formula = krebs ~ rauchverhalten, data = bevoelkerung.10000)
summary(LinearesModel_Rauchverhalten)

schaetzwert <- predict(LinearesModel_Rauchverhalten,bevoelkerung.10000, type= "response")#Prognosewerte aus den Trainingsdaten ermitteln
library(pROC)

roc(bevoelkerung.10000$krebs,schaetzwert,plot=TRUE)
#AUC = 0.8124



# Aufgabe 3b Multivariates lineares Modell, welches lediglich 2 Variablen beinhaltet

#Fleischkonsum + Rauchverhalten
Multi_LinearesModel_Fleischkonsum_Rauchverhalten <- lm (formula = krebs ~ fleischkonsum + rauchverhalten, data = bevoelkerung.10000)
summary(Multi_LinearesModel_Fleischkonsum_Rauchverhalten)

schaetzwert <- predict(Multi_LinearesModel_Fleischkonsum_Rauchverhalten,bevoelkerung.10000, type= "response")#Prognosewerte aus den Trainingsdaten ermitteln
library(pROC)
roc(bevoelkerung.10000$krebs,schaetzwert,plot=TRUE)
#AUC = 0.8236

#Fleischkonsum + KeinKrebsFamilie
Multi_LinearesModel_Fleischkonsum_KeinKrebsFamilie <- lm (formula = krebs ~ fleischkonsum + kein.krebs.familie, data = bevoelkerung.10000)
summary(Multi_LinearesModel_Fleischkonsum_KeinKrebsFamilie)

schaetzwert <- predict(Multi_LinearesModel_Fleischkonsum_KeinKrebsFamilie,bevoelkerung.10000, type= "response")#Prognosewerte aus den Trainingsdaten ermitteln
library(pROC)
roc(bevoelkerung.10000$krebs,schaetzwert,plot=TRUE)
#AUC = 0.6872

#Fleischkonsum + Alter
Multi_LinearesModel_Fleischkonsum_Alter <- lm (formula = krebs ~ fleischkonsum + alter, data = bevoelkerung.10000)
summary(Multi_LinearesModel_Fleischkonsum_Alter)

schaetzwert <- predict(Multi_LinearesModel_Fleischkonsum_Alter,bevoelkerung.10000, type= "response")#Prognosewerte aus den Trainingsdaten ermitteln
library(pROC)
roc(bevoelkerung.10000$krebs,schaetzwert,plot=TRUE)
#AUC = 0.5556

#Fleischkonsum + Grossstadt
Multi_LinearesModel_Fleischkonsum_Grossstadt <- lm (formula = krebs ~ fleischkonsum + Grossstadt, data = bevoelkerung.10000)
summary(Multi_LinearesModel_Fleischkonsum_Grossstadt)

schaetzwert <- predict(Multi_LinearesModel_Fleischkonsum_Grossstadt,bevoelkerung.10000, type= "response")#Prognosewerte aus den Trainingsdaten ermitteln
library(pROC)
roc(bevoelkerung.10000$krebs,schaetzwert,plot=TRUE)
#AUC = 0.5208

#Fleischkonsum + Weiblich
Multi_LinearesModel_Fleischkonsum_Weiblich <- lm (formula = krebs ~ fleischkonsum + weiblich, data = bevoelkerung.10000)
summary(Multi_LinearesModel_Fleischkonsum_Weiblich)

schaetzwert <- predict(Multi_LinearesModel_Fleischkonsum_Weiblich,bevoelkerung.10000, type= "response")#Prognosewerte aus den Trainingsdaten ermitteln
library(pROC)
roc(bevoelkerung.10000$krebs,schaetzwert,plot=TRUE)
#AUC = 0.5211

#Rauchverhalten + KeinKrebsFamilie
Multi_LinearesModel_Rauchverhalten_KeinKrebsFamilie <- lm (formula = krebs ~ rauchverhalten + kein.krebs.familie, data = bevoelkerung.10000)
summary(Multi_LinearesModel_Rauchverhalten_KeinKrebsFamilie)

schaetzwert <- predict(Multi_LinearesModel_Rauchverhalten_KeinKrebsFamilie,bevoelkerung.10000, type= "response")#Prognosewerte aus den Trainingsdaten ermitteln
library(pROC)
roc(bevoelkerung.10000$krebs,schaetzwert,plot=TRUE)
#AUC = 0.9573

#Rauchverhalten + Alter
Multi_LinearesModel_Rauchverhalten_Alter <- lm (formula = krebs ~ rauchverhalten + alter, data = bevoelkerung.10000)
summary(Multi_LinearesModel_Rauchverhalten_Alter)

schaetzwert <- predict(Multi_LinearesModel_Rauchverhalten_Alter,bevoelkerung.10000, type= "response")#Prognosewerte aus den Trainingsdaten ermitteln
library(pROC)
roc(bevoelkerung.10000$krebs,schaetzwert,plot=TRUE)
#AUC = 0.8558

#Rauchverhalten+ Grossstadt
Multi_LinearesModel_Rauchverhalten_Grossstadt <- lm (formula = krebs ~ rauchverhalten + Grossstadt, data = bevoelkerung.10000)
summary(Multi_LinearesModel_Rauchverhalten_Grossstadt)

schaetzwert <- predict(Multi_LinearesModel_Rauchverhalten_Grossstadt,bevoelkerung.10000, type= "response")#Prognosewerte aus den Trainingsdaten ermitteln
library(pROC)
roc(bevoelkerung.10000$krebs,schaetzwert,plot=TRUE)
#AUC = 0.8166

#Rauchverhalten + Weiblich
Multi_LinearesModel_Rauchverhalten_Weiblich <- lm (formula = krebs ~ rauchverhalten + weiblich, data = bevoelkerung.10000)
summary(Multi_LinearesModel_Rauchverhalten_Weiblich)

schaetzwert <- predict(Multi_LinearesModel_Rauchverhalten_Weiblich,bevoelkerung.10000, type= "response")#Prognosewerte aus den Trainingsdaten ermitteln
library(pROC)
roc(bevoelkerung.10000$krebs,schaetzwert,plot=TRUE)
#AUC = 0.8198

#Kein Krebs Familie + Alter
Multi_LinearesModel_KeinKrebsFamilie_Alter <- lm (formula = krebs ~ kein.krebs.familie + alter, data = bevoelkerung.10000)
summary(Multi_LinearesModel_KeinKrebsFamilie_Alter)

schaetzwert <- predict(Multi_LinearesModel_KeinKrebsFamilie_Alter,bevoelkerung.10000, type= "response")#Prognosewerte aus den Trainingsdaten ermitteln
library(pROC)
roc(bevoelkerung.10000$krebs,schaetzwert,plot=TRUE)
#AUC =  0.7156

#Kein Krebs Familie+ Grossstadt
Multi_LinearesModel_KeinKrebsFamilie_Grossstadt <- lm (formula = krebs ~ kein.krebs.familie + Grossstadt, data = bevoelkerung.10000)
summary(Multi_LinearesModel_KeinKrebsFamilie_Grossstadt)

schaetzwert <- predict(Multi_LinearesModel_KeinKrebsFamilie_Grossstadt,bevoelkerung.10000, type= "response")#Prognosewerte aus den Trainingsdaten ermitteln
library(pROC)
roc(bevoelkerung.10000$krebs,schaetzwert,plot=TRUE)
#AUC = 0.6895

#Kein Krebs Familie + Weiblich
Multi_LinearesModel_KeinKrebsFamilie_Weiblich <- lm (formula = krebs ~ kein.krebs.familie + weiblich, data = bevoelkerung.10000)
summary(Multi_LinearesModel_KeinKrebsFamilie_Weiblich)

schaetzwert <- predict(Multi_LinearesModel_KeinKrebsFamilie_Weiblich,bevoelkerung.10000, type= "response")#Prognosewerte aus den Trainingsdaten ermitteln
library(pROC)
roc(bevoelkerung.10000$krebs,schaetzwert,plot=TRUE)
#AUC = 0.6933

#Alter + Grossstadt
Multi_LinearesModel_Alter_Grossstadt <- lm (formula = krebs ~ alter + Grossstadt, data = bevoelkerung.10000)
summary(Multi_LinearesModel_Alter_Grossstadt)

schaetzwert <- predict(Multi_LinearesModel_Alter_Grossstadt,bevoelkerung.10000, type= "response")#Prognosewerte aus den Trainingsdaten ermitteln
library(pROC)
roc(bevoelkerung.10000$krebs,schaetzwert,plot=TRUE)
#AUC = 0.5567

#Alter + Weiblich
Multi_LinearesModel_Alter_Weiblich <- lm (formula = krebs ~ alter + weiblich, data = bevoelkerung.10000)
summary(Multi_LinearesModel_Alter_Weiblich)

schaetzwert <- predict(Multi_LinearesModel_Alter_Weiblich,bevoelkerung.10000, type= "response")#Prognosewerte aus den Trainingsdaten ermitteln
library(pROC)
roc(bevoelkerung.10000$krebs,schaetzwert,plot=TRUE)
#AUC =0.5562

#Grossstadt + Weiblich
Multi_LinearesModel_Grossstadt_Weiblich <- lm (formula = krebs ~ Grossstadt + weiblich, data = bevoelkerung.10000)
summary(Multi_LinearesModel_Grossstadt_Weiblich)

schaetzwert <- predict(Multi_LinearesModel_Grossstadt_Weiblich,bevoelkerung.10000, type= "response")#Prognosewerte aus den Trainingsdaten ermitteln
library(pROC)
roc(bevoelkerung.10000$krebs,schaetzwert,plot=TRUE)
#AUC = 0.5216 


#Ideales Multivariates lineares Modell mit 2 Variablen
Multi_LinearesModel_Rauchverhalten_KeinKrebsFamilie <- lm (formula = krebs ~ rauchverhalten + kein.krebs.familie, data = bevoelkerung.10000)
summary(Multi_LinearesModel_Rauchverhalten_KeinKrebsFamilie)

schaetzwert <- predict(Multi_LinearesModel_Rauchverhalten_KeinKrebsFamilie,bevoelkerung.10000, type= "response")#Prognosewerte aus den Trainingsdaten ermitteln
library(pROC)
roc(bevoelkerung.10000$krebs,schaetzwert,plot=TRUE)
#AUC = 0.9573

#_______________________________________________________________________________________________

# Aufgabe 3c  (beste Modelle)

# Modell Nummer 1 mit 1 Variable (lineares)
LinearesModel_Rauchverhalten <- lm (formula = krebs ~ rauchverhalten, data = bevoelkerung.10000)
summary(LinearesModel_Rauchverhalten)

# Modell Nummer 2 mit mit 2 Variablen (lineares)
Multi_LinearesModel_Rauchverhalten_KeinKrebsFamilie <- lm (formula = krebs ~ rauchverhalten + kein.krebs.familie, data = bevoelkerung.10000)
summary(Multi_LinearesModel_Rauchverhalten_KeinKrebsFamilie)

# Modell Nummer 3 Multivariates logistisches Modell unter Einbeziehung aller beschreibenden Variablen , sodass Fehlerrate der Stichprobe minimiert wird
#install.packages("Metrics")
library(Metrics)

LogistischesModellMSE<- glm(krebs ~ .,data = bevoelkerung.10000,family=binomial("logit"))
summary(LogistischesModellMSE)

predictKrebs <- predict(LogistischesModellMSE, newdata = bevoelkerung.10000, type="response")
mse.log.training = mse(predictKrebs[which(!is.nan(predictKrebs))], bevoelkerung.10000$krebs[which(!is.nan(predictKrebs))])
#Ist gleich zu, aber schoener mseValue=sum((bevoelkerung.10000$krebs[which(!is.nan(predictKrebs))] - predictKrebs[which(!is.nan(predictKrebs))])^2)/length(predictKrebs[which(!is.nan(predictKrebs))])
print(mse.log.training) #MSE = 0.01398492

predictKrebs <- predict(LogistischesModellMSE, bevoelkerung.mod.2, type="response")
mse.log.test = mse(predictKrebs[which(!is.nan(predictKrebs))], bevoelkerung.mod.2$krebs[which(!is.nan(predictKrebs))])
#Ist gleich zu, aber schoener mseValue=sum((bevoelkerung.10000$krebs[which(!is.nan(predictKrebs))] - predictKrebs[which(!is.nan(predictKrebs))])^2)/length(predictKrebs[which(!is.nan(predictKrebs))])
print(mse.log.test) #MSE = 




#_________________________________________  

#Aufgabe 3d Berechnung der MSE mit N = 10.000 Stück

# Modell Nummer 1 mit 1 Variable (lineares)
LinearesModel_Rauchverhalten <- lm (formula = krebs ~ rauchverhalten, data = bevoelkerung.10000)
summary(LinearesModel_Rauchverhalten)

predictKrebs <- predict(LinearesModel_Rauchverhalten, bevoelkerung.10000)
mse.lin.training = mse(predictKrebs, bevoelkerung.10000$krebs)
print(mse.lin.training) #0.1449588

#Berechnung der MSE mit N = 1.000.000 Stück

# Modell Nummer 1 mit 1 Variable (lineares)
LinearesModel_Rauchverhalten <- lm (formula = krebs ~ rauchverhalten, data = bevoelkerung.10000)
summary(LinearesModel_Rauchverhalten)

predictKrebs <- predict(LinearesModel_Rauchverhalten, bevoelkerung.mod.2)
mse.lin.test = mse(predictKrebs, bevoelkerung.mod.2$krebs)
print(mse.lin.test) #0.1429009

#_________________________________________  


# Modell Nummer 2 mit mit 2 Variablen (lineares) N = 10.000 Stück
Multi_LinearesModel_Rauchverhalten_KeinKrebsFamilie <- lm (formula = krebs ~ rauchverhalten + kein.krebs.familie, data = bevoelkerung.10000)
summary(Multi_LinearesModel_Rauchverhalten_KeinKrebsFamilie)
predictKrebs <- predict(Multi_LinearesModel_Rauchverhalten_KeinKrebsFamilie, bevoelkerung.10000)
mse.mult2.training = mse(predictKrebs, bevoelkerung.10000$krebs)
print(mse.mult2.training) #0.09566084


# Modell Nummer 2 mit mit 2 Variablen (lineares) mit N = 1.000.000 Stück
Multi_LinearesModel_Rauchverhalten_KeinKrebsFamilie <- lm (formula = krebs ~ rauchverhalten + kein.krebs.familie, data = bevoelkerung.10000)
summary(Multi_LinearesModel_Rauchverhalten_KeinKrebsFamilie)
predictKrebs <- predict(Multi_LinearesModel_Rauchverhalten_KeinKrebsFamilie, bevoelkerung.mod.2)
mse.mult2.test = mse(predictKrebs, bevoelkerung.mod.2$krebs)
print(mse.mult2.test) #0.09197751

#_________________________________________   


#Multivariates logistisches Modell mit small mse 

trainingsDaten = as.data.frame(as.matrix(bevoelkerung.10000)[which(!is.nan(bevoelkerung.10000$bewegung)), 1:11])
LogistischesModellMSE_optimierung <- glm(krebs ~., data = trainingsDaten,family=binomial("logit"))
predictKrebs <- predict(LogistischesModellMSE_optimierung, trainingsDaten, type="response")
mse.log.all.training = mse(predictKrebs, trainingsDaten$krebs)
print(mse.log.all.training) #0.02142411


# Modell Nummer 3 Multivariates logistisches Modell mit small mse  mit N = 1.000.000 Stück unter Einbeziehung aller vorhandener unabhängiger Variablen
#install.packages("Metrics")
library(Metrics)

trainingsDaten = as.data.frame(as.matrix(bevoelkerung.10000)[which(!is.nan(bevoelkerung.10000$bewegung)), 1:11])

LogistischesModellMSE_optimierung <- glm(krebs ~., data = trainingsDaten,family=binomial("logit"))
summary (LogistischesModellMSE_optimierung)

testDaten = as.data.frame(as.matrix(bevoelkerung.mod.2)[which(!is.nan(bevoelkerung.mod.2$bewegung)), 1:11])
predictKrebs <- predict(LogistischesModellMSE_optimierung, testDaten, type="response")
mse.log.all.test = mse(predictKrebs, testDaten$krebs)
print(mse.log.all.test) #0.02254959


mse.matrix.training <- c(mse.lin.training, mse.mult2.training,mse.log.training, mse.log.all.training)
mse.matrix.test <- c(mse.lin.test, mse.mult2.test, mse.log.test, mse.log.all.test)

anzahl.variablen <- c(1,2,5, 10)

# mse.training.dataframe <- data.frame (anzahl.variablen, mse.matrix.training)
# mse.test.dataframe <- data.frame (anzahl.variablen, mse.matrix.test)

plot( anzahl.variablen, mse.matrix.training, col="green", type = "o", xlab = "Freiheitsgrade", ylab= "MSE")
# par (new=TRUE)
# plot(mse.test.dataframe$anzahl.variablen, mse.test.dataframe$mse.matrix.test, col="red")
lines (anzahl.variablen, mse.matrix.test, type = "o")


LinearesModel_Rauchverhalten <- lm (formula = krebs ~ rauchverhalten, data = bevoelkerung.10000)
summary(LinearesModel_Rauchverhalten)

predictKrebs <- predict(LinearesModel_Rauchverhalten, bevoelkerung.10000)
mse.lin.training = mse(predictKrebs, bevoelkerung.10000$krebs)
print(mse.lin.training)


#_________________________________________   

#Modell Nummer 4 das beste Modell von der Aufgabe 2a mit N = 1.000

testM <- glm(krebs ~ rauchverhalten + kein.krebs.familie + alter + fleischkonsum + weiblich, data= bevoelkerung.10000, family = binomial("logit"))
summary(testM)
#https://www.theanalysisfactor.com/r-tutorial-glm1/
predictKrebs <- predict(testM, bevoelkerung.10000, type="response")
mseValue = mse(predictKrebs, bevoelkerung.10000$krebs)
print(mseValue) #0.02142175
