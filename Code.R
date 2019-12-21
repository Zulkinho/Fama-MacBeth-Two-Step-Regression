#CODE

#2 Fama-MacBeth Regressions

#knjižnica za izvedbo select stavka
library(dplyr)
library(sandwich)
library(lmtest)

#uvoz podatkov
#pri obeh moramo dodati datum ročno da je enako število imens stolpcev kot stolpcev

############################################################################
# This file was created by CMPT_IND_RETS using the 201910 CRSP database.   #
# It contains value- and equal-weighted returns for 49 industry portfolios.#
# The portfolios are constructed at the end of June.                       #
# The annual returns are from January to December.                         #
# Missing data are indicated by -99.99 or -999.                            #
############################################################################

Industry49_data <- read.csv("data/49_Industry_Portfolios.csv", header = TRUE, sep = ",")
#Industry49_data[Industry49_data== -99.99]=NA
View(Industry49_data)

############################################################################
# This file was created by CMPT_IND_RETS using the 201910 CRSP database.   #
# It contains value- and equal-weighted returns for 49 industry portfolios.#
# The portfolios are constructed at the end of June.                       #
# The annual returns are from January to December.                         #
# Missing data are indicated by -99.99 or -999.                            #
############################################################################

FF_Research_Data_Factors <- read.csv("data/F-F_Research_Data_Factors.csv", header = TRUE, sep = ",")
View(FF_Research_Data_Factors)
ff3<-FF_Research_Data_Factors

#naključna izbira 40 industrij
#
naključnaizbirastolpcev<-(sample(colnames(Industry49_data[2:50]), 40))

data1<-select(Industry49_data,Date,naključnaizbirastolpcev)
View(data1)

#1. pasage

#we get alphas from
alphas<-c()
for (i in (2:41)){
y<-data1[i]-ff3[5]
y<-as.matrix(y)
x<-ff3[2]
x<-as.matrix(x)
fit<-lm(y~x)
a<-coeftest(fit,vcov=NeweyWest(fit,verbose=T))
alphas[i-1] <- a[1]
}
#alphe za posamezne industrije
alphas

#we get betas from
betas<-c()
for (i in (2:41)){
  y<-data1[i]-ff3[5]
  y<-as.matrix(y)
  x<-ff3[2]
  x<-as.matrix(x)
  fit<-lm(y~x)
  a<-coeftest(fit,vcov=NeweyWest(fit,verbose=T))
  betas[i-1] <- a[2]
}
names(betas)<-names(data1)[2:41]
#bete za posamezne industrije
betas

#urejanje bete po velikosti
betas_sorted<-sort(betas)
betas_sorted
#glede na velikost bete sedaj razdelimo industrije v 10 portfeljev
portfolio1<-data1[names(betas_sorted)[1:4]]
portfolio2<-data1[names(betas_sorted)[5:8]]
portfolio3<-data1[names(betas_sorted)[9:12]]
portfolio4<-data1[names(betas_sorted)[13:16]]
portfolio5<-data1[names(betas_sorted)[17:20]]
portfolio6<-data1[names(betas_sorted)[21:24]]
portfolio7<-data1[names(betas_sorted)[25:28]]
portfolio8<-data1[names(betas_sorted)[29:32]]
portfolio9<-data1[names(betas_sorted)[33:36]]
portfolio10<-data1[names(betas_sorted)[37:40]]

#seštejemo vrednosti za vsak mesec da dobimo return od celotnega portfelja
#priportfelju 1 ne seštevamo ampak vzamemo samo en stolpec,
#saj je sestavljen iz vrednosti, ki pomenijo "missing data are indicated by -99.99 or -999."

portfolio1_returns<-portfolio1[1]
names(portfolio1_returns)<-"port1"

# portfolio2_returns<-portfolio2[1]+portfolio2[2]+portfolio2[3]+portfolio2[4]
# names(portfolio2_returns)<-"port2"

portfolio2_returns<-portfolio2[1]+portfolio2[2]+portfolio2[3]+portfolio2[4]
names(portfolio2_returns)<-"port2"

portfolio3_returns<-portfolio3[1]+portfolio3[2]+portfolio3[3]+portfolio3[4]
names(portfolio3_returns)<-"port3"

portfolio4_returns<-portfolio4[1]+portfolio4[2]+portfolio4[3]+portfolio4[4]
names(portfolio4_returns)<-"port4"

portfolio5_returns<-portfolio5[1]+portfolio5[2]+portfolio5[3]+portfolio5[4]
names(portfolio5_returns)<-"port5"

portfolio6_returns<-portfolio6[1]+portfolio6[2]+portfolio6[3]+portfolio6[4]
names(portfolio6_returns)<-"port6"

portfolio7_returns<-portfolio7[1]+portfolio7[2]+portfolio7[3]+portfolio7[4]
names(portfolio7_returns)<-"port7"

portfolio8_returns<-portfolio8[1]+portfolio8[2]+portfolio8[3]+portfolio8[4]
names(portfolio8_returns)<-"port8"

portfolio9_returns<-portfolio9[1]+portfolio9[2]+portfolio9[3]+portfolio9[4]
names(portfolio9_returns)<-"port9"

portfolio10_returns<-portfolio10[1]+portfolio10[2]+portfolio10[3]+portfolio10[4]
names(portfolio10_returns)<-"port10"

#tabela vseh portfeljev
portfelji_skupaj<-data.frame(data1[1],portfolio1_returns,portfolio2_returns,portfolio3_returns,portfolio4_returns,portfolio5_returns,portfolio6_returns,portfolio7_returns,portfolio8_returns,portfolio9_returns,portfolio10_returns)
View(portfelji_skupaj)

#1. Pasage for portfolios
#we get portfolios alphas from
alphas_p<-c()
for (i in (2:11)){
  y<-portfelji_skupaj[i]-ff3[5]
  y<-as.matrix(y)
  x<-ff3[2]
  x<-as.matrix(x)
  fit<-lm(y~x)
  a<-coeftest(fit,vcov=NeweyWest(fit,verbose=T))
  alphas_p[i-1] <- a[1]
}
alphas_p
names(alphas_p)<-names(portfelji_skupaj)[2:11]

#we get portfolios betas from
betas_p<-c()
for (i in (2:11)){
  y<-portfelji_skupaj[i]-ff3[5]
  y<-as.matrix(y)
  x<-ff3[2]
  x<-as.matrix(x)
  fit<-lm(y~x)
  a<-coeftest(fit,vcov=NeweyWest(fit,verbose=T))
  betas_p[i-1] <- a[2]
}
betas_p
names(betas_p)<-names(portfelji_skupaj)[2:11]

#
sum(portfelji_skupaj[2])
