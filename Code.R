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
ff3_original<-FF_Research_Data_Factors

#naključna izbira 40 industrij
#
naključnaizbirastolpcev<-(sample(colnames(Industry49_data[2:50]), 40))

data0<-select(Industry49_data,Date,naključnaizbirastolpcev)
View(data0)

#1. pasage
#from 071926-121929
z<-6+12*3
data1<-data0[1:z,]
ff3<-ff3_original[1:42,]

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
portfolio1<-data0[names(betas_sorted)[1:4]]
portfolio2<-data0[names(betas_sorted)[5:8]]
portfolio3<-data0[names(betas_sorted)[9:12]]
portfolio4<-data0[names(betas_sorted)[13:16]]
portfolio5<-data0[names(betas_sorted)[17:20]]
portfolio6<-data0[names(betas_sorted)[21:24]]
portfolio7<-data0[names(betas_sorted)[25:28]]
portfolio8<-data0[names(betas_sorted)[29:32]]
portfolio9<-data0[names(betas_sorted)[33:36]]
portfolio10<-data0[names(betas_sorted)[37:40]]

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
portfelji_skupaj_original<-data.frame(data0[1],portfolio1_returns,portfolio2_returns,portfolio3_returns,portfolio4_returns,portfolio5_returns,portfolio6_returns,portfolio7_returns,portfolio8_returns,portfolio9_returns,portfolio10_returns)
View(portfelji_skupaj)

#1.Pasage for portfolios

#data from 011930-121934
portfelji_skupaj<-portfelji_skupaj_original[(z+1):102,]
ff3<-ff3_original[(z+1):102,]

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
betas_p<-as.matrix(betas_p)
#
# avg_values<-c()
# for(i in (2:11)){
#   avg_values[i-1]<- sum(portfelji_skupaj[i])/nrow(portfelji_skupaj)
# }
# names(avg_values)<-names(portfelji_skupaj)[2:11]
# avg_values<-as.matrix(avg_values)

#2.Pasage for portfolios

#data from 011935-121938
portfelji_skupaj<-portfelji_skupaj_original[103:150,]
ff3<-ff3_original[103:150,]

#bete v tebelo primerno za regresijo
output<-data.frame(matrix(ncol=nrow(betas_p), nrow=nrow(portfelji_skupaj)))
for(i in (1:nrow(betas_p))){
  output[,i] = betas_p[i,]
}
output

aa<-c()
for (i in (2:11)){
  y<-portfelji_skupaj[i]
  y<-as.matrix(y)
  x<-output[,i-1]
  fit<-lm(y~x)
  a<-coeftest(fit,vcov=NeweyWest(fit,verbose=T))
}

for (i in (2:11)){
  y<-portfelji_skupaj[i]
  y<-as.matrix(y)
  x<-output[,i-1]
  x<-as.matrix(x)
  fit<-lm(y~X^2)
  a<-coeftest(fit,vcov=NeweyWest(fit,verbose=T))
}

for (i in (2:11)){
  y<-portfelji_skupaj[i]
  y<-as.matrix(y)
  x<-output[,i-1]
  x<-x^2
  X<-as.matrix(x)
  fit<-lm(y~X)
  a<-coeftest(fit,vcov=NeweyWest(fit,verbose=T))
}
