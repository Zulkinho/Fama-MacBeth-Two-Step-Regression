#CODE

#2 Fama-MacBeth Regressions

#knjižnica za izvedbo select stavka
library(dplyr)
library(sandwich)
library(lmtest)
library(PerformanceAnalytics)

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

#we get betas from
for (i in (2:41)){
y<-data1[i]-ff3[5]
y<-as.matrix(y)
x<-ff3[2]
x<-as.matrix(x)
fit<-lm(y~x)
a<-coeftest(fit,vcov=NeweyWest(fit,verbose=T))
alphas<-c()
alphas[i-1] <- a[1]
}
alphas

