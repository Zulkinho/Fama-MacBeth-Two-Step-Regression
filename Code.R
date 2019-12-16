#CODE

#2 Fama-MacBeth Regressions

#knjižnica za izvedbo select stavka
library(dplyr)
library(sandwich)
library(lmtest)

#uvoz podatkov

############################################################################
# This file was created by CMPT_IND_RETS using the 201910 CRSP database.   #
# It contains value- and equal-weighted returns for 49 industry portfolios.#
# The portfolios are constructed at the end of June.                       #
# The annual returns are from January to December.                         #
# Missing data are indicated by -99.99 or -999.                            #
############################################################################

Industry49_data <- read.csv("data/49_Industry_Portfolios.csv", header = TRUE, sep = ",")

#View(Industry49_data)

#naključna izbira 40 industrij
#
naključnaizbirastolpcev<-(sample(colnames(Industry49_data[2:50]), 40))

data1<-select(Industry49_data,Date,naključnaizbirastolpcev)

View(data1)

#1. pasage

#OLS 
# 
# temp.lm = lm( ~ data1[2]+data1[3]+data1[4]+data1[5]+data1[6]+data1[7]+data1[8]+data1[9]+data1[10]+data1[11]+data1[12]+data1[13]+data1[14]+data1[15]+data1[16]+data1[17]+data1[18]+data1[19]+data1[20]+data1[21]+data1[22]+data1[23]+data1[24]+data1[25]+data1[26]+data1[27]+data1[28]+data1[29]+data1[30]+data1[31]+data1[32]+data1[33]+data1[34]+data1[35]+data1[36]+data1[37]+data1[38]+data1[39]+data1[40]+data1[41]+data1[42])
# temp.summ <- summary(temp.lm)
# temp.summ$coefficients <- unclass(coeftest(temp.lm, vcov. = NeweyWest))
# 
# print (temp.summ$coefficients)



# fm <- lm(RealInv ~ RealGNP + RealInt, data = Investment)
# ## Newey & West (1994) compute this type of estimator
# NeweyWest(fm)
# ## The Newey & West (1987) estimator requires specification
# ## of the lag and suppression of prewhitening
# NeweyWest(fm, lag = 4, prewhite = FALSE)
# ## bwNeweyWest() can also be passed to kernHAC(), e.g.
# ## for the quadratic spectral kernel
# kernHAC(fm, bw = bwNeweyWest)
