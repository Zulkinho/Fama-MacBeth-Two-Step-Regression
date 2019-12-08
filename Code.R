#CODE

#2 Fama-MacBeth Regressions

#knjižnica za izvedbo select stavka
library(dplyr)

#uvoz podatkov

Industry49_data <- read.csv("data/49_Industry_Portfolios.csv", header = TRUE, sep = ",")
#View(Industry49_data)

#naključna izbira 40 industrij
#
naključnaizbirastolpcev<-(sample(colnames(Industry49_data[2:50]), 40))

data1<-select(Industry49_data,Date,naključnaizbirastolpcev)

#1. pasage

#OLS 
# 

library(sandwich)
library(lmtest)

# fm <- lm(RealInv ~ RealGNP + RealInt, data = Investment)
# ## Newey & West (1994) compute this type of estimator
# NeweyWest(fm)
# ## The Newey & West (1987) estimator requires specification
# ## of the lag and suppression of prewhitening
# NeweyWest(fm, lag = 4, prewhite = FALSE)
# ## bwNeweyWest() can also be passed to kernHAC(), e.g.
# ## for the quadratic spectral kernel
# kernHAC(fm, bw = bwNeweyWest)
