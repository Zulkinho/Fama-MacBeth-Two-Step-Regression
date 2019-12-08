#CODE
<<<<<<< HEAD
=======
<<<<<<< HEAD
>>>>>>> 7b2d8036b2af8a95bee038754096a46a1ec315a6

#2 Fama-MacBeth Regressions

#knjižnica za izvedbo select stavka
library(dplyr)
<<<<<<< HEAD
=======
=======
>>>>>>> 39adbe939d749ef1b70a64ac5972745d2985cc95
>>>>>>> 7b2d8036b2af8a95bee038754096a46a1ec315a6

#uvoz podatkov

Industry49_data <- read.csv("data/49_Industry_Portfolios.csv", header = TRUE, sep = ",")
<<<<<<< HEAD
=======
<<<<<<< HEAD
>>>>>>> 7b2d8036b2af8a95bee038754096a46a1ec315a6
#View(Industry49_data)

#naključna izbira 40 industrij
#
naključnaizbirastolpcev<-(sample(colnames(Industry49_data[2:50]), 40))

data1<-select(Industry49_data,Date,naključnaizbirastolpcev)
<<<<<<< HEAD
=======
=======
View(Industry49_data)
>>>>>>> 39adbe939d749ef1b70a64ac5972745d2985cc95
>>>>>>> 7b2d8036b2af8a95bee038754096a46a1ec315a6

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
