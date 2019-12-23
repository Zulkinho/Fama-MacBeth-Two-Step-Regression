#knjižnice
library(lmtest)


#uvoz podatkov
#podatki so že prečiščeni, da se datumsko ujemajo

Portfolios_100 <- read.csv("data/100_Portfolios_ME_OP_10x10.csv", header = TRUE, sep = ",")
#100_Portfolios[Portfolios_100== -99.99]=NA
View(Portfolios_100)

ff5 <- read.csv("data/F-F_Research_Data_5_Factors_2x3.csv", header = TRUE, sep = ",")
View(ff5)

ff_momentum <- read.csv("data/F-F_Momentum_Factor.csv", header = TRUE, sep = ",")
View(ff_momentum)

#regresija

for (i in (2:ncol(Portfolios_100))){
  y<-Portfolios_100[i]-ff5[7]
  y<-as.matrix(y)
  Mkt.RF<-as.matrix(ff5[2])
  SMB<-as.matrix(ff5[3])
  HML<-as.matrix(ff5[4])
  RMW<-as.matrix(ff5[5])
  CMA<-as.matrix(ff5[6])
  fit<-lm(y~Mkt.RF+SMB+HML+RMW+CMA)}

#adding momentum factor

for (i in (2:ncol(Portfolios_100))){
  y<-Portfolios_100[i]-ff5[7]
  y<-as.matrix(y)
  Mkt.RF<-as.matrix(ff5[2])
  SMB<-as.matrix(ff5[3])
  HML<-as.matrix(ff5[4])
  RMW<-as.matrix(ff5[5])
  CMA<-as.matrix(ff5[6])
  MOM<-as.matrix((ff_momentum[2]))
  fit<-lm(y~Mkt.RF+SMB+HML+RMW+CMA+MOM)}

# orthogonalized Rm − rf

for (i in (2:ncol(Portfolios_100))){
  y<-Portfolios_100[i]-ff5[7]
  y<-ortogonali
  y<-as.matrix(y)
  y <- mlr.orthogonalize(y, normalize = FALSE)
  Mkt.RF<-as.matrix(ff5[2])
  SMB<-as.matrix(ff5[3])
  HML<-as.matrix(ff5[4])
  RMW<-as.matrix(ff5[5])
  CMA<-as.matrix(ff5[6])
  MOM<-as.matrix((ff_momentum[2]))
  fit<-lm(y~Mkt.RF+SMB+HML+RMW+CMA+MOM)}
