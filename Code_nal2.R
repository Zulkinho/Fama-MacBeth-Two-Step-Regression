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
koeficienti1<-matrix(nrow = (ncol(Portfolios_100)-1) , ncol = 6)
for (i in (2:ncol(Portfolios_100))){
  y<-Portfolios_100[i]-ff5[7]
  y<-as.matrix(y)
  Mkt.RF<-as.matrix(ff5[2])
  SMB<-as.matrix(ff5[3])
  HML<-as.matrix(ff5[4])
  RMW<-as.matrix(ff5[5])
  CMA<-as.matrix(ff5[6])
  fit<-lm(y~Mkt.RF+SMB+HML+RMW+CMA)
  a<-coeftest(fit)
  koeficienti1[i-1,1]<-a[1]
  koeficienti1[i-1,2]<-a[2]
  koeficienti1[i-1,3]<-a[3]
  koeficienti1[i-1,4]<-a[4]
  koeficienti1[i-1,5]<-a[5]
  koeficienti1[i-1,6]<-a[6]}
koeficienti1

#adding momentum factor
koeficienti2<-matrix(nrow = (ncol(Portfolios_100)-1) , ncol = 7)
for (i in (2:ncol(Portfolios_100))){
  y<-Portfolios_100[i]-ff5[7]
  y<-as.matrix(y)
  Mkt.RF<-as.matrix(ff5[2])
  SMB<-as.matrix(ff5[3])
  HML<-as.matrix(ff5[4])
  RMW<-as.matrix(ff5[5])
  CMA<-as.matrix(ff5[6])
  MOM<-as.matrix((ff_momentum[2]))
  fit<-lm(y~Mkt.RF+SMB+HML+RMW+CMA+MOM)
  a<-coeftest(fit)
  koeficienti2[i-1,1]<-a[1]
  koeficienti2[i-1,2]<-a[2]
  koeficienti2[i-1,3]<-a[3]
  koeficienti2[i-1,4]<-a[4]
  koeficienti2[i-1,5]<-a[5]
  koeficienti2[i-1,6]<-a[6]
  koeficienti2[i-1,7]<-a[7]}
koeficienti2

# orthogonalized Rm − rf

x<-ff5[2]
gramschmidt <- function(x) {
  x <- as.matrix(x)
  # Get the number of rows and columns of the matrix
  n <- ncol(x)
  m <- nrow(x)
  
  # Initialize the Q and R matrices
  q <- matrix(0, m, n)
  r <- matrix(0, n, n)
  
  for (j in 1:n) {
    v = x[,j] # Step 1 of the Gram-Schmidt process v1 = a1
    # Skip the first column
    if (j > 1) {
      for (i in 1:(j-1)) {
        r[i,j] <- t(q[,i]) %*% x[,j] # Find the inner product (noted to be q^T a earlier)
        # Subtract the projection from v which causes v to become perpendicular to all columns of Q
        v <- v - r[i,j] * q[,i] 
      }      
    }
    # Find the L2 norm of the jth diagonal of R
    r[j,j] <- sqrt(sum(v^2))
    # The orthogonalized result is found and stored in the ith column of Q.
    q[,j] <- v / r[j,j]
  }
  
  # Collect the Q and R matrices into a list and return
  qrcomp <- q
  return(qrcomp)
}
x<-gramschmidt(x)

koeficienti3<-matrix(nrow = (ncol(Portfolios_100)-1) , ncol = 6)
for (i in (2:ncol(Portfolios_100))){
  y<-Portfolios_100[i]-ff5[7]
  y<-as.matrix(y)
  Mkt.RF<-as.matrix(x)
  SMB<-as.matrix(ff5[3])
  HML<-as.matrix(ff5[4])
  RMW<-as.matrix(ff5[5])
  CMA<-as.matrix(ff5[6])
  fit<-lm(y~Mkt.RF+SMB+HML+RMW+CMA)
  a<-coeftest(fit)
  koeficienti3[i-1,1]<-a[1]
  koeficienti3[i-1,2]<-a[2]
  koeficienti3[i-1,3]<-a[3]
  koeficienti3[i-1,4]<-a[4]
  koeficienti3[i-1,5]<-a[5]
  koeficienti3[i-1,6]<-a[6]}
koeficienti3

koeficienti4<-matrix(nrow = (ncol(Portfolios_100)-1) , ncol = 7)
for (i in (2:ncol(Portfolios_100))){
  y<-Portfolios_100[i]-ff5[7]
  y<-as.matrix(y)
  Mkt.RF<-as.matrix(x)
  SMB<-as.matrix(ff5[3])
  HML<-as.matrix(ff5[4])
  RMW<-as.matrix(ff5[5])
  CMA<-as.matrix(ff5[6])
  MOM<-as.matrix((ff_momentum[2]))
  fit<-lm(y~Mkt.RF+SMB+HML+RMW+CMA+MOM)
  a<-coeftest(fit)
  koeficienti4[i-1,1]<-a[1]
  koeficienti4[i-1,2]<-a[2]
  koeficienti4[i-1,3]<-a[3]
  koeficienti4[i-1,4]<-a[4]
  koeficienti4[i-1,5]<-a[5]
  koeficienti4[i-1,6]<-a[6]
  koeficienti4[i-1,7]<-a[7]}
koeficienti4
