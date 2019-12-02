#CODE

#naloga1
#knjižnica za izvedbo select stavka
library(dplyr)

#uvoz podatkov

Industry49_data <- read.csv("data/49_Industry_Portfolios.csv", header = TRUE, sep = ",")
#View(Industry49_data)

#naključna izbira 40 industrij
#
naključnaizbirastolpcev<-(sample(colnames(Industry49_data[2:50]), 40))

data1<-select(Industry49_data,Date,naključnaizbirastolpcev)

