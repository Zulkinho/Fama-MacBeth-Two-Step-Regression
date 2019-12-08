#CODE
<<<<<<< HEAD

#naloga1
#knjižnica za izvedbo select stavka
library(dplyr)
=======
>>>>>>> 39adbe939d749ef1b70a64ac5972745d2985cc95

#uvoz podatkov

Industry49_data <- read.csv("data/49_Industry_Portfolios.csv", header = TRUE, sep = ",")
<<<<<<< HEAD
#View(Industry49_data)

#naključna izbira 40 industrij
#
naključnaizbirastolpcev<-(sample(colnames(Industry49_data[2:50]), 40))

data1<-select(Industry49_data,Date,naključnaizbirastolpcev)
=======
View(Industry49_data)
>>>>>>> 39adbe939d749ef1b70a64ac5972745d2985cc95

