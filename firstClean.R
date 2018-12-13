rm(list=ls(all=TRUE))
airbnb_uncleaned = read.csv("airbnb_uncleaned.csv")
attach(airbnb_uncleaned)

# removing useless variables
airbnb = airbnb_uncleaned[,-c(2,3,4,5,6,9,10,11,16,17,18,19,20,21,22,24,27,28,30,31,32,38,39,40,41,42,43,45,46,47,48,62,63,70,76,87,88,89)]

airbnb2 = na.omit(airbnb$square_feet)
airbnb3 = airbnb[!is.na(airbnb$square_feet),]
airbnb4 = airbnb3[airbnb3$square_feet >= 70,]

#Creating Avergae Price per Square Foot
airbnb4$averagepricesquarefeet = as.numeric(airbnb4$price)*365*0.8 / as.numeric(airbnb4$square_feet)

#Creating the Occupancy Rate Column
airbnb4$occupancy_rate = (((airbnb4$reviews_per_month*2)*5.2)/30)
attach(airbnb4)

#Saving this data as a new csv
write.csv(airbnb4, file = "Airbnb2.csv")
