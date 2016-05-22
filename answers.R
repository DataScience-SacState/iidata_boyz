getPickupDay <- function(row){
  substr(row[2],9,10)
}

taxiData$day <- apply(taxiData,1,getPickupDay)
taxiData$day <- as.factor(taxiData$day)
summary(taxiData$day)


mondays <- c("07",14,21,28)
tuesdays <- c("01","08",15,22,29)
wednesdays <- c("02","09",16,23,30)
thursdays <- c("03",10,17,24,31)
fridays <- c("04",11,18,25)
saturdays <- c("06",13,20,27)
sundays <- c("05",12,19,26)


getWeekDay <- function(row){
  if(row[20] %in% mondays){
    return(1)
  }
  if(row[20] %in% tuesdays){
    return(2)
  }
  if(row[20] %in% wednesdays){
    return(3)
  }
  if(row[20] %in% thursdays){
    return(4)
  }
  if(row[20] %in% fridays){
    return(5)
  }
  if(row[20] %in% saturdays){
    return(6)
  }
  if(row[20] %in% sundays){
    return(0)
  }
  return(-1)
}

taxiData$weekDay <- as.factor(apply(taxiData,1,getWeekDay))
summary(taxiData$weekDay)

pickUpTime <- function(row){
  time <- substr(row[2],12,19)
  hour <- as.numeric(substr(time,1,2))*60
  minute <- as.numeric(substr(time,4,5))
  second <- as.numeric(substr(time,7,8))/60
  hour+minute+second
}

dropOffTime <- function(row){
  time <- substr(row[3],12,19)
  hour <- as.numeric(substr(time,1,2))*60
  minute <- as.numeric(substr(time,4,5))
  second <- as.numeric(substr(time,7,8))/60
  hour+minute+second
}
taxiData$pickUp <- apply(taxiData,1,pickUpTime)
taxiData$dropUp <- apply(taxiData,1,dropOffTime)


averageTime <- function(row){
  pick <- as.numeric(row[22])
  drop <- as.numeric(row[23])
  time <- drop - pick
  if(time < 0) time = time + 24*60
  time
}

taxiData$tripTime <- apply(taxiData,1,averageTime)
taxiData$pickUpHour <- floor(taxiData$pickUp) %% 24
taxiData$pickUpHour <- as.factor(taxiData$pickUpHour)                            
sapply(split(taxiData$tripTime,taxiData$pickUpHour),mean)
sapply(split(taxiData$tripTime,taxiData$weekDay),mean)


taxiData$speed <- ifelse(taxiData$tripTime == 0,NA,taxiData$trip_distance/taxiData$tripTime)
summary(taxiData$speed)
sapply(split(taxiData$speed,taxiData$pickUpHour),function(x) mean(x,na.rm=TRUE))

sapply(split(taxiData$tip_amount,taxiData$pickUpHour),mean)

vendor1 <- taxiData[which(taxiData$VendorID==1),]
vendor2 <- taxiData[which(taxiData$VendorID==2),]


##########20

hist(vendor1$passenger_count)
hist(vendor2$passenger_count)
hist(filter(vendor1, trip_distance<50)$trip_distance)
ggplot(taxiData, aes(x=trip_distance,y=fare_amount, color = RatecodeID)) +
    geom_point() + 
    facet_grid(.~VendorID)

mean(filter(vendor1, trip_distance<8)$fare_amount)
mean(filter(vendor2, trip_distance<8)$fare_amount)
