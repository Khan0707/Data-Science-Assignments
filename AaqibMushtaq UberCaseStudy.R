
library(stats)
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(gridExtra)

#Setting the working directory:
#setwd("F:/BIG DATA/IIITb/C2.Statistics and Exploratory Data Analytics/2.EDA/Uber Case Study")

#Importing the Uber data set into R:

Uber<- read.csv("Uber Request Data.csv",stringsAsFactors = FALSE)

#Understanding the structure of the data set:
 #str(Uber)

#Converting various columns to factor:

 Uber[2:4]<-lapply(Uber[2:4],factor)
 
 
#Converting dates to standard format using parse_date_time function of lubridate package:
 
 Uber$Request.timestamp<- parse_date_time(x = Uber$Request.timestamp, orders = c("dmYHM","dmYHMS"))
 Uber$Drop.timestamp<- parse_date_time(x =Uber$Drop.timestamp, orders = c("dmYHM","dmYHMS"))
 
 #calculating Total time taken in each trip:
 Uber$Total_Mins_Taken<-round(as.numeric(Uber$Drop.timestamp-Uber$Request.timestamp),2)
 
 
 #Separating Date and Time in separate columns:
 Uber<- separate(Uber, col = "Request.timestamp", into = c("RequestDate","RequestTime"), sep = " ")
 Uber<- separate(Uber, col = "Drop.timestamp", into = c("DropDate","DropTime"), sep = " ")
 
 #Separate Hours part from Time:
 
 Uber$Req_Hour <- sapply(str_split(Uber$RequestTime, pattern=":"), function(x) x[1][1])
 Uber$Drop_Hour<-sapply(str_split(Uber$DropTime, pattern=":"), function(x) x[1][1])
 
 #Converting above columns to factor
 Uber[10:11]<-lapply(Uber[10:11],factor)
 
 #Creating Time Slots (4 hour gap )based on Request time:
 
 Time <- hour(hm(substr(Uber$RequestTime,1,2)))
 Breaks <- hour(hm("00:00", "4:00","8:00", "12:00", "16:00","20:00","23:59"))
 Labels <- c("LateNight", "EarlyMorning", "Morning","Afternoon", "Evening","LateEvening")
 Uber$TimeSlot<- cut(x=Time, breaks=Breaks, labels=Labels, include.lowest=TRUE)
 
 #Extracting Day of the from Request time column:
 
 Uber$WeekOfDay<-wday(Uber$RequestDate,label = T,abbr = F)
 
 #Understanding the daywise demand and supply:
 table(Uber$WeekOfDay)  #No Requests made on Weekends
  
 # Sorting Data based on Driver.id and Request Date & Time:
 Uber<-Uber[order(Uber$Driver.id,Uber$RequestDate,Uber$RequestTime),]


 
 
 
 ######################################################### VISUALIZATION ############################### 
 
 
 #1.visualizing demands and supply made  at airport:
 
 # 1st Approach is to plot Req_Hour(as aesthetic) in a bar chart to get the count of respective
 #demand requests for airport as the pickup point:
 
 
 Airport<- filter(Uber,Uber$Pickup.point == "Airport")
 
 AirportDemand <- ggplot(Airport, aes(Req_Hour))+geom_bar(fill = "green") + 
   labs(title ="Requests at Airport(Demand)")+theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)
 
 #plot(AirportDemand)
 
 # Now, plotting Drop_Hour(as aesthetic) in a bar chart to get the count of respective
 # supplies  for airport as the pickup point:
 
 
 AirportSupply <- ggplot(filter(Airport, Airport$Drop_Hour!= 'NA'), aes(Drop_Hour))+geom_bar(fill = "red")+
   labs(title ="Supply at Airport(Hours)") + theme(plot.title = element_text(hjust = 0.5)) +ylim(0,500)
 
 #plot(AirportSupply)
 
#Like airport,now visualizing demands and supply made  at city:
 
 City<- filter(Uber,Uber$Pickup.point == "City")
 
 CityDemand <- ggplot(City, aes(Req_Hour))+ geom_bar(fill = "green") + labs(title ="Requests at City(Demand)")+
   theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)
 
 #plot(CityDemand)
 
 
 CitySupply <- ggplot(filter(City, City$Drop_Hour!= 'NA'), aes(Drop_Hour))+ geom_bar(fill = "red") + 
   labs(title ="Supply at City(Hours)")+
   theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)
 
 #plot(CitySupply)
 
 # Assembling the AirportDemand,AirportSupply,CityDemand,CitySupply in a single view:
 
 grid.arrange(AirportDemand,AirportSupply,CityDemand,CitySupply ,nrow = 2, ncol = 2)
 

 
 
 
 # 2nd Approach is to Plot AirportDemand,AirportSupply,CityDemand,CitySupply for different time slots we created:
 
 AirportDemand2 <- ggplot(Airport, aes(TimeSlot))+geom_bar(fill = "blue") + 
   labs(title ="Slot wise Requests at Airport")+geom_text(stat='count',aes(label=..count..),vjust=-1)+
   theme(plot.title = element_text(hjust = 0.5))+ylim(0,2000)
 
 #plot(AirportDemand2)
 
 
 
 AirportSupply2 <- ggplot(filter(Airport, Airport$Drop_Hour!= 'NA'), aes(TimeSlot))+geom_bar(fill = "pink")+
   labs(title ="Slotwise Supply at Airport(Hours)") +geom_text(stat='count',aes(label=..count..),vjust=-1)+
   theme(plot.title = element_text(hjust = 0.5)) +ylim(0,2000)
 
 #plot(AirportSupply2)
 
 CityDemand2 <- ggplot(City, aes(TimeSlot))+ geom_bar(fill = "blue") + labs(title ="Slotwise Requests at City")+
   geom_text(stat='count',aes(label=..count..),vjust=-1)+
   theme(plot.title = element_text(hjust = 0.5))+ylim(0,2000)
 
 #plot(CityDemand2)
 

 
 CitySupply2 <- ggplot(filter(City, City$Drop_Hour!= 'NA'), aes(TimeSlot))+ geom_bar(fill = "pink") + 
   labs(title ="Slotwise Supply at City(Hours)")+geom_text(stat='count',aes(label=..count..),vjust=-1)+
   theme(plot.title = element_text(hjust = 0.5))+ylim(0,2000)
 
 #plot(CitySupply2)
 
 
 
 # Assembling AirportDemand2,AirportSupply2,CityDemand2,CitySupply2 in single view:
 
 grid.arrange(AirportDemand2,AirportSupply2,CityDemand2,CitySupply2,nrow = 2, ncol = 2)
 
 
 
 #2. Plotting Demand & Supply (using dodge function) together for Airport:
 
 AirportDS <- ggplot(Airport) +
   geom_bar(aes(Req_Hour), fill = c("orange"))+ geom_bar(aes(Drop_Hour),fill = c("purple"),position = "dodge")+
   ylim(0,500) + labs(title = "From Airport (Orange = Request for Cab, Purple = Availability (after Drops))") + 
   theme(plot.title = element_text(hjust = 0.5)) + xlab("Hrs of the day")
 
 
 
 # Plotting Demand & Supply together for City:
 
 CityDS <- ggplot(City) +
   geom_bar(aes(Req_Hour), fill = c("orange"))+ geom_bar(aes(Drop_Hour),fill = c("purple"),position = "dodge")+
   ylim(0,500) + labs(title = "From City (Orange = Request for Cab, Purple = Availability (after Drops))") + 
   theme(plot.title = element_text(hjust = 0.5)) + xlab("Hrs of the day")
 

 #Plotting  Demand and Supply together:
 
 grid.arrange(AirportDS, CityDS, ncol = 2) 
 
 
 ## Plotting  request time vs Status based on pickup point:
 
 # when pick up is at Airport
 AirportPick <- ggplot(Airport, aes(Req_Hour, fill = Status))
 AirportPick  <- AirportPick  + geom_bar() + ylim(0,500) + labs(title = "From Airport") + 
   theme(plot.title = element_text(hjust = 0.5))
 
 plot(AirportPick)
 
 
 # when pick up at the city
 CityPick <- ggplot(City, aes(Req_Hour, fill = Status))
 CityPick  <- CityPick + geom_bar() + ylim(0,500) + labs(title = "From City") + 
   theme(plot.title = element_text(hjust = 0.5))
 
 plot(CityPick)
 
 
 
 
 
 # Now, analysing Cancelled and No car available Scenarios at city:
 
 CityCAncel<-filter(City,City$Status=="Cancelled")
 
 Cancelled_At_City <- ggplot(CityCAncel, aes(Req_Hour))+
 geom_bar(fill = "red") + labs(title = "Cancel Status at City")+ ylim(0,200)+
  theme(plot.title = element_text(hjust = 0.5))
 
 
CityNoCar<-filter(City,City$Status=="No Cars Available")
 
 NoCars_At_City <- ggplot(CityNoCar, aes(Req_Hour))+
   geom_bar(fill = "blue") + labs(title = "Car Not Available at City")+ ylim(0,200)+
   theme(plot.title = element_text(hjust = 0.5))
 
 

### Plotting No Car Available and Cancelled Cases  For City together:-
 
 grid.arrange(NoCars_At_City,Cancelled_At_City, ncol = 2)
 
 
 
 # Cancelled and No car available Scenarios at Airport:
 
 AirportCAncel<-filter(Airport,Airport$Status=="Cancelled")
 
 Cancelled_At_Airport <- ggplot(AirportCAncel, aes(Req_Hour))+
   geom_bar(fill = "red") + labs(title = "Cancel Status at Airport")+ 
   theme(plot.title = element_text(hjust = 0.5))
 
 AirportNoCar<-filter(Airport,Airport$Status=="No Cars Available")
 
 NoCars_At_Airport <- ggplot(AirportNoCar, aes(Req_Hour))+
   geom_bar(fill = "blue") + labs(title = "Car Not Available at Airport")+ 
   theme(plot.title = element_text(hjust = 0.5))
 
 ### Plotting No Car Available and Cancelled Cases For Airport together:-
 
 grid.arrange(NoCars_At_Airport,Cancelled_At_Airport, ncol = 2)
 
 
 
 
 
 ####################SLOT-WISE ANALYSIS##############
 
 
 #Finding No Car Available and Cancelled Cases in terms of Time Slots For Airport:
 
AirportCAncel2<-filter(Airport,Airport$Status=="Cancelled")
 
Cancelled_At_Airport2 <- ggplot(AirportCAncel, aes(TimeSlot))+
   geom_bar(fill = "red") +geom_text(stat='count',aes(label=..count..),vjust=-1)+ 
  labs(title = "Cancel Status at Airport")+
   theme(plot.title = element_text(hjust = 0.5))
 
AirportNoCar2<-filter(Airport,Airport$Status=="No Cars Available")
 
 NoCars_At_Airport2 <- ggplot(AirportNoCar, aes(TimeSlot))+
   geom_bar(fill = "blue") + geom_text(stat='count',aes(label=..count..),vjust=-1)+
   labs(title = "Car Not Available at Airport")+ 
   theme(plot.title = element_text(hjust = 0.5))
 
 ### Plotting No Car Available and Cancelled Cases in terms of Time Slots For Airport together:-
 
 grid.arrange(NoCars_At_Airport2,Cancelled_At_Airport2, ncol = 2)
 

 
 
 
 #Finding No Car Available and Cancelled Cases in terms of Time Slots For City:
 
 CityCAncel2<-filter(City,City$Status=="Cancelled")
 
 Cancelled_At_City2<- ggplot(CityCAncel, aes(TimeSlot))+
   geom_bar(fill = "red") + ylim(0,700)+
   labs(title = "Cancel Status at City")+ theme(plot.title = element_text(hjust = 0.5))
 

CityNoCar2<-filter(City,City$Status=="No Cars Available")
 
 NoCars_At_City2 <- ggplot(CityNoCar, aes(TimeSlot))+
   geom_bar(fill = "blue") + labs(title = "Car Not Available at City") +ylim(0,700) +
   theme(plot.title = element_text(hjust = 0.5))
 
 ### Plotting No Car Available and Cancelled Cases in terms of Time Slots For City together:-

 grid.arrange(NoCars_At_City2,Cancelled_At_City2, ncol = 2)

 
 #Exporting to Excel
 #write.csv(Uber, "Uber2.csv")
 
 
 
 
 ###Find the time slots when the highest gap exists:
 
 # the gap is maximum between peak hours at Airport(5:00pm to 10pm) and City(4:00am to 10:00am) both.
 
 ##Find the types of requests (city-airport or airport-city) for which 
 #the gap is the most severe in the identified time slots
 
 #Among  requests (city-airport or airport-city)
 #the gap is the most severe in the identified time slots  for FROM AIRPORT TO CITY.
 
 
 
 
 
 
 
 
 

 
 
 
 