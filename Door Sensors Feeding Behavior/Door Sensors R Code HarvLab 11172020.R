#Set working directory to where your raw outputs are from the HOBO sensors.

setwd("SETWD")

#Read you CSV file for each door. "[ ,1:3]" Skips columns if "coupler attached", "Host Connected", "Stopped", "End of File" were exported from HOBOware.
#View function to check that the data was imported correctly.

library(readr)
library (lubridate)

RawData <- read_csv("YOURFILENAME.csv",col_names = c('#','DateTime',"ZTilt"), skip =2 )[ ,1:3]
View(RawData)

#Convert the "Date Time" column from a character column to a DateTime type column. Class function to check that the data was converted properly

RawData$`DateTime` <- as.POSIXct(RawData$`DateTime`,
                            format="%m/%d/%Y %H:%M:%S")

class(RawData$`DateTime`)

#Subset your data to represent a feeding day (from feeding to feeding). Replicated to obtain all feeding days that were observed. Change to the
#corresponding date and time of your first day of feeding. 

Subset_D1 <- subset(RawData,
                    RawData$`DateTime` >= as.POSIXct('2020-06-28 07:00:00') &
                    RawData$`DateTime` <= as.POSIXct('2020-06-29 05:00:00'))

#Write a csv file of your subset data. Change file path to desired location and change
#file name to whatever you want.

path <- "DESIRED LOCATION FOR NEW FILES"

write.csv(Subset_D1, file.path(path, "NEWFILENAMEDay1.csv"),row.names = FALSE)

#Code to subset for day 2.

Subset_D2 <- subset(RawData,
                    RawData$`DateTime` >= as.POSIXct('2020-06-29 05:00:00') &
                    RawData$`DateTime` <= as.POSIXct('2020-06-30 05:00:00'))

path <- "DESIRED LOCATION FOR NEW FILES"

write.csv(Subset_D2, file.path(path, "NEWFILENAMEDay2.csv"),row.names = FALSE)

#Code to subset for day 3.

Subset_D3 <- subset(RawData,
                    RawData$`DateTime` >= as.POSIXct('2020-06-30 05:00:00') &
                      RawData$`DateTime` <= as.POSIXct('2020-07-1 05:00:00'))

path <- "DESIRED LOCATION FOR NEW FILES"

write.csv(Subset_D3, file.path(path, "NEWFILENAMEDay3.csv"),row.names = FALSE)

#Code to subset for day 4.

Subset_D4 <- subset(RawData,
                    RawData$`DateTime` >= as.POSIXct('2020-07-1 05:00:00') &
                      RawData$`DateTime` <= as.POSIXct('2020-07-2 05:00:00'))

path <- "DESIRED LOCATION FOR NEW FILES"

write.csv(Subset_D4, file.path(path, "NEWFILENAMEDay4.csv"),row.names = FALSE)

#Code to subset for day 5.

Subset_D5 <- subset(RawData,
                    RawData$`DateTime` >= as.POSIXct('2020-07-2 05:00:00') &
                      RawData$`DateTime` <= as.POSIXct('2020-07-3 05:00:00'))

path <- "DESIRED LOCATION FOR NEW FILES"

write.csv(Subset_D5, file.path(path, "NEWFILENAMEDay5.csv"),row.names = FALSE)

#Code to subset for day 6.

Subset_D6 <- subset(RawData,
                    RawData$`DateTime` >= as.POSIXct('2020-07-3 05:00:00') &
                      RawData$`DateTime` <= as.POSIXct('2020-07-4 05:00:00'))

path <- "DESIRED LOCATION FOR NEW FILES"

write.csv(Subset_D6, file.path(path, "NEWFILENAMEDay6.csv"),row.names = FALSE)

#Code to subset for day 7.

Subset_D7 <- subset(RawData,
                    RawData$`DateTime` >= as.POSIXct('2020-07-4 05:00:00') &
                      RawData$`DateTime` <= as.POSIXct('2020-07-5 05:00:00'))

path <- "DESIRED LOCATION FOR NEW FILES"

write.csv(Subset_D7, file.path(path, "NEWFILENAMEDay7.csv"),row.names = FALSE)

#Mutate function to create a column of events, where 0 stands for a tilt angle <30 where the door is closed
#and 1 stands for a tilt angle =>30 where the door is open.

library(dplyr)

Subset_D1_Event <- mutate(Subset_D1, Event =  ifelse(ZTilt<30,0,1))
  write.csv(Subset_D1_Event, file.path(path, "NEWFILENAMEDay1Events.csv"),row.names = FALSE)


Subset_D2_Event <- mutate(Subset_D2, Event =  ifelse(ZTilt<30,0,1))
  write.csv(Subset_D2_Event, file.path(path, "NEWFILENAMEDay2Events.csv"),row.names = FALSE)


Subset_D3_Event <- mutate(Subset_D3, Event =  ifelse(ZTilt<30,0,1))
  write.csv(Subset_D3_Event, file.path(path, "NEWFILENAMEDay3Events.csv"),row.names = FALSE)

  
Subset_D4_Event <- mutate(Subset_D4, Event =  ifelse(ZTilt<30,0,1))
  write.csv(Subset_D4_Event, file.path(path, "NEWFILENAMEDay4Events.csv"),row.names = FALSE)

  
Subset_D5_Event <- mutate(Subset_D5, Event =  ifelse(ZTilt<30,0,1))
  write.csv(Subset_D5_Event, file.path(path, "NEWFILENAMEDay5Events.csv"),row.names = FALSE)
  
  
Subset_D6_Event <- mutate(Subset_D6, Event =  ifelse(ZTilt<30,0,1))
  write.csv(Subset_D6_Event, file.path(path, "NEWFILENAMEDay6Events.csv"),row.names = FALSE)

    
Subset_D7_Event <- mutate(Subset_D7, Event =  ifelse(ZTilt<30,0,1))
  write.csv(Subset_D7_Event, file.path(path, "NEWFILENAMEDay7Events.csv"),row.names = FALSE)

 #Streak_Run function to calculate the "streak" of adjacent equal numbers in the events column. 

library(runner)

Subset_D1_Event_Flags <- mutate(Subset_D1_Event, Flags =  streak_run(Subset_D1_Event$Event))
  write.csv(Subset_D1_Event_Flags, file.path(path, "NEWFILENAMEDay1Flags.csv"),row.names = FALSE)


Subset_D2_Event_Flags <- mutate(Subset_D2_Event, Flags =  streak_run(Subset_D2_Event$Event))
  write.csv(Subset_D2_Event_Flags, file.path(path, "NEWFILENAMEDay2Flags.csv"),row.names = FALSE)

  
Subset_D3_Event_Flags <- mutate(Subset_D3_Event, Flags =  streak_run(Subset_D3_Event$Event))
  write.csv(Subset_D3_Event_Flags, file.path(path, "NEWFILENAMEDay3Flags.csv"),row.names = FALSE)  
  
  
Subset_D4_Event_Flags <- mutate(Subset_D4_Event, Flags =  streak_run(Subset_D4_Event$Event))
  write.csv(Subset_D4_Event_Flags, file.path(path, "NEWFILENAMEDay4Flags.csv"),row.names = FALSE)  

  
Subset_D5_Event_Flags <- mutate(Subset_D5_Event, Flags =  streak_run(Subset_D5_Event$Event))
  write.csv(Subset_D5_Event_Flags, file.path(path, "NEWFILENAMEDay5Flags.csv"),row.names = FALSE)  

  
Subset_D6_Event_Flags <- mutate(Subset_D6_Event, Flags =  streak_run(Subset_D6_Event$Event))
  write.csv(Subset_D6_Event_Flags, file.path(path, "NEWFILENAMEDay6Flags.csv"),row.names = FALSE)  
  
  
Subset_D7_Event_Flags <- mutate(Subset_D7_Event, Flags =  streak_run(Subset_D7_Event$Event))
  write.csv(Subset_D7_Event_Flags, file.path(path, "NEWFILENAMEDay7Flags.csv"),row.names = FALSE)  
  
#End