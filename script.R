

#Data processing

#Load the required packages

if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("reshape2")) install.packages("reshape2")








#Load the data 
if(!dir.exists("Data")){
      
      dir.create("Data")
}

dirData<-"Data/stormData.csv.bz2"   #49 Mb -> uncompressed 409.4 Mb

if(!file.exists(dirData)){
      url<-
      "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
      message("Download in progress.")
      download.file(url,dirData)
      message("Download completed.")
}
stormData<-tbl_df(read.csv(dirData))     #Take a long time - BE PATIENT
names(stormData)
stormData

#Processing data

stormData<-select(stormData,EVTYPE,FATALITIES,INJURIES,PROPDMG,PROPDMGEXP,
                 CROPDMG,CROPDMGEXP)

table(stormData$CROPDMGEXP)   #possible values assumed. EXP is exponent
table(stormData$PROPDMGEXP)

#Calculate the extended value of Property damage and of crop damage

stormData$PROPDMGEXP<-
      plyr::mapvalues(stormData$PROPDMGEXP, from = 
                                      c("","-","?","+","0","1","2","3","4",
                                        "5","6","7","8","B","h","H","K","m","M"),
                                      to = 
                                      c("0","0","0","0","0","1","2","3","4","5",
                                        "6","7","8","9","2","2","3","6","6"))  

stormData$CROPDMGEXP<-
      plyr::mapvalues(stormData$CROPDMGEXP, from = 
                                      c("","?","0","2","B","k","K","m","M"),
                                      to = 
                                      c("0","0","0","2","9","3","3","6","6"))

stormData$PROPDMGEXP<-as.numeric(as.character(stormData$PROPDMGEXP))
stormData$CROPDMGEXP<-as.numeric(as.character(stormData$CROPDMGEXP))


stormData<-
      stormData %>% 
            mutate(
                  PROPDMG = PROPDMG*10^PROPDMGEXP,
                  CROPDMG = CROPDMG*10^CROPDMGEXP) %>%
            group_by(EVTYPE) %>%
            summarise(
                       FATALITIES = sum(FATALITIES),INJURIES = sum(INJURIES),
                       PROPDMG = sum(PROPDMG), CROPDMG = sum(CROPDMG)
                      ) %>%
            mutate(
                     TOT_HEALTHDMG = FATALITIES + INJURIES,
                     TOT_ECONOMICDMG = PROPDMG + CROPDMG
                  )


#Total health data
totalHealthData<-arrange(stormData,desc(TOT_HEALTHDMG))[1:10, ]
totalHealthData$EVTYPE<-as.factor(as.character(totalHealthData$EVTYPE))
totalHealthData$EVTYPE <- factor(
                              totalHealthData$EVTYPE,
                              levels = totalHealthData$EVTYPE)


totalHealthData<-melt(totalHealthData,
                      id = "EVTYPE",
                      measure.vars = c("INJURIES", "FATALITIES"))

p1<-ggplot(totalHealthData, aes(x = EVTYPE, y = value, fill = variable)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(
            palette="Paired",
            labels=c("Injuries", "Fatalities"))+
      xlab("Event types")+
      ylab("Total human fatalities  and injuries") + 
      ggtitle("TOP 10 of  event types most harmful for the people's health") +
      theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.title=element_blank()
            ) 

print(p1)
    
#Total economic damage

totalEconomicData<-arrange(stormData,desc(TOT_ECONOMICDMG))[1:10, ]
totalEconomicData$EVTYPE<-as.factor(as.character(totalEconomicData$EVTYPE))
totalEconomicData$EVTYPE <- factor(totalEconomicData$EVTYPE, levels = totalEconomicData$EVTYPE)


totalEconomicData<-melt(totalEconomicData,
                      id = "EVTYPE",
                      measure.vars = c("PROPDMG", "CROPDMG"))



p2<-ggplot(totalEconomicData, aes(x = EVTYPE, y = value, fill = variable)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(
            palette="Paired",
            labels=c("Property Damage", "Crop Damage")
            )+
      xlab("Event types")+
      ylab("Total property and crop damages") + 
      ggtitle("TOP 10 of  event types for economic damages") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.title=element_blank()) 

print(p2)

