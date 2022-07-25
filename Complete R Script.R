#Set working directory

library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(rgdal)
library(sp)
library(raster)
library(maptools)
library(reshape2)
library(reshape)
library(lubridate)
library(naniar)
library(ggplot2)
library(mgcv)
library(mgcViz)
library(ggOceanMaps)
library(ggOceanMapsData)
library(cowplot)

###Filtering data we want from individual hauls####################################################

###BITS

#Read in Species Data
BITShaul<- read.table("BITS (HL).csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)

#Make "HaulCode" identifier (Survey, Year, Station and Haul number)
BITShaul$HaulCode<- paste(BITShaul$Survey, BITShaul$Year, BITShaul$StNo, BITShaul$HaulNo)

#Filter columns that we want 
BITShaul2<- BITShaul %>% select (HaulCode,Survey,Ship,Gear,Year,ScientificName_WoRMS,HLNoAtLngt)

#Group the individual recordings of the same species (HLNoAtLngt), to have just one row per species with "TotalCaught" for each species
BITShaul3 <- ddply(BITShaul2, c("HaulCode","Survey","Ship","Gear","Year","ScientificName_WoRMS"), summarise, Totalcaught = sum(HLNoAtLngt) )

#Read in Metadata
BITSmetadata <- read.table("BITS (HH).csv",header=TRUE,fill=TRUE,sep=",",check.names=FALSE)

#Make "HaulCode" identifier again
BITSmetadata$HaulCode<- paste(BITSmetadata$Survey, BITSmetadata$Year, BITSmetadata$StNo, BITSmetadata$HaulNo)

#Filter the columns we want 
BITSmetadata2<- BITSmetadata %>% select (HaulCode,Month,ShootLat, ShootLong, Depth, HaulDur, BotTemp, BotSal)

#Merge them into one data frame by matching HaulCode
BITSmerged<-merge(BITShaul3, BITSmetadata2,by=c("HaulCode"))

#Save merged file 
write.csv(BITSmerged, file="BITS.csv",row.names = FALSE)


###BTS

BTShaul<- read.table("BTS (HL).csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)
BTShaul$HaulCode<- paste(BTShaul$Survey, BTShaul$Year, BTShaul$StNo, BTShaul$HaulNo)
BTShaul2<- BTShaul %>% select (HaulCode,Survey,Ship,Gear,Year,ScientificName_WoRMS,HLNoAtLngt)
BTShaul3<- ddply(BTShaul2, c("HaulCode","Survey","Ship","Gear","Year","ScientificName_WoRMS"), summarise, Totalcaught = sum(HLNoAtLngt))
BTSmetadata<- read.table("BTS (HH).CSV", header=TRUE, fill=TRUE, sep=",", check.names=FALSE)
BTSmetadata$HaulCode<- paste(BTSmetadata$Survey, BTSmetadata$Year, BTSmetadata$StNo, BTSmetadata$HaulNo)
BTSmetadata2<- BTSmetadata %>% select (HaulCode,Month,ShootLat, ShootLong, Depth, HaulDur, BotTemp, BotSal)
BTSmerged<- merge(BTShaul3, BTSmetadata2, by=c("HaulCode"))
write.csv(BTSmerged, file="BTS.csv", row.names=FALSE)

###BTS-VIII

BTSVIIIhaul<- read.table("BTS-VIII (HL).csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)
BTSVIIIhaul$HaulCode<- paste(BTSVIIIhaul$Survey, BTSVIIIhaul$Year, BTSVIIIhaul$StNo, BTSVIIIhaul$HaulNo)
BTSVIIIhaul2<- BTSVIIIhaul %>% select (HaulCode,Survey,Ship,Gear,Year,ScientificName_WoRMS,HLNoAtLngt)
BTSVIIIhaul3<- ddply(BTSVIIIhaul2, c("HaulCode","Survey","Ship","Gear","Year","ScientificName_WoRMS"), summarise, Totalcaught = sum(HLNoAtLngt))
BTSVIIImetadata<- read.table("BTS-VIII (HH).CSV", header=TRUE, fill=TRUE, sep=",", check.names=FALSE)
BTSVIIImetadata$HaulCode<- paste(BTSVIIImetadata$Survey, BTSVIIImetadata$Year, BTSVIIImetadata$StNo, BTSVIIImetadata$HaulNo)
BTSVIIImetadata2<- BTSVIIImetadata %>% select (HaulCode,Month,ShootLat, ShootLong, Depth, HaulDur, BotTemp, BotSal)
BTSVIIImerged<- merge(BTSVIIIhaul3, BTSVIIImetadata2, by=c("HaulCode"))
write.csv(BTSVIIImerged, file="BTS-VIII.csv", row.names=FALSE)

###DWS

DWShaul<- read.table("DWS (HL).csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)
DWShaul$HaulCode<- paste(DWShaul$Survey, DWShaul$Year, DWShaul$StNo, DWShaul$HaulNo)
DWShaul2<- DWShaul %>% select (HaulCode,Survey,Ship,Gear,Year,ScientificName_WoRMS,HLNoAtLngt)
DWShaul3<- ddply(DWShaul2, c("HaulCode","Survey","Ship","Gear","Year","ScientificName_WoRMS"), summarise, Totalcaught = sum(HLNoAtLngt))
DWSmetadata<- read.table("DWS (HH).CSV", header=TRUE, fill=TRUE, sep=",", check.names=FALSE)
DWSmetadata$HaulCode<- paste(DWSmetadata$Survey, DWSmetadata$Year, DWSmetadata$StNo, DWSmetadata$HaulNo)
DWSmetadata2<- DWSmetadata %>% select (HaulCode,Month,ShootLat, ShootLong, Depth, HaulDur, BotTemp, BotSal)
DWSmerged<- merge(DWShaul3, DWSmetadata2, by=c("HaulCode"))
write.csv(DWSmerged, file="DWS.csv", row.names=FALSE)

###DYFS

DYFShaul<- read.table("DYFS (HL).csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)
DYFShaul$HaulCode<- paste(DYFShaul$Survey, DYFShaul$Year, DYFShaul$StNo, DYFShaul$HaulNo)
DYFShaul2<- DYFShaul %>% select (HaulCode,Survey,Ship,Gear,Year,ScientificName_WoRMS,HLNoAtLngt)
DYFShaul3<- ddply(DYFShaul2, c("HaulCode","Survey","Ship","Gear","Year","ScientificName_WoRMS"), summarise, Totalcaught = sum(HLNoAtLngt))
DYFSmetadata<- read.table("DYFS (HH).CSV", header=TRUE, fill=TRUE, sep=",", check.names=FALSE)
DYFSmetadata$HaulCode<- paste(DYFSmetadata$Survey, DYFSmetadata$Year, DYFSmetadata$StNo, DYFSmetadata$HaulNo)
DYFSmetadata2<- DYFSmetadata %>% select (HaulCode,Month,ShootLat, ShootLong, Depth, HaulDur, BotTemp, BotSal)
DYFSmerged<- merge(DYFShaul3, DYFSmetadata2, by=c("HaulCode"))
write.csv(DYFSmerged, file="DYFS.csv", row.names=FALSE)

###EVHOE

EVHOEhaul<- read.table("EVHOE (HL).csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)
EVHOEhaul$HaulCode<- paste(EVHOEhaul$Survey, EVHOEhaul$Year, EVHOEhaul$StNo, EVHOEhaul$HaulNo)
EVHOEhaul2<- EVHOEhaul %>% select (HaulCode,Survey,Ship,Gear,Year,ScientificName_WoRMS,HLNoAtLngt)
EVHOEhaul3<- ddply(EVHOEhaul2, c("HaulCode","Survey","Ship","Gear","Year","ScientificName_WoRMS"), summarise, Totalcaught = sum(HLNoAtLngt))
EVHOEmetadata<- read.table("EVHOE (HH).CSV", header=TRUE, fill=TRUE, sep=",", check.names=FALSE)
EVHOEmetadata$HaulCode<- paste(EVHOEmetadata$Survey,EVHOEmetadata$Year, EVHOEmetadata$StNo, EVHOEmetadata$HaulNo)
EVHOEmetadata2<- EVHOEmetadata %>% select (HaulCode,Month,ShootLat, ShootLong, Depth, HaulDur, BotTemp, BotSal)
EVHOEmerged<- merge(EVHOEhaul3, EVHOEmetadata2, by=c("HaulCode"))
write.csv(EVHOEmerged, file="EVHOE.csv", row.names=FALSE)

###FR-CGFS

FRCGFShaul<- read.table("FR-CGFS (HL).csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)
FRCGFShaul$HaulCode<- paste(FRCGFShaul$Survey, FRCGFShaul$Year, FRCGFShaul$StNo, FRCGFShaul$HaulNo)
FRCGFShaul2<- FRCGFShaul %>% select (HaulCode,Survey,Ship,Gear,Year,ScientificName_WoRMS,HLNoAtLngt)
FRCGFShaul3<- ddply(FRCGFShaul2, c("HaulCode","Survey","Ship","Gear","Year","ScientificName_WoRMS"), summarise, Totalcaught = sum(HLNoAtLngt))
FRCGFSmetadata<- read.table("FR-CGFS (HH).CSV", header=TRUE, fill=TRUE, sep=",", check.names=FALSE)
FRCGFSmetadata$HaulCode<- paste(FRCGFSmetadata$Survey,FRCGFSmetadata$Year, FRCGFSmetadata$StNo, FRCGFSmetadata$HaulNo)
FRCGFSmetadata2<- FRCGFSmetadata %>% select (HaulCode,Month,ShootLat, ShootLong, Depth, HaulDur, BotTemp, BotSal)
FRCGFSmerged<- merge(FRCGFShaul3, FRCGFSmetadata2, by=c("HaulCode"))
write.csv(FRCGFSmerged, file="FR-CGFS.csv", row.names=FALSE)

###IE-IAMS

IEIAMShaul<- read.table("IE-IAMS (HL).csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)
IEIAMShaul$HaulCode<- paste(IEIAMShaul$Survey, IEIAMShaul$Year, IEIAMShaul$StNo, IEIAMShaul$HaulNo)
IEIAMShaul2<- IEIAMShaul %>% select (HaulCode,Survey,Ship,Gear,Year,ScientificName_WoRMS,HLNoAtLngt)
IEIAMShaul3<- ddply(IEIAMShaul2, c("HaulCode","Survey","Ship","Gear","Year","ScientificName_WoRMS"), summarise, Totalcaught = sum(HLNoAtLngt))
IEIAMSmetadata<- read.table("IE-IAMS (HH).CSV", header=TRUE, fill=TRUE, sep=",", check.names=FALSE)
IEIAMSmetadata$HaulCode<- paste(IEIAMSmetadata$Survey,IEIAMSmetadata$Year, IEIAMSmetadata$StNo, IEIAMSmetadata$HaulNo)
IEIAMSmetadata2<- IEIAMSmetadata %>% select (HaulCode,Month,ShootLat, ShootLong, Depth, HaulDur, BotTemp, BotSal)
IEIAMSmerged<- merge(IEIAMShaul3, IEIAMSmetadata2, by=c("HaulCode"))
write.csv(IEIAMSmerged, file="IE-IAMS.csv", row.names=FALSE)

###IE-IGFS

IEIGFShaul<- read.table("IE-IGFS (HL).csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)
IEIGFShaul$HaulCode<- paste(IEIGFShaul$Survey, IEIGFShaul$Year, IEIGFShaul$StNo, IEIGFShaul$HaulNo)
IEIGFShaul2<- IEIGFShaul %>% select (HaulCode,Survey,Ship,Gear,Year,ScientificName_WoRMS,HLNoAtLngt)
IEIGFShaul3<- ddply(IEIGFShaul2, c("HaulCode","Survey","Ship","Gear","Year","ScientificName_WoRMS"), summarise, Totalcaught = sum(HLNoAtLngt))
IEIGFSmetadata<- read.table("IE-IGFS (HH).CSV", header=TRUE, fill=TRUE, sep=",", check.names=FALSE)
IEIGFSmetadata$HaulCode<- paste(IEIGFSmetadata$Survey,IEIGFSmetadata$Year, IEIGFSmetadata$StNo, IEIGFSmetadata$HaulNo)
IEIGFSmetadata2<- IEIGFSmetadata %>% select (HaulCode,Month,ShootLat, ShootLong, Depth, HaulDur, BotTemp, BotSal)
IEIGFSmerged<- merge(IEIGFShaul3, IEIGFSmetadata2, by=c("HaulCode"))
write.csv(IEIGFSmerged, file="IE-IGFS.csv", row.names=FALSE)

###NIGFS

NIGFShaul<- read.table("NIGFS (HL).csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)
NIGFShaul$HaulCode<- paste(NIGFShaul$Survey, NIGFShaul$Year, NIGFShaul$StNo, NIGFShaul$HaulNo)
NIGFShaul2<- NIGFShaul %>% select (HaulCode,Survey,Ship,Gear,Year,ScientificName_WoRMS,HLNoAtLngt)
NIGFShaul3<- ddply(NIGFShaul2, c("HaulCode","Survey","Ship","Gear","Year","ScientificName_WoRMS"), summarise, Totalcaught = sum(HLNoAtLngt))
NIGFSmetadata<- read.table("NIGFS (HH).CSV", header=TRUE, fill=TRUE, sep=",", check.names=FALSE)
NIGFSmetadata$HaulCode<- paste(NIGFSmetadata$Survey,NIGFSmetadata$Year, NIGFSmetadata$StNo, NIGFSmetadata$HaulNo)
NIGFSmetadata2<- NIGFSmetadata %>% select (HaulCode,Month,ShootLat, ShootLong, Depth, HaulDur, BotTemp, BotSal)
NIGFSmerged<- merge(NIGFShaul3, NIGFSmetadata2, by=c("HaulCode"))
write.csv(NIGFSmerged, file="NIGFS.csv", row.names=FALSE)

###NS-IBTS

Hauldata <- read.table("NS-IBTS (HL).csv",header=TRUE,fill=TRUE,sep=",",check.names=FALSE)
Hauldata$HaulCode <- paste(Hauldata$Survey,Hauldata$Year,Hauldata$StNo,Hauldata$HaulNo)
Hauldata2 <- Hauldata %>% select(HaulCode,Survey,Ship,Gear,Year,ScientificName_WoRMS,HLNoAtLngt)
Hauldata3 <- ddply(Hauldata2, c("HaulCode","Survey","Ship","Gear","Year","ScientificName_WoRMS"), summarise, Totalcaught = sum(HLNoAtLngt) )
Metadata <- read.table("NS-IBTS (HH).csv",header=TRUE,fill=TRUE,sep=",",check.names=FALSE)
Metadata$HaulCode <- paste(Metadata$Survey,Metadata$Year,Metadata$StNo,Metadata$HaulNo)
Metadata2 <- Metadata %>% select(HaulCode,Month,ShootLat, ShootLong,Depth,HaulDur,BotTemp,BotSal)
Matched<-merge(Hauldata3, Metadata2,by=c("HaulCode"))
write.csv(Matched, file="NS-IBTS.csv",row.names = FALSE)

###PT-IBTS

rm(list = ls())  
Hauldata <- read.table("PT-IBTS (HL).csv",header=TRUE,fill=TRUE,sep=",",check.names=FALSE)
Hauldata$HaulCode <- paste(Hauldata$Survey,Hauldata$Year,Hauldata$StNo,Hauldata$HaulNo)
Hauldata2 <- Hauldata %>% select(HaulCode,Survey,Ship,Gear,Year,ScientificName_WoRMS,HLNoAtLngt)
Hauldata3 <- ddply(Hauldata2, c("HaulCode","Survey","Ship","Gear","Year","ScientificName_WoRMS"), summarise, Totalcaught = sum(HLNoAtLngt) )
Metadata <- read.table("PT-IBTS (HH).csv",header=TRUE,fill=TRUE,sep=",",check.names=FALSE)
Metadata$HaulCode <- paste(Metadata$Survey,Metadata$Year,Metadata$StNo,Metadata$HaulNo)
Metadata2 <- Metadata %>% select(HaulCode,Month,ShootLat, ShootLong,Depth,HaulDur,BotTemp,BotSal)
Matched<-merge(Hauldata3, Metadata2,by=c("HaulCode"))
write.csv(Matched, file="PT-IBTS.csv",row.names = FALSE)

###ROCKALL

rm(list = ls())  
Hauldata <- read.table("ROCKALL (HL).csv",header=TRUE,fill=TRUE,sep=",",check.names=FALSE)
Hauldata$HaulCode <- paste(Hauldata$Survey,Hauldata$Year,Hauldata$StNo,Hauldata$HaulNo)
Hauldata2 <- Hauldata %>% select(HaulCode,Survey,Ship,Gear,Year,ScientificName_WoRMS,HLNoAtLngt)
Hauldata3 <- ddply(Hauldata2, c("HaulCode","Survey","Ship","Gear","Year","ScientificName_WoRMS"), summarise, Totalcaught = sum(HLNoAtLngt) )
Metadata <- read.table("ROCKALL (HH).csv",header=TRUE,fill=TRUE,sep=",",check.names=FALSE)
Metadata$HaulCode <- paste(Metadata$Survey,Metadata$Year,Metadata$StNo,Metadata$HaulNo)
Metadata2 <- Metadata %>% select(HaulCode,Month,ShootLat, ShootLong,Depth,HaulDur,BotTemp,BotSal)
Matched<-merge(Hauldata3, Metadata2,by=c("HaulCode"))
write.csv(Matched, file="ROCKALL.csv",row.names = FALSE)

###SCOROC

rm(list = ls())  
Hauldata <- read.table("SCOROC (HL).csv",header=TRUE,fill=TRUE,sep=",",check.names=FALSE)
Hauldata$HaulCode <- paste(Hauldata$Survey,Hauldata$Year,Hauldata$StNo,Hauldata$HaulNo)
Hauldata2 <- Hauldata %>% select(HaulCode,Survey,Ship,Gear,Year,ScientificName_WoRMS,HLNoAtLngt)
Hauldata3 <- ddply(Hauldata2, c("HaulCode","Survey","Ship","Gear","Year","ScientificName_WoRMS"), summarise, Totalcaught = sum(HLNoAtLngt) )
Metadata <- read.table("SCOROC (HH).csv",header=TRUE,fill=TRUE,sep=",",check.names=FALSE)
Metadata$HaulCode <- paste(Metadata$Survey,Metadata$Year,Metadata$StNo,Metadata$HaulNo)
Metadata2 <- Metadata %>% select(HaulCode,Month,ShootLat, ShootLong,Depth,HaulDur,BotTemp,BotSal)
Matched<-merge(Hauldata3, Metadata2,by=c("HaulCode"))
write.csv(Matched, file="SCOROC.csv",row.names = FALSE)

###SCOWCGFS

rm(list = ls())  
Hauldata <- read.table("SCOWCGFS (HL).csv",header=TRUE,fill=TRUE,sep=",",check.names=FALSE)
Hauldata$HaulCode <- paste(Hauldata$Survey,Hauldata$Year,Hauldata$StNo,Hauldata$HaulNo)
Hauldata2 <- Hauldata %>% select(HaulCode,Survey,Ship,Gear,Year,ScientificName_WoRMS,HLNoAtLngt)
Hauldata3 <- ddply(Hauldata2, c("HaulCode","Survey","Ship","Gear","Year","ScientificName_WoRMS"), summarise, Totalcaught = sum(HLNoAtLngt) )
Metadata <- read.table("SCOWCGFS (HH).csv",header=TRUE,fill=TRUE,sep=",",check.names=FALSE)
Metadata$HaulCode <- paste(Metadata$Survey,Metadata$Year,Metadata$StNo,Metadata$HaulNo)
Metadata2 <- Metadata %>% select(HaulCode,Month,ShootLat, ShootLong,Depth,HaulDur,BotTemp,BotSal)
Matched<-merge(Hauldata3, Metadata2,by=c("HaulCode"))
write.csv(Matched, file="SCOWCGFS.csv",row.names = FALSE)

###SE-SOUND

rm(list = ls())  
Hauldata <- read.table("SE-SOUND (HL).csv",header=TRUE,fill=TRUE,sep=",",check.names=FALSE)
Hauldata$HaulCode <- paste(Hauldata$Survey,Hauldata$Year,Hauldata$StNo,Hauldata$HaulNo)
Hauldata2 <- Hauldata %>% select(HaulCode,Survey,Ship,Gear,Year,ScientificName_WoRMS,HLNoAtLngt)
Hauldata3 <- ddply(Hauldata2, c("HaulCode","Survey","Ship","Gear","Year","ScientificName_WoRMS"), summarise, Totalcaught = sum(HLNoAtLngt) )
Metadata <- read.table("SE-SOUND (HH).csv",header=TRUE,fill=TRUE,sep=",",check.names=FALSE)
Metadata$HaulCode <- paste(Metadata$Survey,Metadata$Year,Metadata$StNo,Metadata$HaulNo)
Metadata2 <- Metadata %>% select(HaulCode,Month,ShootLat, ShootLong,Depth,HaulDur,BotTemp,BotSal)
Matched<-merge(Hauldata3, Metadata2,by=c("HaulCode"))
write.csv(Matched, file="SE-SOUND.csv",row.names = FALSE)

###SNS

rm(list = ls())  
Hauldata <- read.table("SNS (HL).csv",header=TRUE,fill=TRUE,sep=",",check.names=FALSE)
Hauldata$HaulCode <- paste(Hauldata$Survey,Hauldata$Year,Hauldata$StNo,Hauldata$HaulNo)
Hauldata2 <- Hauldata %>% select(HaulCode,Survey,Ship,Gear,Year,ScientificName_WoRMS,HLNoAtLngt)
Hauldata3 <- ddply(Hauldata2, c("HaulCode","Survey","Ship","Gear","Year","ScientificName_WoRMS"), summarise, Totalcaught = sum(HLNoAtLngt) )
Metadata <- read.table("SNS (HH).csv",header=TRUE,fill=TRUE,sep=",",check.names=FALSE)
Metadata$HaulCode <- paste(Metadata$Survey,Metadata$Year,Metadata$StNo,Metadata$HaulNo)
Metadata2 <- Metadata %>% select(HaulCode,Month,ShootLat, ShootLong,Depth,HaulDur,BotTemp,BotSal)
Matched<-merge(Hauldata3, Metadata2,by=c("HaulCode"))
write.csv(Matched, file="SNS.csv",row.names = FALSE)

###SP-ARSA

rm(list = ls())  
Hauldata <- read.table("SP-ARSA (HL).csv",header=TRUE,fill=TRUE,sep=",",check.names=FALSE)
Hauldata$HaulCode <- paste(Hauldata$Survey,Hauldata$Year,Hauldata$StNo,Hauldata$HaulNo)
Hauldata2 <- Hauldata %>% select(HaulCode,Survey,Ship,Gear,Year,ScientificName_WoRMS,HLNoAtLngt)
Hauldata3 <- ddply(Hauldata2, c("HaulCode","Survey","Ship","Gear","Year","ScientificName_WoRMS"), summarise, Totalcaught = sum(HLNoAtLngt) )
Metadata <- read.table("SP-ARSA (HH).csv",header=TRUE,fill=TRUE,sep=",",check.names=FALSE)
Metadata$HaulCode <- paste(Metadata$Survey,Metadata$Year,Metadata$StNo,Metadata$HaulNo)
Metadata2 <- Metadata %>% select(HaulCode,Month,ShootLat, ShootLong,Depth,HaulDur,BotTemp,BotSal)
Matched<-merge(Hauldata3, Metadata2,by=c("HaulCode"))
write.csv(Matched, file="SP-ARSA.csv",row.names = FALSE)

###SP-NORTH

rm(list = ls())  
Hauldata <- read.table("SP-NORTH (HL).csv",header=TRUE,fill=TRUE,sep=",",check.names=FALSE)
Hauldata$HaulCode <- paste(Hauldata$Survey,Hauldata$Year,Hauldata$StNo,Hauldata$HaulNo)
Hauldata2 <- Hauldata %>% select(HaulCode,Survey,Ship,Gear,Year,ScientificName_WoRMS,HLNoAtLngt)
Hauldata3 <- ddply(Hauldata2, c("HaulCode","Survey","Ship","Gear","Year","ScientificName_WoRMS"), summarise, Totalcaught = sum(HLNoAtLngt) )
Metadata <- read.table("SP-NORTH (HH).csv",header=TRUE,fill=TRUE,sep=",",check.names=FALSE)
Metadata$HaulCode <- paste(Metadata$Survey,Metadata$Year,Metadata$StNo,Metadata$HaulNo)
Metadata2 <- Metadata %>% select(HaulCode,Month,ShootLat, ShootLong,Depth,HaulDur,BotTemp,BotSal)
Matched<-merge(Hauldata3, Metadata2,by=c("HaulCode"))
write.csv(Matched, file="SP-NORTH.csv",row.names = FALSE)

###SP-PORC

rm(list = ls())  
Hauldata <- read.table("SP-PORC (HL).csv",header=TRUE,fill=TRUE,sep=",",check.names=FALSE)
Hauldata$HaulCode <- paste(Hauldata$Survey,Hauldata$Year,Hauldata$StNo,Hauldata$HaulNo)
Hauldata2 <- Hauldata %>% select(HaulCode,Survey,Ship,Gear,Year,ScientificName_WoRMS,HLNoAtLngt)
Hauldata3 <- ddply(Hauldata2, c("HaulCode","Survey","Ship","Gear","Year","ScientificName_WoRMS"), summarise, Totalcaught = sum(HLNoAtLngt) )
Metadata <- read.table("SP-PORC (HH).csv",header=TRUE,fill=TRUE,sep=",",check.names=FALSE)
Metadata$HaulCode <- paste(Metadata$Survey,Metadata$Year,Metadata$StNo,Metadata$HaulNo)
Metadata2 <- Metadata %>% select(HaulCode,Month,ShootLat, ShootLong,Depth,HaulDur,BotTemp,BotSal)
Matched<-merge(Hauldata3, Metadata2,by=c("HaulCode"))
write.csv(Matched, file="SP-PORC.csv",row.names = FALSE)

###SWC-IBTS

rm(list = ls())  
Hauldata <- read.table("SWC-IBTS (HL).csv",header=TRUE,fill=TRUE,sep=",",check.names=FALSE)
Hauldata$HaulCode <- paste(Hauldata$Survey,Hauldata$Year,Hauldata$StNo,Hauldata$HaulNo)
Hauldata2 <- Hauldata %>% select(HaulCode,Survey,Ship,Gear,Year,ScientificName_WoRMS,HLNoAtLngt)
Hauldata3 <- ddply(Hauldata2, c("HaulCode","Survey","Ship","Gear","Year","ScientificName_WoRMS"), summarise, Totalcaught = sum(HLNoAtLngt) )
Metadata <- read.table("SWC-IBTS (HH).csv",header=TRUE,fill=TRUE,sep=",",check.names=FALSE)
Metadata$HaulCode <- paste(Metadata$Survey,Metadata$Year,Metadata$StNo,Metadata$HaulNo)
Metadata2 <- Metadata %>% select(HaulCode,Month,ShootLat, ShootLong,Depth,HaulDur,BotTemp,BotSal)
Matched<-merge(Hauldata3, Metadata2,by=c("HaulCode"))
write.csv(Matched, file="SWC-IBTS.csv",row.names = FALSE)


###Combinding all of the hauls#####################################################################

#Read in all datasets 
BITS<-read.table("BITS.csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)
BTS<-read.table("BTS.csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)
BTSVIII<-read.table("BTS-VIII.csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)
DWS<-read.table("DWS.csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)
DYFS<-read.table("DYFS.csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)
EVHOE<-read.table("EVHOE.csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)
FRCGFS<-read.table("FR-CGFS.csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)
IEIAMS<-read.table("IE-IAMS.csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)
IEIGFS<-read.table("IE-IGFS.csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)
NIGFS<-read.table("NIGFS.csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)
NSIBTS<-read.table("NS-IBTS.csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)
PTIBTS<-read.table("PT-IBTS.csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)
ROCKALL<-read.table("ROCKALL.csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)
SCOROC<-read.table("SCOROC.csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)
SCOWCGFS<-read.table("SCOWCGFS.csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)
SESOUND<-read.table("SE-SOUND.csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)
SNS<-read.table("SNS.csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)
SPARSA<-read.table("SP-ARSA.csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)
SPNORTH<-read.table("SP-NORTH.csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)
SPPORC<-read.table("SP-PORC.csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)
SWCIBTS<-read.table("SWC-IBTS.csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)

#Combine all datasets into one big one (AllHauls)
AllHauls<-rbind(BITS, BTS, BTSVIII, DWS, DYFS, EVHOE, FRCGFS, IEIAMS, IEIGFS, NIGFS, NSIBTS, PTIBTS, ROCKALL, SCOROC, SCOWCGFS, SESOUND, SNS, SPARSA, SPNORTH, SPPORC, SWCIBTS)

#Convert all values < 0 in "Totalcaught" column to 0
AllHauls$Totalcaught[AllHauls$Totalcaught < 0] <- 0

#Make a new column for catch per unit effort (CPUE) by dividing Totalcaught by HaulDur (Duration)
AllHauls$CPUE <- paste(AllHauls$Totalcaught/AllHauls$HaulDur*60)

#Make CPUE a numeric and remove unnecessary data from environment 
AllHauls$CPUE <- as.numeric(AllHauls$CPUE)
rm(BITS, BTS, BTSVIII, DWS, DYFS, EVHOE, FRCGFS, IEIAMS, IEIGFS, NIGFS, NSIBTS, PTIBTS, ROCKALL, SCOROC, SCOWCGFS, SESOUND, SNS, SPARSA, SPNORTH, SPPORC, SWCIBTS)

#Flip the dataframe
library(reshape2)
AllHaulsFlipped <- dcast(AllHauls, HaulCode + Survey + Ship + Gear + Month + ShootLat + ShootLong + Depth + BotTemp +  BotSal ~ ScientificName_WoRMS, value.var = "CPUE", fun.aggregate = sum)

#Read in AllHaulsFlipped
AllHaulsFlipped<-read.table("AllHaulsFlipped.csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)

#Retain columns (Species) of interest- Elasmobranchs 
AllhaulsElasmos <- AllHaulsFlipped %>% select("HaulCode", "Survey", "Ship", "Gear", "Month", "ShootLat", "ShootLong", "Depth", "BotTemp", "BotSal", "Heptranchias perlo", "Hexanchus griseus", "Lamna nasus", "Lamna nasus", "Cetorhinus maximus", "Alopias vulpinus", "Apristurus aphyodes", "Apristurus manis", "Apristurus melanoasper", "Apristurus microps", "Galeus atlanticus", "Galeus melastomus", "Galeus murinus", "Scyliorhinus canicula", "Scyliorhinus stellaris", "Galeorhinus galeus", "Mustelus asterias", "Mustelus mustelus", "Prionace glauca", "Etmopterus princeps", "Etmopterus pusillus", "Etmopterus spinax", "Centroscyllium fabricii", "Somniosus microcephalus", "Somniosus rostratus", "Centroscymnus coelolepis", "Centroselachus crepidater", "Scymnodon ringens", "Oxynotus paradoxus", "Dalatias licha", "Centrophorus granulosus", "Centrophorus squamosus", "Deania profundorum", "Squalus acanthias", "Torpedo nobiliana", "Torpedo marmorata", "Torpedo torpedo", "Bathyraja pallida", "Bathyraja richardsoni", "Amblyraja jenseni", "Amblyraja radiata", "Dipturus batis", "Dipturus nidarosiensis", "Dipturus oxyrinchus", "Leucoraja circularis", "Leucoraja fullonica", "Leucoraja naevus", "Malacoraja kreffti", "Neoraja caerulea", "Raja brachyura", "Raja clavata", "Raja microocellata", "Raja miraletus", "Raja montagui", "Raja undulata", "Rajella bathyphila", "Rajella bigelowi", "Rajella fyllae", "Rajella kukujevi", "Rostroraja alba", "Dipturus linteus", "Dasyatis pastinaca", "Myliobatis aquila", "Chimaera monstrosa")

#Save elasmobanch all hauls data
write.csv(AllhaulsElasmos, file="AllHaulsElasmos.csv",row.names = FALSE)

#Retain columns (Species) that we are studying only
Studyelasmos<- AllHaulsElasmos %>% select("HaulCode", "Survey", "Ship", "Gear", "Month", "ShootLat", "ShootLong", "Depth", "BotTemp", "BotSal", "")

#Separate all of the values out of HaulCode (new variable called "HaulCode" is just survey but is just in preparation for the unite in the next step to combine the rest back into HaulCode)
Studyelasmos2 <- separate(Studyelasmos, col=HaulCode, into=c("HaulCode", "Year", "Station", "HaulNo"), sep=" ")

#Combine the rest of the the values (excluding Year) into HaulCode (consist of: HaulNo, Station, and Survey)
Studyelasmos3<- Studyelasmos2 %>% unite(HaulCode, HaulNo, Station, HaulCode, sep=" ")

#Save file
write.csv(Studyelasmos3, file="Studyelasmosnew.csv",row.names = FALSE)

#Ceiling the ShootLat and ShootLong 
Studyelasmos3$lat <- (ceiling(Studyelasmos3$ShootLat)) -0.5
Studyelasmos3$lon <- (ceiling(Studyelasmos3$ShootLong)) -0.5

#Remove unrounded lat and lon
Studyelasmos4<-subset(Studyelasmos3, select= -c(7,8))

#Rename lat and lon to original variable names
Studyelasmos5 <-Studyelasmos4 %>% dplyr::rename (ShootLat=lat)
Studyelasmos6 <-Studyelasmos5 %>% dplyr::rename (ShootLong=lon)

#Move shootlat and shootlong back to start of dataset
Studyelasmos7 <- Studyelasmos6 %>% relocate(`ShootLat`, .before = `HaulCode`)
Studyelasmos8 <- Studyelasmos7 %>% relocate(`ShootLong`, .before = `HaulCode`)

#Save as
write.table(Studyelasmos8, file="StudyelasmosLatLonRounded.csv", row.names = FALSE, sep = ",")


###Configuring Hadley temperature data################################################################

#Extracting the data 
required.packages <- c("ncdf4")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>=1){lapply(new.packages, install.packages)}
lapply(required.packages, require, character.only=T) 

#Read in monthly temperature data file 
hadisst.nc<- nc_open("HadISST_sst.nc")
print(hadisst.nc)

# Extract important info: lat, long and sst (Temperature)
lat <- ncvar_get(hadisst.nc,"latitude")
lon <- ncvar_get(hadisst.nc,"longitude")
time= ncvar_get(hadisst.nc,varid='time') #Access the time field
hadisst.nc$dim$time$units #Not sure but works to get count from start date to work in days not seconds (nb dim is dimensions)
dates=as.Date(time,origin='1870-1-1') #Defines how date formatted and where to start from
Temp <- ncvar_get(hadisst.nc,"sst")

#Create all possible paired combinations of values of lon and lat, creates a vector
lonlat <- expand.grid(lon, lat)

#Make a large numeric value set from an array value set
tmp.vec.long <- as.vector(Temp)
tmp.vec.long

#Make matrix shell of all lat/lon coordinations with using the temperature data
nlon<-dim(lon)
nlat<-dim(lat)
nt<-dim(time)
tmp.mat <- matrix(tmp.vec.long, nrow = nlon * nlat, ncol = nt) 

#Create a data frame from the tmp.mat matrix and stick latlon (2 columns) onto the end of it
lonlat <- expand.grid(lon, lat)
tmp.df02 <- data.frame(cbind(tmp.mat,lonlat))

#Limiting dataset to 1965 onwards, note change column ranges to suit input data and selected time range
tmp.df02_a<-tmp.df02[,1141:1821] 
datesa<-dates[1141:1819]
names(tmp.df02_a)<-c(datesa,"lon","lat") #error here corrected below
names(tmp.df02_a)<-datesa #FIXED THIS
names(tmp.df02_a)[681]<-"lat"
names(tmp.df02_a)[680]<-"lon"

#Limiting lat/lon range so dataset is relevant
tmp.df02_b<-subset(tmp.df02_a,lon>-20&lon<30)
tmp.df02_b<-subset(tmp.df02_b,lat>35&lat<65)

#Output final matrix
write.table(tmp.df02_b, file="tmp.df02_b.csv", row.names = FALSE, sep = ",")

#Melt the data from wide to long 
tmp.df02_b_long <- melt(data = tmp.df02_b, id.vars = c("lon", "lat"), measure.vars = c(1:679))
head(tmp.df02_b_long) 
#"value" column is temp

#Split the date out into month and year
tmp.df03 <- separate(tmp.df02_b_long, variable, sep="-", c("Year", "Month", "Day"))

#Save
write.csv(tmp.df03, file="tmp.df03.csv",row.names = FALSE)

#Convert the characters to integers in the temp data
tmp.df03$Day  <- as.integer(as.character(tmp.df03$Day))
tmp.df03$Month  <- as.integer(as.character(tmp.df03$Month))
tmp.df03$Year  <- as.integer(as.character(tmp.df03$Year))


###Merge Temperature data with hauls data#############################################################

#Make single gridcell and date columns in Studyelasmos4 datafile
Studyelasmos4$GridCell <- paste(Studyelasmos4$ShootLong, "_", Studyelasmos4$ShootLat)
Studyelasmos4$Date <- paste(Studyelasmos4$Month, "_", Studyelasmos4$Year)
head(Studyelasmos4)

#Make matching gridcell and date columns in tmp.df03 datafile
tmp.df03$GridCell <- paste(tmp.df03$lon, "_", tmp.df03$lat)
tmp.df03$Date <- paste(tmp.df03$Month, "_", tmp.df03$Year)
head(tmp.df03)

#Align the two datasets
Elasmostemp<-merge(Studyelasmos4, tmp.df03, by=c("Date", "GridCell"), all.x=TRUE)
head(Elasmostemp)

#Replace -1000 in the temperature (value) column with NA
Elasmostemp$value <- as.numeric(Elasmostemp$value)
Elasmostemp$value <- gsub("-1000", "NA", Elasmostemp$value)

#Count the number of NA's for temperature
sum(is.na(Elasmostemp$value)) #this shows 3272 hauls without a temperature, out of 117912.

# Remove hauls without a temperature
Elasmostemp2 <- Elasmostemp[complete.cases(value),]

#Save combined dataset
write.table(Elasmostemp2, file="Elasmostemp.csv", row.names = FALSE, sep = ",")


###Add standardised salinity data from bio-oracle and merge###########################################

#Adding current standardised (by gridcell) salinity data from Bio-orcale 
Elasmostemp2<- read.table("Elasmostemp.csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)

##(Present, Surface, Mean, PSS)
Salinity <- raster("Present.Surface.Salinity.Mean.asc")
SalFile <- as.data.frame(Salinity, xy=TRUE)
SalFile

#Chnaging the names x and y to lat and lon
names(SalFile)[2]<-"lat"
names(SalFile)[1]<-"lon"

#Limiting lat/lon range so dataset is relevant
SalFile2<-subset(SalFile,lon>-20&lon<30)
SalFile3<-subset(SalFile2,lat>35&lat<65)
SalFile3.1 <- SalFile3[complete.cases(SalFile3$Present.Surface.Salinity.Mean),]

#Save dataset
write.table(SalFile3.1, file="SalinityFile.csv", row.names = FALSE, sep = ",")

#Ceiling longitude and latitude into 0.5 degree
SalFile3.1$lat.x<-(ceiling(SalFile3.1$lat)) -0.5
SalFile3.1$lon.x<-(ceiling(SalFile3.1$lon)) -0.5

#Remove original unrounded lat and lon
SalFile3.2<-subset(SalFile3.1, select= -c(1,2))

#Rename lat and lon to original variable names
SalFile3.2 <-SalFile3.2 %>% dplyr::rename (lat=lat.x)
SalFile3.2 <-SalFile3.2 %>% dplyr::rename (lon=lon.x)

#Average temperatures by Gridcell, so only 1 temperature value per gridcell
SalFile5<- SalFile3.2 %>% group_by(lat,lon) %>% dplyr::summarise(across (c(Present.Surface.Salinity.Mean), mean))

#Make single gridcell column in Tempfile4 datafile
SalFile5$GridCell <- paste(SalFile5$lon, "_", SalFile5$lat)
head(SalFile5)

sum(is.na(SalFile5$Present.Surface.Salinity.Mean))

#Align the two datasets based on gridcell
Elasmostemp2.1<-merge(Elasmostemp2, SalFile5,by=c("GridCell"))
head(Elasmostemp2.1)

#Rename the variable name
Elasmostemp3 <- Elasmostemp2.1 %>% dplyr::rename(CurrentSal = Present.Surface.Salinity.Mean) 

#Count the number of NA's for temperature
sum(is.na(Elasmostemp3$CurrentSal))

#Save combined dataset
write.table(Elasmostemp3, file="Elasmostemp.csv", row.names = FALSE, sep = ",")


###Quality checks on dataset##########################################################################

#Read in elasmostemp data file 
Elasmostemp<- read.table("Elasmostemp.csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)

#Turn the -9 values to NAs in BotTemp and BotSal data
Elasmostemp$BotTemp[Elasmostemp$BotTemp == -9] <- NA
Elasmostemp$BotSal[Elasmostemp$BotSal == -9] <- NA

#Save this
write.table(Elasmostemp, file="Elasmostemp.csv", row.names = FALSE, sep = ",")

#Attach the dataset
attach(Elasmostemp)

#log data and plot against depth to check there isnt anything that looks wrong 
logHperlo<-log(`Heptranchias perlo`)
plot(Depth, logHperlo)
#1 data point
logHgriseus<-log(`Hexanchus griseus`)
plot(Depth, logHgriseus)
#No outliers (good amount of data)
logLnasus<-log(`Lamna nasus`)
plot(Depth, logLnasus)
#~12 datapoints, no outliers 
logCmaximus<-log(`Cetorhinus maximus`)
plot(Depth, logCmaximus)
#1 datapoint
logAvulpinus<-log(`Alopias vulpinus`)
plot(Depth, logAvulpinus)
#error- no values?
logAaphyodes<-log(`Apristurus aphyodes`)
plot(Depth, logAaphyodes)
#Spread across large range from 800-2000, small dataset, no outliers 
logAmanis<-log(`Apristurus manis`)
plot(Depth, logAmanis)
#Small dataset, ranges from 800-1500, no outliers 
logAmelanoasper<-log(`Apristurus melanoasper`)
plot(Depth, logAmelanoasper)
#~12 datapoints, no anomolies, stay at ~1500
logAmicrops<-log(`Apristurus microps`)
plot(Depth, logAmicrops)
#~7 datapoints, no anomolies, stay around 1500-2000
logGatlanticus<-log(`Galeus atlanticus`)
plot(Depth, logGatlanticus)
#Good amount of data, no anomolies, stay within 200-750
logGmelastomus<-log(`Galeus melastomus`)
plot(Depth, logGmelastomus)
#Loads of data, no anomolies, stay between 0-1000
logGmurinus<-log(`Galeus murinus`)
plot(Depth, logGmurinus)
#~20 datapoints, 3 data points anonymous at ~200, stay within 800-1500
logScanicula<-log(`Scyliorhinus canicula`)
plot(Depth, logScanicula)
#Loads of data, no anomolies, stay betweeen 0-800
logSstellaris<-log(`Scyliorhinus stellaris`)
plot(Depth, logSstellaris)
#Good amount if data, no anomolies, stay within 0-500
logGgaleus<-log(`Galeorhinus galeus`)
plot(Depth, logGgaleus)
#Good amount of data, no anomolies, stay within 0-500
logMasterias<-log(`Mustelus asterias`)
plot(Depth, logMasterias)
#Loads of data, no anomolies, stay within 0-400
logPglauca<-log(`Prionace glauca`)
plot(Depth, logPglauca)
#1 datapoint
logEprinceps<-log(`Etmopterus princeps`)
plot(Depth, logEprinceps)
#Small amount of data, wide disribution from 500-2000, no anomolies 
logEpusillus<-log(`Etmopterus pusillus`)
plot(Depth, logEpusillus)
#~12 datapoints, no anomolies, stay between 500-800
logEspinax<-log(`Etmopterus spinax`)
plot(Depth, logEspinax)
#Loads of data, 2 anomolous points one at ~1300 and one at ~1500, stay between 0-800
logCfabricii<-log(`Centroscyllium fabricii`)
plot(Depth, logCfabricii)
#~15 datapoints, 1 anomolie at 500, stay between 1000-1500
logSmicrocephalus<-log(`Somniosus microcephalus`)
plot(Depth, logSmicrocephalus)
#2 datapoints 
logSrostratus<-log(`Somniosus rostratus`)
plot(Depth, logSrostratus)
#1 datapoint
logCcoelolepis<-log(`Centroscymnus coelolepis`)
plot(Depth, logCcoelolepis)
#Small amount of data, large range from 500-2000, no anomolies 
logCcrepidater<-log(`Centroselachus crepidater`)
plot(Depth, logCcrepidater)
#Small amount of data, no anomolies, wide range of 500-2000
logSringens<-log(`Scymnodon ringens`)
plot(Depth, logSringens)
#Ok amount of data, 1 anomolie at ~200, stay between 500-1000
logOparadoxus<-log(`Oxynotus paradoxus`)
plot(Depth, logOparadoxus)
#2 datapoints 
logDlicha<-log(`Dalatias licha`)
plot(Depth, logDlicha)
#Ok amount of data, 1 anomolie at ~1800, stay between 400-1000
logCgranulosus<-log(`Centrophorus granulosus`)
plot(Depth, logCgranulosus)
#error- no values?
logCsquamosus<-log(`Centrophorus squamosus`)
plot(Depth, logCsquamosus)
#Small dataset, wide range of 250-1500, no anomolies 
logDprofundorum<-log(`Deania profundorum`)
plot(Depth, logDprofundorum)
#Small dataset, no outliers, stay between 500-800
logSacanthias<-log(`Squalus acanthias`)
plot(Depth, logSacanthias)
#Loads of data, no anomolies, tend to stay between 0-500
logTnobiliana<-log(`Torpedo nobiliana`)
plot(Depth, logTnobiliana)
#~10 datapoints, no anomolies, stay around 200
logTmarmorata<-log(`Torpedo marmorata`)
plot(Depth, logTmarmorata)
#Small dataset, no anomolies, stay from 0-200
logTtorpedo<-log(`Torpedo torpedo`)
plot(Depth, logTtorpedo)
#4 datapoints, no anomolies, stay between 0-200
logBpallida<-log(`Bathyraja pallida`)
plot(Depth, logBpallida)
#3 datapoints, stay between 1700-2000
logBrichardsoni<-log(`Bathyraja richardsoni`)
plot(Depth, logBrichardsoni)
#4 datapoints, stay between 1700-2000, no anomolies 
logAjenseni<-log(`Amblyraja jenseni`)
plot(Depth, logAjenseni)
#~10 datapoints, wide range form 500-1800 no anomolies 
logAradiata<-log(`Amblyraja radiata`)
plot(Depth, logAradiata)
#Loads of data, no anomolies, range between 0-400
logDbatis<-log(`Dipturus batis`)
plot(Depth, logDbatis)
#Ok dataset, no anomolies, range between 0-1000
logDnidarosiensis<-log(`Dipturus nidarosiensis`)
plot(Depth, logDnidarosiensis)
#Small dataset, no anomolies, range between 0-1000
logDoxyrinchus<-log(`Dipturus oxyrinchus`)
plot(Depth, logDoxyrinchus)
#Small dataset, no anomolies, range between 0-700
logLcircularis<-log(`Leucoraja circularis`)
plot(Depth, logLcircularis)
#Good dataset, no anomolies, range between 
logLfullonica<-log(`Leucoraja fullonica`)
plot(Depth, logLfullonica)
#Ok dataset, 1 anomolie at 700, range between 0-500
logLnaevus<-log(`Leucoraja naevus`)
plot(Depth, logLnaevus)
#Loads of data, no anomolies, range between 0-1000
logMkreffti<-log(`Malacoraja kreffti`)
plot(Depth, logMkreffti)
#2 datapoints 
logNcaerulea<-log(`Neoraja caerulea`)
plot(Depth, logNcaerulea)
#~20 datapoints, no anomolies, large range of 200-2000
logRbrachyura<-log(`Raja brachyura`)
plot(Depth, logRbrachyura)
#Good dataset, 2 anomolies, at ~500, range between 0-250
logRclavata<-log(`Raja clavata`)
plot(Depth, logRclavata)
#Loads of data, no anomolous points, range between 0-1000
logRmicroocellata<-log(`Raja microocellata`)
plot(Depth, logRmicroocellata)
#Ok dataset, no anomolies, range between 0-25
logRmiraletus<-log(`Raja miraletus`)
plot(Depth, logRmiraletus)
#Small dataset, no anomolies, range between 0-400
logRmontagui<-log(`Raja montagui`)
plot(Depth, logRmontagui)
#Loads of data, no anomolies, range between 0-500
logRundulata<-log(`Raja undulata`)
plot(Depth, logRundulata)
#Ok dataset, 1 anomolie at 500, range between 0-200
logRbathyphila<-log(`Rajella bathyphila`)
plot(Depth, logRbathyphila)
#~20 datapoints, wide range from 0-1500, no anomolies 
logRbigelowi<-log(`Rajella bigelowi`)
plot(Depth, logRbigelowi)
#2 datapoints
logRfyllae<-log(`Rajella fyllae`)
plot(Depth, logRfyllae)
#Small dataset, wide range from 0-1800, no anomolies 
logRkukujevi<-log(`Rajella kukujevi`)
plot(Depth, logRkukujevi)
#2 datapoints
logRalba<-log(`Rostroraja alba`)
plot(Depth, logRalba)
#~10 datapoints, no anomolies, range between 0-300
logDlinteus<-log(`Dipturus linteus`)
plot(Depth, logDlinteus)
#3 datapoints 
logDpastinaca<-log(`Dasyatis pastinaca`)
plot(Depth, logDpastinaca)
#Small dataset, no anomolies, range between 0-250
logMaquila<-log(`Myliobatis aquila`)
plot(Depth, logMaquila)
#~20 datapoints, no anomolies, range between 0-100
logCmonstrosa<-log(`Chimaera monstrosa`)
plot(Depth, logCmonstrosa)
#Loads of data, no anomolies, range mainly between 0-700

#Plot CPUEs of each species to see if there are any anomalies 
plot(`Heptranchias perlo`)
plot(`Hexanchus griseus`)
plot(`Lamna nasus`)
plot(`Cetorhinus maximus`)
plot(`Apristurus aphyodes`)
plot(`Apristurus manis`)
plot(`Apristurus melanoasper`)
plot(`Apristurus microps`)
plot(`Galeus atlanticus`)
plot(`Galeus melastomus`)
plot(`Galeus murinus`)
plot(`Scyliorhinus canicula`)
plot(`Scyliorhinus stellaris`)
plot(`Galeorhinus galeus`)
plot(`Mustelus asterius`)
plot(`Prionace glauca`)
plot(`Etmopterus princeps`)
plot(`Etmopterus pusillus`)
plot(`Etmopterus spinax`)
plot(`Centroscyllium fabricii`)
plot(`Somniosus microcephalus`)
plot(`Somniosus rostratus`)
plot(`Centroscymnus coelolepis`)
plot(`Centroselachus crepidater`)
plot(`Scymnodon ringens`)
plot(`Oxynotus paradoxus`)
plot(`Dalatias licha`)
plot(`Centrophorus squamosus`)
plot(`Deania profundorum`)
plot(`Squalus acanthias`)#1 value over 30,000 others are below 20,000
plot(`Torpedo nobiliana`)
plot(`Torpedo marmorata`)
plot(`Torpedo torpedo`)
plot(`Bathyraja pallida`)
plot(`Bathyraja richardsoni`)
plot(`Amblyraja jenseni`)
plot(`Amblyraja radiata`)#1 value -120 and #1 value at 7788 and rest at ~500
plot(`Dipturus batis`)
plot(`Dipturus nidarosiensis`)
plot(`Dipturus oxyrinchus`)
plot(`Leucoraja circularis`)
plot(`Leucoraja fullonica`)
plot(`Leucoraja naevus`)
plot(`Malacoraja kreffti`)
plot(`Neoraja caerulea`)
plot(`Raja brachyura`)
plot(`Raja clavata`)#6 values at 11552 and 1 value at -13.3 and rest ~1000
plot(`Raja microocellata`)
plot(`Raja miraletus`)
plot(`Raja montagui`)
plot(`Raja undulata`)
plot(`Rajella bathyphila`)
plot(`Rajella bigelowi`)
plot(`Rajella fyllae`)
plot(`Rajella kukujevi`)
plot(`Rostroraja alba`)
plot(`Dipturus linteus`)
plot(`Dasyatis pastinaca`)
plot(`Myliobatis aquila`)
plot(`Chimaera monstrosa`)

#Combine Mustelus asterius and Mustelus mustelus columns into 1 as they are the same species
Elasmostemp$MustelusCombined <- rowSums(Elasmostemp[ , c(29,30)], na.rm=TRUE)

#Remove original Mustelus asterius and Mustelus mustelus columns from dataset
Elasmostemp = subset(Elasmostemp, select = -c(29,30) )

#Rename MustelusCombined to Mustelus asterius 
Elasmostemp$`Mustelus asterius` <- Elasmostemp$MustelusCombined

#Move MustelusCombined to where species are in the dataset 
Elasmostemp <- Elasmostemp %>% relocate(MustelusCombined, .before = `Prionace glauca`)

#Remove MustelusCombined from dataset
Elasmostemp = subset(Elasmostemp, select = -c(29) )

#Move Mustelus asterius to where species are in the dataset 
Elasmostemp <- Elasmostemp %>% relocate(`Mustelus asterius`, .before = `Prionace glauca`)

#Save 
write.table(Elasmostemp, file="Elasmostemp2.csv", row.names = FALSE, sep = ",")

#See if there are any values for the 1 species with errors 
plot(Elasmostemp$`Centrophorus granulosus`)
#No values

#Remove C.granulosus species from the dataset with 0 values
Elasmostemp = subset(Elasmostemp, select = -c(42) )

#Save to see if worked
write.table(Elasmostemp, file="Elasmostemp3.csv", row.names = FALSE, sep = ",")


###Removing anomalous hauls###########################################################################

#Read in datafile
Elasmostemp3<- read.table("Elasmostemp3.csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)

###Get rid of anomalous hauls in the 4 species
#R.Clavata 11552 (rows 6973, 41221, 41401, 80177, 89287, 89891)
#A.radiata 7788 (row 71381)
#S.acanthius 32083.33333, 17936, 15116 (rows 27321, 20939, 20635)
#Remove row 23893 as has "inf" in a cell and no other values
#Remove row 44810 as contained negative values 

#-1 from each of the row values as r data set does not treat the top line as a row
Elasmostemp4<- Elasmostemp3 %>% slice(-c(6972, 41220, 41400, 80176, 89286, 89890, 71380, 27320, 20938, 20634, 23892, 44809))

#Check to see if right rows were taken out
plot(Elasmostemp3$`Squalus acanthias`)
plot(Elasmostemp4$`Squalus acanthias`)

plot(Elasmostemp3$`Raja clavata`)
plot(Elasmostemp4$`Raja clavata`)

plot(Elasmostemp3$`Amblyraja radiata`)
plot(Elasmostemp4$`Amblyraja radiata`)

#Save dataset
write.table(Elasmostemp4, file="Elasmostemp4.csv", row.names = FALSE, sep = ",")

#Rename ShootLat and ShootLong to lat and lon
Elasmostemp4 <-Elasmostemp4 %>% dplyr::rename (lat=ShootLat)
Elasmostemp4 <-Elasmostemp4 %>% dplyr::rename (lon=ShootLong)

#Average the CPUE across hauls in the same gridcell, month, haulcode etc
Elasmostemp5<- Elasmostemp4 %>% group_by(Date, GridCell, Survey) %>% summarise(across(c(lat, lon, Depth, value, CurrentSal, `Heptranchias perlo`,	`Hexanchus griseus`,	`Lamna nasus`,	`Cetorhinus maximus`, `Alopias vulpinus`,	`Apristurus aphyodes`,	`Apristurus manis`,	`Apristurus melanoasper`,	`Apristurus microps`,	`Galeus atlanticus`,	`Galeus melastomus`,	`Galeus murinus`,	`Scyliorhinus canicula`,	`Scyliorhinus stellaris`,	`Galeorhinus galeus`,	`Mustelus asterius`,	`Prionace glauca`,	`Etmopterus princeps`,	`Etmopterus pusillus`,	`Etmopterus spinax`,	`Centroscyllium fabricii`,	`Somniosus microcephalus`,	`Somniosus rostratus`,	`Centroscymnus coelolepis`,	`Centroselachus crepidater`,	`Scymnodon ringens`,	`Oxynotus paradoxus`,	`Dalatias licha`,	`Centrophorus squamosus`,	`Deania profundorum`,	`Squalus acanthias`,	`Torpedo nobiliana`,	`Torpedo marmorata`,	`Torpedo torpedo`,	`Bathyraja pallida`,	`Bathyraja richardsoni`,	`Amblyraja jenseni`,	`Amblyraja radiata`,	`Dipturus batis`,	`Dipturus nidarosiensis`,	`Dipturus oxyrinchus`,	`Leucoraja circularis`,	`Leucoraja fullonica`,	`Leucoraja naevus`,	`Malacoraja kreffti`,	`Neoraja caerulea`,	`Raja brachyura`,	`Raja clavata`,	`Raja microocellata`,	`Raja miraletus`,	`Raja montagui`,	`Raja undulata`,	`Rajella bathyphila`,	`Rajella bigelowi`,	`Rajella fyllae`,	`Rajella kukujevi`,	`Rostroraja alba`,	`Dipturus linteus`,	`Dasyatis pastinaca`,	`Myliobatis aquila`,	`Chimaera monstrosa`), mean))

#Save dataset
write.table(Elasmostemp5, file="summarisedelasmos.csv", row.names = FALSE, sep = ",")
write.table(Elasmostemp5, file="FinalisedDataset.csv", row.names = FALSE, sep = ",")

#Read in datafile
FinalDataset<- read.table("FinalisedDataset.csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)
attach(FinalDataset)

#Make dataset for the 6 chosen species 
ChosenSpecies<- subset(FinalDataset, select= c(1:8,23,69,46,24,39,56))

#Save
write.table(ChosenSpecies, file="6Speciesdata.csv", row.names = FALSE, sep = ",")


###Make future Bio-oracle temperature dataset########################################################

#Read in future temp data (2040-2050, RPC85, Surface, Mean, Degree c)
library(raster) 
FutureTemp <- raster("FutureTempdata.asc")
TempFile <- as.data.frame(FutureTemp, xy=TRUE)
TempFile

#Chnaging the names x and y to lat and lon
names(TempFile)[2]<-"lat"
names(TempFile)[1]<-"lon"

#Limiting lat/lon range so dataset is relevant
Tempfile2<-subset(TempFile,lon>-20&lon<30)
Tempfile3<-subset(Tempfile2,lat>35&lat<65)
Tempfile3.1 <- Tempfile3[complete.cases(Tempfile3$FutureTempdata),]

#Save dataset
write.table(Tempfile3.1, file="Futuretempfile.csv", row.names = FALSE, sep = ",")

#Ceiling lat and lon
Tempfile3.1$lat.x<-(ceiling(Tempfile3.1$lat)) -0.5
Tempfile3.1$lon.x<-(ceiling(Tempfile3.1$lon)) -0.5

#Remove original unrounded lat and lon
Tempfile3.2<-subset(Tempfile3.1, select= -c(1,2))

#Rename lat and lon to original variable names
Tempfile3.2 <-Tempfile3.2 %>% dplyr::rename (lat=lat.x)
Tempfile3.2 <-Tempfile3.2 %>% dplyr::rename (lon=lon.x)

#Average each cell so only 1 temp value per lat and lon combination
Tempfile3.3 <- Tempfile3.2 %>% group_by(lat, lon) %>% dplyr::summarise(across(c("FutureTempdata"), mean))

#Make single gridcell column in Tempfile4 datafile
Tempfile3.3$GridCell <- paste(Tempfile3.3$lon, "_", Tempfile3.3$lat)
head(Tempfile3.3)

#Check for NAs
sum(is.na(Tempfile3.3$FutureTempdata))

#Save dataset
write.table(Tempfile3.3, file="Futuretempfile.csv", row.names = FALSE, sep = ",")


###Make predict future file and merge with future temp data############################################

#Read in dataset
dataset1<- read.table("FinalisedDataset.csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)

#Subset data
predictiondata <- subset(dataset1, select = c(lat,lon,Depth,CurrentSal))

#Then summarise so only one row per grid cell and average depth
predictiondata <- predictiondata %>% group_by(lat, lon) %>% dplyr::summarise(across(c("Depth","CurrentSal"), mean))

#Bit of housekeeping then add the standardised variable columns we need
predictiondata <- predictiondata %>% mutate(Month = 6)
predictiondata <- predictiondata %>% mutate(Survey = "NS-IBTS")

#Add gridcell
predictiondata$GridCell <- paste(predictiondata$lon, "_", predictiondata$lat)

#Check data is complete
sum(is.na(predictiondata$Depth))

#Add future temp data- read in file 
FutureTemperature<- read.table("Futuretempfile.csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)

#Add temp data to prediction file
futureprediction<-merge(predictiondata, FutureTemperature, by=c("GridCell"))

#Rename variables should match gam variable names
names(futureprediction)[10] <- "CurrentTemp"
futureprediction = subset(futureprediction, select = -c(lat.x,lon.x) )

#Check NA shouldnt be any
sum(is.na(futureprediction$Depth))
sum(is.na(futureprediction$CurrentTemp))
sum(is.na(futureprediction$CurrentSal))

#Put variables in right form (same as in gam) and log depth
futureprediction$logDepth <- log(futureprediction$Depth+1)
futureprediction1 = subset(futureprediction, select = -c(Depth,GridCell))
names(futureprediction1)[4] <- "lat"
names(futureprediction1)[5] <- "lon"

futureprediction1$Survey<-as.character(futureprediction1$Survey)
futureprediction1$Month <- as.integer(futureprediction1$Month)

#Save future prediction file
write.csv(futureprediction1, file="Predictionfile.csv",row.names = FALSE)


###Make current Bio-oracle Temperature dataset#########################################################

#Read in current temp data (Present, Surface, Mean, Degree c)
library(raster) 
PresentTemp <- raster("Present.Surface.Temperature.Mean.asc")
TempFile <- as.data.frame(PresentTemp, xy=TRUE)
TempFile

#Chnaging the names x and y to lat and lon
names(TempFile)[2]<-"lat"
names(TempFile)[1]<-"lon"

#Limiting lat/lon range so dataset is relevant
Tempfile2<-subset(TempFile,lon>-20&lon<30)
Tempfile3<-subset(Tempfile2,lat>35&lat<65)
Tempfile3.1 <- Tempfile3[complete.cases(Tempfile3$Present.Surface.Temperature.Mean),]

#Save dataset
write.table(Tempfile3.1, file="PresentTempfile.csv", row.names = FALSE, sep = ",")

#Ceiling lat and lon
Tempfile3.1$lat.x<-(ceiling(Tempfile3.1$lat)) -0.5
Tempfile3.1$lon.x<-(ceiling(Tempfile3.1$lon)) -0.5

#Remove original unrounded lat and lon
Tempfile3.2<-subset(Tempfile3.1, select= -c(1,2))

#Rename lat and lon to original variable names
Tempfile3.2 <-Tempfile3.2 %>% dplyr::rename (lat=lat.x)
Tempfile3.2 <-Tempfile3.2 %>% dplyr::rename (lon=lon.x)

#Average each cell so only 1 temp value per lat and lon combination
Tempfile3.3 <- Tempfile3.2 %>% group_by(lat, lon) %>% dplyr::summarise(across(c("Present.Surface.Temperature.Mean"), mean))

#Make single gridcell column in Tempfile4 datafile
Tempfile3.3$GridCell <- paste(Tempfile3.3$lon, "_", Tempfile3.3$lat)
head(Tempfile3.3)

sum(is.na(Tempfile3.3$Present.Surface.Temperature.Mean))

#Change temp variable name to what it would be in gam
Tempfile3.3 <- Tempfile3.3 %>% dplyr::rename(CurrentTemp = Present.Surface.Temperature.Mean)

#Save dataset
write.table(Tempfile3.3, file="PresentTempfile.csv", row.names = FALSE, sep = ",")


###Make current predict file and combine with current temp data#######################################

#Read in dataset
dataset1<- read.table("FinalisedDataset.csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)

Presentdata <- subset(dataset1, select = c(lat,lon,Depth) )

#Then summarise so only one row per grid cell and average depth
Presentdata <- Presentdata %>% group_by(lat, lon) %>% dplyr::summarise(across(c("Depth", "CurrentSal"), mean))

#Bit of housekeeping then add the standardised variable columns we need
Presentdata <- Presentdata %>% mutate(Month = 6)
Presentdata <- Presentdata %>% mutate(Survey = "NS-IBTS")

#Add gridcell
Presentdata$GridCell <- paste(Presentdata$lon, "_", Presentdata$lat)

#Check data is complete
sum(is.na(Presentdata$Depth))

#Add current temp data- read in file 
PresentTemperature<- read.table("PresentTempfile.csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)

#Add temp data to prediction file
presentprediction<-merge(Presentdata, PresentTemperature, by=c("GridCell"))

#Rename vairbales should match gam variable names
names(presentprediction)[2] <- "lat"
names(presentprediction)[3] <- "lon"
presentprediction = subset(presentprediction, select = -c(lat.y,lon.y) )

#Check NA shouldnt be any
sum(is.na(presentprediction$Depth))
sum(is.na(presentprediction$CurrentTemp))
sum(is.na(presentprediction$CurrentSal))

#Put variables in right form (same as in gam) and log depth
presentprediction$logDepth <- log(presentprediction$Depth+1)
presentprecition1 = subset(presentprediction, select = -c(Depth,GridCell) )

presentprecition1$Survey<-as.character(presentprecition1$Survey)
presentprecition1$Month <- as.integer(presentprecition1$Month)

#Save current prediction file
write.csv(presentprecition1, file="Presentfile.csv",row.names = FALSE)


###Make training file for gams#######################################################################

#Read in data
dataset<- read.table("FinalisedDataset.csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)

#Split up date into month and year columns from GridCell
dataset1 <- separate(dataset, Date, sep="_", c("Month", "Year"))

#Log species data and depth
dataset1$logDepth <- log(dataset1$Depth+1)#NANs produced
dataset1$log_Galeorhinus_galeus <- log(dataset1$"Galeorhinus galeus"+1)
dataset1$log_Chimaera_monstrosa <- log(dataset1$"Chimaera monstrosa"+1)
dataset1$log_Amblyraja_radiata <- log(dataset1$"Amblyraja radiata"+1)
dataset1$log_Mustelus_asterias <- log(dataset1$"Mustelus asterius"+1)
dataset1$log_Squalus_acanthias <- log(dataset1$"Squalus acanthias"+1)
dataset1$log_Raja_clavata <- log(dataset1$"Raja clavata"+1)

#Remove NaNs from depth
dataset1$logDepth[which(dataset1$logDepth == Inf)] <- NA
dataset1$logDepth[which(dataset1$logDepth == -Inf)] <- NA
dataset1$logDepth[which(is.nan(dataset1$logDepth))] <- NA

#Name value "CurrentTemp"- has to be same for all files
dataset1 <-dataset1 %>% dplyr::rename (CurrentTemp=value)

#Put variables in the right format
dataset1$Survey<-as.character(dataset1$Survey)
dataset1$Year <- as.integer(dataset1$Year)
dataset1$Month <- as.integer(dataset1$Month)

#Keep the 6 species we want 
finalisedDataset3<- subset(dataset1, select=c(1:9, 71:77))

#Remove -9s in depth from dataset for gams 
dataset1$logDepth[which(dataset1$logDepth <0)] <- NA

#Write out dataset
write.csv(finalisedDataset3, file="FinalisedDataset3.csv",row.names = FALSE)
dataset1<- read.table("FinalisedDataset3.csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)


###RUN GAMS##########################################################################################

###Dataset for Ggaleus 
Gdata<- subset(dataset1, select=c(1:2, 4:6, 8:11))

#Galeorhinus galeus GAM
gamGgaleus <- gam(dataset1$log_Galeorhinus_galeus ~ Survey + s(Month, k=3) + s(logDepth, k=3) + s(CurrentTemp, k=3) + s(CurrentSal, k=3), family=nb, select=TRUE, method="REML", data = Gdata)

#Summary of model
summary(gamGgaleus)

#Check of model
gam.check(gamGgaleus)

#AIC
AIC(gamGgaleus)

#Plots of Species against environmental variables 
plot(gamGgaleus)
plot(gamGgaleus, select=1, ylim=c(-1,1), xlab="Month")
plot(gamGgaleus, select=2, ylim=c(-7,5), xlab="logDepth (m)")
plot(gamGgaleus, select=3, ylim=c(-4,1), xlab="Temperature (°C)")
plot(gamGgaleus, select=4, ylim=c(-30,3), xlab="Salinity (PSS)")

#2D plot
vis.gam(gamGgaleus, view = c("CurrentTemp", "logDepth"), type='response', plot.type='contour',  xlab="Temperature (°C)", ylab="logDepth (m)", main="")


###Dataset for Cmonstrosa 
Cdata<- subset(dataset1, select=c(1:2, 4:6, 8:10, 12))

#Chimaera monstrosa GAM using zero inflated poisson family as nb wasn't fitting
#For this family, the data needs to be a integer not numeric
Cdata$log_Chimaera_monstrosa <- as.integer(Cdata$log_Chimaera_monstrosa)
gamCmonstrosa <- gam(Cdata$log_Chimaera_monstrosa ~ s(Month,k=3) + s(logDepth, k=3) + s(CurrentTemp, k=3) + s(CurrentSal, k=3), family=ziP(), data=Cdata)
#Note we removed Survey for this gam

#Summary of model
summary(gamCmonstrosa)

#Need to calculate r2 as it isn't given in summary
#a function for calculating R2 from a GAM
gamR2 <- function(gam){
  R2 <- 1-((sum(residuals(gam)^2))/
             (sum((gam$y - mean(gam$y))^2)))
  R2adj <- 1- ((1 - R2) * (length(gam$y) - 1)/
                 (length(gam$y) - length(gam$coefficients) - 1))
  a <- data.frame(R2, R2adj)
  return(a)
}

#Using function to generate r2
gamR2(gamCmonstrosa)

#Check of model
gam.check(gamCmonstrosa)

#AIC
AIC(gamCmonstrosa)

#Plots of Species against environmental variables 
plot(gamCmonstrosa)
plot(gamCmonstrosa, select=1, ylim=c(-0.1,0.1), xlab="Month")
plot(gamCmonstrosa, select=2, ylim=c(-11,2), xlab="logDepth (m)")
plot(gamCmonstrosa, select=3, ylim=c(-0.5,0.5), xlab="Temperature (°C)")
plot(gamCmonstrosa, select=4, ylim=c(-1,0.5), xlab="Salinity (PSS)")

#2D plot
vis.gam(gamCmonstrosa, view = c("CurrentTemp", "logDepth"), type='response', plot.type='contour', xlab="Temperature (°C)", ylab="logDepth (m)", main="")


###Dataset for Aradiata 
Adata<- subset(dataset1, select=c(1:2, 4:6, 8:10, 13))

#Amblyraja radiata GAM
gamAradiata <- gam(dataset1$log_Amblyraja_radiata ~ Survey + s(Month, k=3) + s(logDepth, k=3) + s(CurrentTemp, k=3) + s(CurrentSal, k=3), family=nb, select=TRUE, method="REML", data = Adata)

#Summary of model
summary(gamAradiata)

#Check of model
gam.check(gamAradiata)

#AIC
AIC(gamAradiata)

#Plots of Species against environmental variables 
plot(gamAradiata)
plot(gamAradiata, select=1, ylim=c(-0.75,1), xlab="Month")
plot(gamAradiata, select=2, ylim=c(-12,1), xlab="logDepth (m)")
plot(gamAradiata, select=3, ylim=c(-1.5,1), xlab="Temperature (°C)")
plot(gamAradiata, select=4, ylim=c(-6,0.7), xlab="Salinity (PSS)")

#2D plot
vis.gam(gamAradiata, view = c("CurrentTemp", "logDepth"), type='response', plot.type='contour', xlab="Temperature (°C)", ylab="logDepth (m)", main="")


###Dataset for Masterias
Mdata<- subset(dataset1, select=c(1:2, 4:6, 8:10, 14))

#Mustelus asterias radiata GAM
gamMasterias <- gam(dataset1$log_Mustelus_asterias ~ Survey + s(Month, k=3) + s(logDepth, k=3) + s(CurrentTemp, k=3) + s(CurrentSal, k=3), family=nb, select=TRUE, method="REML", data = Mdata)

#Summary of model
summary(gamMasterias)

#Check of model
gam.check(gamMasterias)

#AIC
AIC(gamMasterias)

#Plots of Species against environmental variables 
plot(gamMasterias)
plot(gamMasterias, select=1, ylim=c(-2,3), xlab="Month")
plot(gamMasterias, select=2, ylim=c(-7.5,2), xlab="logDepth (m)")
plot(gamMasterias, select=3, ylim=c(-7,2), xlab="Temperature (°C)")
plot(gamMasterias, select=4, ylim=c(-20,2), xlab="Salinity (PSS)")

#2D plot
vis.gam(gamMasterias, view = c("CurrentTemp", "logDepth"), type='response', plot.type='contour',xlab="Temperature (°C)", ylab="logDepth (m)", main="")


###Dataset for Sacanthias
Sdata<- subset(dataset1, select=c(1:2, 4:6, 8:10, 15))

#Mustelus asterias radiata GAM
gamSacanthias <- gam(dataset1$log_Squalus_acanthias ~ Survey + s(Month, k=3) + s(logDepth, k=3) + s(CurrentTemp, k=3) + s(CurrentSal, k=3), family=nb, select=TRUE, method="REML", data = Sdata)

#Summary of model
summary(gamSacanthias)

#Check of model
gam.check(gamSacanthias)

#AIC
AIC(gamSacanthias)

#Plots of Species against environmental variables 
plot(gamSacanthias)
plot(gamSacanthias, select=1, ylim=c(-0.7,0.7), xlab="Month")
plot(gamSacanthias, select=2, ylim=c(-7.5,1), xlab="logDepth (m)")
plot(gamSacanthias, select=3, ylim=c(-4,1), xlab="Temperature (°C)")
plot(gamSacanthias, select=4, ylim=c(-10,1), xlab="Salinity (PSS)")

#2D plot
vis.gam(gamSacanthias, view = c("CurrentTemp", "logDepth"), type='response', plot.type='contour', xlab="Temperature (°C)", ylab="logDepth (m)", main="")


###Dataset for Rclavata
Rdata<- subset(dataset1, select=c(1:2, 4:6, 8:10, 16))

#Mustelus asterias radiata GAM
gamRclavata <- gam(dataset1$log_Raja_clavata ~ Survey + s(Month, k=3) + s(logDepth, k=3) + s(CurrentTemp, k=3) + s(CurrentSal, k=3), family=nb, select=TRUE, method="REML", data = Rdata)

#Summary of model
summary(gamRclavata)

#Check of model
gam.check(gamRclavata)

#AIC
AIC(gamRclavata)

#Plots of Species against environmental variables 
plot(gamRclavata)
plot(gamRclavata, select=1, ylim=c(-1,1.2), xlab="Month")
plot(gamRclavata, select=2, ylim=c(-4,5), xlab="logDepth (m)")
plot(gamRclavata, select=3, ylim=c(-2,1), xlab="Temperature (°C)")
plot(gamRclavata, select=4, ylim=c(-6.5,1.5), xlab="Salinity (PSS)")

#2D plot
vis.gam(gamRclavata, view = c("CurrentTemp", "logDepth"), type='response', plot.type='contour', xlab="Temperature (°C)", ylab="logDepth (m)", main="")


###RUN PRESENT PREDICT GAMS AND MAPS#################################################################

#Load in data
PresentdataTemp1<- read.table("Presentfile.csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)


###G galeus
present_Ggaleus <- predict.gam(gamGgaleus,PresentdataTemp1, type = "response")
present_Ggaleus2 <- cbind(present_Ggaleus,PresentdataTemp1)

#Change variable name to CPUE and backtransform log
present_Ggaleus2$CPUE <- 10^(present_Ggaleus2$present_Ggaleus-1)

#Summarise the CPUE across lon and lat
present_Ggaleus3 <- plyr::ddply(present_Ggaleus2, c("lon","lat"), summarise, CPUE = mean(CPUE))

#Map
Ggaleuspresentmap1 <- basemap(limits = c(-12, 14, 36, 62), bathymetry = TRUE, bathy.style = "contour_blues", land.col = "#607D3B", land.border.col = NA)  + geom_spatial_point(data = present_Ggaleus3, aes(x = lon, y = lat, size = CPUE), color = "black") 
Ggaleuspresentmap2 <- Ggaleuspresentmap1 +   scale_size(range = c(0,5), breaks = c(0.1000001,0.15,0.20), labels = c("0.10","0.15","0.20"), guide="legend") 
Ggaleuspresentmap2 <- reorder_layers(Ggaleuspresentmap2)


###C monstrosa
present_Cmonstrosa <- predict.gam(gamCmonstrosa,PresentdataTemp1, type = "response")
present_Cmonstrosa2 <- cbind(present_Cmonstrosa,PresentdataTemp1)

#Change variable name to CPUE and backtransform log
present_Cmonstrosa2$CPUE <- 10^(present_Cmonstrosa2$present_Cmonstrosa-1)

#Summarise the CPUE across lon and lat
present_Cmonstrosa3 <- plyr::ddply(present_Cmonstrosa2, c("lon","lat"), summarise, CPUE = mean(CPUE))

#Map
Cmonstrosapresentmap1<- basemap(limits = c(-12, 14, 36, 62), bathymetry = TRUE, bathy.style = "contour_blues", land.col = "#607D3B", land.border.col = NA)  + geom_spatial_point(data = present_Cmonstrosa3, aes(x = lon, y = lat, size = CPUE), color = "black") 
Cmonstrosapresentmap2 <- Cmonstrosapresentmap1 + scale_size(range = c(0,6), breaks = c(0.20,1.0,5.0), labels = c("0.20", "1.00", "5.00"), guide="legend") 
Cmonstrosapresentmap2 <- reorder_layers(Cmonstrosapresentmap2)


###A radiata
present_Aradiata <- predict.gam(gamAradiata,PresentdataTemp1, type = "response")
present_Aradiata2 <- cbind(present_Aradiata,PresentdataTemp1)

#Change variable name to CPUE and back transform log
present_Aradiata2$CPUE <- 10^(present_Aradiata2$present_Aradiata-1)

#Summarise the CPUE across lon and lat
present_Aradiata3 <- plyr::ddply(present_Aradiata2, c("lon","lat"), summarise, CPUE = mean(CPUE))

#Map
Aradiatapresentmap1<- basemap(limits = c(-12, 14, 36, 62), bathymetry = TRUE, bathy.style = "contour_blues", land.col = "#607D3B", land.border.col = NA)  + geom_spatial_point(data = present_Aradiata3, aes(x = lon, y = lat, size = CPUE), color = "black") 
Aradiatapresentmap2 <- Aradiatapresentmap1 + scale_size(range = c(0,4), breaks = c(0.20, 1.00, 2.00), labels = c("0.20", "1.00", "2.00"), guide="legend") 
Aradiatapresentmap2 <- reorder_layers(Aradiatapresentmap2)


###M asterias
present_Masterias <- predict.gam(gamMasterias,PresentdataTemp1, type = "response")
present_Masterias2 <- cbind(present_Masterias,PresentdataTemp1)

#Change variable name to CPUE and backtransform log
present_Masterias2$CPUE <- 10^(present_Masterias2$present_Masterias-1)

#Summarise the CPUE across lon and lat
present_Masterias3 <- plyr::ddply(present_Masterias2, c("lon","lat"), summarise, CPUE = mean(CPUE))

#Map
Masteriaspresentmap1<- basemap(limits = c(-12, 14, 36, 62), bathymetry = TRUE, bathy.style = "contour_blues", land.col = "#607D3B", land.border.col = NA)  + geom_spatial_point(data = present_Masterias3, aes(x = lon, y = lat, size = CPUE), color = "black") 
Masteriaspresentmap2 <- Masteriaspresentmap1 + scale_size(range = c(0,4), breaks = c(0.1000002, 0.13, 0.16), labels = c("0.10", "0.13", "0.16"), guide="legend") 
Masteriaspresentmap2 <- reorder_layers(Masteriaspresentmap2)


###S acanthias
present_Sacanthias <- predict.gam(gamSacanthias,PresentdataTemp1, type = "response")
present_Sacanthias2 <- cbind(present_Sacanthias,PresentdataTemp1)

#Change variable name to CPUE and backtransform log
present_Sacanthias2$CPUE <- 10^(present_Sacanthias2$present_Sacanthias-1)

#Summarise the CPUE across lon and lat
present_Sacanthias3 <- plyr::ddply(present_Sacanthias2, c("lon","lat"), summarise, CPUE = mean(CPUE))

#Map
Sacanthiaspresentmap1<- basemap(limits = c(-12, 14, 36, 62), bathymetry = TRUE, bathy.style = "contour_blues", land.col = "#607D3B", land.border.col = NA)  + geom_spatial_point(data = present_Sacanthias3, aes(x = lon, y = lat, size = CPUE), color = "black") 
Sacanthiaspresentmap2 <- Sacanthiaspresentmap1 + scale_size(range = c(0,3), breaks = c(0.1000330, 0.20, 0.30), labels = c("0.10", "0.20", "0.30"), guide="legend") 
Sacanthiaspresentmap2 <- reorder_layers(Sacanthiaspresentmap2)


###R clavata
present_Rclavata <- predict.gam(gamRclavata,PresentdataTemp1, type = "response")
present_Rclavata2 <- cbind(present_Rclavata,PresentdataTemp1)

#Change variable name to CPUE and backtransform log
present_Rclavata2$CPUE <- 10^(present_Rclavata2$present_Rclavata-1)

#Summarise the CPUE across lon and lat
present_Rclavata3 <- plyr::ddply(present_Rclavata2, c("lon","lat"), summarise, CPUE = mean(CPUE))

#Map
Rclavatapresentmap1<- basemap(limits = c(-12, 14, 36, 62), bathymetry = TRUE, bathy.style = "contour_blues", land.col = "#607D3B", land.border.col = NA)  + geom_spatial_point(data = present_Rclavata3, aes(x = lon, y = lat, size = CPUE), color = "black")
Rclavatapresentmap2 <- Rclavatapresentmap1 + scale_size(range = c(0,6), breaks = c(0.1001029, 0.15, 0.20), labels = c("0.10", "0.15", "0.20"), guide="legend") 
Rclavatapresentmap2 <- reorder_layers(Rclavatapresentmap2)


###RUN FUTURE PREDICT GAMS AND MAPS#################################################################

#Load in data
predictiondata3<- read.table("Predictionfile.csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)


###Galeorhinus galeus

#Run predict gam
predicted_Ggaleus <- predict.gam(gamGgaleus,predictiondata3, type = "response")
predicted_Ggaleus2 <- cbind(predicted_Ggaleus,predictiondata3)

#Change variable name to CPUE and back transform the log
predicted_Ggaleus2$CPUE <- 10^(predicted_Ggaleus2$predicted_Ggaleus-1)

#Summarise the CPUE across lon and lat
predicted_Ggaleus3 <- plyr::ddply(predicted_Ggaleus2, c("lon","lat"), summarise, CPUE = mean(CPUE))

#Map 
Ggaleusfuturemap1 <- basemap(limits = c(-12, 14, 36, 62), bathymetry = TRUE, bathy.style = "contour_blues", land.col = "#607D3B", land.border.col = NA)  + geom_spatial_point(data = predicted_Ggaleus3, aes(x = lon, y = lat, size = CPUE), color = "black") 
Ggaleusfuturemap2 <- Ggaleusfuturemap1 + scale_size(range = c(0,5), breaks = c(0.1000001,0.15,0.20), labels = c("0.10","0.15","0.20"), guide="legend") 
Ggaleusfuturemap2


###Chimaera monstrosa

#Preparing the prediction file 
predicted_Cmonstrosa <- predict.gam(gamCmonstrosa,predictiondata3, type = "response")
predicted_Cmonstrosa2 <- cbind(predicted_Cmonstrosa,predictiondata3)

##Change variable name to logCPUE
predicted_Cmonstrosa2$CPUE <- 10^(predicted_Cmonstrosa2$predicted_Cmonstrosa-1)

#Summarise the CPUE across lon and lat
predicted_Cmonstrosa3 <- plyr::ddply(predicted_Cmonstrosa2, c("lon","lat"), summarise, CPUE = mean(CPUE))

#Map
Cmonstrosafuturemap1<- basemap(limits = c(-12, 14, 36, 62), bathymetry = TRUE, bathy.style = "contour_blues", land.col = "#607D3B", land.border.col = NA)  + geom_spatial_point(data = predicted_Cmonstrosa3, aes(x = lon, y = lat, size = CPUE), color = "black") 
Cmonstrosafuturemap2 <- Cmonstrosafuturemap1 + scale_size(range = c(0,6), breaks = c(0.20,1.0,5.0), labels = c("0.20", "1.00", "5.00"), guide="legend") 
Cmonstrosafuturemap2


###Amblyraja radiata

#Preparing the prediction file 
predicted_Aradiata <- predict.gam(gamAradiata,predictiondata3, type = "response")
predicted_Aradiata2 <- cbind(predicted_Aradiata,predictiondata3)

##Change variable name to CPUE and back transform log
predicted_Aradiata2$CPUE <- 10^(predicted_Aradiata2$predicted_Aradiata-1)

#Summarise the CPUE across lon and lat
predicted_Aradiata3 <- plyr::ddply(predicted_Aradiata2, c("lon","lat"), summarise, CPUE = mean(CPUE))

#Map
Aradiatafuturemap1<- basemap(limits = c(-12, 14, 36, 62), bathymetry = TRUE, bathy.style = "contour_blues", land.col = "#607D3B", land.border.col = NA)  + geom_spatial_point(data = predicted_Aradiata3, aes(x = lon, y = lat, size = CPUE), color = "black") 
Aradiatafuturemap2 <- Aradiatafuturemap1 + scale_size(range = c(0,4), breaks = c(0.20, 1.00, 2.00), labels = c("0.20", "1.00", "2.00"), guide="legend") 
reorder_layers(Aradiatafuturemap2)


###Mustelus asterias

#Preparing the prediction file 
predicted_Masterias <- predict.gam(gamMasterias,predictiondata3, type = "response")
predicted_Masterias2 <- cbind(predicted_Masterias,predictiondata3)

##Change variable name to CPUE and back transform log
predicted_Masterias2$CPUE <- 10^(predicted_Masterias2$predicted_Masterias-1)

#Summarise the CPUE across lon and lat
predicted_Masterias3 <- plyr::ddply(predicted_Masterias2, c("lon","lat"), summarise, CPUE = mean(CPUE))

#Map
Masteriasfuturemap1<- basemap(limits = c(-12, 14, 36, 62), bathymetry = TRUE, bathy.style = "contour_blues", land.col = "#607D3B", land.border.col = NA)  + geom_spatial_point(data = predicted_Masterias3, aes(x = lon, y = lat, size = CPUE), color = "black") 
Masteriasfuturemap2 <- Masteriasfuturemap1 + scale_size(range = c(0,4), breaks = c(0.1000010, 0.13, 0.16), labels = c("0.10", "0.13", "0.16"), guide="legend") 
Masteriasfuturemap2


###Squalus acanthias

#Preparing the prediction file 
predicted_Sacanthias <- predict.gam(gamSacanthias,predictiondata3, type = "response")
predicted_Sacanthias2 <- cbind(predicted_Sacanthias,predictiondata3)

##Change variable name to CPUE and back transform log
predicted_Sacanthias2$CPUE <- 10^(predicted_Sacanthias2$predicted_Sacanthias-1)

#Summarise the CPUE across lon and lat
predicted_Sacanthias3 <- plyr::ddply(predicted_Sacanthias2, c("lon","lat"), summarise, CPUE = mean(CPUE))

#Map
Sacanthiasfuturemap1<- basemap(limits = c(-12, 14, 36, 62), bathymetry = TRUE, bathy.style = "contour_blues", land.col = "#607D3B", land.border.col = NA)  + geom_spatial_point(data = predicted_Sacanthias3, aes(x = lon, y = lat, size = CPUE), color = "black") 
Sacanthiasfuturemap2 <- Sacanthiasfuturemap1 + scale_size(range = c(0,3), breaks = c(0.1000335, 0.20, 0.30), labels = c("0.10", "0.20", "0.30"), guide="legend") 
Sacanthiasfuturemap2


###Raja clavata

#Preparing the prediction file 
predicted_Rclavata <- predict.gam(gamRclavata,predictiondata3, type = "response")
predicted_Rclavata2 <- cbind(predicted_Rclavata,predictiondata3)

##Change variable name to CPUE and back transform log
predicted_Rclavata2$CPUE <- 10^(predicted_Rclavata2$predicted_Rclavata-1)

#Summarise the CPUE across lon and lat
predicted_Rclavata3 <- plyr::ddply(predicted_Rclavata2, c("lon","lat"), summarise, CPUE = mean(CPUE))

#Map
Rclavatafuturemap1<- basemap(limits = c(-12, 14, 36, 62), bathymetry = TRUE, bathy.style = "contour_blues", land.col = "#607D3B", land.border.col = NA)  + geom_spatial_point(data = predicted_Rclavata3, aes(x = lon, y = lat, size = CPUE), color = "black") 
Rclavatafuturemap2 <- Rclavatafuturemap1 + scale_size(range = c(0,6), breaks = c(0.1001255, 0.15, 0.20), labels = c("0.10", "0.15", "0.20"), guide="legend") 
Rclavatafuturemap2


###Make plots of different surveys areas covered################################################

basemap(limits = c(-16, 24, 36, 62), bathymetry = TRUE, bathy.style = "contour_blues", land.col = "#607D3B", land.border.col = NA)  

#Read in data
dataset2<- read.table("Elasmostemp4.csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)

#Subset of coordinates with same CPUE to mark where hauls were done 
Surveymap<- subset (dataset2, select = c(Survey,ShootLat,ShootLong,Depth))

#Subset by survey
nsibts<-subset(Surveymap, Survey == "NS-IBTS")
bts<-subset(Surveymap, Survey == "BTS")
spporc<-subset(Surveymap, Survey == "SP-PORC")
spnorth<-subset(Surveymap, Survey == "SP-NORTH")
swcibts<-subset(Surveymap, Survey == "SWC-IBTS")
frcgfs<-subset(Surveymap, Survey == "FR-CGFS")
ieigfs<-subset(Surveymap, Survey == "IE-IGFS")
evhoe<-subset(Surveymap, Survey == "EVHOE")
nigfs<-subset(Surveymap, Survey == "NIGFS")
scowcgfs<-subset(Surveymap, Survey == "SCOWCGFS")
sparsa<-subset(Surveymap, Survey == "SP-ARSA")
ptibts<-subset(Surveymap, Survey == "PT-IBTS")
ieiams<-subset(Surveymap, Survey == "IE-IAMS")
scoroc<-subset(Surveymap, Survey == "SCOROC")
dws<-subset(Surveymap, Survey == "DWS")
btsviii<-subset(Surveymap, Survey == "BTS-VIII")
bits<-subset(Surveymap, Survey == "BITS")
dyfs<-subset(Surveymap, Survey == "DYFS")
rockall<-subset(Surveymap, Survey == "ROCKALL")
sesound<-subset(Surveymap, Survey == "SE-SOUND")
sns<-subset(Surveymap, Survey == "SNS")

#Map
a<- basemap(limits = c(-12, 14, 36, 62), land.col = "#607D3B", land.border.col = NA) + geom_spatial_point(data = nsibts, aes(x = ShootLong, y = ShootLat), color="red4") + labs(title="NS-IBTS") + labs(x="Longitude (°)") + labs(y="Latitude (°)") + theme(plot.title =element_text(size=8, face="bold"), axis.title=element_text(size=7), plot.margin=margin(c(t=0,r=0,b=2,l=1)))
b<- basemap(limits = c(-12, 14, 36, 62), land.col = "#607D3B", land.border.col = NA) + geom_spatial_point(data = bts, aes(x = ShootLong, y = ShootLat), color="red4") + labs(title="BTS") + labs(x="Longitude (°)") + labs(y="Latitude (°)") + theme(plot.title=element_text(size=8, face="bold"), axis.title=element_text(size=7), plot.margin=margin(c(t=0,r=0,b=2,l=1)))
c<- basemap(limits = c(-12, 14, 36, 62), land.col = "#607D3B", land.border.col = NA) + geom_spatial_point(data = spporc, aes(x = ShootLong, y = ShootLat), color="red4") + labs(title="SP-PORC") + labs(x="Longitude (°)") + labs(y="Latitude (°)") + theme(plot.title=element_text(size=8, face="bold"), axis.title=element_text(size=7), plot.margin=margin(c(t=0,r=0,b=2,l=1)))
d<- basemap(limits = c(-12, 14, 36, 62), land.col = "#607D3B", land.border.col = NA) + geom_spatial_point(data = spnorth, aes(x = ShootLong, y = ShootLat), color="red4") + labs(title="SP-NORTH") + labs(x="Longitude (°)") + labs(y="Latitude (°)") + theme(plot.title=element_text(size=8, face="bold"), axis.title=element_text(size=7), plot.margin=margin(c(t=0,r=0,b=2,l=1)))
e<- basemap(limits = c(-12, 14, 36, 62), land.col = "#607D3B", land.border.col = NA) + geom_spatial_point(data = swcibts, aes(x = ShootLong, y = ShootLat), color="red4") + labs(title="SWC-IBTS") + labs(x="Longitude (°)") + labs(y="Latitude (°)") + theme(plot.title=element_text(size=8, face="bold"), axis.title=element_text(size=7), plot.margin=margin(c(t=0,r=0,b=2,l=1)))
f<- basemap(limits = c(-12, 14, 36, 62), land.col = "#607D3B", land.border.col = NA) + geom_spatial_point(data = frcgfs, aes(x = ShootLong, y = ShootLat), color="red4") + labs(title="FR-CGFS") + labs(x="Longitude (°)") + labs(y="Latitude (°)") + theme(plot.title=element_text(size=8, face="bold"), axis.title=element_text(size=7), plot.margin=margin(c(t=0,r=0,b=2,l=1)))
g<- basemap(limits = c(-12, 14, 36, 62), land.col = "#607D3B", land.border.col = NA) + geom_spatial_point(data = ieigfs, aes(x = ShootLong, y = ShootLat), color="red4") + labs(title="IE-IGFS") + labs(x="Longitude (°)") + labs(y="Latitude (°)") + theme(plot.title=element_text(size=8, face="bold"), axis.title=element_text(size=7), plot.margin=margin(c(t=0,r=0,b=2,l=1)))
h<- basemap(limits = c(-12, 14, 36, 62), land.col = "#607D3B", land.border.col = NA) + geom_spatial_point(data = evhoe, aes(x = ShootLong, y = ShootLat), color="red4") + labs(title="EVHOE") + labs(x="Longitude (°)") + labs(y="Latitude (°)") + theme(plot.title=element_text(size=8, face="bold"), axis.title=element_text(size=7), plot.margin=margin(c(t=0,r=0,b=2,l=1)))
i<- basemap(limits = c(-12, 14, 36, 62), land.col = "#607D3B", land.border.col = NA) + geom_spatial_point(data = nigfs, aes(x = ShootLong, y = ShootLat), color="red4") + labs(title="NIGFS") + labs(x="Longitude (°)") + labs(y="Latitude (°)") + theme(plot.title=element_text(size=8, face="bold"), axis.title=element_text(size=7), plot.margin=margin(c(t=0,r=0,b=2,l=1)))
j<- basemap(limits = c(-12, 14, 36, 62), land.col = "#607D3B", land.border.col = NA) + geom_spatial_point(data = scowcgfs, aes(x = ShootLong, y = ShootLat), color="red4") + labs(title="SCOWCGFS") + labs(x="Longitude (°)") + labs(y="Latitude (°)") + theme(plot.title=element_text(size=8, face="bold"), axis.title=element_text(size=7), plot.margin=margin(c(t=0,r=0,b=2,l=1)))
k<- basemap(limits = c(-12, 14, 36, 62), land.col = "#607D3B", land.border.col = NA) + geom_spatial_point(data = sparsa, aes(x = ShootLong, y = ShootLat), color="red4") + labs(title="SP-ARSA") + labs(x="Longitude (°)") + labs(y="Latitude (°)") + theme(plot.title=element_text(size=8, face="bold"), axis.title=element_text(size=7), plot.margin=margin(c(t=0,r=0,b=2,l=1)))
u<- basemap(limits = c(-12, 14, 36, 62), land.col = "#607D3B", land.border.col = NA) + geom_spatial_point(data = ptibts, aes(x = ShootLong, y = ShootLat), color="red4") + labs(title="PT-IBTS") + labs(x="Longitude (°)") + labs(y="Latitude (°)") + theme(plot.title=element_text(size=8, face="bold"), axis.title=element_text(size=7), plot.margin=margin(c(t=0,r=0,b=2,l=1)))
l<- basemap(limits = c(-12, 14, 36, 62), land.col = "#607D3B", land.border.col = NA) + geom_spatial_point(data = ieiams, aes(x = ShootLong, y = ShootLat), color="red4") + labs(title="IE-IAMS") + labs(x="Longitude (°)") + labs(y="Latitude (°)") + theme(plot.title=element_text(size=8, face="bold"), axis.title=element_text(size=7), plot.margin=margin(c(t=0,r=0,b=2,l=1)))
m<- basemap(limits = c(-12, 14, 36, 62), land.col = "#607D3B", land.border.col = NA) + geom_spatial_point(data = scoroc, aes(x = ShootLong, y = ShootLat), color="red4") + labs(title="SCOROC") + labs(x="Longitude (°)") + labs(y="Latitude (°)") + theme(plot.title=element_text(size=8, face="bold"), axis.title=element_text(size=7), plot.margin=margin(c(t=0,r=0,b=2,l=1)))
n<- basemap(limits = c(-12, 14, 36, 62), land.col = "#607D3B", land.border.col = NA) + geom_spatial_point(data = dws, aes(x = ShootLong, y = ShootLat), color="red4") + labs(title="DWS") + labs(x="Longitude (°)") + labs(y="Latitude (°)") + theme(plot.title=element_text(size=8, face="bold"), axis.title=element_text(size=7), plot.margin=margin(c(t=0,r=0,b=2,l=1)))
o<- basemap(limits = c(-12, 14, 36, 62), land.col = "#607D3B", land.border.col = NA) + geom_spatial_point(data = btsviii, aes(x = ShootLong, y = ShootLat), color="red4") + labs(title="BTS-VIII") + labs(x="Longitude (°)") + labs(y="Latitude (°)") + theme(plot.title=element_text(size=8, face="bold"), axis.title=element_text(size=7), plot.margin=margin(c(t=0,r=0,b=2,l=1)))
p<- basemap(limits = c(-12, 14, 36, 62), land.col = "#607D3B", land.border.col = NA) + geom_spatial_point(data = bits, aes(x = ShootLong, y = ShootLat), color="red4") + labs(title="BITS") + labs(x="Longitude (°)") + labs(y="Latitude (°)") + theme(plot.title=element_text(size=8, face="bold"), axis.title=element_text(size=7), plot.margin=margin(c(t=0,r=0,b=2,l=1)))
q<- basemap(limits = c(-12, 14, 36, 62), land.col = "#607D3B", land.border.col = NA) + geom_spatial_point(data = dyfs, aes(x = ShootLong, y = ShootLat), color="red4") + labs(title="DYFS") + labs(x="Longitude (°)") + labs(y="Latitude (°)") + theme(plot.title=element_text(size=8, face="bold"), axis.title=element_text(size=7), plot.margin=margin(c(t=0,r=0,b=2,l=1)))
r<- basemap(limits = c(-12, 14, 36, 62), land.col = "#607D3B", land.border.col = NA) + geom_spatial_point(data = rockall, aes(x = ShootLong, y = ShootLat), color="red4") + labs(title="ROCKALL") + labs(x="Longitude (°)") + labs(y="Latitude (°)") + theme(plot.title=element_text(size=8, face="bold"), axis.title=element_text(size=7), plot.margin=margin(c(t=0,r=0,b=2,l=1)))
s<- basemap(limits = c(-12, 14, 36, 62), land.col = "#607D3B", land.border.col = NA) + geom_spatial_point(data = sesound, aes(x = ShootLong, y = ShootLat), color="red4") + labs(title="SE-SOUND") + labs(x="Longitude (°)") + labs(y="Latitude (°)") + theme(plot.title=element_text(size=8, face="bold"), axis.title=element_text(size=7), plot.margin=margin(c(t=0,r=0,b=2,l=1)))
t<- basemap(limits = c(-12, 14, 36, 62), land.col = "#607D3B", land.border.col = NA) + geom_spatial_point(data = sns, aes(x = ShootLong, y = ShootLat), color="red4") + labs(title="SNS") + labs(x="Longitude (°)") + labs(y="Latitude (°)") + theme(plot.title=element_text(size=8, face="bold"), axis.title=element_text(size=7), plot.margin=margin(c(t=0,r=0,b=2,l=1)))

library(cowplot)

plot_grid(a,b,c,d,e,f,g,h,i,j,k,u,l,m,n,o,p,q,r,t)

#Save as 9.0 x 10.5


###Making environmental variable plots###############################################################

dataset1<- read.table("FinalisedDataset3.csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)
dataset1$logDepth[which(dataset1$logDepth <0)] <- NA

Gdata<- subset(dataset1, select=c(1:2, 4:6, 8:11))
gamGgaleus <- gam(dataset1$log_Galeorhinus_galeus ~ Survey + s(Month, k=3) + s(logDepth, k=3) + s(CurrentTemp, k=3) + s(CurrentSal, k=3), family=nb, select=TRUE, method="REML", data = Gdata)

Cdata<- subset(dataset1, select=c(1:2, 4:6, 8:10, 12))
Cdata$log_Chimaera_monstrosa <- as.integer(Cdata$log_Chimaera_monstrosa)
gamCmonstrosa <- gam(Cdata$log_Chimaera_monstrosa ~ s(Month,k=3) + s(logDepth, k=3) + s(CurrentTemp, k=3) + s(CurrentSal, k=3), family=ziP(), data=Cdata)

Adata<- subset(dataset1, select=c(1:2, 4:6, 8:10, 13))
gamAradiata <- gam(dataset1$log_Amblyraja_radiata ~ Survey + s(Month, k=3) + s(logDepth, k=3) + s(CurrentTemp, k=3) + s(CurrentSal, k=3), family=nb, select=TRUE, method="REML", data = Adata)

Mdata<- subset(dataset1, select=c(1:2, 4:6, 8:10, 14))
gamMasterias <- gam(dataset1$log_Mustelus_asterias ~ Survey + s(Month, k=3) + s(logDepth, k=3) + s(CurrentTemp, k=3) + s(CurrentSal, k=3), family=nb, select=TRUE, method="REML", data = Mdata)

Sdata<- subset(dataset1, select=c(1:2, 4:6, 8:10, 15))
gamSacanthias <- gam(dataset1$log_Squalus_acanthias ~ Survey + s(Month, k=3) + s(logDepth, k=3) + s(CurrentTemp, k=3) + s(CurrentSal, k=3), family=nb, select=TRUE, method="REML", data = Sdata)

Rdata<- subset(dataset1, select=c(1:2, 4:6, 8:10, 16))
gamRclavata <- gam(dataset1$log_Raja_clavata ~ Survey + s(Month, k=3) + s(logDepth, k=3) + s(CurrentTemp, k=3) + s(CurrentSal, k=3), family=nb, select=TRUE, method="REML", data = Rdata)


par(mfrow=c(6,4))
par(mar=c(3,3,0.5,0.5))
par(oma=c(0,10,2,0))
plot(gamGgaleus, select=1, ylim=c(-1,1), xlab="", ylab="")
plot(gamGgaleus, select=2, ylim=c(-7,5), xlab="", ylab="")
plot(gamGgaleus, select=3, ylim=c(-4,1), xlab="", ylab="")
plot(gamGgaleus, select=4, ylim=c(-30,3), xlab="", ylab="")
plot(gamCmonstrosa, select=1, ylim=c(-0.1,0.1), xlab="", ylab="")
plot(gamCmonstrosa, select=2, ylim=c(-11,2), xlab="", ylab="")
plot(gamCmonstrosa, select=3, ylim=c(-0.5,0.5), xlab="", ylab="")
plot(gamCmonstrosa, select=4, ylim=c(-1,0.5), xlab="", ylab="")
plot(gamAradiata, select=1, ylim=c(-0.75,1), xlab="", ylab="")
plot(gamAradiata, select=2, ylim=c(-12,1), xlab="", ylab="")
plot(gamAradiata, select=3, ylim=c(-1.5,1), xlab="", ylab="")
plot(gamAradiata, select=4, ylim=c(-6,0.7), xlab="", ylab="")
plot(gamMasterias, select=1, ylim=c(-2,3), xlab="", ylab="")
plot(gamMasterias, select=2, ylim=c(-7.5,2), xlab="", ylab="")
plot(gamMasterias, select=3, ylim=c(-7,2), xlab="", ylab="")
plot(gamMasterias, select=4, ylim=c(-20,2), xlab="", ylab="")
plot(gamSacanthias, select=1, ylim=c(-0.7,0.7), xlab="", ylab="")
plot(gamSacanthias, select=2, ylim=c(-7.5,1), xlab="", ylab="")
plot(gamSacanthias, select=3, ylim=c(-4,1), xlab="", ylab="")
plot(gamSacanthias, select=4, ylim=c(-10,1), xlab="", ylab="")
plot(gamRclavata, select=1, ylim=c(-1,1.2), xlab="", ylab="")
plot(gamRclavata, select=2, ylim=c(-4,5), xlab="", ylab="")
plot(gamRclavata, select=3, ylim=c(-2,1), xlab="", ylab="")
plot(gamRclavata, select=4, ylim=c(-6.5,1.5), xlab="", ylab="")

mtext("Modelled standardised abundance",side=2,line=0,outer=TRUE,cex=1)

mtext('G.galeus',las=1, at=.93,side=2,outer=T,cex=1,line=2.5,font=3) 
mtext('C.monstrosa',las=1, at=.76,side=2,outer=T,cex=1,line=2.5,font=3) 
mtext('A.radiata',las=1, at=.60,side=2,outer=T,cex=1,line=2.5,font=3) 
mtext('M.asterias',las=1, at=.43,side=2,outer=T,cex=1,line=2.5,font=3)
mtext('S.acanthias',las=1, at=.27,side=2,outer=T,cex=1,line=2.5,font=3) 
mtext('R.clavata',las=1, at=.10,side=2,outer=T,cex=1,line=2.5,font=3)

mtext('Month',at=.14,side=3,outer=T,cex=1) 
mtext('logDepth (m)',at=.40,side=3,outer=T,cex=1) 
mtext('SST (°C)',at=.65,side=3,outer=T,cex=1) 
mtext('Salinity (PSS)',at=.9,side=3,outer=T,cex=1)

#Save as 10 x 10


###Change in distribution plots########################################################################

#Make values not contain E
options(scipen = 999)

###G.galeus###

#Make subset including the difference between the current and future CPUEs
present_Ggaleus3$currentCPUE<-present_Ggaleus3$CPUE
predicted_Ggaleus3$futureCPUE<-predicted_Ggaleus3$CPUE
Ggaleusmap<-merge(present_Ggaleus3, predicted_Ggaleus3, by=c("lat", "lon"))
Ggaleusmap<-subset(Ggaleusmap, select= -c(3,5))

#Make a variable of the difference between them
Ggaleusmap$dCPUE<-Ggaleusmap$futureCPUE-Ggaleusmap$currentCPUE
Ggaleusmap$dCPUE<-as.numeric(Ggaleusmap$dCPUE)
Ggaleusmap$lon<-as.numeric(Ggaleusmap$lon)
Ggaleusmap$lat<-as.numeric(Ggaleusmap$lat)

write.csv(Ggaleusmap, file="Ggaleusdifference.csv",row.names = FALSE)
Ggaleusmap<- read.table("Ggaleusdifference.csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)

#Map
Ggaleusmap1<-basemap(limits = c(-12, 14, 36, 62), land.col = "#607D3B", land.border.col = NA) + geom_spatial_tile(data=Ggaleusmap, aes(x=lon, y=lat, fill=dCPUE)) + scale_fill_gradient2(high="blue4", mid="white", low="red4", limits=c(-0.0005,0.01)) 
#Excluded dCPUE value 0.019 as anomalie
Ggaleusmap2<-reorder_layers(Ggaleusmap1)

###Cmonstrosa###

#Make subset including the difference between the current and future CPUEs
present_Cmonstrosa3$currentCPUE<-present_Cmonstrosa3$CPUE
predicted_Cmonstrosa3$futureCPUE<-predicted_Cmonstrosa3$CPUE
Cmonstrosamap<-merge(present_Cmonstrosa3, predicted_Cmonstrosa3, by=c("lat", "lon"))
Cmonstrosamap<-subset(Cmonstrosamap, select= -c(3,5))

#Make a variable of the difference between them
Cmonstrosamap$dCPUE<-Cmonstrosamap$futureCPUE-Cmonstrosamap$currentCPUE
Cmonstrosamap$dCPUE<-as.numeric(Cmonstrosamap$dCPUE)
Cmonstrosamap$lon<-as.numeric(Cmonstrosamap$lon)
Cmonstrosamap$lat<-as.numeric(Cmonstrosamap$lat)

write.csv(Cmonstrosamap, file="Cmonstrosadifference.csv",row.names = FALSE)
Cmonstrosamap<- read.table("Cmonstrosadifference.csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)

#Map
Cmonstrosamap1<-basemap(limits = c(-12, 14, 36, 62), land.col = "#607D3B", land.border.col = NA) + geom_spatial_tile(data=Cmonstrosamap, aes(x=lon, y=lat, fill=dCPUE)) + scale_fill_gradient2(high="blue4", mid="white", low="red4", limits=c(-0.3,0.82)) 
#Excluded dCPUE value 4.35 as anomalie
Cmonstrosamap2<-reorder_layers(Cmonstrosamap1)

###Aradiata###

#Make subset including the difference between the current and future CPUEs
present_Aradiata3$currentCPUE<-present_Aradiata3$CPUE
predicted_Aradiata3$futureCPUE<-predicted_Aradiata3$CPUE
Aradiatamap<-merge(present_Aradiata3, predicted_Aradiata3, by=c("lat", "lon"))
Aradiatamap<-subset(Aradiatamap, select= -c(3,5))

#Make a variable of the difference between them
Aradiatamap$dCPUE<-Aradiatamap$futureCPUE-Aradiatamap$currentCPUE
Aradiatamap$dCPUE<-as.numeric(Aradiatamap$dCPUE)
Aradiatamap$lon<-as.numeric(Aradiatamap$lon)
Aradiatamap$lat<-as.numeric(Aradiatamap$lat)

write.csv(Aradiatamap, file="Aradiatadifference.csv",row.names = FALSE)
Aradiatamap<- read.table("Aradiatadifference.csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)

#Map
Aradiatamap1<-basemap(limits = c(-12, 14, 36, 62), land.col = "#607D3B", land.border.col = NA) + geom_spatial_tile(data=Aradiatamap, aes(x=lon, y=lat, fill=dCPUE)) + scale_fill_gradient2(high="blue4", mid="white", low="red4", limits=c(-1.1,0.01)) 
Aradiatamap2<-reorder_layers(Aradiatamap1)

###Masterias###

#Make subset including the difference between the current and future CPUEs
present_Masterias3$currentCPUE<-present_Masterias3$CPUE
predicted_Masterias3$futureCPUE<-predicted_Masterias3$CPUE
Masteriasmap<-merge(present_Masterias3, predicted_Masterias3, by=c("lat", "lon"))
Masteriasmap<-subset(Masteriasmap, select= -c(3,5))

#Make a variable of the difference between them
Masteriasmap$dCPUE<-Masteriasmap$futureCPUE-Masteriasmap$currentCPUE
Masteriasmap$dCPUE<-as.numeric(Masteriasmap$dCPUE)
Masteriasmap$lon<-as.numeric(Masteriasmap$lon)
Masteriasmap$lat<-as.numeric(Masteriasmap$lat)

write.csv(Masteriasmap, file="Masteriasdifference.csv",row.names = FALSE)
Masteriasmap<- read.table("Masteriasdifference.csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)

#Map
Masteriasmap1<-basemap(limits = c(-12, 14, 36, 62), land.col = "#607D3B", land.border.col = NA) + geom_spatial_tile(data=Masteriasmap, aes(x=lon, y=lat, fill=dCPUE)) + scale_fill_gradient2(high="blue4", mid="white", low="red4", limits=c(-0.0035,0.019)) 
Masteriasmap2<-reorder_layers(Masteriasmap1)

###Sacanthias###

#Make subset including the difference between the current and future CPUEs
present_Sacanthias3$currentCPUE<-present_Sacanthias3$CPUE
predicted_Sacanthias3$futureCPUE<-predicted_Sacanthias3$CPUE
Sacanthiasmap<-merge(present_Sacanthias3, predicted_Sacanthias3, by=c("lat", "lon"))
Sacanthiasmap<-subset(Sacanthiasmap, select= -c(3,5))

#Make a variable of the difference between them
Sacanthiasmap$dCPUE<-Sacanthiasmap$futureCPUE-Sacanthiasmap$currentCPUE
Sacanthiasmap$dCPUE<-as.numeric(Sacanthiasmap$dCPUE)
Sacanthiasmap$lon<-as.numeric(Sacanthiasmap$lon)
Sacanthiasmap$lat<-as.numeric(Sacanthiasmap$lat)

write.csv(Sacanthiasmap, file="Sacanthiasdifference.csv",row.names = FALSE)
Sacanthiasmap<- read.table("Sacanthiasdifference.csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)

#Map
Sacanthiasmap1<-basemap(limits = c(-12, 14, 36, 62), land.col = "#607D3B", land.border.col = NA) + geom_spatial_tile(data=Sacanthiasmap, aes(x=lon, y=lat, fill=dCPUE)) + scale_fill_gradient2(high="blue4", mid="white", low="red4", limits=c(-0.052,0.0014)) 
Sacanthiasmap2<-reorder_layers(Sacanthiasmap1)

###Rclavata###

#Make subset including the difference between the current and future CPUEs
present_Rclavata3$currentCPUE<-present_Rclavata3$CPUE
predicted_Rclavata3$futureCPUE<-predicted_Rclavata3$CPUE
Rclavatamap<-merge(present_Rclavata3, predicted_Rclavata3, by=c("lat", "lon"))
Rclavatamap<-subset(Rclavatamap, select= -c(3,5))

#Make a variable of the difference between them
Rclavatamap$dCPUE<-Rclavatamap$futureCPUE-Rclavatamap$currentCPUE
Rclavatamap$dCPUE<-as.numeric(Rclavatamap$dCPUE)
Rclavatamap$lon<-as.numeric(Rclavatamap$lon)
Rclavatamap$lat<-as.numeric(Rclavatamap$lat)

write.csv(Rclavatamap, file="Rclavatadifference.csv",row.names = FALSE)
Rclavatamap<- read.table("Rclavatadifference.csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)

#Map
Rclavatamap1<-basemap(limits = c(-12, 14, 36, 62), land.col = "#607D3B", land.border.col = NA) + geom_spatial_tile(data=Rclavatamap, aes(x=lon, y=lat, fill=dCPUE)) + scale_fill_gradient2(high="blue4", mid="white", low="red4", limits=c(-0.00022,0.0135)) 
Rclavatamap2<-reorder_layers(Rclavatamap1)
#Removed CPUE value of 0.084 as anomalie

aa<-Ggaleuspresentmap2 + labs(fill = "") + labs(title="Current") + labs(subtitle="G.galeus") + labs(x="Longitude (°)") + labs(y="Latitude (°)") + theme(plot.title =element_text(size=17, face="bold", hjust = 0.5), plot.subtitle =element_text(size=13, face="bold", vjust=3), axis.title=element_text(size=13), plot.margin=margin(c(t=5,r=5,b=4,l=10)), legend.key.size = unit(0.75, 'cm'),  legend.text = element_text(size=10))
a<-Ggaleusmap2 + labs(fill = "") + labs(title="2040-2050") + labs(x="Longitude (°)") + labs(y="Latitude (°)") + theme(plot.title =element_text(size=17, face="bold", hjust=0.5), axis.title=element_text(size=13), plot.margin=margin(c(t=5,r=5,b=4,l=10)), legend.key.size = unit(0.75, 'cm'),  legend.text = element_text(size=10))
bb<-Cmonstrosapresentmap2 + labs(fill = "") + labs(title="") + labs(subtitle="C.monstrosa") + labs(x="Longitude (°)") + labs(y="Latitude (°)") + theme(plot.title =element_text(size=17, face="bold", hjust=0.5), plot.subtitle =element_text(size=13, face="bold", vjust=3), axis.title=element_text(size=13), plot.margin=margin(c(t=5,r=5,b=4,l=10)), legend.key.size = unit(0.75, 'cm'),  legend.text = element_text(size=10))
b<-Cmonstrosamap2 + labs(fill = "") + labs(title="") + labs(x="Longitude (°)") + labs(y="Latitude (°)") + theme(plot.title =element_text(size=17, face="bold", hjust=0.5), axis.title=element_text(size=13), plot.margin=margin(c(t=5,r=5,b=4,l=10)), legend.key.size = unit(0.75, 'cm'),  legend.text = element_text(size=10))
cc<-Aradiatapresentmap2 + labs(fill = "") + labs(subtitle="A.radiata") + labs(x="Longitude (°)") + labs(y="Latitude (°)") + theme(plot.subtitle =element_text(size=13, face="bold", vjust=3), axis.title=element_text(size=13), plot.margin=margin(c(t=5,r=5,b=4,l=10)), legend.key.size = unit(0.75, 'cm'),  legend.text = element_text(size=10))
c<-Aradiatamap2 + labs(fill = "") + labs(title="") + labs(x="Longitude (°)") + labs(y="Latitude (°)") + theme(plot.subtitle =element_text(size=15, face="bold", hjust=0.5), axis.title=element_text(size=13), plot.margin=margin(c(t=5,r=5,b=4,l=10)), legend.key.size = unit(0.75, 'cm'),  legend.text = element_text(size=10))
dd<-Masteriaspresentmap2 + labs(fill = "") + labs(subtitle="M.asterias") + labs(x="Longitude (°)") + labs(y="Latitude (°)") + theme(plot.subtitle =element_text(size=13, face="bold", vjust=3), axis.title=element_text(size=13), plot.margin=margin(c(t=5,r=5,b=4,l=10)), legend.key.size = unit(0.75, 'cm'),  legend.text = element_text(size=10))
d<-Masteriasmap2 + labs(fill = "") + labs(title="") + labs(x="Longitude (°)") + labs(y="Latitude (°)") + theme(plot.subtitle =element_text(size=15, face="bold", hjust=0.5), axis.title=element_text(size=13), plot.margin=margin(c(t=5,r=5,b=4,l=10)), legend.key.size = unit(0.75, 'cm'),  legend.text = element_text(size=10))
ee<-Sacanthiaspresentmap2 + labs(fill = "") + labs(subtitle="S.acanthias") + labs(x="Longitude (°)") + labs(y="Latitude (°)") + theme(plot.subtitle =element_text(size=13, face="bold", vjust=3), axis.title=element_text(size=13), plot.margin=margin(c(t=5,r=5,b=4,l=10)), legend.key.size = unit(0.75, 'cm'),  legend.text = element_text(size=10))
e<-Sacanthiasmap2 + labs(fill = "") + labs(title="") + labs(x="Longitude (°)") + labs(y="Latitude (°)") + theme(plot.subtitle =element_text(size=15, face="bold", hjust=0.5), axis.title=element_text(size=13), plot.margin=margin(c(t=5,r=5,b=4,l=10)), legend.key.size = unit(0.75, 'cm'),  legend.text = element_text(size=10))
ff<-Rclavatapresentmap2 + labs(fill = "") + labs(subtitle="R.clavata") + labs(x="Longitude (°)") + labs(y="Latitude (°)") + theme(plot.subtitle =element_text(size=13, face="bold", vjust=3), axis.title=element_text(size=13), plot.margin=margin(c(t=5,r=5,b=4,l=10)), legend.key.size = unit(0.75, 'cm'),  legend.text = element_text(size=10))
f<-Rclavatamap2 + labs(fill = "") + labs(title="") + labs(x="Longitude (°)") + labs(y="Latitude (°)") + theme(plot.subtitle =element_text(size=15, face="bold", hjust=0.5), axis.title=element_text(size=13), plot.margin=margin(c(t=5,r=5,b=4,l=10)), legend.key.size = unit(0.75, 'cm'),  legend.text = element_text(size=10))

plot_grid(aa,a,bb,b,cc,c,dd,d,ee,e,ff,f, ncol=2, nrow=6, align="hv")

###Making plots for temperature map################################################################

dtemp<- read.table("tempdifference.csv", header=TRUE, fill=TRUE,sep=",", check.names=FALSE)

dtempmap<-basemap(limits = c(-12, 14, 36, 62), land.col = "#607D3B", land.border.col = NA) + geom_spatial_tile(data=dtemp, aes(x=lon, y=lat, fill=difference)) + scale_fill_gradient2(high="#CC3300", mid="white", low="#0000CC", limits=c(-0.5,1.7), name="Difference (°C)") 
dtempmap1<-reorder_layers(dtempmap)
dtempmap1


ptempmap<-basemap(limits = c(-12, 14, 36, 62), land.col = "#607D3B", land.border.col = NA) + geom_spatial_tile(data=dtemp, aes(x=lon, y=lat, fill=present)) + scale_fill_gradient(high="orange", low="#90CAF9", limits=c(8,21), name="Temperature (°C)") 
ptempmap1<-reorder_layers(ptempmap)
ptempmap1

ftempmap<-basemap(limits = c(-12, 14, 36, 62), land.col = "#607D3B", land.border.col = NA) + geom_spatial_tile(data=dtemp, aes(x=lon, y=lat, fill=future)) + scale_fill_gradient(high="#FF7043", low="#90CAF9", limits=c(8,21), name="Temperature (°C)") 
ftempmap1<-reorder_layers(ftempmap)
ftempmap1

ptm1<-ptempmap1 + labs(title="Present") + labs(x="Longitude (°)") + labs(y="Latitude (°)") + theme(plot.title =element_text(size=17, face="bold", hjust = 0.5), axis.title=element_text(size=13), plot.margin=margin(c(t=5,r=5,b=4,l=10)), legend.key.size = unit(0.75, 'cm'),  legend.text = element_text(size=10))
ftm1<-ftempmap1 + labs(title="2040-2050") + labs(x="Longitude (°)") + labs(y="Latitude (°)") + theme(plot.title =element_text(size=17, face="bold", hjust = 0.5), axis.title=element_text(size=13), plot.margin=margin(c(t=5,r=5,b=4,l=10)), legend.key.size = unit(0.75, 'cm'),  legend.text = element_text(size=10))
dtm1<-dtempmap1 + labs(title="2040-2050") + labs(x="Longitude (°)") + labs(y="Latitude (°)") + theme(plot.title =element_text(size=17, face="bold", hjust = 0.5), axis.title=element_text(size=13), plot.margin=margin(c(t=5,r=5,b=4,l=10)), legend.key.size = unit(0.75, 'cm'),  legend.text = element_text(size=10))

plot_grid(ptm1, dtm1, ncol=2, nrow=1, align="hv")