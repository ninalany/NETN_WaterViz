library('tidyverse')

df<-read.csv("./Data/NETN_Water_Data.csv", stringsAsFactors = F) #observation table
md<-read.csv("./Data/MetaData.csv", stringsAsFactors = F)  #metadata table

#recode Ponds so they are included as Lakes
md$Type <- ifelse(md$Type == "Pond", "Lake", md$Type) 

#rename some columns for merging
colnames(md)[colnames(md) == "CharacteristicName"] <- "Local.Characteristic.Name"
colnames(md)[colnames(md) == "SiteCode"] <- "StationID"

#merge the tables
df <- merge(df, md, by = c("StationID", "Local.Characteristic.Name"), all.x=T, all.y=F) 

#make some necessary columns
df$date<-as.POSIXct(df$Visit.Start.Date,  format= "%m/%d/%y") #required format and colname for openair::TheilSen
df$Visit.Start.Date <- as.Date(df$Visit.Start.Date, format= "%m/%d/%y")
df$Year<-as.numeric(format(df$Visit.Start.Date,"%y"))#extract Year
df$Month <- as.numeric(format(df$Visit.Start.Date, "%m")) #extract month

#make separate symbols and colors for values below quantification limit         
df$value <- as.numeric(ifelse(df$Result.Value.Text == "*Present <QL", df$Lower.Quantification.Limit, df$Result.Value.Text))
df$plotting.symbol <- ifelse(df$Result.Value.Text == "*Present <QL", 1, 19)
df$plotting.color <- ifelse(df$Result.Value.Text == "*Present <QL", "red", "black")

ParkList<-unique(df$ParkCode)
ParkNameList<-unique(df$LongName)

SiteList<-unique(df$SiteName)

VarList<-unique(df$Local.Characteristic.Name)
DisplayVarList<-unique(df$DisplayName)