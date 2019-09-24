###NPS Water - prep data
#NKL
#7/2/2019

rm(list=ls())

#To work with Microsoft Access database on a Mac, it is necessary to install mdb tools. In the Terminal, run:
#ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)" </dev/null 2> /dev/null
#brew install mdbtools

# Check for and install required packages
for (package in c('tidyverse', 'Hmisc')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

### function to count the number of unique levels of a character variable:
how.many <- function(x){length(unique(x))}

#########################
#Import water data from Access database:
setwd("~/Google Drive File Stream/My Drive/NETN Water R coding project")
db_path <- 'NETN_H2Ov4_BE_20190319.accdb'
## Import the entire data base:
db <- mdb.get(db_path, stringsAsFactors=F)
contents(db)
## To grab one table at a time (you need the mdbexport_args to read in the fk_eventID type columns correctly.):
#event <- mdb.get(db_path, tables = "tbl_Events", mdbexportArgs = '')

## Should I change the column names use periods instead of "_" so replace to match the AccessDB?
#names(event) <- gsub("\\.", "_", names(event))

#########################
#write L0 (raw) data to Google Drive:
#make a vector to store table names
table_names <- names(db)
#write each data table in the DB as a flat file:
for (i in seq_along(table_names)) {
write.csv(db[[i]], file = paste0("~/Google Drive File Stream/My Drive/NETN Water R coding project/NETN_H2Ov4_BE_20190319/", table_names[i],".csv"), row.names=F)
}

#########################
#create data frame in the template 'Water Data.csv' for the NCRN Water R package and Rshiny WaterViz:
#put a dataframe for each list item in the Global Environment:
list2env(db, envir=.GlobalEnv)

#prep ancillary data for merging:
Samples <- Sample %>%
	select(c('PK.Sample', 'FK.Event'))
Events <- Event %>%
	select(c('PK.Event', 'FK.Location', 'StartDate'))
Locations <- Location %>%
	select(c('PK.Location', 'NPStoretSiteCode', 'LocationType'))

#set column order for 'Water data.csv', long form:
col_order <- c("NPSTORET.Org.ID.Code", "StationID", "Visit.Start.Date", "SampleDepth", "Depth.Units", "Local.Characteristic.Name", "Result.Value.Text", "Lower.Quantification.Limit", "Upper.Quantification.Limit")

#select variables of interest from Chemistry table:
Chem <- Chemistry %>%
	select(-c('SEC2016', 'LabCode', 'SampleTime', 'SampleStation', 'SampleType', 'FK.WaterQualityMethod', 'Project', 'pH.Lab', 'eqPH', 'AppColorFlag', 'AppColor.PCU', 'TrueColor.PCU', 'TColor.Flag', 'pH.Lab.Method', 'CONDMETH', 'COLORMETH', 'ALKMETH', 'ChemComments')) 
	
#merge in ancillary info on date and site:
Chem <- merge(Chem, Samples, by.x="FK.Sample", by.y = "PK.Sample")
Chem <- merge(Chem, Events, by.x="FK.Event", by.y = "PK.Event") 
Chem <- merge(Chem, Locations, by.x="FK.Location", by.y = "PK.Location") 
#format Date
Chem$StartDate <- as.Date(Chem$StartDate, format = "%m/%d/%y")
#filter out everything but QCtype = "ENV", rename columns, re-code Sample Depth:
Chem <- Chem %>%
	filter(QCtype == "ENV") %>%
	rename(StationID=NPStoretSiteCode, Visit.Start.Date=StartDate, SampleDepth = SampleDepth.m) %>%
	mutate(NPSTORET.Org.ID.Code = "NETN") %>%
	mutate(SampleDepth = case_when(LocationType == 'Stream' ~ 'stream',
	 LocationType == 'Pond' ~ 'epilimnion', 
	 LocationType == 'Lake' ~ 'epilimnion'))
Chem <- Chem[order(Chem$StationID, Chem$Visit.Start.Date),]


#check to be sure there is only measurement (representing the epilimnion) per visit. This takes about 15 seconds to run.
try(if(max(as.vector(tapply(Chem$SampleDepth, list(Chem$Visit.Start.Date, Chem$StationID), how.many)), na.rm=T) > 1) stop ('More than one sampling depth per site visit'))

#split into two dataframes - measurments and quality flags.
Chem_dat <- Chem[,c('NPSTORET.Org.ID.Code','StationID', 'Visit.Start.Date', 'SampleDepth','Al.ugL', 'ANC.ueqL', 'Ca.ueqL', 'Ca.ueqL', 'ChlA.ugL', 'DIC.mgL','DOC.mgL', 'K.ueqL','Mg.ueqL', 'Na.ueqL', 'NH3.mgL', 'NH4.mgL', 'NO2.mgL', 'NO2.NO3.mgL', 'NO3.ueqL', 'PO4.ugL', 'Si.mgL', 'SO4.ueqL', 'TN.mgL', 'TotDissN.mgL', 'TotDissP.ugL', 'TP.ugL')]
Chem_flag <- Chem[,c('StationID', 'Visit.Start.Date', 'AlFlag', 'ANCFlag', 'CaFlag', 'CaFlag', 'ChlAFlag', 'DICFlag','DOCFlag', 'KFlag','MgFlag', 'NaFlag', 'NH3Flag', 'NH4Flag', 'NO2Flag', 'NO2.NO3Flag', 'NO3Flag', 'PO4Flag', 'SiFlag', 'SO4Flag', 'TNFlag', 'TotDissNFlag', 'TotDissPFlag', 'TPFlag')]

#convert both dataframes to long form and remove rows with NA for observation value
Chem_dat_long <- Chem_dat %>%
	gather(key = Local.Characteristic.Name, value = Result.Value.Text, -c(SampleDepth, Visit.Start.Date, StationID, NPSTORET.Org.ID.Code)) %>%
	mutate(Depth.Units = NA)

Chem_flag_long <- Chem_flag %>%
	gather(key = Flag.Characteristic.Name, value = Flag.Value, -c(Visit.Start.Date, StationID)) %>%
	rename(Visit.Start.Date.F = Visit.Start.Date, StationID.F = StationID)
	
#merge together:
Chem_long <- cbind(Chem_dat_long, Chem_flag_long)

#remove NA and create Upper and Lower Quantification Limit columns:
test <- Chem_long %>%
	mutate(Lower.Quantification.Limit = NA) %>%
	mutate(Upper.Quantification.Limit = NA) 

#extract Lower and Upper limits from Flag.Value column and replace Result.Value.Text with character string if value was outside of qunatification limits:
unique(test$Flag.Value)
#lower MRL
LDL_rows <- setdiff(grep("MRL", test$Flag.Value), grep("E", test$Flag.Value))
LDL_vals <-sapply(strsplit(test$Flag.Value[LDL_rows], " "), "[",2)
test$Lower.Quantification.Limit <- replace(test$Lower.Quantification.Limit, LDL_rows, LDL_vals)
test$Result.Value.Text[!is.na(test$Lower.Quantification.Limit)] <- '*Present <QL'

#hack job replacing lower MDL values until the codes are standardized in the ACCESS database:
test$Lower.Quantification.Limit <- ifelse(
	test$Flag.Value == 'MDL<0.005', 0.005, ifelse(
	test$Flag.Value == '<MDL 0.005', 0.005, ifelse(
	test$Flag.Value == '<MDL 1', 1, ifelse(
	test$Flag.Value == 'MDL<1', 1, ifelse(
	test$Flag.Value == '<MDL 0.73', 0.73,test$Lower.Quantification.Limit)))))

#upper
UDL_rows <- union(grep("UDL", test$Flag.Value), grep("ULQ", test$Flag.Value))
UDL_vals <- rep(2.9, length(UDL_rows))
test$Upper.Quantification.Limit <- replace(test$Upper.Quantification.Limit, UDL_rows, UDL_vals)
test$Result.Value.Text[!is.na(test$Upper.Quantification.Limit)] <- '*Present >QL'

#clean up
Chem_long <- test %>%
	dplyr::filter(!is.na(Result.Value.Text)) %>%
	select(-c(StationID.F, Visit.Start.Date.F, Flag.Characteristic.Name, Flag.Value))
Chem_long <- Chem_long[,col_order]


#select variables of interest from WQInSitu table:
WQ <- WQInSitu %>%
	select(c('PK.WQInSitu', 'FK.Sample', 'Depth.m','BP.mmHg', 'DOsat.pct', 'DO.mgL', 'pH', 'SpCond.uScm','Temp.C', 'QCType'))
WQ <- WQ %>%
	rename(SampleDepth = Depth.m) %>%
	filter(QCType == "0") %>% #Remove calibration samples.
	select(-QCType)
#merge in ancillary info on date and site:
WQ <- merge(WQ, Samples, by.x="FK.Sample", by.y = "PK.Sample")
WQ <- merge(WQ, Events, by.x="FK.Event", by.y = "PK.Event") 
WQ <- merge(WQ, Locations, by.x="FK.Location", by.y = "PK.Location") 
#format Date
WQ$StartDate <- as.Date(WQ$StartDate, format = "%m/%d/%y")
#clean up column names, remove rows with SampleDepth = NA
WQ <- WQ %>%
	rename(StationID=NPStoretSiteCode, Visit.Start.Date=StartDate) %>%
	mutate(NPSTORET.Org.ID.Code = "NETN") %>%
	filter(!is.na(SampleDepth)) %>%
	select(-c(FK.Location, FK.Event, FK.Sample, PK.WQInSitu))
WQ <- WQ[order(WQ$StationID, WQ$Visit.Start.Date),]

#make long, remove rows with NA for observation value, record Depth units
WQ_long <- WQ %>%
	select(-LocationType) %>%
	gather(key = Local.Characteristic.Name, value = Result.Value.Text, -c(SampleDepth, Visit.Start.Date, StationID, NPSTORET.Org.ID.Code)) %>%
	dplyr::filter(!is.na(Result.Value.Text)) %>%
	mutate(Upper.Quantification.Limit = NA) %>%
	mutate(Lower.Quantification.Limit = NA) %>%
	mutate(Depth.Units = "m")
WQ_long <- WQ_long[,col_order]

#calculate median value for all depths 2m or less to represent the stream/epilimnion value.	
temp <- WQ %>%
	filter(SampleDepth <= 2) %>%
	gather(key = Local.Characteristic.Name, value = Result.Value.Text, -c(SampleDepth, Visit.Start.Date, StationID, NPSTORET.Org.ID.Code)) %>%
	mutate(Result.Value.Text = as.numeric(Result.Value.Text)) %>%
	dplyr::filter(!is.na(Result.Value.Text)) %>%
	group_by(StationID, Visit.Start.Date, Local.Characteristic.Name) %>%
	dplyr::summarize(Result.Value.Text = median(as.numeric(Result.Value.Text))) %>%
	ungroup() 	%>%
	spread(key = Local.Characteristic.Name, value = Result.Value.Text) %>%
	mutate(NPSTORET.Org.ID.Code = "NETN")	
temp <- merge(temp, Locations, by.x="StationID", by.y = "NPStoretSiteCode", all.x=T, all.y=F) 

temp <- temp %>%
	mutate(SampleDepth = case_when(LocationType == 'Stream' ~ 'stream',
	 LocationType == 'Pond' ~ 'epilimnion', 
	 LocationType == 'Lake' ~ 'epilimnion')) %>%
	select(-c(PK.Location, LocationType))
#make long, remove rows with NA for observation value, add column for Depth.Units:
temp_long <- temp %>%
	gather(key = Local.Characteristic.Name, value = Result.Value.Text, -c(SampleDepth, Visit.Start.Date, StationID, NPSTORET.Org.ID.Code)) %>%
	dplyr::filter(!is.na(Result.Value.Text)) %>%
	mutate(Upper.Quantification.Limit = NA) %>%
	mutate(Lower.Quantification.Limit = NA) %>%
	mutate(Depth.Units = NA)
temp_long <- temp_long[,col_order]
#add summaries to WQ_long df:
WQ_long <- rbind(WQ_long, temp_long)

#select variables of interest from StreamDischarge table:
SD <- StreamDischarge %>%
	select(c('PK.StreamDischarge', 'FK.Sample', 'Discharge.cfs'))
#merge in ancillary info on date and site:
SD <- merge(SD, Samples, by.x="FK.Sample", by.y = "PK.Sample")
SD <- merge(SD, Events, by.x="FK.Event", by.y = "PK.Event") 
SD <- merge(SD, Locations, by.x="FK.Location", by.y = "PK.Location") 
#format Date
SD$StartDate <- as.Date(SD$StartDate, format = "%m/%d/%y")
SD <- SD %>%
	rename(StationID=NPStoretSiteCode, Visit.Start.Date=StartDate) %>%
	mutate(NPSTORET.Org.ID.Code = "NETN") %>%
	mutate(SampleDepth = NA) %>%
	select(-c(FK.Location, FK.Event, FK.Sample, PK.StreamDischarge, LocationType))
SD <- SD[order(SD$StationID, SD$Visit.Start.Date),]
#make long and remove rows with NA for observation value
SD_long <- SD %>%
	gather(key = Local.Characteristic.Name, value = Result.Value.Text, -c(SampleDepth, Visit.Start.Date, StationID, NPSTORET.Org.ID.Code)) %>%
	dplyr::filter(!is.na(Result.Value.Text)) %>%
	mutate(Upper.Quantification.Limit = NA) %>%
	mutate(Lower.Quantification.Limit = NA) %>%
	mutate(Depth.Units = NA)
SD_long <- SD_long[,col_order]


#select variables of interest from Turbidity table:
Turb <- Turbidity %>%
	select(c('PK.Turbidity', 'FK.Sample', 'Turbidity.NTU'))
#merge in ancillary info on date and site:
Turb <- merge(Turb, Samples, by.x="FK.Sample", by.y = "PK.Sample")
Turb <- merge(Turb, Events, by.x="FK.Event", by.y = "PK.Event") 
Turb <- merge(Turb, Locations, by.x="FK.Location", by.y = "PK.Location") 
#format Date
Turb$StartDate <- as.Date(Turb$StartDate, format = "%m/%d/%y")
Turb <- Turb %>%
	rename(StationID=NPStoretSiteCode, Visit.Start.Date=StartDate) %>%
	mutate(NPSTORET.Org.ID.Code = "NETN") %>%
	mutate(SampleDepth = NA) %>%
	select(-c(FK.Location, FK.Event, FK.Sample, PK.Turbidity, LocationType))
Turb <- Turb[order(Turb$StationID, Turb$Visit.Start.Date),]
#make long and remove rows with NA for observation value
Turb_long <- Turb %>%
	gather(key = Local.Characteristic.Name, value = Result.Value.Text, -c(SampleDepth, Visit.Start.Date, StationID, NPSTORET.Org.ID.Code)) %>%
	dplyr::filter(!is.na(Result.Value.Text)) %>%
	mutate(Upper.Quantification.Limit = NA) %>%
	mutate(Lower.Quantification.Limit = NA) %>%
	mutate(Depth.Units = NA)
Turb_long <- Turb_long[,col_order]

#select variables of interest from Secchi table:
Sec <- Secchi %>%
	select(c('PK.Secchi', 'FK.Sample','SDepth1.m', 'Bot.SD1'))
#merge in ancillary info on date and site:
Sec <- merge(Sec, Samples, by.x="FK.Sample", by.y = "PK.Sample")
Sec <- merge(Sec, Events, by.x="FK.Event", by.y = "PK.Event") 
Sec <- merge(Sec, Locations, by.x="FK.Location", by.y = "PK.Location") 
#format Date
Sec$StartDate <- as.Date(Sec$StartDate, format = "%m/%d/%y")
Sec$Lower.Quantification.Limit <- ifelse(Sec$Bot.SD1 == 'B', Sec$SDepth1.m, NA)
Sec$SDepth1.m <- ifelse(Sec$Bot.SD1 == 'B', "*Present <QL", Sec$SDepth1.m)
Sec <- Sec %>%
	rename(StationID=NPStoretSiteCode, Visit.Start.Date=StartDate) %>%
	mutate(NPSTORET.Org.ID.Code = "NETN") %>%
	mutate(Upper.Quantification.Limit = NA) %>%
	mutate(SampleDepth = case_when(LocationType == 'Stream' ~ 'stream',
	 LocationType == 'Pond' ~ 'epilimnion', 
	 LocationType == 'Lake' ~ 'epilimnion',
	 LocationType == 'Bottom' ~ 'bottom')) %>%
	select(-c(FK.Location, FK.Event, FK.Sample, PK.Secchi, LocationType, Bot.SD1))
Sec <- Sec[order(Sec$StationID, Sec$Visit.Start.Date),]
#make long and remove rows with NA for observation value
Sec_long <- Sec %>%
	gather(key = Local.Characteristic.Name, value = Result.Value.Text, -c(SampleDepth, Visit.Start.Date, StationID, NPSTORET.Org.ID.Code, Upper.Quantification.Limit, Lower.Quantification.Limit)) %>%
	dplyr::filter(!is.na(Result.Value.Text)) %>%
	mutate(Depth.Units = NA)
Sec_long <- Sec_long[,col_order]

#select variables of interest from LightPenetration table:
Light <- LightPenetration %>%
	select(c('PK.LightPenetration', 'FK.Sample', 'Depth.m', 'PenetrationRatio'))
#merge in ancillary info on date and site:
Light <- merge(Light, Samples, by.x="FK.Sample", by.y = "PK.Sample")
Light <- merge(Light, Events, by.x="FK.Event", by.y = "PK.Event") 
Light <- merge(Light, Locations, by.x="FK.Location", by.y = "PK.Location") 
#format Date
Light$StartDate <- as.Date(Light$StartDate, format = "%m/%d/%y")
Light <- Light %>%
	rename(StationID=NPStoretSiteCode, Visit.Start.Date=StartDate, SampleDepth = Depth.m) %>%
	mutate(NPSTORET.Org.ID.Code = "NETN") %>%
	select(-c(FK.Location, FK.Event, FK.Sample, PK.LightPenetration))
Light <- Light[order(Light$StationID, Light$Visit.Start.Date),]
#make long and remove rows with NA for observation value
Light_long <- Light %>%
	select(-LocationType) %>%
	gather(key = Local.Characteristic.Name, value = Result.Value.Text, -c(SampleDepth, Visit.Start.Date, StationID, NPSTORET.Org.ID.Code)) %>%
	dplyr::filter(!is.na(Result.Value.Text)) %>%
	mutate(Upper.Quantification.Limit = NA) %>%
	mutate(Lower.Quantification.Limit = NA) %>%
	mutate(Depth.Units = "m")
Light_long <- Light_long[,col_order]

#aggregrate to find median light penetration in top 2m:
temp2 <- Light %>%
	filter(SampleDepth <= 2) %>%
	gather(key = Local.Characteristic.Name, value = Result.Value.Text, -c(SampleDepth, Visit.Start.Date, StationID, NPSTORET.Org.ID.Code)) %>%
	mutate(Result.Value.Text = as.numeric(Result.Value.Text)) %>%
	dplyr::filter(!is.na(Result.Value.Text)) %>%
	group_by(StationID, Visit.Start.Date, Local.Characteristic.Name) %>%
	dplyr::summarize(Result.Value.Text = median(as.numeric(Result.Value.Text))) %>%
	ungroup() 	%>%
	spread(key = Local.Characteristic.Name, value = Result.Value.Text) %>%
	mutate(NPSTORET.Org.ID.Code = "NETN")	
temp2 <- merge(temp2, Locations, by.x="StationID", by.y = "NPStoretSiteCode", all.x=T, all.y=F) 

temp2 <- temp2 %>%
	mutate(SampleDepth = case_when(LocationType == 'Stream' ~ 'stream',
	 LocationType == 'Pond' ~ 'epilimnion', 
	 LocationType == 'Lake' ~ 'epilimnion')) %>%
	select(-c(PK.Location, LocationType))
#make long, remove rows with NA for observation value, add column for Depth.Units:
temp2_long <- temp2 %>%
	gather(key = Local.Characteristic.Name, value = Result.Value.Text, -c(SampleDepth, Visit.Start.Date, StationID, NPSTORET.Org.ID.Code)) %>%
	dplyr::filter(!is.na(Result.Value.Text)) %>%
	mutate(Upper.Quantification.Limit = NA) %>%
	mutate(Lower.Quantification.Limit = NA) %>%
	mutate(Depth.Units = NA)
temp2_long <- temp2_long[,col_order]
#add summaries to WQ_long df:
Light_long <- rbind(Light_long, temp2_long)



#merge primary observations together
#First, make 'NETN_Water_Data.csv' (full datset with all depths and aggregations). 
#Next, make 'Water Data.csv' (with data for depth profiles removed so that only samples that represent the stream/epilimnion remain) for the NCRN Water data and WaterViz packages.
waterDat <- Reduce(function(x,y) rbind(x,y), list(Chem_long, WQ_long, SD_long, Turb_long, Sec_long, Light_long))

#write out data as 'NETN_Water_Data.csv':
write.csv(waterDat, file = "Data/NETN_Water_Data.csv", row.names=F)

#########################
#create data frame in the template 'MetaData.csv' for the Rshiny WaterViz. Each row represents a variable (characteristic) at a location.
sites <- unique(waterDat$StationID)
MD <- merge(Location, tluParkCode, by = "ParkCode", all.x=T, all.y=F)
MD <- MD %>%
	rename(ShortName = PARKNAME, Type = LocationType, SiteCode = NPStoretSiteCode, SiteName = ShortSiteName, Lat = StartLat.DD, Long = StartLon.DD) %>%
	mutate(Network = "NETN") %>%
	mutate(LongName = paste(ShortName, PARKTYPE, sep = " ")) %>%
	dplyr::filter(SiteCode %in% sites) %>%
	select(c(Network, ParkCode, ShortName, LongName, SiteCode, SiteName, Lat, Long, Type))

#what variables are measured at each site?
how.many <- function(x){length(unique(x))}
tapply(waterDat$Local.Characteristic.Name, waterDat$StationID, how.many)
#create rows only for the variables measured at each site and extract units:
SiteCode <- vector(length = 0)
CharacteristicName <- vector(length = 0)
Units <- vector(length = 0)
DataName <- vector(length = 0)
for (i in seq_along(sites)) {
	temp <- subset(waterDat, StationID == sites[i])
	vars <- unique(temp$Local.Characteristic.Name)
	pc <- rep(sites[i], length(vars))
	units <- sapply(strsplit(vars, "[.]"), "[",2)
	datanames <- sapply(strsplit(vars, "[.]"), "[",1)
	SiteCode <- append(SiteCode, pc)
	CharacteristicName <- append(CharacteristicName, vars)
	Units <- append(Units, units)
	DataName <- append(DataName, datanames)
}

temp2 <- as.data.frame(cbind(SiteCode, CharacteristicName, DataName, Units), stringsAsFactors=F)

MD <- merge(MD, temp2, by = "SiteCode", all=T)

#create Display names
MD <- MD %>%
	mutate(DisplayName = case_when(
	CharacteristicName == "Al.ugL" ~ "Aluminium",
	CharacteristicName == "ANC.ueqL" ~ "Acid Neutralizing Capacity",
	CharacteristicName == "Ca.ueqL" ~ "Calcium",
	CharacteristicName == "DOC.mgL" ~ "Dissolved Organic Carbon",
	CharacteristicName == "K.ueqL" ~ "Potassium",
	CharacteristicName == "Mg.ueqL" ~ "Magnesium",
	CharacteristicName == "Na.ueqL" ~ "Sodium",
	CharacteristicName == "NH3.mgL" ~ "Ammonia",
	CharacteristicName == "NH4.mgL" ~ "Ammonium",
	CharacteristicName == "NO3.ueqL" ~ "Nitrate",
	CharacteristicName == "SO4.ueqL" ~ "Sulfate",
	CharacteristicName == "TN.mgL" ~ "Total Nitrogen",
	CharacteristicName == "BP.mmHg" ~ "Air Pressure",
	CharacteristicName == "DOsat.pct" ~ "Dissolved Oxygen (percent)",
	CharacteristicName == "DO.mgL" ~ "Dissolved Oxygen",
	CharacteristicName == "pH" ~ "pH",
	CharacteristicName == "SpCond.uScm" ~ "Specific Conductance",
	CharacteristicName == "Temp.C" ~ "Water Temperature",
	CharacteristicName == "SDepth1.m" ~ "Secchi Depth",
	CharacteristicName == "ChlA.ugL" ~ "Chlorophyll A",
	CharacteristicName == "PO4.ugL" ~ "Phosphate",
	CharacteristicName == "TotDissN.mgL" ~ "Total Dissolved Nitrogen",
	CharacteristicName == "TotDissP.ugL" ~ "Total Dissolved Phosphorus",
	CharacteristicName == "TP.ugL" ~ "Total Phosphorus",
	CharacteristicName == "PenetrationRatio" ~ "Light Penetration Ratio",
	CharacteristicName == "NO2.NO3.mgL" ~ "Nitrate + Nitrite",
	CharacteristicName == "Discharge.cfs" ~ "Discharge",
	CharacteristicName == "Turbidity.NTU" ~ "Turbidity",
	CharacteristicName == "NO2.mgL" ~ "Nitrite"	
	))

#create extra columns. Column for Data Type: these are all numeric, so I took a shortcut. Will need tobe changed if factor or ordinal data are added.
MD$DataType <- "numeric"
MD$LowerPoint <- 0 #needs to be Num
MD$UpperPoint <- 100 #needs to be Num
MD$LowerDescription <- 'testing'
MD$UpperDescription <- 'testing2'
MD$AssessmentDetails <- 'testing3'
MD$Category <- MD$DataName  
MD$CategoryDisplay <- MD$DisplayName


#write out data in wide form...
write.csv(MD, file = "~/Google Drive File Stream/My Drive/NETN Water R coding project/NETN_Water_data/MetaData.csv", row.names=F)