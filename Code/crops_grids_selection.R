library(tidyverse)
library(data.table)
library(tidyr)
library(dplyr)
library(lubridate)
library(viridisLite)
library(viridis)
library(devtools)
library(car)
library(ggpubr)
library(corrplot)
library(RColorBrewer)
library(reshape2)
library(readxl)
library(stringi)

#rm(list = ls())

####WSDA Master file of 2020 WA####
## dataframe used in this code for crop selection and cleaning is in "/data_folder/raw/crop_selection" I just used from my local
WSDA_master<- read.csv("C:/Users/mdredwanahmad.khan/OneDrive - Washington State University (email.wsu.edu)/Projects/Partial leasing/Crops and grids/MasterFile/WA_Crops_Masterfile_WSDA_20.csv")
selected<- subset(WSDA_master, Selected_for_Partial_Leasing== "YES")
soil<- fread("D:/Coordinate_shutdown/Stuffs_from_fabio_n_matt/Fabio/CropSyst_file_Fabio/all_calibrated_plus_uncalibrated_soil_210106.txt")
soil<- soil[,c(2:4)]
names(soil)<- c("Grid_Number","lat","long")

#################
##Okanogan 2020##
#################

# read the Okanogan irrigated acreages and select required columns
Okanogangrids<-  read.csv("D:/Coordinate_shutdown/Intersected_Watersheds/Okanogan_intersected_joined_WRIA.csv")
Okanogangrids<- Okanogangrids[,c("OBJECTID","CropType","Acres","Irrigation","CropGroup","long","lat","WRIA_ID","WRIA_NM","CropArea")]
names(Okanogangrids)<- c("FID_WSDA","Crop_Name","Acres","Irrigation","CropGroup","lat","long","WRIA_ID1","WRIA","CropArea")


##Okanogan_Selected
Okanogan<- Okanogangrids%>% 
  dplyr::select(c("FID_WSDA","Crop_Name","Acres","Irrigation","lat","WRIA_ID1","long","WRIA","CropArea"))%>%
  left_join(soil%>% dplyr::select("lat","long","Grid_Number")) %>% 
  filter(Crop_Name %in% selected$Crop_Name ) %>% 
  mutate(region= "Okanogan")


##WallaWalla###
# read the WallaWalla irrigated acreages and select required columns
WallaWallagrids<-  read.csv("F:/Coordinated shutoff/Intersected_Watersheds/WallaWalla_intersected_joined_WRIA.csv")
WallaWallagrids<- WallaWallagrids[,c("OBJECTID","CropType","Acres","Irrigation","CropGroup","long","lat","WRIA_ID","WRIA_NM","CropArea")]
names(WallaWallagrids)<- c("FID_WSDA","Crop_Name","Acres","Irrigation","CropGroup","lat","long","WRIA_ID1","WRIA","CropArea")

#WallaWalla_Selected
WallaWalla<- WallaWallagrids%>% 
  dplyr::select(c("FID_WSDA","Crop_Name","Acres","Irrigation","lat","WRIA_ID1","long","WRIA","CropArea"))%>%
  left_join(soil%>% dplyr::select("lat","long","Grid_Number")) %>% 
  filter(Crop_Name %in% selected$Crop_Name ) %>% 
  mutate(region= "WallaWalla")

####Yakima###
# read the Yakima irrigated acreages and select required column
Yakimagrids<-  read.csv("F:/Coordinated shutoff/Intersected_Watersheds/Yakima_intersected_joined_WRIA.csv")
Yakimagrids<- Yakimagrids[,c("OBJECTID","CropType","Acres","Irrigation","CropGroup","long","lat","WRIA_ID","WRIA_NM","CropArea")]
names(Yakimagrids)<- c("FID_WSDA","Crop_Name","Acres","Irrigation","CropGroup","lat","long","WRIA_ID1","WRIA","CropArea")

#Yakima_Selected
Yakima<- Yakimagrids%>% 
  dplyr::select(c("FID_WSDA","Crop_Name","Acres","Irrigation","lat","WRIA_ID1","long","WRIA","CropArea"))%>%
  left_join(soil%>% dplyr::select("lat","long","Grid_Number")) %>% 
  filter(Crop_Name %in% selected$Crop_Name ) %>% 
  mutate(region= "Yakima") #%>% rename("WRIA"="WRIA_NM")


###################################
#####Methow 2020 including grids###
###################################
# read the Methow irrigated acreages and select required columns
Methowgrids<-  read.csv("F:/Coordinated shutoff/Intersected_Watersheds/Methow_intersected_joined_WRIA.csv")
Methowgrids<- Methowgrids[,c("OBJECTID","CropType","Acres","Irrigation","CropGroup","lat","long","WRIA_ID","WRIA_NM","CropArea")] 
names(Methowgrids)<- c("FID_WSDA","Crop_Name","Acres","Irrigation","CropGroup","lat","long","WRIA_ID1","WRIA","CropArea")

#Methow
Methow<- Methowgrids%>% 
  dplyr::select(c("FID_WSDA","Crop_Name","Acres","Irrigation","lat","WRIA_ID1","long","WRIA","CropArea"))%>%
  left_join(soil%>% dplyr::select("lat","long","Grid_Number")) %>% 
  filter(Crop_Name %in% selected$Crop_Name ) %>% 
  mutate(region= "Methow")

###Wenatchee
# read the Wenatchee irrigated acreages and select required columns

Wenatcheegrids<-  read.csv("F:/Coordinated shutoff/Intersected_Watersheds/Wenatchee_intersected_joined_WRIA.csv")
Wenatcheegrids<- Wenatcheegrids[,c("OBJECTID","CropType","Acres","Irrigation","CropGroup","lat","lnog","WRIA_ID","WRIA_NM","CropArea")]
names(Wenatcheegrids)<- c("FID_WSDA","Crop_Name","Acres","Irrigation","CropGroup","lat","long","WRIA_ID1","WRIA","CropArea")


Wenatchee<- Wenatcheegrids%>% 
  dplyr::select(c("FID_WSDA","Crop_Name","Acres","Irrigation","lat","WRIA_ID1","long","WRIA","CropArea"))%>%
  left_join(soil%>% dplyr::select("lat","long","Grid_Number")) %>% 
  filter(Crop_Name %in% selected$Crop_Name ) %>% 
  mutate(region= "Wenatchee")

####Naches
# read the Naches irrigated acreages and select required columns
Nachesgrids<-  read.csv("F:/Coordinated shutoff/Intersected_Watersheds/Naches_intersected_joined_WRIA.csv")
Nachesgrids<- Nachesgrids[,c("OBJECTID","CropType","Acres","Irrigation","CropGroup","lat","long","WRIA_ID","WRIA_NM","CropArea")]
names(Nachesgrids)<- c("FID_WSDA","Crop_Name","Acres","Irrigation","CropGroup","lat","long","WRIA_ID1","WRIA","CropArea")

Naches<- Nachesgrids%>% 
  dplyr::select(c("FID_WSDA","Crop_Name","Acres","Irrigation","lat","WRIA_ID1","long","WRIA","CropArea"))%>%
  left_join(soil%>% dplyr::select("lat","long","Grid_Number")) %>% 
  filter(Crop_Name %in% selected$Crop_Name ) %>% 
  mutate(region= "Naches")


##Combine all watersheds into one dataframe
df_list1 <- list(Okanogangrids,WallaWallagrids,Yakimagrids,Nachesgrids,Methowgrids, Wenatcheegrids)      
ALLCRPGRIDS<- Reduce(function(x, y) merge(x, y, all=TRUE), df_list1) %>% 
  mutate(Long= gsub("-","",long),
         Lat= paste(lat,"N",sep = ""),
         Long= paste(Long,"W", sep = ""),
         site= paste(Lat,Long,sep=""))
#write.csv(ALLCRPGRIDS, "F:/Coordinated shutoff/Intersected_Watersheds/Combined_datset/ALLCRPGRIDS.csv")



##Combine all dataframe
df_list <- list(Okanogan,WallaWalla,Yakima,Naches,Methow, Wenatchee)      
SubbasinWR<- Reduce(function(x, y) merge(x, y, all=TRUE), df_list)  
#check<- filter(SubbasinWR, Crop_Name %in% selected$Crop_Name)

##Cleaning the lat long to add N and W for further analysis
SubbasinWR<- SubbasinWR %>% 
  mutate(Long= gsub("-","",long),
         Lat= paste(lat,"N",sep = ""),
         Long= paste(Long,"W", sep = ""),
         site= paste(Lat,Long,sep=""))


####Creating data table for Generator (to create ####
#Seperating Alfalfa/Grass Hay
cpa<- subset(SubbasinWR, Crop_Name=="Alfalfa/Grass Hay")
cpa$Crop_Name<- gsub('Alfalfa/Grass Hay','Alfalfa Hay', cpa$Crop_Name)

drop<- c("Alfalfa/Grass Hay","Onion Seed","Sod Farm","Buckwheat","Wildlife Feed")
SubbasinWR<- subset(SubbasinWR, !Crop_Name %in% drop)
SubbasinWR<- merge(SubbasinWR, cpa, all = TRUE)


#Renaming crop 
SubbasinWR$Crop_Name<- stri_replace_all_regex(SubbasinWR$Crop_Name,
                                               pattern = c("Alfalfa Hay","Alfalfa Seed","Barley","Barley Hay","Canola","Corn Seed",
                                                           "Corn, Field","Corn, Sweet","Grass Hay", "Hops",
                                                           "Oat","Oat Hay", "Onion","Rye", "Sudangrass", "Timothy", "Triticale", 
                                                           "Triticale Hay", "Wheat", "Wheat Fallow", "Yellow Mustard"),
                                               replacement = c("Alfalfa_Hay","Alfalfa_Seed","Barley_Spring","Barley_Hay","Canola",
                                                               "Corn_grain","Corn_grain","Sweet_Corn","Grass_Hay", "Hops",
                                                               "Oats","Oats_hay", "Onion","Rye", "Sudangrass", "Timothy", 
                                                               "Triticale", "Triticale_Hay", "Winter_wheat", "Winter_wheat", "Yellow_Mustard"),
                                               vectorize= FALSE)



SubbasinWR$Crop_Name<- gsub('Oats Hay','Oats_hay', SubbasinWR$Crop_Name)
###Separating Barley hay and spring to distinguish name
#Renaiming required as it renamed in previous chunk following a pattern
Barley_Spring<- filter(SubbasinWR, Crop_Name=="Barley_Spring")
Barley_Hay <- filter(SubbasinWR, Crop_Name=="Barley_Spring Hay")
Barley_Hay$Crop_Name<- gsub('Barley_Spring Hay','Barley_Hay', Barley_Hay$Crop_Name)
Barley<- rbind(Barley_Spring,Barley_Hay)

###Separating Winter wheat to add spring wheat in the data frame
SubbasinWR$Crop_Name<- gsub('Winter_wheat Fallow', 'Winter_wheat', SubbasinWR$Crop_Name)
winter<- filter(SubbasinWR, Crop_Name== "Winter_wheat")
winter$Crop_Name<- gsub('Winter_wheat','Spring_wheat', winter$Crop_Name)
drop1<- c("Barley_Spring", "Barley_Spring Hay")
SubbasinWR<- subset(SubbasinWR, !Crop_Name %in% drop1)

##Combining data frame
df_list1 <- list(SubbasinWR,winter,Barley)      
SubbasinWR<- Reduce(function(x, y) merge(x, y, all=TRUE), df_list1) 
##Write this dataset for further use
write.csv(SubbasinWR, "F:/Coordinated shutoff/Intersected_Watersheds/Combined_datset/crops_grids_FID_ID.csv")


###Summarising the area
crops_grids_info<- SubbasinWR %>% 
  mutate(Crop_site_region= paste0(Crop_Name,"_",site,"_",region)) %>% 
  group_by(Crop_site_region,Irrigation) %>% 
  summarise(Acres= sum(Acres, na.rm=T),
            Crop_area= sum(CropArea, na.rm = T),
            Crop_Name=Crop_Name,
            lat=lat,
            long=long,
            site=site,
            Grid_Number=Grid_Number,
            WRIA_ID1=WRIA_ID1,
            WRIA=WRIA,
            region=region) %>% distinct()

crops_grids<- read.csv("F:/Coordinated shutoff/Intersected_Watersheds/Combined_datset/crops_grids_Newfile.csv")

crops_grids<- read.csv("F:/Coordinated shutoff/Intersected_Watersheds/Combined_datset/crops_grids_Newfile.csv")
###Summarising by avoiding irrigation and then join the irrigation like previous parameter
crops_grids_info1<- crops_grids_info %>% 
  filter(Crop_site_region %in% crops_grids$Crop_site_region) %>% 
  group_by(Crop_Name,lat,long,site,Grid_Number,region,Crop_site_region,WRIA_ID1,WRIA) %>% 
  summarise(Acres= sum(Acres, na.rm=T),
            CropArea= sum(Crop_area, na.rm=T)) %>% 
  rename("crop"="Crop_Name") %>% 
  left_join(crops_grids %>% dplyr::select("crop","lat","long","Irrigation","WR_DOC_ID","areaacres_huc10","HUC10_NAME","Grid_Number","region","site","Crop_site_region"))

##Load data
#write.csv(crops_grids_info1, "F:/Coordinated shutoff/Intersected_Watersheds/Combined_datset/crops_grids_new.csv")

##Removing duplicates from the dataframe based on crop name, grid number,site, region
crops_grids_info1<- crops_grids_info[!duplicated(data.frame(t(apply(crops_grids_info[c("Crop_Name","Grid_Number","site","region")],1,sort)))),]
##number of the rowa are slightly high than previoos "Crops_grids" one

##Water_right data overlayed with WSDA field laer
Water_right<- read.csv("D:/Coordinated shutoff/Stuffs_from_fabio_n_matt/Matt/WSDA2020_PriorityDates_new_VIC.csv") %>% 
  filter(CropType %in% selected$Crop_Name) %>% 
  rename("Crop_Name"="CropType")

##Subsetting to rename crop names
cpa<- subset(Water_right, Crop_Name=="Alfalfa/Grass Hay")
cpa$Crop_Name<- gsub('Alfalfa/Grass Hay','Alfalfa Hay', cpa$Crop_Name)

##Drop some crops which are not listed for our analysis
drop<- c("Alfalfa/Grass Hay","Onion Seed","Sod Farm","Buckwheat","Wildlife Feed","Bluegrass Seed")
Water_right<- subset(Water_right, !Crop_Name %in% drop)
Water_right<- merge(Water_right, cpa, all = TRUE)


#Renaming required crop name fro our analysis to match crop names with simulation setup
Water_right$Crop_Name<- stri_replace_all_regex(Water_right$Crop_Name,
                                              pattern = c("Alfalfa Hay","Alfalfa Seed","Barley","Barley Hay","Canola","Corn Seed",
                                                          "Corn, Field","Corn, Sweet","Grass Hay", "Hops",
                                                          "Oat","Oat Hay", "Onion","Rye", "Sudangrass", "Timothy", "Triticale", 
                                                          "Triticale Hay", "Wheat", "Wheat Fallow", "Yellow Mustard"),
                                              replacement = c("Alfalfa_Hay","Alfalfa_Seed","Barley_Spring","Barley_Hay","Canola",
                                                              "Corn_grain","Corn_grain","Sweet_Corn","Grass_Hay", "Hops",
                                                              "Oats","Oats_hay", "Onion","Rye", "Sudangrass", "Timothy", 
                                                              "Triticale", "Triticale_Hay", "Winter_wheat", "Winter_wheat", "Yellow_Mustard"),
                                              vectorize= FALSE)

Water_right$Crop_Name<- gsub('Oats Hay','Oats_hay', Water_right$Crop_Name)

###Separating Barley hay and spring to distinguish name
Barley_Spring<- filter(Water_right, Crop_Name=="Barley_Spring")
Barley_Hay <- filter(Water_right, Crop_Name=="Barley_Spring Hay")
Barley_Hay$Crop_Name<- gsub('Barley_Spring Hay','Barley_Hay', Barley_Hay$Crop_Name)
Barley<- rbind(Barley_Spring,Barley_Hay)

###Separating Winter wheat to add spring wheat in the data frame
Water_right$Crop_Name<- gsub('Winter_wheat Fallow', 'Winter_wheat', Water_right$Crop_Name)
winter<- filter(Water_right, Crop_Name== "Winter_wheat")
winter$Crop_Name<- gsub('Winter_wheat','Spring_wheat', winter$Crop_Name)
drop1<- c("Barley_Spring", "Barley_Spring Hay")
Water_right<- subset(Water_right, !Crop_Name %in% drop1)

##Combining data frame
df_list1 <- list(Water_right,winter,Barley)      
Water_right<- Reduce(function(x, y) merge(x, y, all=TRUE), df_list1) 

write.csv(Water_right,"F:/Coordinated shutoff/Stuffs_from_fabio_n_matt/Matt/WSDA2020_PriorityDate_Final.csv") 







#################
## Fifteenmile ##
#################

##selected crops of the WA simulation
crops_grids_info1<- read.csv("F:/Coordinated shutoff/Intersected_Watersheds/Combined_datset/crops_grids_new.csv")

##Fifteen data
Fifteenmilegrids<-  read.csv("C:/Users/mdredwanahmad.khan/OneDrive - Washington State University (email.wsu.edu)/Coordinated Shutdown/Fifteen_m_OR/Joined_Fifteenmile_Eightmile_VIC_grids.csv")
Fifteenmilegrids<- Fifteenmilegrids[,c("lat","long","Name")]
names(Fifteenmilegrids)<- c("lat","long","HUC10_Name")


Fifteenmile1<- Fifteenmilegrids%>% 
  left_join(soil%>% dplyr::select("lat","long","Grid_Number"))

##Required modification
Fifteenmile1<- Fifteenmile1 %>% 
  mutate(Long= gsub("-","",long), 
         Lat= paste(lat,"N",sep = ""),
         Long= paste(Long,"W", sep = ""),
         site= paste(Lat,Long,sep=""),
         lat= as.character(lat),
         long= as.character(long))
# write.csv(Fifteenmile1,"C:/Users/mdredwanahmad.khan/OneDrive - Washington State University (email.wsu.edu)/Coordinated Shutdown/Fifteen_m_OR/fifteenmile_grids.csv")


##
Fifteenmile_crops<- read.csv("C:/Users/mdredwanahmad.khan/OneDrive - Washington State University (email.wsu.edu)/Coordinated Shutdown/Fifteen_m_OR/fifteenmile_crops_V1.csv") %>% 
  left_join(Fifteenmile1, by=c("CellId"="Grid_Number"))

Fifteenmile_crops$CropNM<- stri_replace_all_regex(Fifteenmile_crops$CropNM,
                                            pattern = c("AlfalfaHay","GrassHay","SodSeed","Barley","WinterWheat",
                                                        "Canola","CornGrain","Onion","SpringWheat", "Triticale","Rye"),
                                            replacement = c("Alfalfa_Hay","Grass_Hay","Alfalfa_Seed","Barley_Spring",
                                                            "Winter_wheat","Canola","Corn_grain", 
                                                            "Onion","Spring_wheat", "Triticale","Rye"),
                                            vectorize= FALSE)

Fifteenmile_crops$IrrType<- stri_replace_all_regex(Fifteenmile_crops$IrrType,
                                                  pattern = c("CENTER_PIVOT","FLOOD","SPRINKLER","DRIP","BIG_GUN"),
                                                  replacement = c("Center Pivot","Flood","Sprinkler","Drip","Big Gun"),
                                                  vectorize= FALSE)

##Required modification
Fifteenmile_crops<- Fifteenmile_crops %>% 
  mutate(Long= gsub("-","",long),
         Lat= paste(lat,"N",sep = ""),
         Long= paste(Long,"W", sep = ""),
         site= paste(Lat,Long,sep=""))

Fifteenmile_selected<- filter(Fifteenmile_crops, CropNM %in% crops_grids$crop) %>% 
  filter(CropNM != "Hops")

#write.csv(Fifteenmile_selected,"C:/Users/mdredwanahmad.khan/OneDrive - Washington State University (email.wsu.edu)/Coordinated Shutdown/Fifteen_m_OR/fifteenmile_crops_selected_V1.csv")



### Water rights data to accommodate interruptible water rights

## To create the supply curve, we first removed fields associated with interruptible water rights from the analysis.
## This dataset includes water rights (WRTS) data overlaid with the WSDA 2020 and WRIA files.
Water_right <- read.csv("raw/water_rights/WSDA2020_PriorityDate.csv") %>%
  rename("FID_WSDA" = "OBJECTID_1", "lat" = "join_lat", "long" = "join_lon", "crop" = "Crop_Name", 
         "PriorityDate" = "PriorityDa", "WRIA" = "WRIA_NM") %>%
  # Format longitude and latitude to create a unique 'site' identifier
  mutate(LONG = gsub("-", "", long),
         LAT = paste(lat, "N", sep = ""),
         LONG = paste(LONG, "W", sep = ""),
         site = paste(LAT, LONG, sep = "")) %>%
  # Selecting key columns for further analysis
  dplyr::select(c("FID_WSDA", "crop", "PriorityDate", "WRDocID", "FieldAcres", "Lat", "Long", "IntersectA", 
                  "lat", "long", "site", "WRIA")) %>%
  # Format latitude and longitude to 5 decimal places for consistency with other analysis
  mutate(lat = formatC(lat, digits = 5, format = "f"),
         long = formatC(long, digits = 5, format = "f"))


## PriorityDate column contains two different patterns: one with "-" and the other with "/" and month,year and day have different patern
## To ensure consistency, the following steps convert all patterns to the "/" format:

# Filter rows with PriorityDate in "-" format
Water_rightV1 <- filter(Water_right, grepl("-", PriorityDate))

# Filter rows with PriorityDate in "/" format
Water_rightV2 <- filter(Water_right, !PriorityDate %in% Water_rightV1$PriorityDate)

# Convert PriorityDate from "-" to "/" format
Water_rightV1 <- Water_rightV1 %>%
  separate(PriorityDate, into = c("Year", "Month", "Day"), sep = "-") %>%
  mutate(Month = as.numeric(Month),
         Day = as.numeric(Day),
         PriorityDate = paste0(Month, "/", Day, "/", Year)) %>%
  dplyr::select(c("FID_WSDA", "crop", "PriorityDate", "WRDocID", "FieldAcres", "Lat", "Long", 
                  "IntersectA", "lat", "long", "site", "WRIA"))

# Merge the two datasets back together
Water_right <- merge(Water_rightV1, Water_rightV2, all.x = TRUE, all.y = TRUE, sort = TRUE)

# Load crops and grids dataset including field IDs from WSDA CDL
crops_grids_FID_WSDA <- read.csv("raw/irrigated_crops_inf/selected_crops_grid.csv") %>% 
  rename("crop" = "Crop_Name", "Acres_PF" = "Acres") %>% 
  dplyr::select(c("FID_WSDA", "crop", "Acres_PF", "lat", "long", "WRIA_ID1", "WRIA", "Grid_Number", "region", "Long", "Lat", "site")) %>% 
  mutate(lat = formatC(lat, digits = 5, format = "f"),
         long = formatC(long, digits = 5, format = "f"))

# Merge selected_crops_grids_inf data with another dataset to get our targeted crop grids combination, which has been used in "comp_irrigation_saved_yield_reduction.R"
simulated_crops_grids <- read.csv("input_data/crops_grids_information_for_simulation.csv") %>% 
  mutate(lat= as.character(lat),
         long= as.character(long))

# Filter out WRIA values that are not present in the Water_right dataset and remove missing values
crops_grids_FID_WSDA1 <- left_join(crops_grids_FID_WSDA, simulated_crops_grids) %>% 
  rename("LAT" = "Lat", "LONG" = "Long") %>% 
  filter(WRIA %in% Water_right$WRIA) %>% 
  na.omit() #removes NA value from Yakima and Wenatchee watersheds

# Merge crops grids with water right priority date data into a single dataframe.
# This allows us to join on the Water Right diversion point "WRDocID" and subsequently remove fields associated with interruptible water rights.
crops_grids_V1 <- left_join(crops_grids_FID_WSDA1, Water_right) %>% 
  na.omit() %>% 
  dplyr::select(c("FID_WSDA", "crop", "Acres_PF", "Acres", "lat", "long", "Grid_Number", "site", "region", "Crop_site_region", "Irrigation", "PriorityDate", "WRDocID", "Lat", "Long")) %>% 
  rename("Field_lat" = "Lat", "Field_long" = "Long") %>% 
  mutate(fraction_acres = Acres_PF / Acres)

# Calculate fraction of field acres relative to total grid acres
crops_grids_V1 <- crops_grids_V1 %>% 
  mutate(fraction_acres = Acres_PF / Acres)

## write back the data file
#write.csv(crops_grids_V1,"C:/Users/mdredwanahmad.khan/OneDrive - Washington State University (email.wsu.edu)/Coordinated Shutdown/Paper/data_code_figure/data_folder/raw/water_rights/crops_grids_field.csv")


### WallaWalla water calls
touchet_Water_Call<- read.csv("D:/Coordinate_shutdown/Stuffs_from_fabio_n_matt/Matt/Touchet_junior_crop_areas.csv") %>% 
  rename("crop" = "CropType","Grid_Number" = "CellID") %>% 
  mutate(crop = case_when(crop== "Wheat" ~ "Spring_wheat",
                          crop== "Alfalfa Seed" ~ "Alfalfa_Seed",
                          crop== "Wheat Fallow" ~ "Spring_wheat",
                          crop== "Alfalfa Hay" ~ "Alfalfa_Hay",
                          crop== "Corn Seed" ~"Corn_grain",
                          crop== "Grass Hay" ~ "Grass_Hay",
                          crop== "Barley" ~ "Barley_Spring",
                          crop== "Triticale Hay" ~ "Triticale_Hay",
                          crop== "Oat Hay" ~ "Oats_hay",
                          crop== "Barley Hay" ~ "Barley_Hay",
                          crop== "Alfalfa/Grass Hay" ~"Alfalfa_Hay")) %>% na.omit() %>% left_join(soil) #na.omit will remove crops which are not renamed with case when, that are out of our study
write.csv(touchet_Water_Call, "data_folder/Check_data_to be deleted/water_calls.csv")

## As I have done same grids and acres of spring and winter wheat
Spring_wheat<- touchet_Water_Call %>% filter(crop=="Spring_wheat")
winter_wheat<- Spring_wheat %>% mutate(crop = case_when(crop== "Spring_wheat"~"Winter_wheat"))

## Combine both dataframe together
touchet_Water_Call<- rbind(touchet_Water_Call,winter_wheat) %>% rename("Acres_rm" = "Acres") %>% 
  mutate(crop_grid = paste0(crop,"_",Grid_Number), 
         crop_grid1 = crop_grid)

## Actual crops and grids
selected_crops_grids_inf <- read.csv("input_data/crops_grids_information_for_simulation.csv")

## Join both dataset to remove water call acreages
Joined<- left_join(selected_crops_grids_inf,touchet_Water_Call) %>% 
  mutate(Acres_rm = round(Acres_rm), 
         Acres_rm = ifelse(is.na(Acres_rm),0, Acres_rm),
         Acres1 = Acres- Acres_rm)

#Find duplicate in the dataset
duplicate<- Joined[duplicated(Joined$Number) | duplicated(Joined$Number, fromLast = TRUE),]

# All unique without duplicate below
Joined1<- Joined[!duplicated(data.frame(t(apply(Joined[c("Crop_site_region","Grid_Number","site","region")],1,sort)))),]
Joined2 <-  Joined1 %>% 
  dplyr::select(c("crop","lat","long","site","Grid_Number","region","Crop_site_region", "WRIA","Acres1","Acres_rm",
                  "Acres","Irrigation","WR_DOC_ID")) %>% 
  rename("Acres_Prev"="Acres", "Acres" = "Acres1") %>% filter(Acres>0)

## Write dataset into original folder
#write.csv(Joined1,"C:/Users/mdredwanahmad.khan/OneDrive - Washington State University (email.wsu.edu)/Coordinated Shutdown/Paper/data_code_figure/data_folder/input_data/crops_grids_information_for_simulation_v1.csv")

