library(lubridate)
library(dplyr)
### Chewck the NFTDA gage station responding AE_18 (NFTDA gage station)
nftda<- Calc_ratio %>% filter(Site_Code == "NFTDA")
nftda1<- nftda %>% filter(Aug_Need== "Yes_Need",simulation == "Scenario05", RelativeRatioOfAug_adj >= 0.5)

nftda2<- nftda %>% filter(Aug_Need== "Yes_Need",simulation == "Scenario05", RelativeRatioOfAug_adj <= 0.5)
nftda<- Consecutive_2_days_req %>% filter(Site_Code == "NFTDA")
nftda1<- nftda %>% filter(Can_meet_MIFR == "YES")

### TOUBO Station Check as per the AE comments
Toubo_check<- AugmentationRequiredSimulations %>% filter(Site_Code == "TOUBO")

## emodify simulation name to make it consistent with the simulations name in papers
Toubo_check1<- Toubo_check %>% 
  dplyr::select(c("Site_Code","DOY","Year","simulation","streamflow_aug_cfs","Discharge_cfs","MIFR_rules","Aug_Need","Can_meet_MIFR")) %>% 
  mutate(simulation = case_when(
    simulation == "Scenario01" ~ "May H1",
    simulation == "Scenario02" ~ "May H2",
    simulation == "Scenario03" ~ "Jun H1",
    simulation == "Scenario04" ~ "Jun H2",
    simulation == "Scenario05" ~ "Jul H1",
    simulation == "Scenario06" ~ "Jul H2",
    simulation == "Scenario07" ~ "Aug H1",
    simulation == "Scenario08" ~ "Aug H2"
  ),
  # Set order for the simulation labels to improve visualization
  simulation = factor(simulation, ordered = TRUE, levels = c("May H1","May H2","Jun H1","Jun H2","Jul H1","Jul H2","Aug H1","Aug H2")))

## Days when augmentation need
Toubo_check2<- Toubo_check1 %>% filter(Aug_Need =="Yes_Need") %>% #Discharge_cfs <30
  group_by(Year,simulation) %>% 
  summarise(streamflow_aug_cfs = mean(streamflow_aug_cfs, na.rm = T),
            Discharge_cfs_mean = mean(Discharge_cfs, na.rm=T),
            Discharge_cfs_med = median(Discharge_cfs, na.rm=T),
            Discharge_cfs_min = min(Discharge_cfs, na.rm=T),
            Discharge_cfs_max = max(Discharge_cfs, na.rm=T),
            MIFR_rules = mean(MIFR_rules))
#write.csv(Toubo_check2, "C:/Users/mdredwanahmad.khan/OneDrive - Washington State University (email.wsu.edu)/Coordinated Shutdown/Paper/data_code_figure/Figure/After_removing_Water_calls/Streamflow_aug_need_DOY_n_timeframe.csv")


## create figure for the water call need for comments R4_C2
P1<- ggplot(Toubo_check1, aes(x=simulation,y= Discharge_cfs,fill=simulation))+ 
  geom_boxplot(outlier.shape = NA)+ 
  geom_line(Toubo_check1, mapping=aes(x = simulation, y = MIFR_rules, group=1), 
            color = "black", size = 1.5)+
  stat_boxplot(geom='errorbar')+
  scale_fill_brewer(palette= "Paired")+
  scale_y_continuous("Discharge (cfs)",limits=c(0,210), breaks=seq(0,210, by=30))+ 
  geom_hline(yintercept = c(30), linetype="dashed", color="black", size = 1.5)+
  labs(x= "", y= "Discharge (cfs)", title = paste0(" "))+theme_bw()+ 
  theme(plot.title = element_text(size = 20),axis.title = element_text(size = 20), 
        legend.text = element_text(size=18), legend.title = element_blank(),
        axis.text.x = element_text(size = 22,color = "black", angle = 90, vjust = 0.5,hjust = 1),
        #axis.text.x = element_blank(),
        axis.text.y = element_text(size = 22,color = "black",angle = 0, hjust=1),
        legend.position = "none",axis.text.y.right = element_blank(),
        strip.text = element_text(size=22,color = "black"))

#Save the figure
ggsave(P1, file="Figure/TOUBO_WaterCalls.png", width = 25, height = 20,dpi = 300, units = "cm")


## Toubo Jul H1 check in responding AE20
## FIlter dataframe from consecutive two days required dataframe  #Consecutive_2_days_req
toubo_Jul_h1<- Calc_ratio %>% filter(Site_Code == "TOUBO",simulation == "Scenario05", Aug_Need == "Yes_Need") %>% 
  mutate(Diff_bet._MIFR_Discharge = MIFR_rules - Discharge_cfs) %>% 
  dplyr::select(c("Site_Code","DOY","Year","simulation","streamflow_aug_cfs","Discharge_cfs","MIFR_rules","Aug_Need","Diff_bet._MIFR_Discharge","Can_meet_MIFR","MIFR_R_75","MIFR_R_50","Can_meet_MIFR_75","Can_meet_MIFR_50"))

## Filter out columns need to create plot
toubo<- toubo_Jul_h1 %>% 
  dplyr::select(c("Site_Code","DOY","Year","simulation","streamflow_aug_cfs","Diff_bet._MIFR_Discharge")) %>% 
  mutate(Year = as.character(Year)) %>% 
  rename("streamflow_aug_available" = "streamflow_aug_cfs")

# organize dataframe
toubo<- melt(toubo, id= c("Site_Code","DOY","Year","simulation"))
#write.csv(toubo_Jul_h1,"C:/Users/mdredwanahmad.khan/OneDrive - Washington State University (email.wsu.edu)/Coordinated Shutdown/Paper/data_code_figure/Figure/toubo_julh1.csv")

## Create a boxplots
P1<- ggplot(toubo, aes(x=Year,y= value,fill=variable))+ 
  geom_boxplot()+ 
  stat_boxplot(geom='errorbar')+
  scale_fill_brewer(palette= "Paired")+
  scale_y_continuous("Discharge (cfs)",limits=c(0,100), breaks=seq(0,100, by=20))+ 
  #geom_hline(yintercept = c(30), linetype="dashed", color="black", size = 1.5)+
  labs(x= "", y= "Discharge (cfs)", title = paste0(" "))+theme_bw()+ 
  theme(plot.title = element_text(size = 20),axis.title = element_text(size = 20), 
        legend.text = element_text(size=18), legend.title = element_blank(),
        axis.text.x = element_text(size = 22,color = "black", angle = 90, vjust = 0.5,hjust = 1),
        #axis.text.x = element_blank(),
        axis.text.y = element_text(size = 22,color = "black",angle = 0, hjust=1),
        legend.position = "bottom",axis.text.y.right = element_blank(),
        strip.text = element_text(size=22,color = "black"))

#Save the figure
ggsave(P1, file="Figure/TOUBO_JulH1.png", width = 25, height = 20,dpi = 300, units = "cm")



## Toubo Call simulation timeframes
toubo_all<- Calc_ratio %>% filter(Site_Code == "TOUBO", Aug_Need == "Yes_Need") %>% #Consecutive_2_days_req
  dplyr::select(c("Site_Code","DOY","Year","simulation","streamflow_aug_cfs","Discharge_cfs","MIFR_rules","Aug_Need","MIFR_Dff",
                  "Can_meet_MIFR","MIFR_R_75","MIFR_R_50", "Can_meet_MIFR_75","Can_meet_MIFR_50"))


## Filter out columns need to create plot
toubo_1<- toubo_all %>% 
  dplyr::select(c("Site_Code","DOY","Year","simulation","streamflow_aug_cfs","MIFR_Dff")) %>% 
  mutate(Year = as.character(Year)) %>% 
  rename("Streamflow aug. available (cfs)" = "streamflow_aug_cfs","Streamflow aug. need (cfs)" = "MIFR_Dff")

# organize dataframe
toubo_1<- melt(toubo_1, id= c("Site_Code","DOY","Year","simulation")) %>% 
  mutate(simulation = case_when(
    simulation == "Scenario01" ~ "May H1",
    simulation == "Scenario02" ~ "May H2",
    simulation == "Scenario03" ~ "Jun H1",
    simulation == "Scenario04" ~ "Jun H2",
    simulation == "Scenario05" ~ "Jul H1",
    simulation == "Scenario06" ~ "Jul H2",
    simulation == "Scenario07" ~ "Aug H1",
    simulation == "Scenario08" ~ "Aug H2"
  ),
  # Set order for the simulation labels to improve visualization
  simulation = factor(simulation, ordered = TRUE, levels = c("May H1","May H2","Jun H1","Jun H2","Jul H1","Jul H2","Aug H1","Aug H2")))


## Create a boxplots for A@_20 which has been used in response document
P1<- ggplot(toubo_1, aes(x=simulation,y= value,fill=variable))+ 
  geom_boxplot(outlier.shape = NA)+ 
  stat_boxplot(geom='errorbar')+
  scale_fill_brewer(palette= "Paired")+
  scale_y_continuous("Discharge (cfs)",limits=c(0,100), breaks=seq(0,100, by=20))+
  labs(x= "", y= "Discharge (cfs)", title = paste0(" "))+theme_bw()+ 
  theme(plot.title = element_text(size = 20),axis.title = element_text(size = 20), 
        legend.text = element_text(size=18), legend.title = element_blank(),
        axis.text.x = element_text(size = 22,color = "black", angle = 90, vjust = 0.5,hjust = 1),
        #axis.text.x = element_blank(),
        axis.text.y = element_text(size = 22,color = "black",angle = 0, hjust=1),
        legend.position = "bottom",axis.text.y.right = element_blank(),
        strip.text = element_text(size=22,color = "black"))

#Save the figure
ggsave(P1, file="Figure/New/TOUBO_all.png", width = 25, height = 20,dpi = 300, units = "cm")



## Create the similar comparison plot for each stations
aug_need_for_all<- Calc_ratio %>% filter(Aug_Need == "Yes_Need") %>% #Consecutive_2_days_req
  dplyr::select(c("Site_Code","DOY","Year","simulation","streamflow_aug_cfs","Discharge_cfs","MIFR_rules","MIFR_Dff","Aug_Need","Can_meet_MIFR",
                  "MIFR_R_75","MIFR_R_50","Can_meet_MIFR_75","Can_meet_MIFR_50"))

## Filter out columns need to create plot
aug_need_for_all_1<- aug_need_for_all %>% 
  dplyr::select(c("Site_Code","DOY","Year","simulation","streamflow_aug_cfs","MIFR_Dff")) %>% 
  mutate(Year = as.character(Year)) %>% 
  rename("Streamflow aug. available (cfs)" = "streamflow_aug_cfs","Streamflow aug. need (cfs)" = "MIFR_Dff")

## Run a for loops that will produce comparison plots for each stations
sites<- unique(aug_need_for_all_1$Site_Code)
for ( i in 1: length(sites)){
  site_1<- aug_need_for_all_1 %>% filter(Site_Code == sites[i])
  
  # organize dataframe
  site_1<- melt(site_1, id= c("Site_Code","DOY","Year","simulation")) %>% 
    mutate(simulation = case_when(
      simulation == "Scenario01" ~ "May H1",
      simulation == "Scenario02" ~ "May H2",
      simulation == "Scenario03" ~ "Jun H1",
      simulation == "Scenario04" ~ "Jun H2",
      simulation == "Scenario05" ~ "Jul H1",
      simulation == "Scenario06" ~ "Jul H2",
      simulation == "Scenario07" ~ "Aug H1",
      simulation == "Scenario08" ~ "Aug H2"
    ),
    # Set order for the simulation labels to improve visualization
    simulation = factor(simulation, ordered = TRUE, levels = c("May H1","May H2","Jun H1","Jun H2","Jul H1","Jul H2","Aug H1","Aug H2")))
  
  ## Create a boxplots
  P1<- ggplot(site_1, aes(x=simulation,y= value,fill=variable))+ 
    geom_boxplot(outlier.shape = NA)+ 
    stat_boxplot(geom='errorbar')+
    scale_fill_brewer(palette= "Paired")+
    #scale_y_continuous("Discharge (cfs)",limits=c(0,100), breaks=seq(0,100, by=20))+ 
    #geom_hline(yintercept = c(30), linetype="dashed", color="black", size = 1.5)+
    labs(x= "", y= "Discharge (cfs)", title = paste0(sites[i]))+theme_bw()+ 
    theme(plot.title = element_text(size = 20),axis.title = element_text(size = 20), 
          legend.text = element_text(size=18), legend.title = element_blank(),
          axis.text.x = element_text(size = 22,color = "black", angle = 90, vjust = 0.5,hjust = 1),
          #axis.text.x = element_blank(),
          axis.text.y = element_text(size = 22,color = "black",angle = 0, hjust=1),
          legend.position = "bottom",axis.text.y.right = element_blank(),
          strip.text = element_text(size=22,color = "black"))
  
  #Save the figure
  ggsave(P1, file=paste0("Figure/sites/V1/",sites[i],".png"), width = 25, height = 20,dpi = 300, units = "cm")
  
}

## comparison plot for augmentation availabale with respect to MIFr and augmentation need wrt MIFR
## Create the augmentation need vs augmentation available in compare to the MIFR
## Augmenytation available should be check wrt all years even when streamflow augmentation is not required 
aug_need_for_all<- Consecutive_2_days_req %>% #MIFR_ComplianceCalc
  mutate(ratio_of_aug_need = MIFR_Dff/MIFR_rules,
         ratio_of_aug_available = streamflow_aug_cfs/MIFR_rules) %>% 
  dplyr::select(c("Site_Code","Year","DOY","simulation","streamflow_aug_cfs","Discharge_cfs","MIFR_rules","MIFR_Dff","Aug_Need","Can_meet_MIFR","MIFR_R_75","MIFR_R_50","Can_meet_MIFR_75","Can_meet_MIFR_50","ratio_of_aug_need","ratio_of_aug_available"))

## Filter out columns need to create plot
aug_need_for_all_1<- aug_need_for_all %>% 
  dplyr::select(c("Site_Code","Year", "DOY","simulation","ratio_of_aug_available")) %>% 
  mutate(Year = as.character(Year))
sites<- unique(aug_need_for_all_1$Site_Code)
for ( i in 1: length(sites)){
  site_1<- aug_need_for_all_1 %>% filter(Site_Code == sites[i])
  
  # organize dataframe
  site_1<- melt(site_1, id= c("Site_Code","DOY","Year","simulation")) %>% 
    mutate(simulation = case_when(
      simulation == "Scenario01" ~ "May H1",
      simulation == "Scenario02" ~ "May H2",
      simulation == "Scenario03" ~ "Jun H1",
      simulation == "Scenario04" ~ "Jun H2",
      simulation == "Scenario05" ~ "Jul H1",
      simulation == "Scenario06" ~ "Jul H2",
      simulation == "Scenario07" ~ "Aug H1",
      simulation == "Scenario08" ~ "Aug H2"
    ),
    # Set order for the simulation labels to improve visualization
    simulation = factor(simulation, ordered = TRUE, levels = c("May H1","May H2","Jun H1","Jun H2","Jul H1","Jul H2","Aug H1","Aug H2")))
  
  ## Create a boxplots
  P1<- ggplot(site_1, aes(x=simulation,y= value,fill=variable))+ 
    geom_boxplot(outlier.shape = NA)+ 
    stat_boxplot(geom='errorbar')+
    scale_fill_brewer(palette= "Paired")+
    labs(x= "", y= "Ratio of aug. need vs available wrt MIFR", title = paste0(sites[i]))+theme_bw()+ 
    theme(plot.title = element_text(size = 20),axis.title = element_text(size = 20), 
          legend.text = element_text(size=18), legend.title = element_blank(),
          axis.text.x = element_text(size = 22,color = "black", angle = 90, vjust = 0.5,hjust = 1),
          #axis.text.x = element_blank(),
          axis.text.y = element_text(size = 22,color = "black",angle = 0, hjust=1),
          legend.position = "bottom",axis.text.y.right = element_blank(),
          strip.text = element_text(size=22,color = "black"))
  
  #Save the figure
  ggsave(P1, file=paste0("Figure/sites/ratio/",sites[i],"_ratio.png"), width = 25, height = 20,dpi = 300, units = "cm")
  
}


#### Streamflow available is the percentage of the MIFR for all stations, years, and timeframes
## Modify columns as need
MIFR_to_Aug_Available_1<- MIFR_to_Aug_Available %>% 
  dplyr::select(c("Site_Code","Year", "DOY","simulation","ratio_of_aug_available","ratio_of_need")) %>% 
  mutate(ratio_of_aug_available_P = ratio_of_aug_available*100,
         simulation = case_when(
           simulation == "Scenario01" ~ "May H1",
           simulation == "Scenario02" ~ "May H2",
           simulation == "Scenario03" ~ "Jun H1",
           simulation == "Scenario04" ~ "Jun H2",
           simulation == "Scenario05" ~ "Jul H1",
           simulation == "Scenario06" ~ "Jul H2",
           simulation == "Scenario07" ~ "Aug H1",
           simulation == "Scenario08" ~ "Aug H2"),
         simulation = factor(simulation, ordered = TRUE, levels = c("May H1","May H2","Jun H1","Jun H2","Jul H1","Jul H2","Aug H1","Aug H2"))
         )

## Summarise the dataset for aug available median
MIFR_to_Aug_Available_summ <- MIFR_to_Aug_Available_1 %>% 
  group_by(Site_Code,simulation) %>% 
  summarise(aug_availabe_median = round(median(ratio_of_aug_available_P), digits = 2)) %>% 
  pivot_wider(names_from = simulation, values_from = aug_availabe_median)

## Summarise the dataset for aug need median
MIFR_to_Aug_Available_summ1 <- MIFR_to_Aug_Available_1 %>% 
  group_by(Site_Code,simulation) %>% 
  summarise(aug_need_median = round(median(ratio_of_need), digits = 2)) %>% 
  pivot_wider(names_from = simulation, values_from = aug_need_median)



## WSDA data to analyze irrigated extent and crop mix in answering R4_c1 and R4_C3
## Dfine croptype and group first
non_irrig<- c("None","Unknown","None/Sprinkler","None/Wheel Line","None/Sprinkler/Wheel Line","None/Rill")

crops<- c("Grass, Hay","Timothy","Alfalfa/Grass, Hay", "Alfalfa, Hay", "Wheat","Oat","Corn, Field","Barley","Triticale",
          "Corn, Unknown","Rye","Corn, Seed","Corn, Sweet","Onion","Hay/Silage, Unknown","Canola","Sudangrass","Alfalfa, Seed",
          "Wheat Fallow","Alfalfa Hay","Grass Hay","Alfalfa/Grass Hay","Alfalfa Seed",
          "Corn Seed","Barley Hay","Oat Hay","Triticale Hay","Clover/Grass Hay","Onion Seed",
          "Yellow Mustard","Grass Seed","Grass, Seed","Ryegrass, Seed")

grain<- c("Wheat","Oat","Corn, Field","Barley","Triticale","Fallow",
           "Corn, Unknown","Rye","Corn, Seed","Wheat Fallow","Corn Seed")


hay<- c("Grass, Hay","Timothy","Alfalfa/Grass, Hay", "Alfalfa, Hay", "Triticale",
          "Rye","Hay/Silage, Unknown","Sudangrass","Alfalfa, Seed",
          "Grass, Seed","Ryegrass, Seed","Alfalfa Hay","Grass Hay","Alfalfa/Grass Hay","Alfalfa Seed",
          "Bluegrass Seed","Ryegrass Seed","Barley Hay","Oat Hay","Triticale Hay","Clover/Grass Hay",
          "Grass Seed","Clover/Grass Hay" )

vegetables<- c("Bean, Dry","Garlic","Dill","Corn, Sweet","Squash","Pepper","Bean, Green","Potato", "Carrot, Seed","Onion",
               "Tomato","Cabbage","Pumpkin","Cucumber","Sunflower, Seed","Watermelon","Broccoli, Seed","Chickpea","Vegetable, Unknown",
               "Canola", "Pea, Dry","Spinach","Tobacco","Carrot Seed","Soybean","Bean Seed","Melon, Unknown","Sunflower","Rutabaga",
               "Radish","Pea/Vetch","Medicinal Herb","Lentil","Onion Seed","Yellow Mustard","Carrot","Marijuana","Mustard","Pea Seed",
               "Bean, Garbanzo","Hemp","Radish Seed","Beet Seed")


## load all WSDA crop land data layer 2007 - 2014
bdf<- rep()
files<- list.files("D:/Coordinate_shutdown/Geodata/WSDA_dbf/")
path<- "D:/Coordinate_shutdown/Geodata/WSDA_dbf/"
for ( i in 1: length(files[1:8])){
  data<- fread(paste0(path,files[i]), fill = TRUE, nrows = Inf) %>% 
    mutate(file = files[i],
           Year = substr(file, 1, 4)) %>% 
    #filter(!Irrigation %in% non_irrig) %>% 
    rename("lat1" = "long", "long1" = "lat") %>% rename("lat" = "lat1", "long" = "long1") %>% 
    mutate(LONG = gsub("-", "", long),
           LAT = paste(lat, "N", sep = ""),
           LONG = paste(LONG, "W", sep = ""),
           site = paste(LAT, LONG, sep = "")) %>% 
    dplyr::select(c("Year","OBJECTID","CropGroup","CropType","TotalAcres","Irrigation","InitialSur",
                    "LastSurvey","County","lat","long","WRIA_NM","site")) %>% rename("Acres" = "TotalAcres")
  # combine each year dataset into same year
  bdf <- rbind(bdf,data)
}

## load all WSDA crop land data layer 2015 - 2021
bdf1<- rep()
files<- list.files("D:/Coordinate_shutdown/Geodata/WSDA_dbf/Acres/")
path<- "D:/Coordinate_shutdown/Geodata/WSDA_dbf/Acres/"
for ( i in 1: length(files)){
  data<- fread(paste0(path,files[i]), fill = TRUE, nrows = Inf) %>% 
    mutate(file = files[i],
           Year = substr(file, 1, 4)) %>% 
    #filter(!Irrigation %in% non_irrig) %>% 
    rename("lat1" = "long", "long1" = "lat") %>% rename("lat" = "lat1", "long" = "long1") %>% 
    mutate(LONG = gsub("-", "", long),
           LAT = paste(lat, "N", sep = ""),
           LONG = paste(LONG, "W", sep = ""),
           site = paste(LAT, LONG, sep = "")) %>% 
    dplyr::select(c("Year","OBJECTID","CropGroup","CropType","Acres","Irrigation","InitialSur",
                    "LastSurvey","County","lat","long","WRIA_NM","site")) 
  
  # combine each year dataset into same year
  bdf1 <- rbind(bdf1,data)
}


## combine all years into a single dataframe
bdf2<- rbind(bdf,bdf1) %>% 
  mutate(CropType = as.character(CropType),
         irrig_type= ifelse(Irrigation %in% non_irrig, "Not_IR", "IR1"),
         category= ifelse(CropType %in% crops, "simulated","NON"),
         InitialSur = case_when(grepl("\\d{4}/\\d{2}/\\d{2}", InitialSur) ~ as.character(ymd_hms(InitialSur)),
           TRUE ~ as.character(mdy(InitialSur))),
         #InitialSur = as.Date(InitialSur),
         LastSurvey = case_when(grepl("\\d{4}/\\d{2}/\\d{2}", LastSurvey) ~ as.character(ymd_hms(LastSurvey)),
                                TRUE ~ as.character(mdy(LastSurvey)))) %>% 
         #Date = pmax(InitialSur,LastSurvey, na.rm = T))%>% 
  filter(irrig_type == "IR1",WRIA_NM %in% c("Methow","Okanogan","Walla Walla")) %>% 
  mutate(Date1 = LastSurvey) %>% 
  separate(Date1, into = c("Year_1","Month","Day"), sep = "-")

## Defining crop group
vegetables<- c("Vegetable","Mint","Seed","Oilseed","Melon","Herb")
grain<- c("Cereal Grain", "Unknown")
hay <- c("Turfgrass","Hay/Silage","Green Manure","Pasture","Non-Crop","Other")
perennial <- c("Orchard","Vineyard","Nursery","Christmas Tree","Commercial Tree","Berry","Research Station")
annual<- c("Vegetable","Mint","Seed","Oilseed","Melon","Herb","Cereal Grain", "Unknown")

## Filter dataset when data years and the last survey years are same
bdf3<- bdf2 %>% 
  mutate(category= ifelse(CropGroup  %in% grain, "grain",
                          ifelse(CropGroup  %in% hay, "hay",
                                 ifelse(CropGroup  %in% vegetables, "vegetables", "perennial"))),
         Year_1= as.numeric(Year_1),
         Year= as.numeric(Year)) %>% filter(Year == Year_1, Year>=2010) %>% 
  group_by(Year,WRIA_NM) %>% 
  summarise(Acres = sum(Acres))

## combine dataset by the dataset years
bdf4<- bdf2 %>% filter(Year >= 2010) %>% 
  group_by(Year,WRIA_NM) %>% 
  summarise(Acres_total = sum(Acres)) %>% mutate(Year= as.numeric(Year))

## Combine both datasets (bdf3 & bdf4) together to find out percentages by last survey year
bdf5<- left_join(bdf3,bdf4) %>% 
  mutate(ratioSurvey_b_year=(Acres/Acres_total)*100) %>% filter(ratioSurvey_b_year>=65)


## filter out dataset when last survey and dataset years are same and percentage in greater than 65
# unique watersheds
watershed<- unique(bdf5$WRIA_NM)
bdf6<- rep() # define empty dataset
# loops to filter years from original dataset
for ( i in 1: length(watershed)){
  # filter watersheds
  data1<- bdf5 %>% filter(WRIA_NM == watershed[i])
  # unique years when surveyed year and datset years are samee
  years<- unique(data1$Year)
  # loops to figure out dataset
  for ( j in 1:length(years)){
    data2<- bdf2 %>% filter(WRIA_NM == watershed[i], Year== years[j])
    # append to cmbine
    bdf6<- rbind(bdf6,data2)
  }
}

## Find percentage of the irrigated crop acreages by crop group
bdf7<- bdf6 %>% 
  mutate(Crp_grp_category= ifelse(CropGroup  %in% grain, "grain",
                          ifelse(CropGroup  %in% hay, "hay",
                                 ifelse(CropGroup  %in% vegetables, "vegetables", "perennial")))) %>% 
  group_by(Year,Crp_grp_category,WRIA_NM) %>% 
  summarise(Acres = sum(Acres)) %>% 
  group_by(Year,WRIA_NM) %>% 
  mutate(Acres_CRP = sum(Acres),
         ratio = (Acres/Acres_CRP)*100)



## Ratio of simulated crops acreage vs total crops acreage
P1<- ggplot(bdf7, mapping= aes(x=Year, y= ratio, fill= Crp_grp_category))+ geom_bar(stat = "identity")+
  facet_wrap(~WRIA_NM, scales = "free")+
  #scale_y_continuous("Percentage of total irrigated acrages",limits=c(0,100), breaks=seq(0,100, by=20))+
  labs(x= "", y= "Per. of total irrigated acrages", title = paste0(" "))+theme_bw()+ 
  theme(plot.title = element_text(size = 20),axis.title = element_text(size=17), 
        legend.text = element_text(size=18), legend.title = element_blank(),
        axis.text.x = element_text(size = 22,color = "black",angle = 45, hjust=1),
        axis.text.y = element_text(size = 22,color = "black",angle = 0, hjust=1),
        legend.position = "top",axis.text.y.right = element_blank(),
        strip.text = element_text(size=22,color = "black"))

## Save figure
ggsave(P1, file="Figure/irrigated_ratio_stack.png", width = 30, height = 20,dpi = 300, units = "cm")

# filter out to remove repetativeness by crop group
bdf71<- bdf7 %>% filter(Crp_grp_category=="hay")


P2<- ggplot(bdf71, mapping= aes(x=WRIA_NM, y= Acres_CRP, color= WRIA_NM))+ geom_boxplot()+
  facet_wrap(~WRIA_NM, scales = "free")+
  #scale_y_continuous("Percentage of total irrigated acrages",limits=c(0,75), breaks=seq(0,75, by=15))+
  labs(x= "", y= "Total irrigated acreages", title = paste0(" "))+theme_bw()+ 
  theme(plot.title = element_text(size = 20),axis.title = element_text(size=18), 
        legend.text = element_text(size=18), legend.title = element_blank(),
        axis.text.x = element_text(size = 22,color = "black",angle = 0, hjust=0.5),
        axis.text.y = element_text(size = 22,color = "black",angle = 0, hjust=1),
        legend.position = "none",axis.text.y.right = element_blank(),
        strip.text = element_text(size=22,color = "black"))

#library(patchwork)
P3 <- P1/P2

## Save figure
ggsave(P3, file="Figure/crop_mix_extent.png", width = 35, height = 22,dpi = 300, units = "cm")

## line plot of irrigated extent
P2<- ggplot()+geom_line(bdf71, mapping= aes(x=Year, y= Acres_CRP, group = WRIA_NM), size = 0.75)+ 
  facet_wrap(~WRIA_NM, scales = "free")+
  labs(x= "", y= "Total irrigated acreages", title = paste0(" "))+theme_bw()+ 
  theme(plot.title = element_text(size = 20),axis.title = element_text(size=18), 
        legend.text = element_text(size=18), legend.title = element_blank(),
        axis.text.x = element_text(size = 22,color = "black",angle = 45, hjust=1),
        axis.text.y = element_text(size = 22,color = "black",angle = 0, hjust=1),
        legend.position = "none",axis.text.y.right = element_blank(),
        strip.text = element_text(size=22,color = "black"))

## combine both plots together
P3 <- P1/P2

## Save figure
ggsave(P3, file="Figure/crop_mix_extent_v1.png", width = 35, height = 22,dpi = 300, units = "cm")

## filter out annual crops
bdf8<- bdf6 %>% mutate(crp_category = ifelse(CropGroup %in% annual, "Annual", "Perennial")) %>% 
  filter(crp_category == "Annual")

## major annual crops
crops_u<- c("Fallow","Wheat","Alfalfa Seed","Corn, Field","Wheat Fallow","Potato","Corn Seed","Corn, Sweet")

# summarize the datset by major crops
bdf9<- bdf8 %>% filter(CropType %in% crops_u) %>% 
  mutate(CropType = ifelse(CropType == "Corn Seed", "Corn, Field", CropType)) %>% 
  group_by(Year,CropType,WRIA_NM) %>% 
  summarise(sum_acres = sum(Acres)) %>% 
  group_by(Year,WRIA_NM) %>% 
  mutate(acres_total = sum(sum_acres), 
         Percentage = (sum_acres/acres_total)*100) #%>% filter(Percentage>10)

## drought response plot (stack plot)
P1<- ggplot(bdf9, mapping= aes(x=Year, y= Percentage, fill= CropType))+ geom_bar(stat = "identity")+
  facet_wrap(~WRIA_NM, scales = "free")+
  #scale_y_continuous("Major crops percentage out of total annual crop acreages",limits=c(0,100), breaks=seq(0,100, by=20))+
  labs(x= "", y= "Percentage acreages w.r.t total annual crop acreages", title = paste0(" "))+theme_bw()+ 
  theme(plot.title = element_text(size = 20),axis.title = element_text(size=17), 
        legend.text = element_text(size=18), legend.title = element_blank(),
        axis.text.x = element_text(size = 22,color = "black",angle = 45, hjust=1),
        axis.text.y = element_text(size = 22,color = "black",angle = 0, hjust=1),
        legend.position = "right",axis.text.y.right = element_blank(),
        strip.text = element_text(size=22,color = "black"))

## Save figure
ggsave(P1, file="Figure/Pecentage_crp.png", width = 30, height = 20,dpi = 300, units = "cm")




#### 
## Check crop acreas on the simulated watersheds for simulated crops
## Check acres on year when surveyed 
bdf_acres<- bdf6 %>% 
  filter(CropType %in% crops) %>% 
  group_by(Year,WRIA_NM) %>% 
  summarise(Acres_total = sum(Acres)) %>% 
  group_by(WRIA_NM) %>% 
  mutate( Acres_mean= mean(Acres_total),
          Acres_sd = sd(Acres_total),
          acres_min= min(Acres_total),
          acres_max = max(Acres_total),
          Percentage= ifelse(Acres_mean>Acres_total,((Acres_mean-Acres_total)/Acres_mean)*100, ((Acres_total-Acres_mean)/Acres_mean)*100),
          standard_dev_per = (Acres_sd/Acres_mean)*100)

## Check acres on every years
bdf_acres1<- bdf2 %>% 
  filter(CropType %in% crops, Year %in% c(2010:2021)) %>% 
  group_by(Year,WRIA_NM) %>% 
  summarise(Acres_total = sum(Acres)) %>% 
  group_by(WRIA_NM) %>% 
  mutate( Acres_mean= mean(Acres_total),
          Acres_sd = sd(Acres_total),
          acres_min= min(Acres_total),
          acres_max = max(Acres_total),
          Percentage= ifelse(Acres_mean>Acres_total,((Acres_mean-Acres_total)/Acres_mean)*100, ((Acres_total-Acres_mean)/Acres_mean)*100),
          standard_dev_per = (Acres_sd/Acres_mean)*100)






## If streamflow is already above MIFR for any two days and then again are counting


#### Filter out Two Consecutive Days with Streamflow Augmentation Requirement
# This section identifies and stores instances where at least two consecutive days
# of streamflow augmentation were required at specific stations and for particular simulations.
# Step 1: Create a list of unique sites from the dataset

sites<- unique(AugmentationRequiredSimulations$Site_Code)

# Initialize an empty list to store results for each instance of consecutive days
result_list_v1 <- list()

# Iterate over each site
for (i in 1: length(sites)){
  # Filter data for the current site
  site1<- AugmentationRequiredSimulations %>% filter(Site_Code==sites[i])
  # Get unique years for this site
  Yr<- unique(site1$Year)
  
  # Iterate over each year for the current site
  for (j in 1: length(Yr)){
    # Filter data for the current year
    site11<- site1 %>% filter(Year==Yr[j])
    # Get unique simulations for the current site and year which need streamflow augmentation
    simulations<- unique(site11$simulation)
    
    # Iterate over each simulation within the site and year
    for ( k in 1:length(simulations)){
      # Filter data for the current simulation where augmentation is needed
      site111<- site11 %>% filter(simulation==simulations[k],Aug_Need=="Not_Need") %>% arrange(DOY) 
      
      # Check if there is more than one row of data (indicating multiple days)
      if (nrow(site111>1)){
        # Identify rows with consecutive DOY values (indicating consecutive days)
        consecutive_days <- which(diff(site111$DOY) == 1)
        
        # If there are any consecutive days, store the relevant rows
        if (length(consecutive_days) > 0) {
          # Store the relevant rows
          for (l in 1:length(consecutive_days)) {
            # Store both consecutive rows for each pair of consecutive days
            result_list_v1[[length(result_list_v1) + 1]] <- site111[c(consecutive_days[l], consecutive_days[l] + 1), ]
          }
        }
      }
    }
  }
}

#### Combine Consecutive Days and Calculate Required Augmentation ####

# Step 1: Combine all consecutive days from the loop into a single data frame
Consecutive_2_days_not_req <- do.call(rbind, result_list_v1)

# Remove any duplicate rows that may exist
Consecutive_2_days_not_req <- Consecutive_2_days_not_req[!duplicated(Consecutive_2_days_not_req), ] 


## Check years
summary_not_req <- Consecutive_2_days_not_req %>% 
  group_by(Site_Code,Year, simulation) %>% 
  summarise(Day_not_required = n()) %>%
  mutate(Required = 0,
         Year = as.character(Year),
         simulation = case_when(
           simulation == "Scenario01" ~ "May H1",
           simulation == "Scenario02" ~ "May H2",
           simulation == "Scenario03" ~ "Jun H1",
           simulation == "Scenario04" ~ "Jun H2",
           simulation == "Scenario05" ~ "Jul H1",
           simulation == "Scenario06" ~ "Jul H2",
           simulation == "Scenario07" ~ "Aug H1",
           simulation == "Scenario08" ~ "Aug H2")
  )


## Check years
summary_req <- Consecutive_2_days_req %>% 
  group_by(Site_Code,Year, simulation) %>% 
  summarise(Day_required = n()) %>% 
  mutate(req = 1,
         Year = as.character(Year),
         simulation = case_when(
           simulation == "Scenario01" ~ "May H1",
           simulation == "Scenario02" ~ "May H2",
           simulation == "Scenario03" ~ "Jun H1",
           simulation == "Scenario04" ~ "Jun H2",
           simulation == "Scenario05" ~ "Jul H1",
           simulation == "Scenario06" ~ "Jul H2",
           simulation == "Scenario07" ~ "Aug H1",
           simulation == "Scenario08" ~ "Aug H2"))

#Panel a (cost/acre)
P1<- ggplot(summary_req, mapping = aes(x=simulation, y= Year))+
  geom_tile(aes(fill= req))+
  geom_text(aes(label = req, color = ifelse(req > 1, "black", "white")), size = 6) + facet_wrap(~Site_Code)+
  #geom_text(aes(label= Cost_acre), color="red", size= 8)+
  scale_fill_viridis(limits=c(0,2), breaks=seq(0,2, by=1), na.value = NA)+
  scale_color_identity() + 
  scale_x_discrete(labels = c("May H1","May H2","Jun H1","Jun H2","Jul H1","Jul H2","Aug H1","Aug H2"))+
  theme_bw()+labs(x= "", y= " ",title = paste0("Years streamflow augmentation required for consecutive two days"))+ 
  theme(
    axis.text = element_text(size = 20, color = "black"),
    plot.title = element_text(size = 20),
    axis.text.x = element_text(size = 20, color = "black", vjust = 0.5, hjust = 1, angle = 90),
    axis.title = element_text(size = 22),
    legend.text = element_text(size = 20, color = "black"), 
    legend.title = element_blank(),
    legend.position = "none", #c(0.07, 0.82),
    #legend.key.height = unit(0.8, "cm"),
    legend.background = element_blank()
  )
#Save the figure
ggsave(P1, file="Figure/Conc_2_days_req.png", width = 30, height = 25,dpi = 300, units = "cm")


## Check if any simulation time frme have counted in both
counted_both<- inner_join(summary_not_req,summary_req)

set_aug_target<-  Calc_ratio %>% 
  group_by(Site_Code,Year, simulation) %>% 
  summarise(Days_count = n()) %>% 
  mutate(Year = as.character(Year),
         simulation = case_when(
           simulation == "Scenario01" ~ "May H1",
           simulation == "Scenario02" ~ "May H2",
           simulation == "Scenario03" ~ "Jun H1",
           simulation == "Scenario04" ~ "Jun H2",
           simulation == "Scenario05" ~ "Jul H1",
           simulation == "Scenario06" ~ "Jul H2",
           simulation == "Scenario07" ~ "Aug H1",
           simulation == "Scenario08" ~ "Aug H2"))

counted_both1<- inner_join(set_aug_target,summary_not_req)

we_meet<- Combined_sites_2Days1 %>% 
  group_by(Site_Code,Year, simulation) %>% 
  summarise(Day_meet = n()) %>% 
  mutate(met = "Yes",
         Year = as.character(Year))

## Figure
we_meet_check <- inner_join(we_meet, counted_both) %>% 
  mutate(Counted= 2)

we_meet_check1<- anti_join(we_meet, counted_both) %>% 
  mutate(Counted= 1)

we_meet_c <- rbind(we_meet_check,we_meet_check1)%>% 
  mutate(simulation= factor(simulation, ordered = TRUE, c(c("May H1","May H2","Jun H1","Jun H2","Jul H1","Jul H2","Aug H1","Aug H2"))))
P1<- ggplot(we_meet_c, mapping = aes(x=simulation, y= Year))+
  geom_tile(aes(fill= Counted))+
  geom_text(aes(label = Counted, color = ifelse(Counted > 1, "black", "white")), size = 5) + facet_wrap(~Site_Code)+
  #geom_text(aes(label= Cost_acre), color="red", size= 8)+
  scale_fill_viridis(limits=c(1,2), breaks=seq(1,2, by=1), na.value = NA)+
  scale_color_identity() + 
  scale_x_discrete(labels = c("May H1","May H2","Jun H1","Jun H2","Jul H1","Jul H2","Aug H1","Aug H2"))+
  theme_bw()+labs(x= "", y= " ",title = paste0("Simulations counted twice"))+ 
  theme(
    axis.text = element_text(size = 20, color = "black"),
    plot.title = element_text(size = 20),
    axis.text.x = element_text(size = 20, color = "black", vjust = 0.5, hjust = 1, angle = 90),
    axis.title = element_text(size = 22),
    legend.text = element_text(size = 20, color = "black"), 
    legend.title = element_blank(),
    legend.position = "none", #c(0.07, 0.82),
    #legend.key.height = unit(0.8, "cm"),
    legend.background = element_blank()
  )
#Save the figure
ggsave(P1, file="Figure/Conc_2_days_req.png", width = 30, height = 25,dpi = 300, units = "cm")

