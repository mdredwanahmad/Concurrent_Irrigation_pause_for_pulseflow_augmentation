# library(tidyverse)
# library(data.table)
# library(tidyr)
# library(dplyr)
# library(lubridate)
# library(Metrics)


## To create the supply curve, we first removed fields associated with interruptible water rights from the analysis.
## load the dataframe which has crops, grids, and field information along with water right dates and diversion point
## This dataset is a processed dataset from merging WSDA 2020 and Ecology WRTS 2024 dataset
crops_grids_fields_WR <- read.csv("data_folder/input_data/crops_grids_field.csv") %>% 
  # Filter the data to include only the regions of interest: Walla Walla, Okanogan, and Methow
  filter(region %in% c("WallaWalla", "Okanogan", "Methow"))

# Load interruptible water rights data and filter non-interruptible fields removing field associated with interruptible water rights
Interruptable_water_right <- read.csv("data_folder/raw/water_rights/interruptibles.csv")
# removed fields associated with interruptible water right by removing fields associated with water right diversion point 
non_interruptable <- filter(crops_grids_fields_WR, !WRDocID %in% Interruptable_water_right$WR_Doc_ID) %>% 
  # convert the lat long into character to join with next dataframe
  mutate(lat = as.character(lat),
         long = as.character(long))


####################NON_INTERRUPTIBLE#####
# Remove fields with interruptible water rights, disaggregate to fields and again aggregate to grid level
# Join simulation data with non-interruptible water rights, calculate field-level metrics
## fifteendays_sc_t1 dataframe is uaisng from "comp_irrigation_saved_yield_reduction.R" script
NON_Interruptible <- inner_join(fifteendays_sc_t1 %>% dplyr::select("Year","crop","WM_yield_kgha","irrig","simulation","site","lat","long","region","Acres","irrig_optimal_mm","WM_yield_optimal_kgha","WM_yield_loss","total_WM_yield_kg","Total_economic_return_shut","Revenue_loss","sold_unit_opt","sold_unit_shut","streamflow_aug_mm","streamflow_aug_ft","streamflow_aug_acrft","streamflow_aug_cfs","Crop_site_region","Percentage_irrig","Per_irrig_saved","Percentage_yield","Per_yield_reduction","target_harvest_date","harvest_date1"),non_interruptable %>% dplyr::select(c("crop","Acres_PF","Acres","fraction_acres","lat","long","site","Grid_Number","region","Crop_site_region","PriorityDate","WRDocID","Field_lat","Field_long"))) %>% 
  
  # Calculate values at field level by multiplying with fractional acres
  mutate(streamflow_aug_acft_fld=streamflow_aug_acrft*fraction_acres,
         streamflow_aug_cfs_fld= streamflow_aug_cfs*fraction_acres,
         streamflow_aug_mm_fld= streamflow_aug_mm*fraction_acres,
         streamflow_aug_ft_fld= streamflow_aug_ft*fraction_acres,
         yield_gain_field= total_WM_yield_kg*fraction_acres,
         yield_loss_fld= WM_yield_loss*fraction_acres,
         Revenue_loss_fld=Revenue_loss*fraction_acres,
         Total_economic_return_shut= Total_economic_return_shut*fraction_acres,
         cost_acft_fld= Revenue_loss_fld/streamflow_aug_acft_fld,
         cost_acre_fld= Revenue_loss_fld/Acres_PF) %>% 
  # Aggregate back to grid level aggregating by crop, region, site, grid, year, and simulation
  group_by(crop,region,site,lat,long,Grid_Number,Year, simulation,harvest_date1,target_harvest_date) %>% 
  summarise(Acres_PF= sum(Acres_PF, na.rm=T),
            Acres=mean(Acres, na.rm=T),
            streamflow_aug_mm = sum(streamflow_aug_mm_fld, na.rm=T),
            streamflow_aug_ft = sum(streamflow_aug_ft_fld, na.rm=T),
            streamflow_aug_acrft= sum(streamflow_aug_acft_fld, na.rm=T),
            streamflow_aug_cfs= sum(streamflow_aug_cfs_fld, na.rm=T),
            Total_Yield= sum(yield_gain_field, na.rm=T),
            WM_yield_loss= sum(yield_loss_fld, na.rm=T),
            Revenue_loss= sum(Revenue_loss_fld,na.rm=T),
            Total_economic_return_shut= sum(Total_economic_return_shut, na.rm=T),
            cost_acrft= Revenue_loss/streamflow_aug_acrft,
            cost_acr= Revenue_loss/Acres) %>% 
  # use distinct function to remove duplicates of rows
  distinct()

## store this data to processed folder
#write.csv(NON_Interruptible,"processed/NON_Interruptible.csv")



#### Calculate marginal cost for each gage location and create dataset for supply curve ####
## Filter data for N.F. Touchet R. abv Dayton gage location
NFT_D_C<- NON_Interruptible %>% filter(site %in% TR_Dayton_grids$site) %>% 
  #select required variables from the dataset
  dplyr::select(c("region","Year","crop","site","lat","long","simulation","Acres","streamflow_aug_mm",
                  "streamflow_aug_ft","streamflow_aug_acrft","Revenue_loss",
                  "cost_acrft","cost_acr","Total_Yield","WM_yield_loss","target_harvest_date","harvest_date1")) %>% 
  #defining a unique identifier for each gage station
  mutate(Site_Code= "NFTDA")

## Calculate marginal cost using custom function "marginal_cost_calculation" from "req_functions.R" and add gage location name
marginal_nfd<- marginal_cost_calculation(NFT_D_C) %>% 
  mutate(SITENAME= "N.F. Touchet R. abv Dayton")

##WallaWalla Outlet ## Filter data for Walla Walla R.  E. Detour Rd. gage location
Detour_Rd_C<- NON_Interruptible %>% filter(site %in% WR_Detour_Rd_grids$site) %>% 
  dplyr::select(c("region","Year","crop","site","lat","long","simulation","Acres","streamflow_aug_mm",
                  "streamflow_aug_ft","streamflow_aug_acrft",
                  "Revenue_loss","cost_acrft","cost_acr","Total_Yield","WM_yield_loss","target_harvest_date","harvest_date1")) %>% 
  mutate(Site_Code= "WALDE")

## Calculate marginal cost using custom function "marginal_cost_calculation" from "req_functions.R" and add gage location name
marginal_Detour<- marginal_cost_calculation(Detour_Rd_C) %>% 
  mutate(SITENAME= "Walla Walla R.  E. Detour Rd.")

## Filter data for "Touchet R. at Bolles" gage location
Touchet_R_C<- NON_Interruptible %>% filter(site %in% TR_Bolles_grids$site) %>% 
  dplyr::select(c("region","Year","crop","site","lat","long","simulation","Acres",
                  "streamflow_aug_mm","streamflow_aug_ft","streamflow_aug_acrft",
                  "Revenue_loss","cost_acrft","cost_acr","Total_Yield","WM_yield_loss","target_harvest_date","harvest_date1")) %>%
  mutate(Site_Code= "TOUBO")

## Calculate marginal cost using custom function "marginal_cost_calculation" from "req_functions.R" and add gage location name
marginal_Touchet<- marginal_cost_calculation(Touchet_R_C) %>% 
  mutate(SITENAME= "Touchet R. at Bolles")


## Filter data for "OKANOGAN RIVER NEAR TONASKET, WA" gage location
Okn_Tonasket_R_C<- NON_Interruptible %>% filter(site %in% Tonasket_grids$site) %>% 
  dplyr::select(c("region","Year","crop","site","lat","long","simulation","Acres",
                  "streamflow_aug_mm","streamflow_aug_ft","streamflow_aug_acrft",
                  "Revenue_loss","cost_acrft","cost_acr","Total_Yield","WM_yield_loss","target_harvest_date","harvest_date1")) %>% 
  mutate(Site_Code= "OKATO")

## Calculate marginal cost using custom function "marginal_cost_calculation" from "req_functions.R" and add gage location name
marginal_Tonasket<- marginal_cost_calculation(Okn_Tonasket_R_C) %>% 
  mutate(SITENAME= "OKANOGAN RIVER NEAR TONASKET, WA")


## Filter data for "SIMILKAMEEN RIVER NEAR NIGHTHAWK, WA" gage location
Okn_Similkameen_R_C<- NON_Interruptible %>% filter(site %in% Similkameen_grids$site) %>% 
  dplyr::select(c("region","Year","crop","site","lat","long","simulation","Acres",
                  "streamflow_aug_mm","streamflow_aug_ft","streamflow_aug_acrft",
                  "Revenue_loss","cost_acrft","cost_acr","Total_Yield","WM_yield_loss","target_harvest_date","harvest_date1")) %>% 
  mutate(Site_Code= "SIMNI")

## Calculate marginal cost using custom function "marginal_cost_calculation" from "req_functions.R" and add gage location name
marginal_similkameen<- marginal_cost_calculation(Okn_Similkameen_R_C) %>% 
  mutate(SITENAME= "SIMILKAMEEN RIVER NEAR NIGHTHAWK, WA")

##Okanogan Outlet ## Filter data for "OKANOGAN RIVER AT MALOTT, WA" gage location
Okn_Malott_R_C<- NON_Interruptible %>% filter(site %in% Malott_grids$site) %>% #df_tmp_malott$site1
  dplyr::select(c("region","Year","crop","site","lat","long","simulation","Acres",
                  "streamflow_aug_mm","streamflow_aug_ft","streamflow_aug_acrft",
                  "Revenue_loss","cost_acrft","cost_acr","Total_Yield","WM_yield_loss","target_harvest_date","harvest_date1")) %>% 
  mutate(Site_Code= "OKAMA")

## Calculate marginal cost using custom function "marginal_cost_calculation" from "req_functions.R" and add gage location name
marginal_malott<- marginal_cost_calculation(Okn_Malott_R_C) %>% 
  mutate(SITENAME= "OKANOGAN RIVER AT MALOTT, WA")


##Methow Outlet ## Filter data for "METHOW RIVER NEAR PATEROS, WA" gage location
Mtw_Pateros_R_C<- NON_Interruptible %>% filter(site %in% Pateros_grids$site) %>% 
  dplyr::select(c("region","Year","crop","site","lat","long","simulation","Acres",
                  "streamflow_aug_mm","streamflow_aug_ft","streamflow_aug_acrft",
                  "Revenue_loss","cost_acrft","cost_acr","Total_Yield","WM_yield_loss","target_harvest_date","harvest_date1")) %>% 
  mutate(Site_Code= "METPA")

## Calculate marginal cost using custom function "marginal_cost_calculation" from "req_functions.R" and add gage location name
marginal_pateros<- marginal_cost_calculation(Mtw_Pateros_R_C) %>% 
  mutate(SITENAME= "METHOW RIVER NEAR PATEROS, WA")

## Filter data for "METHOW RIVER AT TWISP, WA" gage location
Mtw_Twisp_R_C<- NON_Interruptible %>% filter(site %in% MR_Twisp_grids$site) %>% 
  dplyr::select(c("region","Year","crop","site","lat","long","simulation","Acres",
                  "streamflow_aug_mm","streamflow_aug_ft","streamflow_aug_acrft",
                  "Revenue_loss","cost_acrft","cost_acr","Total_Yield","WM_yield_loss","target_harvest_date","harvest_date1")) %>% 
  mutate(Site_Code= "METTW")

## Calculate marginal cost using custom function "marginal_cost_calculation" from "req_functions.R" and add gage location name
marginal_twisp<- marginal_cost_calculation(Mtw_Twisp_R_C) %>% 
  mutate(SITENAME= "METHOW RIVER AT TWISP, WA")


## Filter data for "TWISP RIVER NEAR TWISP, WA" gage location
Twisp_Twisp_R_C<- NON_Interruptible%>% filter(site %in% TRN_Twisp_grids$site) %>% 
  dplyr::select(c("region","Year","crop","site","lat","long","simulation","Acres",
                  "streamflow_aug_mm","streamflow_aug_ft","streamflow_aug_acrft",
                  "Revenue_loss","cost_acrft","cost_acr","Total_Yield","WM_yield_loss","target_harvest_date","harvest_date1")) %>% 
  mutate(Site_Code= "TWITW")

## Calculate marginal cost using custom function "marginal_cost_calculation" from "req_functions.R" and add gage location name
marginal_twispntwisp<- marginal_cost_calculation(Twisp_Twisp_R_C) %>% 
  mutate(SITENAME= "TWISP RIVER NEAR TWISP, WA")

#### Combine all marginal cost data calculated for each station into a single dataframe ####
## List of marginal cost data for different sites
list_all_marginal_cost_data<- list(marginal_nfd,marginal_Detour,marginal_Touchet,marginal_Tonasket,marginal_malott,
                                   marginal_pateros,marginal_twisp,marginal_twispntwisp,marginal_similkameen)

## Combine all dataframes into one
combined_supply_curve_sites<- do.call(rbind,list_all_marginal_cost_data) %>% 
  # Renaming shutoff scenarios for better readability and naming in a convention which we used in the later part for analysis
  mutate(simulation = case_when(
    simulation == "Scenario01" ~ "May H1",
    simulation == "Scenario02" ~ "May H2",
    simulation == "Scenario03" ~ "Jun H1",
    simulation == "Scenario04" ~ "Jun H2",
    simulation == "Scenario05" ~ "Jul H1",
    simulation == "Scenario06" ~ "Jul H2",
    simulation == "Scenario07" ~ "Aug H1",
    simulation == "Scenario08" ~ "Aug H2"
    ))




### Create Supply Curve Figure 05
## 2015 taken as an example year
## Panel a (Okanogan River at Malott)
site_sp<- combined_supply_curve_sites %>% 
  filter(Site_Code=="OKAMA")  #OKAMA is the site code for Malott
site_sp1<- site_sp %>% 
  filter(Year==2015)  

### Filter data frame for specific timeframe curve
# May H2 
mayH2<- filter(site_sp1,simulation=="May H2")

# Jul H1 
julH1<- filter(site_sp1,simulation=="Jul H1")

# Aug H2
augH2<- filter(site_sp1,simulation=="Aug H2")

# Create Figure 5(a)
P1<- ggplot()+
  geom_line(mayH2 , mapping=aes(x=cum_streamflow , y=cost_acrft,color="May H2"),lwd= 1.5)+
  geom_line(julH1 , mapping=aes(x=cum_streamflow , y=cost_acrft,color="Jul H1"),lwd= 1.5)+
  geom_line(augH2 , mapping=aes(x=cum_streamflow , y=cost_acrft,color="Aug H2"),lwd= 1.5)+
  theme_bw()+ scale_y_continuous("Marginal cost ($/ac-ft)",limits=c(0,1000), breaks=seq(0,1000,200))+
  scale_x_continuous("Streamflow augmentation (ac-ft)",limits=c(0,1500), breaks=seq(0,1500,300))+
  scale_color_manual(values = c(
    "May H2" = "#1F78B4",  # Blue
    "Jul H1" = "#FB9A99",  # Dark Blue
    "Aug H2" = "#FF7F00"   # Black
  ))+       
  labs(x= "Streamflow augmentation (ac-ft)", y= "Marginal cost ($/ac-ft)", title = paste0("OKAMA (a)"))+theme_bw()+ 
  theme(plot.title = element_text(size = 15),axis.title = element_blank(),
        strip.text = element_text(size = 20,face="bold"),
        axis.text.x = element_text(size = 22,color = "black",angle = 0, hjust=0.5),
        axis.text.y = element_text(size = 22,color = "black",angle = 0, hjust=1),
        legend.text = element_text(size=16, face="bold", color="black"), legend.title = element_blank(),legend.position = "right")
#Save supply curve
ggsave(P1, file="Figure/Malott_supply_curve_2015.png", width = 20, height = 15,dpi = 300, units = "cm")

## Panel b (Methow River at Pateros)
site_sp<- combined_supply_curve_sites %>% 
  filter(Site_Code=="METPA") 
site_sp1<- site_sp %>% 
  filter(Year==2015) 

### Filter data frame for specific timeframe curve
# May H2
mayH2<- filter(site_sp1,simulation=="May H2")

# Jul H1
julH1<- filter(site_sp1,simulation=="Jul H1")

# Aug H2
augH2<- filter(site_sp1,simulation=="Aug H2")

# Create Figure 5(b)
P1<- ggplot()+
  geom_line(mayH2 , mapping=aes(x=cum_streamflow , y=cost_acrft,color="May H2"),lwd= 1.5)+
  geom_line(julH1 , mapping=aes(x=cum_streamflow , y=cost_acrft,color="Jul H1"),lwd= 1.5)+
  geom_line(augH2 , mapping=aes(x=cum_streamflow , y=cost_acrft,color="Aug H2"),lwd= 1.5)+
  theme_bw()+ scale_y_continuous("Marginal cost ($/ac-ft)",limits=c(0,1000), breaks=seq(0,1000,200))+
  scale_x_continuous("Streamflow augmentation (ac-ft)",limits=c(0,750), breaks=seq(0,750,150))+
  scale_color_manual(values = c(
    "May H2" = "#1F78B4",  # Blue
    "Jul H1" = "#FB9A99",  # Dark Blue
    "Aug H2" = "#FF7F00"   # Black
  ))+       
  labs(x= "Streamflow augmentation (ac-ft)", y= "Marginal cost ($/ac-ft)", title = paste0("METPA (b)"))+theme_bw()+ 
  theme(plot.title = element_text(size = 15),axis.title = element_blank(),
        strip.text = element_text(size = 20,face="bold"),
        axis.text.x = element_text(size = 22,color = "black",angle = 0, hjust=0.5),
        axis.text.y = element_text(size = 22,color = "black",angle = 0, hjust=1),
        legend.text = element_text(size=16, face="bold", color="black"), legend.title = element_blank(),legend.position = "right")
#Save figure
ggsave(P1, file="Figure/Pateros_supply_curve_2015.png", width = 20, height = 15,dpi = 300, units = "cm")


## Panel c (Touchet River at Bolles)
site_sp<- combined_supply_curve_sites %>% 
  filter(Site_Code=="TOUBO") 
site_sp1<- site_sp %>% 
  filter(Year==2015) 

###Filter data frame for specific timeframe curve
#May H2
mayH2<- filter(site_sp1,simulation=="May H2")

#Jul H1
julH1<- filter(site_sp1,simulation=="Jul H1")

#Aug H2
augH2<- filter(site_sp1,simulation=="Aug H2")

# Create Figure 5(c)
P1<- ggplot()+
  geom_line(mayH2 , mapping=aes(x=cum_streamflow , y=cost_acrft,color="May H2"),lwd= 1.5)+
  geom_line(julH1 , mapping=aes(x=cum_streamflow , y=cost_acrft,color="Jul H1"),lwd= 1.5)+
  geom_line(augH2 , mapping=aes(x=cum_streamflow , y=cost_acrft,color="Aug H2"),lwd= 1.5)+
  theme_bw()+ scale_y_continuous("Marginal cost ($/ac-ft)",limits=c(0,1000), breaks=seq(0,1000,200))+
  scale_x_continuous("Streamflow augmentation (ac-ft)",limits=c(0,800), breaks=seq(0,800,200))+
  scale_color_manual(values = c(
    "May H2" = "#1F78B4",  # Blue
    "Jul H1" = "#FB9A99",  # Dark Blue
    "Aug H2" = "#FF7F00"   # Black
  ))+       
  labs(x= "Streamflow augmentation (ac-ft)", y= "Marginal cost ($/ac-ft)", title = paste0("TOUBO (c)"))+theme_bw()+ 
  theme(plot.title = element_text(size = 15),axis.title = element_blank(),
        strip.text = element_text(size = 20,face="bold"),
        axis.text.x = element_text(size = 22,color = "black",angle = 0, hjust=0.5),
        axis.text.y = element_text(size = 22,color = "black",angle = 0, hjust=1),
        legend.text = element_text(size=16, face="bold", color="black"), legend.title = element_blank(),legend.position = "right")
#Save figure
ggsave(P1, file="Figure/TR_Bolles_supply_curve_2015.png", width = 20, height = 15,dpi = 300, units = "cm")



## Stack bar chart associateed with crop acreages information
######Creates acres data####

## Filter data for N.F. Touchet River above Dayton and process it for analysis
NFT_D_C<- NON_Interruptible %>% 
  # Filter for sites within the TR_Dayton_grids and ungroup the data
  filter(site %in% TR_Dayton_grids$site) %>% ungroup() %>% 
  # Select relevant columns: region, year, crop type, site, simulation, and acres
  dplyr::select(c("region","Year","crop","site","simulation","Acres")) %>% 
  # Group by region, crop type, simulation scenario, and site, and calculate mean Acres across multiple years as acres are same
  group_by(region,crop,simulation,site) %>% 
  summarise(Acres= mean(Acres, na.rm=T)) %>% 
  # Further group by region, crop, and simulation to get total Acres by summing across sites
  group_by(region,crop,simulation) %>% 
  summarise(Acres= sum(Acres, na.rm=T)) %>% 
  # Filter out any of the simulation scenario as acreages are same across simulation, here we filtered out "Scenario01"
  filter(simulation=="Scenario01") %>% 
  #Adding the unique gage station code 
  mutate(Site_Code= "NFTDA")

## Walla Walla R.  E. Detour Rd.
Detour_Rd_C<- NON_Interruptible  %>% filter(site %in% WR_Detour_Rd_grids$site) %>% 
  ungroup() %>% 
  dplyr::select(c("region","Year","crop","site","simulation","Acres")) %>% 
  group_by(region,crop,simulation,site) %>% 
  summarise(Acres= mean(Acres, na.rm=T)) %>% 
  group_by(region,crop,simulation) %>% 
  summarise(Acres= sum(Acres, na.rm=T)) %>% filter(simulation=="Scenario01") %>% 
  mutate(Site_Code= "WALDE")

## Touchet R. at Bolles
Touchet_R_C<- NON_Interruptible %>% filter(site %in% TR_Bolles_grids$site) %>%  
  ungroup() %>% 
  dplyr::select(c("region","Year","crop","site","simulation","Acres")) %>% 
  group_by(region,crop,simulation,site) %>% 
  summarise(Acres= mean(Acres, na.rm=T)) %>% 
  group_by(region,crop,simulation) %>% 
  summarise(Acres= sum(Acres, na.rm=T)) %>% filter(simulation=="Scenario01") %>% 
  mutate(Site_Code= "TOUBO")

## OKANOGAN RIVER NEAR TONASKET, WA
Okn_Tonasket_R_C<- NON_Interruptible  %>% filter(site %in% Tonasket_grids$site) %>% 
  ungroup() %>% 
  dplyr::select(c("region","Year","crop","site","simulation","Acres")) %>% 
  group_by(region,crop,simulation,site) %>% 
  summarise(Acres= mean(Acres, na.rm=T)) %>% 
  group_by(region,crop,simulation) %>% 
  summarise(Acres= sum(Acres, na.rm=T)) %>% filter(simulation=="Scenario01") %>% 
  mutate(Site_Code= "OKATO")

## SIMILKAMEEN RIVER NEAR NIGHTHAWK, WA
Okn_Similkameen_R_C<- NON_Interruptible  %>% filter(site %in% Similkameen_grids$site) %>% 
  ungroup() %>% 
  dplyr::select(c("region","Year","crop","site","simulation","Acres")) %>% 
  group_by(region,crop,simulation,site) %>% 
  summarise(Acres= mean(Acres, na.rm=T)) %>% 
  group_by(region,crop,simulation) %>% 
  summarise(Acres= sum(Acres, na.rm=T)) %>% filter(simulation=="Scenario01") %>% 
  mutate(Site_Code= "SIMNI")

## OKANOGAN RIVER AT MALOTT, WA
Okn_Malott_R_C<- NON_Interruptible %>% filter(site %in% Malott_grids$site) %>% 
  ungroup() %>% 
  dplyr::select(c("region","Year","crop","site","simulation","Acres")) %>% 
  group_by(region,crop,simulation,site) %>% 
  summarise(Acres= mean(Acres, na.rm=T)) %>% 
  group_by(region,crop,simulation) %>% 
  summarise(Acres= sum(Acres, na.rm=T)) %>% filter(simulation=="Scenario01") %>% 
  mutate(Site_Code= "OKAMA")


## METHOW RIVER NEAR PATEROS, WA
Mtw_Pateros_R_C<- NON_Interruptible %>% filter(site %in% Pateros_grids$site) %>% 
  ungroup() %>% 
  dplyr::select(c("region","Year","crop","site","simulation","Acres")) %>% 
  group_by(region,crop,simulation,site) %>% 
  summarise(Acres= mean(Acres, na.rm=T)) %>% 
  group_by(region,crop,simulation) %>% 
  summarise(Acres= sum(Acres, na.rm=T)) %>% filter(simulation=="Scenario01") %>% 
  mutate(Site_Code= "METPA")


## METHOW RIVER AT TWISP, WA
Mtw_Twisp_R_C<- NON_Interruptible %>% filter(site %in% MR_Twisp_grids$site) %>% 
  ungroup() %>% 
  dplyr::select(c("region","Year","crop","site","simulation","Acres")) %>% 
  group_by(region,crop,simulation,site) %>% 
  summarise(Acres= mean(Acres, na.rm=T)) %>% 
  group_by(region,crop,simulation) %>% 
  summarise(Acres= sum(Acres, na.rm=T)) %>% filter(simulation=="Scenario01") %>% 
  mutate(Site_Code= "METTW")

## TWISP RIVER NEAR TWISP, WA
Twisp_Twisp_R_C<- NON_Interruptible %>% filter(site %in% TRN_Twisp_grids$site) %>% 
  ungroup() %>% 
  dplyr::select(c("region","Year","crop","site","simulation","Acres")) %>% 
  group_by(region,crop,simulation,site) %>% 
  summarise(Acres= mean(Acres, na.rm=T)) %>% 
  group_by(region,crop,simulation) %>% 
  summarise(Acres= sum(Acres, na.rm=T)) %>% filter(simulation=="Scenario01") %>% 
  mutate(Site_Code= "TWITW")


### Combine all site-specific acreage data and categorize crop groups

# Create a list of data frames for each site's acreage
list_all_site_acres<- list(NFT_D_C,Detour_Rd_C,Touchet_R_C,Okn_Tonasket_R_C,Okn_Similkameen_R_C,
                           Okn_Malott_R_C,Mtw_Pateros_R_C,Mtw_Twisp_R_C,Twisp_Twisp_R_C)
# Define the crops "Onion" and "Corn_grain" as a listed category
listed_crop<- c("Onion","Corn_grain")

# Combine all data frames into a single dataset and categorize crops into groups
combined_sites_acres<- do.call(rbind,list_all_site_acres) %>% 
  # Categorizing crops into specific types: "Hay crops", "Grain crops", and "Corn and onion"
  mutate(group= ifelse(crop %in% bmass, "Hay crops", ifelse(!crop %in% listed_crop & crop %in% crp_y, "Grain crops", 
                                                            ifelse(crop %in% listed_crop, "Corn and onion",0))))


## Modifying dataset to create crop groups and summarize acreage information
# Grouping by site and crop group, summarizing acres, and calculating percentages
Acres_info<- combined_sites_acres %>% 
  group_by(Site_Code,group) %>% 
  summarise(Acres= sum(Acres, na.rm=T)) %>% 
  group_by(Site_Code) %>% 
  mutate(Total_ac= sum(Acres, na.rm=T),
         Peercentage= round(Acres/Total_ac*100,digits = 1),
         group1= factor(group,ordered = T, levels=c("Corn and onion","Grain crops","Hay crops")),
         Total_ac_v1= Acres/1000)


# Reshaping the dataset to a wider format for easier comparison of crop group acres across sites
Acres_info_1<- Acres_info %>% dplyr::select(c("Site_Code","group","Acres" )) %>% 
  # Convert 'group1' into columns for each crop group to create plot
  pivot_wider(names_from = group, values_from = Acres)


# Ensure the "Figure/stackbar" folder exists
if (!dir.exists("Figure/stackbar")) {
  dir.create("Figure/stackbar", recursive = TRUE)
}

# Get unique site names
sites<- unique(Acres_info$Site_Code)

# Loop through each site to generate and save figures
for ( i in 1: length(sites)){
  # Filter data for the current site
  count_yr<- filter(Acres_info, Site_Code==sites[i] ) 
  
  ## Create and save the first plot (stacked bar without labels)
  P1<- ggplot(count_yr, aes(x="", y=Peercentage, fill=group1)) +
    geom_bar(width = 0.5, stat = "identity") +  # Stacked bar chart
    scale_fill_manual(values = c("Hay crops" = "#C5C6C7",
                                 "Grain crops" = "#CBC1AE",
                                 "Corn and onion" = "#6e6f71")) +
    theme_bw() +
    labs(x=NULL, y=NULL, title=sites[i]) +  # Remove axis labels
    theme(plot.title = element_blank(),
          axis.text = element_blank(),  # Remove axis text
          axis.ticks = element_blank(), # Remove axis ticks
          panel.grid = element_blank(), # Remove grid lines
          panel.border = element_blank(), # Remove border
          legend.position = "top"
    ) +coord_flip()
  
  # Save the first plot
  ggsave(P1, file=paste0("Figure/stackbar/",sites[i],"_Percentage.png"), width = 20, height = 5,dpi = 300, units = "cm")
  
  ## Create and save the second plot (stacked bar with percentage labels)
  P1<- ggplot(count_yr, aes(x="", y=Peercentage, fill=group1)) +
    geom_bar(width = 0.5, stat = "identity") +  # Stacked bar chart
    scale_fill_manual(values = c("Hay crops" = "#C5C6C7",
                                 "Grain crops" = "#CBC1AE",
                                 "Corn and onion" = "#6e6f71")) +
    theme_bw() +
    labs(x=NULL, y=NULL, title=sites[i]) +  # Remove axis labels
    theme(plot.title = element_blank(),
          axis.text = element_blank(),  # Remove axis text
          axis.ticks = element_blank(), # Remove axis ticks
          panel.grid = element_blank(), # Remove grid lines
          panel.border = element_blank(), # Remove border
          legend.position = "top"
    ) +coord_flip()+geom_text(aes(label = Peercentage), position = position_stack(vjust = 0.5), size = 12, color="black")
  
  # Save the second plot
  ggsave(P1, file=paste0("Figure/stackbar/",sites[i],"_Percentage_withlabels.png"), width = 20, height = 5,dpi = 300, units = "cm")
  
}



