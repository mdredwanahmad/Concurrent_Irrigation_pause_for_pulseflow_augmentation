
# Load the crop grid information which has been selected using filtering conditions mentioned in methodology for the simulation from WSDA 2020 CDL
# This crop grid information has been selected as Adam et al. 2022

selected_crops_grids_inf_fifteenmile<- read.csv("data_folder/input_data/fifteenmile_selected_crops_grids.csv") %>% 
  # renaming column name to make it consistent 
  rename("crop"="Crop_Name","WR_DOC_ID"="WR_DOC_ID..Dummy.","CropArea"="CropAcres") %>% 
  # Join with irrigation efficiency data, selecting relevant columns: Irrigation (irrigation type), Efficiency, and irrigation multiplier
  left_join(irrig_eff %>% dplyr::select("Irrigation","Efficiency","irrig_multiplier")) %>%
  # Join with moisture content data, selecting crop and yield multiplier
  left_join(moisture_content %>% dplyr::select("crop","yield_multiplier")) %>%
  # add watershed name as region
  mutate(region= "Fifteenmile") %>% 
  # Format latitude and longitude to 5 decimal places for consistency
  mutate(lat = formatC(lat, digits = 5, format = "f"),
         long = formatC(long, digits = 5, format = "f"))




### Load the optimal dataset with all crops, years, and grid combinations 
optimal_fifteenmile<- fread("data_folder/CropSyst/CropSyst_output/Fifteenmile_creek/optimal/result.dat", fill = TRUE, nrows = Inf) %>%
  # Rename the date column and split it into Year, Month, and Day
  rename("Date" = "YYYY-MM-DD(DOY)") %>% 
  separate(Date, into= c("Year", "Month","Day"), sep="-") %>%
  # Add a new column indicating this is the 'optimal' simulation
  mutate(simulation= "optimal")%>% 
  # Select relevant columns for further analysis
  dplyr::select(c("Year","planting_date","harvest_date", "yield", "used_biomass", "irrig", "region", "crop", "site","simulation"))

# CropSyst annual(seasonal) output contains grain crops yield in the 'yield' column
# and hay crops yield in the 'used_biomass' column (sum of all clippings).
# We need to combine both yield types into a single column for further calculations.

## Filter out grain crops and store the yield in a new column 'yield_kgha'
annual_crops<- filter(optimal_fifteenmile, crop %in% crp_y)
annual_crops$yield_kgha<- annual_crops$yield
## Filter out hay crops and store the used biomass (yield) in the new column 'yield_kgha'
Hay_crops<- filter(optimal_fifteenmile, crop %in% bmass)
Hay_crops$yield_kgha<- Hay_crops$used_biomass

## Combine both grain and hay crops into one dataframe
optimal_fifteenmile<- merge(Hay_crops,annual_crops, all.x = TRUE, all.y = TRUE,sort = TRUE) %>% 
  # Join with the 'selected_crops_grids_inf' dataframe to bring in irrigation type, efficiency multiplier, 
  # and yield multiplier for accounting irrigation efficiency and converting yield to wet matter
  left_join(selected_crops_grids_inf_fifteenmile %>% dplyr::select("crop", "lat", "long", "site", "Grid_Number", "region", "Acres",
                                                                   "Irrigation", "irrig_multiplier", "yield_multiplier", "WR_DOC_ID")) %>% 
  ## Add required columns for further analysis
  mutate(
    irrig = irrig * irrig_multiplier, # Account for irrigation efficiency by applying the multiplier
    WM_yield_kgha = yield_kgha * yield_multiplier, # Convert yield to wet matter yield using the multiplier
    Year = as.numeric(Year),  # Convert Year to numeric for proper calculations
    irrig_optimal_mm = irrig, # Create a copy of the optimal irrigation column for further analysis
    WM_yield_optimal_kgha = WM_yield_kgha,  # Create a copy of the optimal yield column for further analysis
    area_ha_opt = Acres * 0.405, # Convert acres to hectares for area calculations
    total_WM_yield_optimal_kg = WM_yield_kgha * area_ha_opt, # Calculate total wet matter yield in kg
    total_irrig_mm_opt = irrig * area_ha_opt, # Calculate total irrigation in mm for the field
    # Create a unique identifier for each crop-site-region-year combination
    Crop_site_region = paste0(crop, "_", site, "_", region, "_", Year),
    # Extract the harvest day of the year (DOY) from the harvest date for further calculations
    harvest_date = as.numeric(substr(harvest_date, 5, 7)),
    # Convert total yield to market units based on crop type using NASS STATS conversion factors
    sold_unit_opt = ifelse(crop %in% bushels, (total_WM_yield_optimal_kg / 25.40), # Convert kg to bushels
                           ifelse(crop %in% tons, (total_WM_yield_optimal_kg * 0.001), # Convert kg to tons
                                  ifelse(crop %in% cwt, (total_WM_yield_optimal_kg * 0.019), 0))) # Convert kg to hundredweight (cwt)
  ) %>%
  # Join with the crop price dataset to calculate economic returns
  left_join(crop_price1) %>%
  # Calculate total economic return from crop production under the optimal irrigation scenario
  mutate(Total_economic_return_opt = sold_unit_opt * mean_Price) %>%
  # Filter out specific crops not included in the analysis
  filter(!crop %in% c("Alfalfa_Seed", "Rye"))



##################################
##### Fifteen Days shutdown  #####
##################################

fifteendays_fifteenmile<- fread("data_folder/CropSyst/CropSyst_output/Fifteenmile_creek/DOY175_190/result.dat", fill = TRUE, nrows = Inf) %>%
  # Rename the date column and split it into Year, Month, and Day
  rename("Date" = "YYYY-MM-DD(DOY)") %>% 
  separate(Date, into= c("Year", "Month","Day"), sep="-") %>%
  # Add a new column indicating this is the 'optimal' simulation
  mutate(simulation= "DOY175_190")%>% 
  # Select relevant columns for further analysis
  dplyr::select(c("Year","planting_date","harvest_date", "yield", "used_biomass", "irrig", "region", "crop", "site","simulation"))

# Separate annual crops and hay crops to make a common yield column, and calculate yield in kg/ha
annual_crops<- filter(fifteendays_fifteenmile, crop %in% crp_y)
annual_crops$yield_kgha<- annual_crops$yield
Hay_crops<- filter(fifteendays_fifteenmile, crop %in% bmass)
Hay_crops$yield_kgha<- Hay_crops$used_biomass

# Merge hay crops and annual crops data
fifteendays_fifteenmile<- merge(Hay_crops,annual_crops, all.x = TRUE, all.y = TRUE,sort = TRUE) %>% 
  # Join with the 'selected_crops_grids_inf' dataframe to bring in irrigation type, efficiency multiplier, 
  # and yield multiplier for accounting irrigation efficiency and converting yield to wet matter
  left_join(selected_crops_grids_inf_fifteenmile %>% 
              dplyr::select("crop", "lat", "long", "site", "Grid_Number", "region", 
                            "Acres","Irrigation", "irrig_multiplier", "yield_multiplier", "WR_DOC_ID")) %>% 
  # Adjusting irrigation and yield by their respective multipliers to account irrigation efficiency based on irrigation types
  mutate(irrig = irrig * irrig_multiplier,
         WM_yield_kgha = yield_kgha * yield_multiplier,
         Year = as.numeric(Year),
         # Creating a unique identifier for each crop-site-region-year combination
         Crop_site_region = paste0(crop, "_", site, "_", region, "_", Year)) %>%
  # Filtering rows to include only combinations that exist in the optimal dataset
  filter(Crop_site_region %in% optimal_fifteenmile$Crop_site_region) %>% 
  # Select relevant columns for analysis from the input dataset
  dplyr::select(region, site, simulation,crop,Year, planting_date, harvest_date, yield, used_biomass, yield_kgha, 
                WM_yield_kgha, irrig, Irrigation, irrig_multiplier, 
                Crop_site_region) %>% 
  
  # Join the data with the optimal irrigation scenario dataset for yield loss and irrigation saved comparison and get the acres info
  left_join(optimal_fifteenmile %>% 
              dplyr::select(Year, region, crop, site, lat, long, Grid_Number, Acres, 
                            Crop_site_region, Irrigation, planting_date, irrig_optimal_mm, 
                            WM_yield_optimal_kgha, total_WM_yield_optimal_kg, sold_unit_opt, 
                            Total_economic_return_opt)) %>% 
  
  # Convert areas from acres to hectares and square feet for calculations
  mutate(area_ha = Acres * 0.405, # Convert acres to hectares
         area_ft2 = Acres * 43560, # Convert acres to square feet
         
         # Calculate irrigation demand for a 15-day irrigation shutoff period
         streamflow_aug_mm = irrig_optimal_mm - irrig, # Difference between optimal and actual irrigation
         streamflow_aug_mm = ifelse(streamflow_aug_mm < 1, 0, streamflow_aug_mm), # Values less than 1 are set to 0 to avoid small decimal discrepancies created from r decimal calculation
         
         # Convert irrigation from millimeters to feet and acre-feet
         streamflow_aug_ft = streamflow_aug_mm * 0.00328084, # Convert mm to feet
         streamflow_aug_acrft = streamflow_aug_mm * 0.00328084 * Acres, # Convert mm to acre-feet
         
         # Calculate streamflow augmentation in cubic feet per second (cfs) over 15 days
         streamflow_aug_cfs = (streamflow_aug_mm * 0.00328084 * area_ft2) / (3600 * 24 * 15), 
         
         # Calculate total yield (kg) and yield loss compared to the optimal scenario
         total_WM_yield_kg = WM_yield_kgha * area_ha, # Total yield in kg
         WM_yield_loss = total_WM_yield_optimal_kg - total_WM_yield_kg, # Yield loss compared to optimal scenario
         WM_yield_loss = ifelse(WM_yield_loss < 1, 0, WM_yield_loss), # Yield loss less than 1 is set to 0
         
         # Calculate percentages of irrigation used and saved
         Percentage_irrig = (irrig / irrig_optimal_mm) * 100, # Percentage of optimal irrigation used
         Per_irrig_saved = 100 - Percentage_irrig, # Percentage of optimal irrigation saved for streamflow augmentation
         
         # Calculate yield percentage and reduction compared to the optimal scenario
         Percentage_yield = (WM_yield_kgha / WM_yield_optimal_kgha) * 100, # Yield percentage relative to optimal
         Per_yield_reduction = 100 - Percentage_yield, # Yield reduction in percentage
         
         # Create a unique identifier for each crop-site-region-year combination
         Crop_site_region_Year = paste0(crop, "_", site, "_", region, "_", Year)) %>%
  
  # Convert total yield to selling units based on the crop type
  mutate(sold_unit_shut = ifelse(crop %in% bushels, (total_WM_yield_kg / 25.40), # Kg to bushels conversion
                                 ifelse(crop %in% tons, (total_WM_yield_kg * 0.001), # Kg to tons conversion
                                        ifelse(crop %in% cwt, (total_WM_yield_kg * 0.019), 0)))) %>% # Kg to hundredweight (cwt) conversion
  
  # Join with the crop price dataset to calculate economic returns
  left_join(crop_price1) %>% 
  
  # Calculate total economic return during the shutoff period and revenue loss compared to the optimal scenario
  mutate(Total_economic_return_shut = sold_unit_shut * mean_Price, # Economic return in shutoff scenario
         Revenue_loss = Total_economic_return_opt - Total_economic_return_shut,
         cost_acrft = (Revenue_loss/streamflow_aug_acrft),
         cost_acr = (Revenue_loss/Acres))

#####Calculating marginal cost########

##Create dataframe
#if any harvest date is a withing first five days of irrigation shutoof, farmers in reality will shuttoff irrigation automatically so to account these we are filtering out these rows which have harvest within first 5 days of shutoff
data_marginal_15mile<- fifteendays_fifteenmile %>%  #filter(site %in% Contributing_grids) %>% 
  dplyr::select(c("region","Year","crop","planting_date","harvest_date","site","lat","long","simulation","Acres",
                  "streamflow_aug_acrft","streamflow_aug_cfs","Percentage_irrig",
                  "Per_irrig_saved","Percentage_yield","Per_yield_reduction" ,"Revenue_loss","cost_acrft","cost_acr")) %>% 
  mutate(harvest_date=as.numeric(substr(harvest_date, 5,7))) %>% filter(harvest_date>180)


###Marginal cost calculation to create table 04 and figure 09
###Run the function 
marginal_cost_15mile<- marginal_cost_calculation(data_marginal_15mile)


##Calculate cost to create table
#https://www.rrnw.org/wp-content/uploads/2016Barter.pdf 
#2.5 cfs = (2.5*24*3600*15)/43560 = 74.38 ac-ft (option 1)
#1.39 cfs = (1.39*24*3600*15)/43560 = 41.36 ac-ft (option 2)
#total 74.38+41.36= 115.74 ac-ft (option 1+2)
data1<- marginal_cost_15mile %>% filter(Year==2015,cum_streamflow<=131)
cost<- 17.87*115.74/sum(data1$Acres) ##Need to adjust exact acres
print(cost)


###Create Supply Curve########### Figure 09
#Filter out supply curve data for 2015
data1<- marginal_cost_15mile %>% filter(Year %in% c(2015))
#Fiter out the amount required to meet in Fifteenmile creek in 2015
option2_1<- data1 %>% filter(cum_streamflow<131)
x21<- 115.74
y21<- max(option2_1$cost_acrft)


P1<- ggplot()+geom_line(data1 , mapping=aes(x=cum_streamflow , y=cost_acrft),color="#33A02C",size=1.5)+
  geom_point(option2_1,mapping=aes(x=x21, y=y21),color="#FF7F00", size=6)+
  theme_bw()+scale_y_continuous("Marginal cost ($/ac-ft)",limits=c(0,1000), breaks=seq(0,1000,200))+
  scale_x_continuous("Streamflow augmentation (ac-ft)",limits=c(0,800), breaks=seq(0,800,100))+
  labs(x= "Streamflow augmentation (ac-ft)", y= "Marginal cost ($/ac-ft)", title = paste0())+theme_bw()+ 
  theme(plot.title = element_text(size = 15),axis.title = element_text(size = 22,color = "black"), 
        strip.text = element_text(size = 20,face="bold"),
        axis.text.x = element_text(size = 22,color = "black",angle = 0, hjust=0.5),
        axis.text.y = element_text(size = 22,color = "black",angle = 0, hjust=1),
        legend.text = element_text(size=16, face="bold", color="black"), legend.title = element_blank(),legend.position = "right")

## Save figure
ggsave(P1, file="Figure/Fifteen_2015.png", width = 20, height = 15,dpi = 300, units = "cm")
