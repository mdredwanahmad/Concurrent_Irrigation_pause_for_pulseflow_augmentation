# library(tidyverse)
# library(data.table)
# library(tidyr)
# library(dplyr)
# library(lubridate)
# library(Metrics)
#########


# Load the crop grid information which has been selected using filtering conditions mentioned in methodology for the simulation from WSDA 2020 CDL
# This data includes information for the Yakima and Wenatchee watersheds too
# We will filter the data to focus only on Walla Walla, Okanogan, and Methow regions for our analysis.

selected_crops_grids_inf <- read.csv("data_folder/input_data/crops_grids_information_for_simulation.csv") %>% 
  # Join with irrigation efficiency data, selecting relevant columns: Irrigation (irrigation type), Efficiency, and irrigation multiplier
  left_join(irrig_eff %>% dplyr::select("Irrigation", "Efficiency", "irrig_multiplier")) %>% 
  
  # Join with moisture content data, selecting crop and yield multiplier
  left_join(moisture_content %>% dplyr::select("crop", "yield_multiplier")) %>% 
  
  # Format latitude and longitude to 5 decimal places for consistency
  mutate(lat = formatC(lat, digits = 5, format = "f"),
         long = formatC(long, digits = 5, format = "f")) %>% 
  
  # Filter the data to include only the regions of interest: Walla Walla, Okanogan, and Methow
  filter(region %in% c("WallaWalla", "Okanogan", "Methow"))

## look at the data
head(selected_crops_grids_inf)


### Load the optimal dataset with all crops, years, and grid combinations for WallaWalla, Okanogan, and Methow
optimal <- fread("data_folder/CropSyst/CropSyst_output/Washigton_watersheds/optimal/result.dat", fill = TRUE, nrows = Inf) %>%
  # Rename the date column and split it into Year, Month, and Day
  rename("Date" = "YYYY-MM-DD(DOY)") %>% 
  separate(Date, into = c("Year", "Month", "Day"), sep = "-") %>%
  # Add a new column indicating this is the 'optimal' simulation
  mutate(simulation = "optimal") %>%
  # Select relevant columns for further analysis
  dplyr::select(c("Year", "planting_date", "harvest_date", "yield", "used_biomass", "irrig", "region", "crop", "site", "simulation"))

# CropSyst annual(seasonal) output contains grain crops yield in the 'yield' column
# and hay crops yield in the 'used_biomass' column (sum of all clippings).
# We need to combine both yield types into a single column for further calculations.

## Filter out grain crops and store the yield in a new column 'yield_kgha'
annual_crops <- filter(optimal, crop %in% crp_y) 
annual_crops$yield_kgha <- annual_crops$yield

## Filter out hay crops and store the used biomass (yield) in the new column 'yield_kgha'
Hay_crops <- filter(optimal, crop %in% bmass) 
Hay_crops$yield_kgha <- Hay_crops$used_biomass

## Combine both grain and hay crops into one dataframe
optimal <- merge(Hay_crops, annual_crops, all.x = TRUE, all.y = TRUE, sort = TRUE) %>%
  # Join with the 'selected_crops_grids_inf' dataframe to bring in irrigation type, efficiency multiplier, 
  # and yield multiplier for accounting irrigation efficiency and converting yield to wet matter
  left_join(selected_crops_grids_inf %>% dplyr::select("crop", "lat", "long", "site", "Grid_Number", "region", "Acres",
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
  filter(!crop %in% c("Alfalfa_Seed", "Rye")) %>% 
  mutate(revenue_gain = Total_economic_return_opt/Acres)


##################################
##### Fifteen Days shutdown  #####
##################################

##Listing all fifteendays shutoff scenarios exist in the Fifteendays shutoff folder
filesfolder<- list.files("data_folder/CropSyst/CropSyst_output/Washigton_watersheds/Fifteendays_shutoff/")
# Specifying the path to load the dataset
path<- "data_folder/CropSyst/CropSyst_output/Washigton_watersheds/Fifteendays_shutoff/"

### Reading all annual data files for the 15 days shutdown scenarios
fifteendays_sc <- read_data(path, filesfolder) %>%
  # Joining with irrigation and yield multipliers from selected crops info
  left_join(selected_crops_grids_inf %>% dplyr::select("crop", "site", "region", "Irrigation", "irrig_multiplier", "yield_multiplier")) %>%
  # Adjusting irrigation and yield by their respective multipliers to account irrigation efficiency based on irrigation types
  mutate(irrig = irrig * irrig_multiplier,
         WM_yield_kgha = yield_kgha * yield_multiplier,
         Year = as.numeric(Year),
         # Creating a unique identifier for each crop-site-region-year combination
         Crop_site_region = paste0(crop, "_", site, "_", region, "_", Year)) %>%
  # Filtering rows to include only combinations that exist in the optimal dataset
  filter(Crop_site_region %in% optimal$Crop_site_region)



## Calculating percentage yield and irrigation loss, and weighting variables with the area
## The calculations are done using a custom function defined in "req_functions.R"
fifteendays_sc_t <- Compute_irrigation_n_yield_loss(fifteendays_sc)


## We can calculate irrigation demand for each shutdown period by subtracting the irrigation demand 
## from the optimal scenario and shutdown scenario using the annual CropSyst output file. 
## However, to consider coincidence of harvest date and shutoff periods we need irrigation frequencies and amount at daily steps, so that if any irrigation events happened within first five days of 
## we use daily data. The daily dataset is large, so we read the summarized version here.
irrigation_demand_calc_fm_daily<- read.csv("data_folder/processed/CropSyst/Processed_seasonal_fm_daily.csv")[2:8]

## Joining irrigation demand calculated from daily optimal irrigation values with the seasonal data
fifteendays_sc_t1 <- fifteendays_sc_t %>%   
  # Ensure that percentage yield values above 100 are capped at 100
  mutate(Percentage_yield = ifelse(Percentage_yield > 100, 100, Percentage_yield),
         # Revenue loss below 1 is set to 0 (less than 4% of total dataset)
         Revenue_loss = ifelse(Revenue_loss < 1, 0, Revenue_loss),
         # Recreating the unique crop-site-region identifier for consistency
         Crop_site_region = paste0(crop, "_", site, "_", region)) %>% 
  # Joining with the summarized irrigation demand data from daily output
  left_join(irrigation_demand_calc_fm_daily) %>% 
  # Extracting the harvest date in terms of Julian day (DOY)
  mutate(harvest_date1 = as.numeric(substr(harvest_date, 5, 7)),
         # Replacing NA irrigation demand values with 0 (explained below)
         irrigation_demand_mm = ifelse(is.na(irrigation_demand_mm), 0, irrigation_demand_mm),
         # Calculating streamflow augmentation in acre-feet
         streamflow_aug_acrft = irrigation_demand_mm * 0.00328084 * Acres,
         # Converting streamflow to cubic feet per second (cfs) for a 15-day period
         streamflow_aug_cfs = ((irrigation_demand_mm * 0.00328084 * area_ft2) / (3600 * 24 * 15)),
         # Calculating cost per acre-foot of water augmentation based on revenue loss
         Cost_acre_ft = (Revenue_loss / streamflow_aug_acrft),
         # Calculating cost per acre
         Cost_acre = (Revenue_loss / Acres)) 

#write.csv(fifteendays_sc_t1, "data_folder/processed/fifteendays_sc_t1.csv")

## Explanation of percentage yield adjustment:
# Some crops show slight percentage yield values higher than 100%, but they are a very small fraction (0.3% of the dataset),
# returning values like 100.001 (can be considered decimal discrepencies in r), which is still less than 101%. These values are adjusted back to 100 for consistency.

## Explanation for replacing NA with 0 in 'irrigation_demand_mm':
## NA values in the "irrigation_demand_mm" column have been replaced with zero. This column is derived from the daily summarized dataset "irrigation_demand1".
## In certain years, for specific crops and simulation timeframes, there were no irrigation events recorded. When filtering the daily dataset to include only irrigation events greater than zero, 
## some combinations were dropped, leading to NA values when joined with the seasonal dataset ("fifteendays_sc_t1"). 
## However, in cases where there is no irrigation during shutoff periods, the seasonal dataset returns a value of zero instead of NA, 
## so we replaced the NA values with zero to ensure consistency.




### Filter and prepare dataset for box plots to compare irrigation shutoff impact

# Filter the dataset for the sites of interest and clean up crop names for better readability
irrigation_shutoff_analysis <- fifteendays_sc_t1 %>% 
  filter(site %in% all_sites) %>%  # Filter dataset for specified sites
  mutate(
    # Standardize crop names for clarity
    crop = ifelse(crop == "Oats_hay", "Oats_Hay",
           ifelse(crop == "Barley_Spring", "Barley_Grain",
           ifelse(crop == "Spring_wheat", "Spring Wheat",
           ifelse(crop == "Winter_wheat", "Winter Wheat",
           ifelse(crop == "Corn_grain", "Corn Grain",
           ifelse(crop == "Alfalfa_Hay", "Alfalfa Hay", crop)))))),
    
    # Convert crop names to ordered factors for plotting
    crop = factor(crop, ordered = TRUE, 
                  levels = c("Grass_Hay", "Oats_Hay", "Timothy", "Barley_Hay", 
                             "Sudangrass", "Alfalfa Hay", "Triticale_Hay", "Rye", 
                             "Oats", "Canola", "Yellow_Mustard", "Spring Wheat", 
                             "Winter Wheat", "Triticale", "Sweet_Corn", 
                             "Barley_Grain", "Corn Grain", "Alfalfa_Seed", "Onion"))) %>% 
    
  # Exclude crops not required for this analysis
  filter(!crop %in% c("Timothy", "Rye", "Alfalfa_Seed")) %>% 
  
  # Calculate yield and revenue metrics per acre
  mutate(
    yield_loss_acre = WM_yield_loss / Acres,  # Yield loss per acre
    Production_loss_acre = (sold_unit_opt - sold_unit_shut) / Acres,  # Production loss based on sold units per acre
    Economic_return_ac = Total_economic_return_shut / Acres,  # Economic return per acre
    Revenue_loss_acre = Revenue_loss / Acres,  # Revenue loss per acre
    Production_acre = sold_unit_shut / Acres,  # Production per acre during the shutdown scenario

    # Renaming shutoff scenarios for better readability
    simulation_abb = case_when(
      simulation == "Scenario01" ~ "May H1",
      simulation == "Scenario02" ~ "May H2",
      simulation == "Scenario03" ~ "Jun H1",
      simulation == "Scenario04" ~ "Jun H2",
      simulation == "Scenario05" ~ "Jul H1",
      simulation == "Scenario06" ~ "Jul H2",
      simulation == "Scenario07" ~ "Aug H1",
      simulation == "Scenario08" ~ "Aug H2"
    ),
    
    # Convert shutoff scenario names to ordered factors for plotting
    simulation_abb = factor(simulation_abb, ordered = TRUE, 
                            levels = c("May H1", "May H2", "Jun H1", "Jun H2", "Jul H1", "Jul H2", "Aug H1", "Aug H2"))) %>% 

  # Filter out representative crops for boxplot comparison
  filter(crop %in% c("Alfalfa Hay", "Corn Grain", "Spring Wheat", "Onion"))

#write.csv(irrigation_shutoff_analysis,"processed/irrigation_shutoff_analysis.csv")




###Create Plots for Figure 04

# # Ensure the 'Figure' folder exists, create it if not present
if (!dir.exists("Figure")) {
dir.create("Figure", recursive = TRUE)
}

# Filter the dataset to exclude outliers for yield reduction, irrigation saved, and cost per acre
# We are removing any data points where:
# - Percent yield reduction is below 0% or above 40%
# - Cost per acre is considered an outlier if greater than 1000
#crop_sp_data<- irrigation_shutoff_analysis %>% filter(Per_irrig_saved>=0 & Per_irrig_saved<=40)
crop_sp_data<- irrigation_shutoff_analysis %>% filter(Per_yield_reduction>=0 & Per_yield_reduction<=40) 


# Calculate median values for key metrics (Cost per acre, Irrigation saved, Yield reduction, Streamflow augmentation)
# for each crop and shutoff scenario ('simulation_abb')
median_values <- crop_sp_data %>% 
  group_by(crop, simulation_abb) %>%
  summarize(Cost_acre = median(Cost_acre, na.rm = TRUE),
            Per_irrrig_saved= median(Per_irrig_saved, na.rm=T),
            Per_yield_reduction= median(Per_yield_reduction, na.rm=T), 
            streamflow_aug_mm = median(streamflow_aug_mm, na.rm=T))

### Create panel (a)
# Streamflow augmentation potential (in mm)
P1<- ggplot(crop_sp_data, aes(x=simulation_abb,y= streamflow_aug_mm,fill=simulation_abb))+ #streamflow_aug_mm
  geom_boxplot(outlier.shape = NA)+ facet_wrap(~crop, ncol = 4, nrow = 1)+
  stat_boxplot(geom='errorbar')+
  geom_line(median_values, mapping=aes(x = simulation_abb, y = streamflow_aug_mm, group=1), 
            size = 0.75)+
  scale_fill_brewer(palette= "Paired")+
  scale_y_continuous(" ",limits=c(0,250), breaks=seq(0,250, by=50))+ #there are no outliers above this range in irrigation demand
  labs(x= "", y= " ", title = paste0(" "))+theme_bw()+ 
  theme(plot.title = element_text(size = 20),axis.title = element_blank(), 
        legend.text = element_text(size=18), legend.title = element_blank(),
        axis.text.x = element_text(size = 22,color = "black", angle = 90, vjust = 0.5),
        #axis.text.x = element_blank(),
        axis.text.y = element_text(size = 22,color = "black",angle = 0, hjust=1),
        legend.position = "none",axis.text.y.right = element_blank(),
        strip.text = element_text(size=22,color = "black"))
##save figure to Figure folder
ggsave(P1, file="Figure/streamflow_aug_Per.png", width = 25, height = 12,dpi = 300, units = "cm")


### Create panel (b)
# Percentage yield reduction 
P1<- ggplot(crop_sp_data, aes(x=simulation_abb,y= Per_yield_reduction,fill=simulation_abb))+ 
  geom_boxplot(outlier.shape = NA)+ facet_wrap(~crop, ncol = 4, nrow = 1)+
  stat_boxplot(geom='errorbar')+
  geom_line(median_values, mapping=aes(x = simulation_abb, y = Per_yield_reduction, group=1), 
            size = 0.75)+
  scale_fill_brewer(palette= "Paired")+
  scale_y_continuous(" ",limits=c(0,40), breaks=seq(0,40, by=10))+ #there are no outliers above this range in irrigation demand
  labs(x= "", y= " ", title = paste0(" "))+theme_bw()+ 
  theme(plot.title = element_text(size = 20),axis.title = element_blank(), 
        legend.text = element_text(size=18), legend.title = element_blank(),
        axis.text.x = element_text(size = 22,color = "black", angle = 90, vjust = 0.5),
        #axis.text.x = element_blank(),
        axis.text.y = element_text(size = 22,color = "black",angle = 0, hjust=1),
        legend.position = "none",axis.text.y.right = element_blank(),
        strip.text = element_text(size=22,color = "black"))
##Save the plot
ggsave(P1, file="Figure/Per_yield_reduction_mm.png", width = 25, height = 12,dpi = 300, units = "cm")

### Create panel (c)
# - Cost per acre is considered an outlier if greater than 1000
crop_sp_data<- irrigation_shutoff_analysis %>% filter(Cost_acre<=1000) 

# Calculate median values for key metrics (Cost per acre, Irrigation saved, Yield reduction, Streamflow augmentation)
# for each crop and shutoff scenario ('simulation_abb')
median_values <- crop_sp_data %>% 
  group_by(crop, simulation_abb) %>%
  summarize(Cost_acre = median(Cost_acre, na.rm = TRUE),
            Per_irrrig_saved= median(Per_irrig_saved, na.rm=T),
            Per_yield_reduction= median(Per_yield_reduction, na.rm=T), 
            streamflow_aug_mm = median(streamflow_aug_mm, na.rm=T))

P1<- ggplot(crop_sp_data, aes(x=simulation_abb,y= Cost_acre,fill=simulation_abb))+ #Cost_acre
  geom_boxplot(outlier.shape = NA)+ facet_wrap(~crop, ncol = 4, nrow = 1)+
  stat_boxplot(geom='errorbar')+
  geom_line(median_values, mapping=aes(x = simulation_abb, y = Cost_acre, group=1), 
            size = 0.75)+
  scale_fill_brewer(palette= "Paired")+
  scale_y_continuous(" ",limits=c(0,1000), breaks=seq(0,1000, by=200))+ #there are no outliers above this range in irrigation demand
  labs(x= "", y= " ", title = paste0(" "))+theme_bw()+ 
  theme(plot.title = element_text(size = 20),axis.title = element_blank(), 
        legend.text = element_text(size=18), legend.title = element_blank(),
        axis.text.x = element_text(size = 22,color = "black", angle = 90, vjust = 0.5,hjust = 1),
        #axis.text.x = element_blank(),
        axis.text.y = element_text(size = 22,color = "black",angle = 0, hjust=1),
        legend.position = "none",axis.text.y.right = element_blank(),
        strip.text = element_text(size=22,color = "black"))
##Save the plot
ggsave(P1, file="Figure/Cost_acre.png", width = 25, height = 12,dpi = 300, units = "cm")


