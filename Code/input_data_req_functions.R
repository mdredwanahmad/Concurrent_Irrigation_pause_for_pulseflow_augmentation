# Load necessary libraries to run all scripts
library(tidyverse)
library(data.table)
library(tidyr)
library(dplyr)
library(lubridate)
library(devtools)
library(RColorBrewer)
library(viridis)
library(reshape2)
library(readxl)
library(ggplot2)
library(stringi)
library(future)
library(purrr)
library(furrr)
library(dataRetrieval)

# Set the working directory (adjust this to your local directory)
# Ensure the data folder is placed inside the working directory
setwd("C:/Users/mdredwanahmad.khan/OneDrive - Washington State University (email.wsu.edu)/Coordinated Shutdown/Paper/data_code_figure/")


###### INPUT DATA & VECTORS FOR ANALYSIS  #######

##Grain and Hay Crops used in the simulation
# Define the list of grain crops used in the simulation
crp_y<- c("Corn_grain", "Spring_wheat", "Sweet_Corn", "Winter_wheat", "Alfalfa_Seed",  
          "Barley_Spring","Canola","Oats","Onion","Triticale","Yellow_Mustard")

# Define the list of hay crops used in the simulation
bmass<- c("Alfalfa_Hay","Barley_Hay", "Grass_Hay", "Sudangrass","Triticale_Hay",
          "Oats_hay", "Rye", "Timothy")


## Define selling units as vectors for each crop type based on NASS STATS #https://quickstats.nass.usda.gov/ 
bushels<-c("Barley_Spring","Corn_grain","Oats","Spring_wheat","Winter_wheat","Alfalfa_Seed","Triticale")
tons<- c("Alfalfa_Hay","Grass_Hay","Oats_hay","Timothy","Barley_Hay","Sudangrass","Triticale_Hay","Rye")
cwt<- c("Canola","Yellow_Mustard","Sweet_Corn","Onion")


# moisture content for each crop to convert dry matter CropSyst yield to wet matter
moisture_content<- read.csv("data_folder/input_data/Moisture_content.csv")
## Irrigation Efficiency  y irrigation type 
irrig_eff<- read.csv("data_folder/input_data/Irrigation efficiency.csv")


## Crop price data sourced from USDA NASS Quick Stats (quickstats.nass.usda.gov/#FFECA65E-FAAD-354D-B567-4FF1308F78BD)
#### The Optimal Price plot was created following the DR_15 analysis.
# The price for sweet corn has been adjusted to the mean from 2019-2022 (34.2, 51.3, 35.07, 45.46), yielding a value of 34.2. This data was obtained from the USDA Washington Outlook and recommended by Dr. Mike (PDF).
Crop_price <- fread("data_folder/input_data/crop_price.csv")
crop_price1 <- Crop_price %>% 
  dplyr::select(c("crop", "Mean_price_2020_2023", "Mean_Price_of_all_time_1979_2023")) %>% 
  rename("mean_Price" = "Mean_price_2020_2023", "mean_price_bef" = "Mean_Price_of_all_time_1979_2023")


## All simulated crop list in one vector, which has been used in calculation from daily file "see script daily_data_calc.R" 
crp_all<- c("Corn_grain", "Spring_wheat", "Sweet_Corn", "Winter_wheat", "Alfalfa_Seed",  
            "Barley_Spring","Canola","Oats","Onion","Triticale","Yellow_Mustard","Alfalfa_Hay","Barley_Hay", 
            "Grass_Hay", "Sudangrass","Triticale_Hay","Oats_hay", "Rye", "Timothy")

##Simulation end_date is required to summarized all shutoff scenarios based on auto irrigation resume so load scenario end date information
end_date<- read.csv("data_folder/input_data/Scenarios_start_end_date_inf.csv") %>% 
  filter(str_detect(simulation,"^Scenario"))



### Fifteendays shutoff simulation name and shutoff timeframe 
# define vectors
Scenario01<- c(121:135)
Scenario02<- c(136:150)
Scenario03<- c(151:164)
Scenario04<- c(165:179)
Scenario05<- c(180:194)
Scenario06<- c(195:209)
Scenario07<- c(210:224)
Scenario08<- c(225:239)



###Contributing grids
###Two WallaWalla gage stations
##These two station doesn't have any designated outlet in #CBCCSP_Station_Database (Columbia river basin otlets of all gage stations), and got contributing grids from station outlet latlong
#so that's need to be loaded from different data files
### Load Station Details
# Read station details from the CSV file, selecting relevant columns
station_details <- fread("data_folder/processed/contributing_grids/Stations_outlet.csv", fill = TRUE, nrows = Inf) %>%
  dplyr::select(c("SITENO", "SITENAME", "LONGDD", "LATDD", "VIC lat", "VIC long", "StationCode", "SITENAME1", "outlet"))

### Contributing Grids for WallaWalla Gage Stations
# Load contributing grid data, separate lat/long, and format for further processing
Contributing_grids <- fread("data_folder/processed/contributing_grids/cell_ids_PWL_all_v1", fill = TRUE, nrows = Inf) %>%
  mutate(Contributing_grids = Grid) %>% 
  separate(Grid, into = c("lat", "long"), sep = "_") %>%  # Split Grid column into latitude and longitude
  mutate(LAT = paste0(lat, "N"),                        # Append 'N' to latitude
         LONG = gsub("-", "", long),                    # Remove negative sign from longitude
         LONG = paste0(LONG, "W"),                      # Append 'W' to longitude
         site = paste0(LAT, LONG)) %>%                  # Create the site ID based on formatted lat/long
  left_join(station_details %>% dplyr::select("SITENO", "StationCode", "SITENAME1"))  # Join station details

# Filter for specific WallaWalla stations
WR_Detour_Rd_grids <- Contributing_grids %>% filter(SITENAME1 == "WR_Detour_Rd")  # Filter for WR_Detour_Rd station grids
TR_Dayton_grids <- Contributing_grids %>% filter(SITENAME1 == "TR_Dayton")        # Filter for TR_Dayton station grids


### Load Additional Station Details with Outlets which data have in CBCSP dataset
# Reload station details from an updated file to get stations with designated outlets
station_details <- fread("data_folder/processed/contributing_grids/Stations_outlet_v1.csv", fill = TRUE, nrows = Inf) %>%
  dplyr::select(c("SITENO", "SITENAME", "LONGDD", "LATDD", "VIC lat", "VIC long", "StationCode", "SITENAME1", "outlet"))

### Contributing Grids for Additional Stations
# Load contributing grid data for these stations, format and link with station details
Contributing_grids <- fread("data_folder/processed/contributing_grids/cell_ids_PWL_all_v2", fill = TRUE, nrows = Inf) %>%
  mutate(Contributing_grids = Grid) %>% 
  separate(Grid, into = c("lat", "long"), sep = "_") %>% 
  mutate(LAT = paste0(lat, "N"),
         LONG = gsub("-", "", long),
         LONG = paste0(LONG, "W"),
         site = paste0(LAT, LONG)) %>% 
  left_join(station_details %>% dplyr::select("SITENO", "StationCode", "SITENAME1"))

# Filter grids for specific stations
Malott_grids <- Contributing_grids %>% filter(SITENAME1 == "Malott")
Oroville_grids <- Contributing_grids %>% filter(SITENAME1 == "Oroville")
Similkameen_grids <- Contributing_grids %>% filter(SITENAME1 == "Similkameen")
Tonasket_grids <- Contributing_grids %>% filter(SITENAME1 == "Tonasket")
TRN_Twisp_grids <- Contributing_grids %>% filter(SITENAME1 == "TRN_Twisp")
MR_Twisp_grids <- Contributing_grids %>% filter(SITENAME1 == "MR_Twisp")
Pateros_grids <- Contributing_grids %>% filter(SITENAME1 == "Pateros")
TR_Bolles_grids <- Contributing_grids %>% filter(SITENAME1 == "TR_Bolles")

#write.csv(Malott_grids, "C:/Users/mdredwanahmad.khan/OneDrive - Washington State University (email.wsu.edu)/Coordinated Shutdown/Paper/data_code_figure/data_folder/Malott_grids.csv")

### Combine All Contributing Grids
# Create a combined list of all contributing grid sites for various stations
all_sites <- c(WR_Detour_Rd_grids$site, TR_Dayton_grids$site, Malott_grids$site, 
               Oroville_grids$site, Similkameen_grids$site, Tonasket_grids$site, 
               TR_Bolles_grids$site, Pateros_grids$site, MR_Twisp_grids$site, TRN_Twisp_grids$site)



#### streamflow_augmentation_potential_cost.R
## Load all the gauge stations list with original lat long and vic lat long
gauges_and_VIC<- read.csv("data_folder/processed/vic_grids_gauge/study_area_gauges_VIC.csv") %>% 
  dplyr::select(c("SITENO","SITENAME", "LONGDD", "LATDD","SITEURL","join_lat","join_lon")) %>% 
  # Convert latitude and longitude to character to prevent scientific notation
  mutate(vic_lat= as.character(join_lat),
         vic_long= as.character(join_lon))

# Fix SITENO values that were converted to scientific notation
gauges_and_VIC$SITENO[gauges_and_VIC$SITENO == "3.20E+51"] <- "32E050"

# Load the MIFR gauge names data
MIFR_gauges<- read.csv("data_folder/processed/vic_grids_gauge/MIFR_USGS_gauge.csv")
MIFR_gauges$SITENO[MIFR_gauges$SITENO == "3.20E+51"]<- "32E050" ##as it was converting into scientific



## Load Minimum Instream Flow Requirement (MIFR) data for each watershed

# Load and transform Methow Watershed MIFR data
methow <- read.csv("data_folder/raw/MIFR/Methow.csv")  # Load Methow dataset
methow <- melt(methow, id = c("Month", "Start_date", "End_date"))  # Reshape data to long format

# Load and transform Okanogan Watershed MIFR data
Okanogan <- read.csv("data_folder/raw/MIFR/Okanogan.csv")  # Load Okanogan dataset
Okanogan <- melt(Okanogan, id = c("Month", "Start_date", "End_date"))  # Reshape data to long format

# Load and transform Walla Walla Watershed MIFR data
WallaWalla <- read.csv("data_folder/raw/MIFR/WallaWalla.csv")  # Load Walla Walla dataset
WallaWalla <- melt(WallaWalla, id = c("Month", "Start_date", "End_date"))  # Reshape data to long format

## DOY and Month dataframe to convert MIFR 15 days values to daily values
DOY_Month<- read.csv("data_folder/input_data/Month_day_DOY.csv")


### Functions required for whole analysis #### 

# Function to read and process annual (seasonal) CropSyst output for each simulation scenario
read_data<- function(path_mn,folder){
  path<- path_mn 
  rest<- rep()
  for (t in 1:length (folder)){
    fld<- folder[[t]]
    
    # Read the result.dat file for each simulation
    result<- fread(paste0(path,fld,"/result.dat"), fill = TRUE, nrows = Inf) %>% 
      rename("Date" = "YYYY-MM-DD(DOY)") %>% 
      separate(Date, into= c("Year", "Month","Day"), sep="-") %>%
      mutate(simulation= fld) %>% 
      dplyr::select(c("Year", "crop","planting_date", "harvest_date", "yield", "used_biomass", "irrig", "region", "site","simulation"))
    
    # Separate annual crops and hay crops to make a common yield column, and calculate yield in kg/ha
    annual_crops<- filter(result, crop %in% crp_y)
    annual_crops$yield_kgha<- annual_crops$yield
    Hay_crops<- filter(result, crop %in% bmass)
    Hay_crops$yield_kgha<- Hay_crops$used_biomass
    
    # Merge hay crops and annual crops data
    result<- merge(Hay_crops,annual_crops, all.x = TRUE, all.y = TRUE,sort = TRUE)
    
    # Combine results from all folders
    data<- rbind(rest, result)
    rest<- data
  }
  return(rest)
}


# Function to process 15-day conversion for crop yield and irrigation analysis
Compute_irrigation_n_yield_loss<- function(data){
  data1<- data %>%
    # Select relevant columns for analysis from the input dataset
    dplyr::select(region, site, simulation,crop,Year, planting_date, harvest_date, yield, used_biomass, yield_kgha, 
                  WM_yield_kgha, irrig, Irrigation, irrig_multiplier, 
                  Crop_site_region) %>% 
    
    # Join the data with the optimal irrigation scenario dataset for yield loss and irrigation saved comparison and get the acres info
    left_join(optimal %>% 
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
           Revenue_loss = Total_economic_return_opt - Total_economic_return_shut) # Revenue loss
  
  return(data1) # Return the processed data
}


## Function to read large simulated daily data files if needed
read_data_daily <- function(path, folder) {
  rest <- rep()  # Initialize an empty vector to store the results
  
  # Loop through each folder to read the daily result data
  for (t in 1:length(folder)) {
    fld <- folder[[t]]  # Get the folder name for the current iteration
    
    # Read the daily result file for the current folder
    daily_result <- fread(paste0(path, fld, "/daily_result.dat"), fill = TRUE, nrows = Inf) %>%
      rename("irrig" = "Irrigation cum(timestep)",  # Rename irrigation column
             "DOY" = "DOY ") %>%                    # Rename DOY column
      dplyr::select(c("Year", "DOY", "irrig", "region", "crop", "site")) %>%  # Select relevant columns
      filter(DOY %in% c(55:241)) %>%                # Filter data assuming irrigation will happen within this timeframe
      filter(crop %in% crp_all) %>%                 # Filter based on simulated crops vectorized above
      filter(irrig > 0) %>%                         # Keep rows with irrigation data greater than 0
      mutate(simulation = fld) %>%                  # Add a simulation column for the current folder
      na.omit() %>%  
      # Join with crops_grids data for more detailed info# Remove any rows with NA values
      left_join(crops_grids %>% dplyr::select("crop", "site", "region", "Irrigation", "ML_Fraction", "WM_ML_factor")) %>% 
      mutate(irrig1 = irrig * ML_Fraction)          # Calculate adjusted irrigation using efficiency by irrigation type
    
    # Combine the result of the current folder with the previous results
    data1 <- rbind(rest, daily_result)
    rest <- data1  # Update the rest variable to store the combined data
  }
  
  return(rest)  # Return the final combined dataset
}





#####Function to findout irrigation saved due to shutoff to leave water to the stream
#calculation from optimal daily irrigation file
# Function to calculate irrigation saved during the shutoff periods for streamflow augmentation from optimal daily file
# Arguments:
# scenario: A vector of shutoff scenarios to analyze
# path: File path to save the output data (if needed)
Shutoff_irrig<- function(scenario, path){ #scenario- Shutoff Scenarios for figuring out
  yr<- rep()  # Initialize an empty vector to store yearly data
  
  # Loop through each shutoff scenario
  for (i in 1: length(scenario)) {
    # Get start and end dates for the current scenario
    sim_start<- end_date[end_date$simulation==scenario[i], "start_date"]%>% as.numeric()
    sim_end<- end_date[end_date$simulation==scenario[i], "stop_date"]%>% as.numeric()
  
    
    # Filter daily data between the start and end dates
    st_aug<- daily_data %>% 
      filter(DOY>=sim_start, DOY<=sim_end) %>% 
      mutate(simulation= scenario[i]) # Add the scenario to the data
    
    
    
    # Option to save each scenario data as CSV if required
    # Uncomment the next line to save the file locally for each scenario
    #write.csv(st_aug, paste0(path,scenario[i],".csv")) 
    
    # Combine current scenario data with previous iterations
    data<- rbind(yr,st_aug)
    yr<- data
    
  }
  
  # Option to save the combined data as CSV
  # Uncomment the next line if you want to save the entire combined file
  #write.csv(yr, paste0(path,"combined_scenario.csv")) 
  return(yr)
}


# Function to summarize data from field level (from multiple crops) to grid level
# Arguments:
# data1: The input data to be summarized

grid_level_accumulation <- function(data1) {
  calc_sum_grid <- data1 %>%
    group_by(region, site, lat, long, Grid_Number, Year, simulation) %>%  # Group data by location, grid, and year
    summarise(
      T_Acres = sum(Acres, na.rm = TRUE),  # Total acres for each group
      Streamflow_aug_vol_acft = sum(streamflow_aug_acrft, na.rm = TRUE),  # Sum of streamflow augmentation volume in acre-feet
      streamflow_aug_cfs = sum(streamflow_aug_cfs, na.rm = TRUE),  # Sum of streamflow augmentation in cubic feet per second
      WM_Yield_loss = sum(WM_yield_loss, na.rm = TRUE),  # Total weighted mean yield loss
      T_Revenue_loss = sum(Revenue_loss, na.rm = TRUE),  # Total revenue loss
      cost_per_acft_v = (T_Revenue_loss / Streamflow_aug_vol_acft)  # Calculate cost per acre-foot of streamflow augmentation
    )
  
  return(calc_sum_grid)  # Return the summarized data
}



# Function to download daily streamflow discharge data for specified gauge sites
# Arguments:
# siteinf: A list containing gauge site numbers and site names to download data for

discharge <- function(siteinf) { 
  SiteNO <- siteinf  # List of site numbers (gauges) to process
  res <- rep()  # Initialize an empty vector to store results
  
  # Loop through each site number in the list
  for (i in 1:length(SiteNO)) {
    
    # Fetch daily discharge data for the current site using readNWISdv function
    # Arguments:
    # siteNumbers: The gauge site number
    # parameterCd: "00060" is the parameter code for discharge (in cubic feet per second)
    # startDate: The start date for the data (January 1, 1979)
    # endDate: The end date for the data (December 31, 2022)
    data <- readNWISdv(siteNumbers = SiteNO[i], 
                       parameterCd = "00060",  # Discharge
                       startDate = "1979-01-01", 
                       endDate = "2022-12-31")
    
    # If no data is returned for the site, print a message and move to the next site
    if (nrow(data) == 0) {
      print(paste0("This site ", SiteNO[i], " provides an empty value"))
      next
    }
    
    # Process the data by adding Day of Year (DOY) and separating the date into Year, Month, and Day
    gaugeflow <- data %>%
      mutate(DOY = yday(Date)) %>%  # Add Day of Year (DOY) from the Date column
      separate(Date, into = c("Year", "Month", "Day"), sep = "-") %>%  # Separate the Date into Year, Month, and Day
      dplyr::select(site_no, Year, Month, Day, DOY, X_00060_00003) %>%  # Select relevant columns
      rename(Discharge_cfs = X_00060_00003) %>%  # Rename the discharge column for clarity
      lapply(as.numeric) %>%  # Convert all columns to numeric
      as.data.frame()  # Convert the result back into a data frame
    
    # Optional: Write the individual station data to CSV (uncomment if needed)
    # write.csv(gaugeflow, paste0("F:/Coordinated shutoff/Streamflow/", SiteNO[i], ".csv"))
    
    # Combine the current gauge flow data with the previous data
    gaugeflow1 <- rbind(res, gaugeflow)
    res <- gaugeflow1  # Update the result variable with the cumulative data
  }
  
  return(res)  # Return the final combined data for all gauge sites
}





# Function to calculate marginal cost for each year,timeframe, locations 

marginal_cost_calculation <- function(data) {
  dat <- rep()  # Initialize an empty vector to store results
  
  simulations <- unique(data$simulation)  # Get unique simulations from the data
  
  # Loop through each simulation in the dataset
  for (i in 1:length(simulations)) {
    
    # Filter data for the current simulation and adjust region names where necessary
    data1 <- data[data$simulation == simulations[i], ] %>%
      mutate(region = gsub("Naches", "Yakima", region))  # Replace 'Naches' with 'Yakima' for regions contributing flow into Yakima
    
    # Loop through each region for the current simulation
    region <- unique(data1$region)  # Get unique regions
    for (j in 1:length(region)) {
      data2 <- data1[data1$region == region[j], ]  # Filter data for the current region
      
      Yr <- unique(data2$Year)  # Get unique years for the region
      for (k in 1:length(Yr)) {
        data3 <- data2[data2$Year == Yr[k], ]  # Filter data for the current year
        
        # Sort data by cost per acre-foot (cost_acrft) and remove infinite/NA values
        data3 <- data3[order(data3$cost_acrft), ] %>%
          filter(!is.infinite(cost_acrft)) %>%  # Exclude rows with infinite costs
          na.omit()  # Remove rows with NA values
        
        # Calculate cumulative streamflow augmentation (in acre-feet) for the year
        data3$cum_streamflow <- cumsum(data3$streamflow_aug_acrft)  # Compute cumulative sum of augmented streamflow
        
        # Combine the processed data with the results from previous iterations
        dat <- rbind(dat, data3)
      }
    }
  }
  
  return(dat)  # Return the final dataset with marginal costs and cumulative streamflow
}


# Define a function to calculate streamflow augmentation for a given outlet
calculate_streamflow_aug_pot <- function(grid_data,cont_grid_list, site_no, site_code) {
  grid_data %>% 
    # Filter for contributing sites contributing to outlet
    filter(site %in% cont_grid_list) %>% 
    # Group data by region, year, and simulation scenario
    group_by(region, Year, simulation) %>% 
    summarise(# Sum augmented streamflow volume (acre-feet),
      Streamflow_aug_vol_acft = sum(Streamflow_aug_vol_acft, na.rm = TRUE),
      # Sum augmented streamflow in cubic feet per second
      streamflow_aug_cfs = sum(streamflow_aug_cfs, na.rm = TRUE)) %>% 
    # add unique sitename and sitecode for each station
    mutate(SITENO = site_no, Site_Code = site_code)
}
