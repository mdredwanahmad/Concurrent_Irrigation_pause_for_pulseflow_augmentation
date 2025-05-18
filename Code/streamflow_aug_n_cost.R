library(tidyverse)
library(data.table)
library(tidyr)
library(dplyr)
library(lubridate)
library(Metrics)
library(dataRetrieval)
##Ensure you are still in same working directory
getwd()

## Combine and process instream flow rules across multiple watersheds

# List of individual watershed MIFR datasets to be combined
instream <- list(methow, Okanogan, WallaWalla)  # List all watershed data frames

# Combine all MIFR datasets and join with additional information
instreamflow_rules <- do.call(rbind, instream) %>%  # Combine datasets by rows
  rename("MIFR_SITE_NAME" = "variable", "MIFR_rules" = "value") %>%  # Rename columns for clarity
  left_join(MIFR_gauges, by = c("MIFR_SITE_NAME" = "MIFR_NAME")) %>%  # Join with gauge information
  
  # Process 'MIFR_rules' column and add conditions
  mutate(Condition = ifelse(grepl("(Closure)", MIFR_rules), "Closure", "Open"),  # Identify if rule is 'Closure' or 'Open'
         MIFR_rules = str_replace(MIFR_rules, " \\s*\\([^\\)]+\\)", ""),  # Remove text within parentheses
         MIFR_rules = gsub(',', "", MIFR_rules),  # Remove commas from numeric values
         MIFR_rules = as.numeric(MIFR_rules)) %>%  # Convert to numeric
  
  # Join with Day of Year (DOY) data and filter specific period
  left_join(DOY_Month) %>%  # Join with DOY information
  filter(DOY %in% c(120:240))  # Filter for DOY within specified range (approx. early May to late August)



### Streamflow Data Retrieval and Preparation

# Streamflow discharge data can be downloaded using a custom function in the "req_functions.R" script.
# However, for convenience and readability, we have pre-downloaded and saved this data into a CSV file.
# Note: Alternatively, you can access real-time USGS streamflow data directly using the "dataRetrieval" package but Department of Ecology dataset need to be downloaded from their website

# Load pre-downloaded streamflow data from CSV
streamflow_data<- read.csv("data_folder/raw/Streamflow_data/USGS_DE_Streamflow.csv")

# Correct scientific notation conversion in site_no column
# Change entries where 'site_no' was incorrectly read as "3.20E+51" back to the intended value "32E050"
streamflow_data$SITENO[streamflow_data$SITENO == "3.20E+51"] <- "32E050"
streamflow_data$site_no[streamflow_data$site_no == "3.20E+51"]<- "32E050" 



## Calculating streamflow augmentation and revenue loss from field level (from multiple crops and fields) to grid lavel for streamflow augmentation potential analysis
## The calculations are done using a custom function "grid_level_analysis" defined in "input_param_req_functions.R"
# "NON_Interruptible" dataset has been calculated in "supply_curve.R"
grid_non_interruptible<- grid_level_accumulation(NON_Interruptible)

# write the data back to processed folder for future use
#write.csv(grid_non_interruptible,"data_folder/processed/grid_non_interruptible.csv")


#### Calculate Streamflow at Targeted Outlet Locations for Each Station

# Streamflow calculations are based on contributing grid information from the 'input_param_req_functions.R' script.
# Here, we calculate the total augmented streamflow volume (in acre-feet) and augmented streamflow (in cfs) 
# for the NFT_Dayton location, aggregating by region, year, and simulation scenario.

## Calculate streamflow augmentation for the "N.F. Touchet R. abv Dayton" outlet
# Using a custom function from "input_param_req_functions.R" that requires grid-level streamflow augmentation potential,
# contributing grids, site number (SITENO), and site code (Site_Code).
NFTDA_downstream_calc <- calculate_streamflow_aug_pot(grid_non_interruptible, TR_Dayton_grids$site, "32E050", "NFTDA")

##WallaWalla Outlet
## Calculate streamflow augmentation for the "Walla Walla R. E. Detour Rd." outlet
WALDE_downstream_calc <- calculate_streamflow_aug_pot(grid_non_interruptible, WR_Detour_Rd_grids$site, "32A100", "WALDE")


## Calculate streamflow augmentation for the "Touchet R. at Bolles" outlet
TUOBO_downstream_calc <- calculate_streamflow_aug_pot(grid_non_interruptible, TR_Bolles_grids$site, "32B100", "TOUBO")

## Calculate streamflow augmentation for the "OKANOGAN RIVER NEAR TONASKET, WA" outlet
OKATO_downstream_calc <- calculate_streamflow_aug_pot(grid_non_interruptible, Tonasket_grids$site, "12445000", "OKATO")

## Calculate streamflow augmentation for the "SIMILKAMEEN RIVER NEAR NIGHTHAWK, WA" outlet
SIMNI_downstream_calc <- calculate_streamflow_aug_pot(grid_non_interruptible, Similkameen_grids$site, "12442500", "SIMNI")


## Calculate streamflow augmentation for the "OKANOGAN RIVER AT MALOTT, WA" outlet
#okanogan Outlet
OKAMA_downstream_calc <- calculate_streamflow_aug_pot(grid_non_interruptible, Malott_grids$site, "12447200", "OKAMA")

## Methow Outlet
## Calculate streamflow augmentation for the "METHOW RIVER NEAR PATEROS, WA" outlet
METPA_downstream_calc <- calculate_streamflow_aug_pot(grid_non_interruptible, Pateros_grids$site, "12449950", "METPA")

## Calculate streamflow augmentation for the "METHOW RIVER AT TWISP, WA" outlet
METTW_downstream_calc <- calculate_streamflow_aug_pot(grid_non_interruptible, MR_Twisp_grids$site, "12449500", "METTW")

## Calculate streamflow augmentation for the "TWISP RIVER NEAR TWISP, WA" outlet
TWITW_downstream_calc <- calculate_streamflow_aug_pot(grid_non_interruptible, TRN_Twisp_grids$site, "12448998", "TWITW")



## Combine all downstream calculations for further analysis
# Create a list to hold all calculated downstream flow data for various sites
list_all_flow<- list(NFTDA_downstream_calc,WALDE_downstream_calc,TUOBO_downstream_calc,
                     OKATO_downstream_calc,SIMNI_downstream_calc,OKAMA_downstream_calc,
                     METPA_downstream_calc,METTW_downstream_calc,TWITW_downstream_calc)

# Combine all individual downstream flow data into a single data frame for further analysis
combine_down_contr_flow<- do.call(rbind,list_all_flow)

####### FIFTEEN DAYS SHUTOFF SCENARIOS CALCULATIONS #######

# Merge combined downstream flow data with gauge station information for additional attributes
streamflow_aug_pot_gauge<- inner_join(combine_down_contr_flow,gauges_and_VIC, by=c("SITENO"))

## Creating group columns based on simulation dates for streamflow data
# Filtering streamflow data to focus on day-of-year (DOY) 121 to 240 for each station 
Stream_group<- streamflow_data[streamflow_data$DOY %in% (121:240),] %>% 
  # Categorize each row into a scenario group based on the DOY value so that we can add streamflow augmentation potentail from shutoff scenarios
  # Multiple scenarios (01 to 08) represent different subsets of irrigation shutoff scenarios allowing comparison of simulated timeframes
  mutate(simulation= ifelse(DOY %in% Scenario01,"Scenario01",
                            ifelse(DOY %in% Scenario02,"Scenario02",
                                   ifelse(DOY %in% Scenario03,"Scenario03",
                                          ifelse(DOY %in% Scenario04,"Scenario04",
                                                 ifelse(DOY %in% Scenario05,"Scenario05",
                                                        ifelse(DOY %in% Scenario06,"Scenario06",
                                                               ifelse(DOY %in% Scenario07,"Scenario07",
                                                                      ifelse(DOY %in% Scenario08,"Scenario08",0)))))))))




### Join the grouped streamflow data with gauges vic grids information and join with instream flow rules
Stream_flow_gauges<- left_join(Stream_group,gauges_and_VIC, by=c("site_no"="SITENO")) %>% 
  # Ensure site_no is a character to match the instreamflow_rules dataset
  mutate(site_no=as.character(site_no)) %>% 
  # Perform an inner join with instream flow rules, matching on both site_no and DOY
  inner_join(instreamflow_rules, by=c("site_no"="SITENO","DOY")) %>% na.omit()



### Join Calculated streamfow data with gage locations to create plots and further calculations
streamflow_aug_pot_gauge1<- inner_join(
  # Select relevant columns from streamflow_aug_pot_gauge for augmentation data
  streamflow_aug_pot_gauge %>% dplyr::select(c("region","Year","simulation","SITENO","SITENAME","Site_Code","streamflow_aug_cfs","LONGDD","LATDD")),
  # Join with Stream_flow_gauges to bring in discharge data and gauge information
  Stream_flow_gauges, by=c("SITENAME","SITENO","Year","simulation")) %>% 
  # Calculate total contributing flow by adding augmented streamflow with base discharge
  mutate(Contributing_flow= (streamflow_aug_cfs+Discharge_cfs))


## Mininimum instreamflow requirement (MIFR) complience and calculation to figure streamflow augmentation required
## Augmentation required is when streamflow discharge is lower than MIFR
## Calculate MIFR (Minimum Instream Flow Requirement) Compliance and Required Modifications
MIFR_ComplianceCalc<- streamflow_aug_pot_gauge1 %>% 
  # Determine if MIFR can be met based on current discharge and contributing flow
  mutate(Can_meet_MIFR=ifelse(Discharge_cfs < MIFR_rules & Contributing_flow > MIFR_rules, "YES", "NO")) %>% 
  # Select relevant columns for the analysis
  dplyr::select(c("region","SITENAME","Site_Code","DOY","Year","simulation","streamflow_aug_cfs","Discharge_cfs",
                  "MIFR_rules", "Contributing_flow","Can_meet_MIFR")) %>% 
  # Indicate if augmentation is needed to meet MIFR
  mutate(Aug_Need= ifelse(Discharge_cfs < MIFR_rules, "Yes_Need","Not_Need"),
         # Calculate difference between MIFR and actual discharge
         MIFR_Dff= (MIFR_rules-Discharge_cfs), 
         # Set negative differences (i.e., when discharge exceeds MIFR) to zero
         MIFR_Dff1= ifelse(MIFR_Dff<=0,0,MIFR_Dff), 
         MIFR_R_75= MIFR_rules*0.75, #75% of the MIFR
         MIFR_R_50= MIFR_rules*0.50, #50% of the MIFR
         # Determine if 75% or 50% of MIFR can be met with the current contributing flow
         Can_meet_MIFR_75= ifelse(Discharge_cfs < MIFR_rules & Discharge_cfs < MIFR_R_75 & Contributing_flow > MIFR_R_75, "YES", "NO"),
         Can_meet_MIFR_50 = ifelse(Discharge_cfs < MIFR_rules & Discharge_cfs < MIFR_R_75 & Discharge_cfs < MIFR_R_50 & Contributing_flow > MIFR_R_50, "YES", "NO"),
         # Calculate the amount of augmentation required to meet MIFR
         Aug_2_mt_MIFR= ifelse(MIFR_rules>Discharge_cfs,MIFR_rules-Discharge_cfs,0), #Amount required to meet MIFR
         # Calculate percentage of required augmentation achieved
         Percentage_aug= ifelse(Contributing_flow>=MIFR_rules,100,(streamflow_aug_cfs/MIFR_Dff)*100),
         # Percentage of required augmentation met when MIFR cannot be met by maximum streamflow augmentation potentails
         per_aug_not_meet_MIFR= ifelse(Can_meet_MIFR=="NO",Percentage_aug,0))


## Filter Out Simulations Requiring Augmentation (at least one day in any specific timeframe and years)
AugmentationRequiredSimulations<- MIFR_ComplianceCalc %>% 
  group_by(Site_Code) %>% 
  mutate(Analyzed_yrs= (max(Year)-min(Year)+1)) %>% 
  # Group by site, year, and simulation for targeted analysis
  group_by(Site_Code,Year,simulation) %>% 
  # Filter simulations that require at least one day of streamflow augmentation in any year, site, and timeframe
  filter(any(Aug_Need=="Yes_Need"))



####Filterout two consecutive days streamflow required 
###Filter out stations and simulation which augmented streamflow for atleast for two consecutive days
#Step 1: filter out all consecutive days

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
      site111<- site11 %>% filter(simulation==simulations[k],Aug_Need=="Yes_Need") %>% arrange(DOY) 
      
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
Consecutive_2_days_req <- do.call(rbind, result_list_v1)

# Remove any duplicate rows that may exist
Consecutive_2_days_req <- Consecutive_2_days_req[!duplicated(Consecutive_2_days_req), ] 

#### Step 2: Calculate Required Augmentation (Req_aug) based on MIFR deficit ####

Req_aug <- Consecutive_2_days_req %>%
  # Group by site name, year, and simulation to perform calculations per unique scenario
  group_by(SITENAME, Year, simulation) %>%
  # Arrange by Day of Year (DOY) to ensure sequential order
  arrange(DOY) %>%
  # Calculate a moving sum of the MIFR deficit (MIFR_Dff) over two consecutive days
  mutate(MovingSum_MIFRdef = (MIFR_Dff + lag(MIFR_Dff, 1)),
         
         # Calculate the minimum moving sum for each group
         MinMovingSum = min(MovingSum_MIFRdef, na.rm = TRUE),
         
         # For the rows with the minimum moving sum, calculate the maximum of two consecutive values
         MaxOfTwo = if_else(MovingSum_MIFRdef == MinMovingSum, pmax(MIFR_Dff, lag(MIFR_Dff)), NA_real_)
  ) %>%
  # Remove rows with NA values generated by lag function
  na.omit()

#### Step 3: Filter Consecutive Days for Minimum Moving Sum Requirement ####

Req_aug1 <- Consecutive_2_days_req %>%
  group_by(SITENAME, Year, simulation) %>%
  arrange(DOY) %>%
  mutate(
    # Calculate DOY difference to identify consecutive days
    DOY_diff = DOY - lag(DOY, 1),
    
    # Compute the moving sum for MIFR deficit for consecutive DOYs (DOY_diff == 1)
    MovingSum_MIFRdef = if_else(DOY_diff == 1, (MIFR_Dff + lag(MIFR_Dff, 1)), NA_real_),
    
    # Calculate the minimum moving sum for the moving sum values
    MinMovingSum = min(MovingSum_MIFRdef, na.rm = TRUE),
    
    # For rows where moving sum equals minimum, keep the maximum MIFR deficit value in the pair
    MaxOfTwo = if_else(MovingSum_MIFRdef == MinMovingSum, pmax(MIFR_Dff, lag(MIFR_Dff)), NA_real_)
  ) %>%
  # Omit NA values from the dataset
  na.omit()

#### Step 4: Extract Unique Required Augmentation (Req_aug) Values ####

Req_aug_distinct <- Req_aug1 %>%
  # Summarize by taking the maximum MaxOfTwo value within each group
  group_by(SITENAME, Year, simulation) %>%
  summarize(MaxOfTwo = max(MaxOfTwo, na.rm = TRUE)) %>%
  ungroup()

#### Step 5: Join Calculated Requirements with Original Consecutive Days Data ####
Consecutive_2_days_req_V2 <- Consecutive_2_days_req %>%
  # Perform a left join to add calculated values to the main dataset
  left_join(Req_aug_distinct, by = c("SITENAME", "Year", "simulation")) %>% 
  # Add required augmentation (Req_aug), convert to acre-feet, and calculate 50% augmentation
  mutate(
    Req_aug = MaxOfTwo,
    Req_aug_Acft = ((Req_aug * 3600 * 24 * 15) / 43560), # Convert from cubic feet per second to acre-feet
    Fiftyp_aug = Req_aug_Acft * 0.5,  # Calculate 50% of the required augmentation
    simulation = case_when(
      simulation == "Scenario01" ~ "May H1",
      simulation == "Scenario02" ~ "May H2",
      simulation == "Scenario03" ~ "Jun H1",
      simulation == "Scenario04" ~ "Jun H2",
      simulation == "Scenario05" ~ "Jul H1",
      simulation == "Scenario06" ~ "Jul H2",
      simulation == "Scenario07" ~ "Aug H1",
      simulation == "Scenario08" ~ "Aug H2"
    ))


#### Calculate Ratio of Potential Augmentation and Required Augmentation ####

Calc_ratio <- Consecutive_2_days_req %>%
  # Group data by site code, year, and simulation to calculate ratios for each scenario
  group_by(Site_Code, Year, simulation) %>%
  
  # Arrange by DOY to ensure sequential order for consecutive day calculations
  arrange(DOY) %>%
  
  # Step 1: Identify consecutive days and calculate augmentation requirements
  mutate(
    # Calculate the difference in DOY to identify consecutive days
    DOY_diff = DOY - lag(DOY, 1),
    
    # Calculate the moving sum of MIFR deficit for consecutive days (DOY_diff == 1)
    MovingSum_MIFRdef = if_else(DOY_diff == 1, (MIFR_Dff + lag(MIFR_Dff, 1)), NA_real_),
    
    # Calculate the minimum moving sum for each group
    MinMovingSum = min(MovingSum_MIFRdef, na.rm = TRUE),
    
    # For rows where moving sum equals the minimum, take the maximum of the two MIFR deficit values
    MaxOfTwo = if_else(MovingSum_MIFRdef == MinMovingSum, pmax(MIFR_Dff, lag(MIFR_Dff)), NA_real_)
  ) %>%
  
  # Remove rows with NA values from lag function calculations
  na.omit() %>%
  
  # Filter to keep only rows with the maximum MIFR deficit (MaxOfTwo) for each group
  filter(MaxOfTwo == max(MaxOfTwo)) %>%
  
  # Calculate the ratio of potential streamflow augmentation to required MIFR deficit
  mutate(
    RelativeRatioOfAug = streamflow_aug_cfs / MIFR_Dff,
    
    # Adjust ratio values greater than 1 to cap at 1
    RelativeRatioOfAug_adj = ifelse(RelativeRatioOfAug > 1, 1, RelativeRatioOfAug),
    
    # Add a simplified label for each simulation scenario
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
    
    # Set order for the simulation labels to improve visualization
    simulation_abb = factor(simulation_abb, ordered = TRUE, levels = c("May H1","May H2","Jun H1","Jun H2","Jul H1","Jul H2","Aug H1","Aug H2")),
    
    # Set order for Site_Code to improve plot consistency
    Site_Code = factor(Site_Code, ordered = TRUE,levels = c("METPA","OKAMA","WALDE","METTW","OKATO","TOUBO","TWITW","SIMNI","NFTDA"))
  )


#### create plot to calculate ratio of potential maximum augmentation and and need
P1<- ggplot(Calc_ratio, aes(x=simulation_abb,y= RelativeRatioOfAug_adj,fill=simulation))+ #Cost_acre
  geom_hline(yintercept = c(0.25,0.5,0.75), linetype="dashed", color="red")+
  geom_boxplot(outlier.shape = NA)+ facet_wrap(~Site_Code)+ #outlier.shape = NA
  stat_boxplot(geom='errorbar')+
  scale_fill_brewer(palette= "Paired")+
  scale_y_continuous(" ",limits=c(0,1), breaks=seq(0,1, by=0.25))+ 
  geom_hline(yintercept = c(0.5,0.75), linetype="dashed", color="red")+
  labs(x= "", y= " ", title = paste0(" "))+theme_bw()+ 
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 20),axis.title = element_blank(), 
        legend.text = element_text(size=18), legend.title = element_blank(),
        axis.text.x = element_text(size = 18,angle = 90,hjust = 1, vjust=0.5,color="black"),
        axis.text.y = element_blank(), # Remove y-axis text from all facets
        axis.text.y.left = element_text(size = 20, color = "black"),
        legend.position = "none",axis.text.y.right = element_blank(),
        strip.text = element_text(size=20,color = "black"))
##Save figure
ggsave(P1, file="Figure/RatioOfAugmentationNeedVsAugreq_with_outlier.png", width = 30, height = 20,dpi = 300, units = "cm")




### Create Figure 08

# Filter out rows where the harvest date is higher than the targeted harvest date.
# This ensures that any records with harvest dates that do not meet the target
# (i.e., where harvest_date1 is not greater than target_harvest_date) are excluded from the dataset.
# Note: If any targeted harvest date is higher than the actual harvest date, these should have been filtered
# out earlier during marginal cost calculation in "supply_curve.R" script, but this step serves as an additional check to remove any
# remaining non-compliant records.
combined_supply_curve_sites_v1<- combined_supply_curve_sites %>% filter(harvest_date1>target_harvest_date)


### Calculate Marginal and Required Costs for Streamflow Augmentation
### The loop is designed to calculate costs for different years, locations, and policy scenarios.
# Get unique site names
sites<- unique(combined_supply_curve_sites_v1$Site_Code)
# Get unique site names
Combined_sites_2Days<- rep()

# Loop through each site
for (h in 1:length(sites)){
  # Filter data for the current site
  site1<- Consecutive_2_days_req_V2 %>% filter(Site_Code== sites[h])
  # Get unique years for the current site
  Yr<- unique(site1$Year)
  
  # Loop through each year
  for ( i in 1: length(Yr)) {
    # Filter data for the current year
    site11<- filter(site1, Year==Yr[i])
    # Filter supply curve data for the current site and year from "supply_curve.R" script
    mc_area1<- filter(combined_supply_curve_sites_v1,Site_Code== sites[h],Year==Yr[i])
    # Get unique policy scenarios (simulations) for the current site and year
    scn<- unique(site11$simulation)
    
    # Loop through each simulation scenario
    for ( j in 1:length(scn)){
      # Filter data for the current simulation
      un_data<- filter(site11,simulation==scn[j]) 
      # Get the marginal cost data for the current scenario, renaming a column for consistency
      mc_scn<- mc_area1[mc_area1$simulation==scn[j],] %>% rename("cost_acft"="cost_acrft")
      # Select the maximum augmentation required as cum_streamflow and marginal cost
      un_data1<- un_data %>% filter(Req_aug_Acft== max(Req_aug_Acft)) %>% slice(1)
      # Find the closest matching cost and volume that meets the augmentation requirement
      nearest_cos_vol<- mc_scn[min(which(mc_scn$cum_streamflow>= un_data1$Req_aug_Acft)),
                               c("cost_acft","cum_streamflow","Cost_Acre_for_fallowing","Cost_acrft_for_fallowing")] %>% 
        na.omit() %>%
        mutate(Meet_MIFR="YES")
      
      ##Pick the 50% of the augmentation and marginal cost
      #nearest_cos_vol1<- mc_scn[min(which(mc_scn$cum_streamflow>= un_data1$Fiftyp_aug)),c("cost_acft","cum_streamflow")] %>% na.omit()
      
      # If no matching value is found, pick the next closest value for augmentation requirements
      if (nrow(nearest_cos_vol)==0) {
        nearest_cos_vol<- mc_scn[max(which(mc_scn$cum_streamflow<= un_data1$Max_aug)),
                                 c("cost_acft","cum_streamflow","Cost_Acre_for_fallowing","Cost_acrft_for_fallowing")] %>% na.omit()
        if (nrow (un_data1>1)){
          un_data1<- un_data %>% filter(Req_aug_Acft== max(Req_aug_Acft)) %>% slice(1)
        }
        # Find the closest matching cost and volume that meets the augmentation requirement
        nearest_cos_vol<- mc_scn[max(which(mc_scn$cum_streamflow<= un_data1$Req_aug_Acft)),
                                 c("cost_acft","Cost_Acre_for_fallowing","Cost_acrft_for_fallowing")] %>% na.omit() %>% 
          mutate(cum_streamflow=un_data1$Req_aug_Acft,
                 Meet_MIFR="NO")
      }
      
      # if (nrow(nearest_cos_vol1)==0) {
      #   nearest_cos_vol1<- mc_scn[min(which(mc_scn$cum_streamflow<= un_data1$Fiftyp_aug)),c("cost_acft","cum_streamflow")] %>% na.omit() }
       
      
      # Filter for marginal cost within the required cost range to calculate per-acre cost
      mc_v1<- filter(mc_scn,cost_acft<= nearest_cos_vol$cost_acft, cum_streamflow<= nearest_cos_vol$cum_streamflow) %>% 
        mutate(Fallowing_cost_crp = Cost_Acre_for_fallowing* Acres,
               cost_acre_str_aug = cost_acr*Acres)
      
      # Add calculated cost values to the un_data dataframe
      un_data<- un_data %>% 
        mutate(MC_Cost= ifelse (nrow(nearest_cos_vol) > 0, nearest_cos_vol$cost_acft,0),
               #MC_Cost_fallow= ifelse (nrow(nearest_cos_vol) > 0, nearest_cos_vol$Cost_acrft_for_fallowing,0),
               #MC_Cost_fallow_acre= ifelse (nrow(nearest_cos_vol) > 0, nearest_cos_vol$Cost_Acre_for_fallowing,0),
               nearest_cum_stream=ifelse (nrow(nearest_cos_vol) > 0, nearest_cos_vol$cum_streamflow,0),
               #Meet_MIFR_C= ifelse (nrow(nearest_cos_vol) > 0, nearest_cos_vol$Meet_MIFR,0),
               Total_Cost=MC_Cost*Req_aug_Acft,
               Total_cost_fallowing = sum(mc_v1$Fallowing_cost_crp, na.rm=T),
               Total_cost_aug_fm_acre = sum(mc_v1$cost_acre_str_aug, na.rm=T),
               #MC_Cost_50 = ifelse (nrow(nearest_cos_vol1) > 0, nearest_cos_vol1$cost_acft,0),
               #nearest_cum_stream_50= ifelse (nrow(nearest_cos_vol1) > 0, nearest_cos_vol1$cum_streamflow,0),
               Contributing_acres= sum(mc_v1$Acres, na.rm=T),
               Cost_acre= Total_Cost/Contributing_acres,
               cost_AF= Total_Cost/Req_aug_Acft)
      
      # Append the updated un_data to the results dataframe
      Combined_sites_2Days<- rbind(Combined_sites_2Days,un_data)
    }
  }
}



#### Filter Out Two Consecutive Days for Years and Locations that Meet Required Augmentation
# Get unique site code
sites<- unique(Combined_sites_2Days$Site_Code)
# Initialize an empty list to store results
result_list <- list()
# Loop through each site
for (i in 1: length(sites)){
  site1<- Combined_sites_2Days %>% filter(Site_Code==sites[i])
  Yr<- unique(site1$Year)
  for (j in 1: length(Yr)){
    site11<- site1 %>% filter(Year==Yr[j])
    simulations<- unique(site11$simulation)
    for ( k in 1:length(simulations)){
      # Filter data for the current simulation and check for MIFR compliance
      site111<- site11 %>% filter(simulation==simulations[k],Can_meet_MIFR=="YES") %>% arrange(DOY)
      # Check if there are multiple rows (days) that meet MIFR
      if (nrow(site111>1)){
        # Identify indices where DOY (Day of Year) has consecutive days
        consecutive_days <- which(diff(site111$DOY) == 1)
        # If consecutive days exist, store these pairs in the result list
        if (length(consecutive_days) > 0) {
          # Loop through each pair of consecutive days
          for (l in 1:length(consecutive_days)) {
            # Add each pair of consecutive days to the result list
            result_list[[length(result_list) + 1]] <- site111[c(consecutive_days[l], consecutive_days[l] + 1), ]
          }
        }
      }
    }
  }
}



# Combine all the results into a single dataframe
Combined_sites_2Days1 <- do.call(rbind, result_list)

# Remove duplicates, if any
Combined_sites_2Days1 <- Combined_sites_2Days1[!duplicated(Combined_sites_2Days1), ]

#Just keeping the dataset to compare eith previous
#Combined_sites_2Days1_new<- Combined_sites_2Days1 

##Taking mean across the years and ading and modifying columns to create figure
Combined_sites_2Days22<- Combined_sites_2Days1 %>% 
  group_by(Site_Code,Year, simulation,Aug_Need,Can_meet_MIFR) %>% 
  summarise(Total_Cost= mean(Total_Cost,na.rm=T),
            Cost_acre= mean(Cost_acre, na.rm=T),
            cost_AF = mean(cost_AF),
            Total_cost_aug_fm_acre = mean(Total_cost_aug_fm_acre,na.rm=T),
            Total_cost_fallowing= mean(Total_cost_fallowing, na.rm=T)) %>% 
  group_by(Site_Code, simulation,Aug_Need,Can_meet_MIFR) %>% 
  summarise(Total_Cost= round(mean(Total_Cost,na.rm=T)),
            TC1 =round(Total_Cost/1000),
            Cost_acre= round(mean(Cost_acre, na.rm=T)),
            cost_AF = round(mean(cost_AF)),
            Total_cost_aug_fm_acre = mean(Total_cost_aug_fm_acre,na.rm=T),
            Total_cost_fallowing = mean(Total_cost_fallowing, na.rm=T),
            TC_frac = round((Total_Cost/Total_cost_fallowing)*100, digits = 1))

#Just keeping the dataset to compare eith previous
#Combined_sites_2Days22_new <- Combined_sites_2Days22

##Add a an empty dataframe to keep to identify the streamflow augmentation not required at all.
# added a value -1, to identify it.
add_dataframe<- data.frame(Site_Code= c("OKATO","TWITW","SIMNI"), simulation=c("May H1","May H2","May H1"),Cost_acre = c(-1,NA,-1),
                           TC1= c(-1,NA,-1))
Combined_sites_2Days221<- rbind(Combined_sites_2Days22,add_dataframe)%>% 
  mutate(simulation= factor(simulation, ordered = TRUE, c(c("May H1","May H2","Jun H1","Jun H2","Jul H1","Jul H2","Aug H1","Aug H2"))),
         Site_Code= factor(Site_Code, ordered = TRUE, levels= c(c("WALDE","TOUBO","NFTDA","OKAMA","OKATO","SIMNI","METPA","METTW","TWITW"))))


#Panel a (cost/acre)
P1<- ggplot(Combined_sites_2Days221, mapping = aes(x=simulation, y= Site_Code))+
  geom_tile(aes(fill= Cost_acre))+
  geom_text(aes(label = Cost_acre, color = ifelse(Cost_acre > 80, "black", "white")), size = 8) + 
  #geom_text(aes(label= Cost_acre), color="red", size= 8)+
  scale_fill_viridis(limits=c(-1,125), breaks=seq(0,125, by=25), na.value = NA)+
  scale_color_identity() + 
  scale_x_discrete(labels = c("May H1","May H2","Jun H1","Jun H2","Jul H1","Jul H2","Aug H1","Aug H2"))+
  theme_bw()+labs(x= "", y= " ",title = paste0("Cost per acre (mean)"))+ 
  theme(
    axis.text = element_text(size = 20, color = "black"),
    plot.title = element_text(size = 20),
    axis.text.x = element_text(size = 20, color = "black", vjust = 0.5, hjust = 1, angle = 90),
    axis.title = element_text(size = 22),
    legend.text = element_text(size = 20, color = "black"), 
    legend.title = element_blank(),
    legend.position = c(0.07, 0.82),
    legend.key.height = unit(0.8, "cm"),
    legend.background = element_blank()
  )
#Save the figure
ggsave(P1, file="Figure/Cost_acre_mean.png", width = 25, height = 20,dpi = 300, units = "cm")

##Panel b (total cost)
P1<- ggplot(Combined_sites_2Days221, mapping = aes(x=simulation, y= Site_Code)) +
  geom_tile(aes(fill = TC1)) +
  geom_text(aes(label = TC1, color = ifelse(TC1 > 800, "black", "white")), size = 8) +  # Dynamic text color
  scale_fill_viridis(limits = c(-1, 1000), breaks = seq(0, 1000, by = 250), na.value = NA) +  # Keep Viridis color scale
  scale_color_identity() +  # Ensure the dynamic text color is applied
  scale_x_discrete(labels = c("May H1", "May H2", "Jun H1", "Jun H2", "Jul H1", "Jul H2", "Aug H1", "Aug H2")) +
  theme_bw() +
  labs(x = "", y = " ", title = paste0("Total cost (mean)")) + 
  theme(
    axis.text = element_text(size = 20, color = "black"),
    plot.title = element_text(size = 20),
    axis.text.x = element_text(size = 20, color = "black", vjust = 0.5, hjust = 1, angle = 90),
    axis.title = element_text(size = 22),
    legend.text = element_text(size = 20, color = "black"), 
    legend.title = element_blank(),
    legend.position = c(0.07, 0.82),
    legend.key.height = unit(0.8, "cm"),
    legend.background = element_blank()
  )
#Save figure
ggsave(P1, file="Figure/Total_cost_mean.png", width = 25, height = 20,dpi = 300, units = "cm")


##Panel c (leasing cost as a fraction of fallowing)
P1<- ggplot(Combined_sites_2Days221, mapping = aes(x=simulation, y= Site_Code)) +
  geom_tile(aes(fill = TC_frac )) +
  geom_text(aes(label = TC_frac , color = ifelse(TC_frac > 10, "black", "white")), size = 8) +  # Dynamic text color
  scale_fill_viridis(limits = c(-1, 13), breaks = seq(0, 13, by = 3), na.value = NA) +  # Keep Viridis color scale
  scale_color_identity() +  # Ensure the dynamic text color is applied
  scale_x_discrete(labels = c("May H1", "May H2", "Jun H1", "Jun H2", "Jul H1", "Jul H2", "Aug H1", "Aug H2")) +
  theme_bw() +
  labs(x = "", y = " ", title = paste0("Total cost fraction in %")) + 
  theme(
    axis.text = element_text(size = 20, color = "black"),
    plot.title = element_text(size = 20),
    axis.text.x = element_text(size = 20, color = "black", vjust = 0.5, hjust = 1, angle = 90),
    axis.title = element_text(size = 22),
    legend.text = element_text(size = 20, color = "black"), 
    legend.title = element_blank(),
    legend.position = c(0.07, 0.82),
    legend.key.height = unit(0.8, "cm"),
    legend.background = element_blank()
  )
#Save figure
ggsave(P1, file="Figure/Total_cost_fr.png", width = 25, height = 20,dpi = 300, units = "cm")


##### Create plot for appendix #####
###Create dataset to create plots  for appendix (Augmentation available and Augmentation need)
selected_dataset<- Calc_ratio %>% 
  ungroup() %>% 
  dplyr::select("Site_Code","DOY","Year","simulation_abb","Discharge_cfs","MIFR_rules","MIFR_Dff1","streamflow_aug_cfs",
                "MaxOfTwo","RelativeRatioOfAug","RelativeRatioOfAug_adj") %>% 
  mutate(Req_aug_acft=((MaxOfTwo*3600*24*15)/43560),
         Aug_avail_acft= ((streamflow_aug_cfs*3600*24*15)/43560))

##create Appendix plot A2
#### create plot to calculate ratio of potential maximum augmentation and and need
P1<- ggplot(selected_dataset, aes(x=simulation_abb,y= streamflow_aug_cfs,fill=simulation_abb))+ 
  geom_boxplot(outlier.shape = NA)+ facet_wrap(~Site_Code, scales = "free_y")+
  stat_boxplot(geom='errorbar')+
  scale_fill_brewer(palette= "Paired")+
  labs(x= "", y= "Available augmentation (cfs)", title = paste0(" "))+theme_bw()+ 
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 20),axis.title = element_blank(), 
        legend.text = element_text(size=18), legend.title = element_blank(),
        axis.text.x = element_text(size = 18,angle = 90,hjust = 1, vjust=0.5,color="black"),
        axis.text.y = element_blank(), # Remove y-axis text from all facets
        axis.text.y.left = element_text(size = 20, color = "black"),
        axis.title.y = element_text(size = 20, color = "black"),
        legend.position = "none",axis.text.y.right = element_blank(),
        strip.text = element_text(size=20,color = "black"))
##Save figure
ggsave(P1, file="Figure/Aug_avail.png", width = 30, height = 20,dpi = 300, units = "cm")

##create Appendix plot A3
#### create plot to calculate ratio of potential maximum augmentation and and need
P1<- ggplot(selected_dataset, aes(x=simulation_abb,y= MaxOfTwo,fill=simulation_abb))+ 
  geom_boxplot(outlier.shape = NA)+ facet_wrap(~Site_Code, scales = "free_y")+
  stat_boxplot(geom='errorbar')+
  scale_fill_brewer(palette= "Paired")+
  labs(x= "", y= "Augmentation need (cfs)", title = paste0(" "))+theme_bw()+ 
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 20),axis.title = element_blank(), 
        legend.text = element_text(size=18), legend.title = element_blank(),
        axis.text.x = element_text(size = 18,angle = 90,hjust = 1, vjust=0.5,color="black"),
        axis.text.y = element_blank(), # Remove y-axis text from all facets
        axis.text.y.left = element_text(size = 20, color = "black"),
        axis.title.y = element_text(size = 20, color = "black"),
        legend.position = "none",axis.text.y.right = element_blank(),
        strip.text = element_text(size=20,color = "black"))
#Save figure
ggsave(P1, file="Figure/Aug_need.png", width = 30, height = 20,dpi = 300, units = "cm")


##Summarize it across days and required modification to create a figure about yearly percentage of the augmentation need
Percen_data1<- Calc_ratio %>%  
  group_by(Site_Code,Year, simulation_abb) %>% 
  summarise(Percen_med= median(Percentage_aug, na.rm=T),
            Percen_mn= mean(Percentage_aug, na.rm=T),
            Percen_min= min(Percentage_aug, na.rm=T),
            Percen_max= max(Percentage_aug, na.rm=T)) %>% 
  mutate(Percent_mn_1= ifelse(Percen_mn>0 & Percen_mn <= 25, "0 – 25", 
                              ifelse(Percen_mn> 25 & Percen_mn <= 50, "25 – 50", 
                                     ifelse(Percen_mn> 50 & Percen_mn <= 75, "50 – 75",
                                            ifelse(Percen_mn> 75 & Percen_mn < 100,"75 – 100",
                                                   ifelse(Percen_mn== 100,"100","0"))))),
         Percent_mn_1= as.character(Percent_mn_1),
         Percent_md_1= ifelse(Percen_med>0 & Percen_med <= 25, "0 – 25", 
                              ifelse(Percen_med> 25 & Percen_med <= 50, "25 – 50", 
                                     ifelse(Percen_med> 50 & Percen_med <= 75, "50 – 75",
                                            ifelse(Percen_med> 75 & Percen_med < 100,"75 – 100",
                                                   ifelse(Percen_med == 100,"100","0"))))),
         Percent_md_1= as.character(Percent_md_1),
         Percent_min_1= ifelse(Percen_min>0 & Percen_min <= 25, "0 – 25", 
                               ifelse(Percen_min> 25 & Percen_min <= 50, "25 – 50", 
                                      ifelse(Percen_min> 50 & Percen_min <= 75, "50 – 75",
                                             ifelse(Percen_min> 75 & Percen_min < 100,"75 – 100",
                                                    ifelse(Percen_min== 100 ,"100","0"))))),
         Percent_min_1= as.character(Percent_min_1),
         Percen_max = as.numeric(Percen_max),
         Percent_max_1= ifelse(Percen_max>= 0 & Percen_max <= 25, "[0, 25]", 
                               ifelse(Percen_max> 25 & Percen_max <= 50, "(25, 50]", 
                                      ifelse(Percen_max> 50 & Percen_max <= 75, "(50, 75]",
                                             ifelse(Percen_max> 75 & Percen_max < 100 ,"(75, 100)",
                                                    ifelse(Percen_max == 100 ,"100","0"))))),
         Percent_max_1= as.character(Percent_max_1),
         simulation_abb= factor(simulation_abb, ordered = TRUE, c(c("May H1","May H2","Jun H1","Jun H2","Jul H1","Jul H2","Aug H1","Aug H2"))),
         Site_Code= factor(Site_Code, ordered = TRUE, levels= c(c("WALDE","TOUBO","NFTDA","OKAMA","OKATO","SIMNI","METPA","METTW","TWITW"))))


##create plate to print color value manually from RocolorBrewer color blindness friendly color
Percen_plate<- c("#FFFFCC","#FED976","#FD8D3C","#E31A1C","#800026")
names(Percen_plate)<- as.factor(c("[0, 25]","(25, 50]","(50, 75]","(75, 100)","100"))


## Create maximum percentage plot
P1<- ggplot(Percen_data1, mapping = aes(x=Year, y= Site_Code))+geom_tile(aes(fill= Percent_max_1))+
  scale_fill_manual(values= Percen_plate, breaks=c("[0, 25]","(25, 50]","(50, 75]","(75, 100)","100"),
                    guide="legend")+ 
  facet_wrap(~simulation_abb, ncol=4, nrow=2)+
  labs(x= "", y= " ",title = paste0("Percentage_streamflow_can be met by 15 days shutoff (Max)"))+ 
  theme(axis.text.x= element_text(size = 16,angle = 90, vjust=0.5,color="black"),
        axis.text.y= element_text(size = 16,color="black"),
        plot.title = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.text = element_text(size=15), strip.text = element_text(size = 15,face="bold"), 
        strip.background = element_rect(colour = "black", fill = "lightgray", size = 1),
        legend.title = element_text(size=17),
        legend.position = "right",
        legend.key.height = unit(1.5,"cm"))
#save plot
ggsave(P1, file="Figure/Max_v2.png", width = 30, height = 20,dpi = 300, units = "cm")


##Create table instead of this plot which has been provided in appendix
Count_years1<- Combined_sites_2Days1 %>% 
  group_by(Site_Code,simulation) %>% 
  summarise(met_years= n_distinct(Year))

###Seperate a dataframe to create percentatge augmentation plot
Percen_meet_15d<- MIFR_ComplianceCalc %>% 
  group_by(Site_Code) %>% 
  mutate(Analyzed_yrs= (max(Year)-min(Year)+1)) %>% filter(Aug_Need=="Yes_Need") %>% 
  dplyr::select(c( "Site_Code","Year","DOY","simulation","streamflow_aug_cfs","Discharge_cfs","Contributing_flow","MIFR_rules","MIFR_R_75","MIFR_R_50","Aug_Need","Can_meet_MIFR","Can_meet_MIFR_75","Can_meet_MIFR_50","Analyzed_yrs"))


#Ensure table folder exists
if (!dir.exists("Figure/table")) {
  dir.create("Figure/table", recursive = TRUE)
}

##calculate when its need augmentation
## Table A2
Count_years<- Percen_meet_15d %>% 
  group_by(Site_Code,simulation,Analyzed_yrs) %>% 
  summarise(Req_years= n_distinct(Year)) %>% 
  #left_join(Count_years1) %>% 
  mutate(Analyzed_years = ifelse(Site_Code == "TOUBO",14,Analyzed_yrs),
         Analyzed_years = ifelse(Site_Code == "TOUBO" & simulation %in% c("Scenario01","Scenario02"),13,Analyzed_years),
         Percentage= round(Req_years/Analyzed_years*100,digits=1),
         simulation_abb= case_when(simulation== "Scenario01" ~ "May H1",
                                   simulation== "Scenario02" ~ "May H2",
                                   simulation== "Scenario03" ~ "Jun H1",
                                   simulation== "Scenario04" ~ "Jun H2",
                                   simulation== "Scenario05" ~ "Jul H1",
                                   simulation== "Scenario06" ~ "Jul H2",
                                   simulation== "Scenario07" ~ "Aug H1",
                                   simulation== "Scenario08" ~ "Aug H2")) %>% ungroup() %>%  
  dplyr::select(c(Site_Code,simulation_abb, Percentage)) %>% 
  pivot_wider(names_from = simulation_abb,
              values_from = Percentage)

##Write table to the folder
#write.csv(Count_years,"Figure/table/count_years.csv")
