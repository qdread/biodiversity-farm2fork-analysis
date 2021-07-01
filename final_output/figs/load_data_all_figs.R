# Script to load packages and data for making figures
# Split off into separate script, 15 Feb 2021

# Load data ---------------------------------------------------------------

library(tidyverse)
library(cowplot) # For labels of faceted plots
library(sf)
library(glue)
library(grid)
library(gridExtra)

fp_fig <- 'data/cfs_io_analysis/scenario_v2_figs'

### Diet
diet_lancet <- read_csv('data/cfs_io_analysis/proportion_diet_lancet.csv')
diet_usa <- read_csv('data/cfs_io_analysis/proportion_diet_usaguidelines.csv')
lafa_joined <- read_csv('data/cfs_io_analysis/lafa_joined_with_diet_proportions.csv')

### County-level consumption and production
# I've loaded only the summed data here. The raw data can be loaded separately later (some are big).
# county_production <- read_csv('data/cfs_io_analysis/county_production2012.csv')
# county_consumption <- read_csv('data/cfs_io_analysis/county_consumption2012_allscenarios.csv') # 320 MB
# county_totaldemand <- read_csv('data/cfs_io_analysis/county_totaldemand2012_allscenarios.csv') # 430 MB
totaldemand_sums <- read_csv('data/cfs_io_analysis/scenarios/totaldemand_sums_all_scenarios.csv')

### Scenario factors by BEA category
bea_scenario_factors <- read_csv('data/cfs_io_analysis/bea_consumption_factors_diet_waste_scenarios.csv')

### State land exchange tables
load('data/cfs_io_analysis/state_land_exchange_tables.RData')

# For flows of goods and land between counties, land between ecoregions, and species between ecoregions,
# I've loaded only the summed data here. The raw data can be loaded separately later (some are big).

# Flows of goods between counties
county_goods_flow_sums <- read_csv('data/cfs_io_analysis/scenarios/goodsflows_county_sums_all_scenarios.csv')

# Flows of land between counties
county_land_flow_sums <- read_csv('data/cfs_io_analysis/scenarios/landflows_county_sums_all_scenarios.csv')

# Flows of land between ecoregions
tnc_land_flow_sums <- read_csv('data/cfs_io_analysis/scenarios/landflows_tnc_sums_all_scenarios.csv')
#tnc_landflows <- read_csv('data/cfs_io_analysis/scenarios/landflows_tnc_x_tnc_all_scenarios.csv')

# Flows of species extinctions between counties
county_extinction_flow_sums <- read_csv('data/cfs_io_analysis/scenarios/species_lost_county_sums_all_scenarios_med.csv', col_types = 'ccccdd')

# Flows of species extinctions between states (raw pairwise)
state_extinction_flows <- read_csv('data/cfs_io_analysis/scenarios/species_lost_state_x_state_all_scenarios_med.csv', col_types = 'cccccd')

# Foreign virtual land transfers into USA, by country x tnc combination exporting them and by county importing them
foreign_vlt_export <- read_csv('data/cfs_io_analysis/foreign_VLT_by_country_x_TNC.csv')
foreign_vlt_import <- read_csv('data/cfs_io_analysis/foreign_VLT_to_counties.csv')

# Foreign flows of species extinctions into USA, by country x tnc combination exporting them and by county importing them
foreign_extinction_export <- read_csv('data/cfs_io_analysis/scenarios/foreign_species_lost_by_export_country_x_tnc.csv')
foreign_extinction_import <- read_csv('data/cfs_io_analysis/scenarios/foreign_species_lost_by_import_county.csv')

# Foreign goods transfers into USA, one for animals and one for crops
foreign_animal_export <- read_csv('data/cfs_io_analysis/fao_production_trade_animals.csv')
foreign_crop_export <- read_csv('data/cfs_io_analysis/fao_production_trade_crops.csv')

# Map of counties in AEA
county_map <- st_read('data/raw_data/landuse/USA/USA_county_2014_aea.gpkg')
# Map of TNC ecoregions in AEA
tnc_map <- st_read('data/raw_data/landuse/ecoregions/tnc_usa_aea.gpkg')

# Plotting functions
source('figs/us_map_fxns.R')

# Rename fips to county in county_map for compatibility.
county_map <- county_map %>%
  rename(county = fips)

# Merge county map to state map.
state_map <- county_map %>%
  group_by(fips_state) %>%
  summarize

# Index of Alaska and Hawaii in ecoregion map
tnc_ak_idx <- substr(tnc_map$ECO_CODE, 1, 4) %in% c('NA06', 'NA11') | tnc_map$ECO_CODE %in% c('NA0509', 'NA0518')
tnc_hi_idx <- substr(tnc_map$ECO_CODE, 1, 2) == 'OC'

# Get index of Alaska and Hawaii in county map. AK 02 HI 15
county_ak_idx <- substr(county_map$county, 1, 2) == '02'
county_hi_idx <- substr(county_map$county, 1, 2) == '15'

# Additional lookup tables of category labels ---------------------------------

# Order levels for the different scenarios
diet_levels_ordered <- c('baseline', 'usstyle', 'medstyle', 'vegetarian', 'planetaryhealth')
waste_levels_ordered <- c('baseline', 'preconsumer', 'consumer', 'allavoidable')
land_levels_ordered <- c('annual', 'permanent', 'pasture')

# Lookup tables for longer legend names
diet_long_names <- data.frame(scenario_diet = diet_levels_ordered,
                              long_name = c('baseline diet', 'healthy US-style (USDA)', 'healthy Mediterranean-style (USDA)', 'healthy vegetarian (USDA)', 'planetary health (Lancet)'),
                              medium_name = c('baseline diet', 'USDA US-style', 'USDA Mediterranean-style', 'USDA vegetarian', 'planetary health'))
waste_long_names <- data.frame(scenario_waste = waste_levels_ordered,
                               long_name = c('no waste reduction', 'pre-consumer waste cut 50%', 'consumer waste cut 50%', 'all waste cut 50%'),
                               medium_name = c('no reduction', 'pre-consumer -50%', 'consumer -50%', 'all -50%'))

# Labeller function with character vector lookup tables for 2x2 scenarios
scenario_labeller <- labeller(scenario_diet = setNames(diet_long_names$long_name, diet_long_names$scenario_diet),
                              scenario_waste = setNames(waste_long_names$long_name, waste_long_names$scenario_waste))

# Shorter labeller
scenario_labeller_medium <- labeller(scenario_diet = setNames(diet_long_names$medium_name, diet_long_names$scenario_diet),
                              scenario_waste = setNames(waste_long_names$medium_name, waste_long_names$scenario_waste))

# Labeller that can specify long or medium name for each one.
scenario_labeller_fn <- function(diet, waste) {
  if (diet == 'long') diet_names_use <- diet_long_names$long_name
  if (diet == 'medium') diet_names_use <- diet_long_names$medium_name
  if (waste == 'long') waste_names_use <- waste_long_names$long_name
  if (waste == 'medium') waste_names_use <- waste_long_names$medium_name
  labeller(scenario_diet = setNames(diet_names_use, diet_long_names$scenario_diet),
           scenario_waste = setNames(waste_names_use, waste_long_names$scenario_waste))
}

# Short names of the ten agricultural goods in BEA, plus wild-caught fish
ag_names_lookup <- data.frame(
  BEA_389_code = c("1111A0", "1111B0", "111200", "111300", "111400", "111900", "112120", "1121A0", "112300", "112A00", "114000"
  ), 
  BEA_389_def = c("Fresh soybeans, canola, flaxseeds, and other oilseeds",
                  "Fresh wheat, corn, rice, and other grains", 
                  "Fresh vegetables, melons, and potatoes", 
                  "Fresh fruits and tree nuts", 
                  "Greenhouse crops, mushrooms, nurseries, and flowers", 
                  "Tobacco, cotton, sugarcane, peanuts, sugar beets, herbs and spices, and other crops", 
                  "Dairies", 
                  "Cattle ranches and feedlots", 
                  "Poultry farms", 
                  "Animal farms and aquaculture ponds (except cattle and poultry)", 
                  "Wild-caught fish and game"),
  short_name = c('oilseeds & soybeans', 'grains', 'vegetables & potatoes', 'fruits & nuts', 'greenhouse crops', 'peanuts, sugar, etc.', 'dairy', 'beef cattle', 'poultry & eggs', 'other meat incl. aquaculture', 'wild-caught seafood'),
  kingdom = rep(c('plant', 'animal'), c(6, 5))) %>%
  mutate(short_name = factor(short_name, levels = unique(short_name)))

theme_set(theme_bw() + theme(strip.background = element_blank()))
fill_dark <- scale_fill_brewer(palette = 'Dark2')
okabe_colors <- palette.colors(n = 9, palette = 'Okabe-Ito')

# Function to make a "dummy axis" so I can label the secondary axis.
dummy_axis <- function(label) sec_axis(~ . , name = label, labels = NULL, breaks = NULL)

# function to make category labels for the two facets
label_scenario_categories <- function(p) {
  ggdraw(p + theme(plot.margin = unit(c(25, 25, 5.5, 5.5), 'points'))) +
    draw_label(label = 'diet scenario', x = 0.5, y = 0.97) +
    draw_label(label = 'waste scenario', x = 0.99, y = 0.5, angle = -90)
}