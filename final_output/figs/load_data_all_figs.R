# Script to load packages and data for making figures
# Split off into separate script, 15 Feb 2021

# Load data ---------------------------------------------------------------

library(tidyverse)
library(cowplot) # For labels of faceted plots
library(sf)
library(glue)
library(grid)
library(gridExtra)

### Diet
diet_lancet <- read_csv(file.path(intermediate_output_path, 'proportion_diet_lancet.csv'))
diet_usa <- read_csv(file.path(intermediate_output_path, 'proportion_diet_usaguidelines.csv'))
lafa_joined <- read_csv(file.path(intermediate_output_path, 'lafa_joined_with_diet_proportions.csv'))

### County-level consumption and production
totaldemand_sums <- read_csv(file.path(final_output_path, 'totaldemand_sums_all_scenarios.csv'))

### Scenario factors by BEA category
bea_scenario_factors <- read_csv(file.path(intermediate_output_path, 'bea_consumption_factors_diet_waste_scenarios.csv'))

### State land exchange tables
load(file.path(intermediate_output_path, 'state_land_exchange_tables.RData'))

# For flows of goods and land between counties, land between ecoregions, and species between ecoregions,
# I've loaded only the summed data here. The raw data can be loaded separately later (some are big).

# Flows of goods between counties
county_goods_flow_sums <- read_csv(file.path(final_output_path, 'goodsflows_county_sums_all_scenarios.csv'))

# Flows of land between counties
county_land_flow_sums <- read_csv(file.path(final_output_path, 'landflows_county_sums_all_scenarios.csv'))

# Flows of land between ecoregions
tnc_land_flow_sums <- read_csv(file.path(final_output_path, 'landflows_tnc_sums_all_scenarios.csv'))

# Flows of species extinctions between counties
county_extinction_flow_sums <- read_csv(file.path(final_output_path, 'species_lost_county_sums_all_scenarios_med.csv'), col_types = 'ccccdd')

# Flows of species extinctions between states (raw pairwise)
state_extinction_flows <- read_csv(file.path(final_output_path, 'species_lost_state_x_state_all_scenarios_med.csv'), col_types = 'cccccd')

# Foreign flows of species extinctions into USA, by country x tnc combination exporting them and by county importing them
foreign_extinction_export <- read_csv(file.path(final_output_path, 'foreign_species_lost_by_export_country_x_tnc.csv'))
foreign_extinction_import <- read_csv(file.path(final_output_path, 'foreign_species_lost_by_import_county.csv'))

# Foreign goods transfers into USA, one for animals and one for crops
foreign_animal_export <- read_csv(file.path(intermediate_output_path, 'fao_production_trade_animals.csv'))
foreign_crop_export <- read_csv(file.path(intermediate_output_path, 'fao_production_trade_crops.csv'))

# Foreign virtual land transfers into USA, by country x tnc combination exporting them and by county importing them
foreign_vlt_export <- read_csv(file.path(final_output_path, 'foreign_VLT_by_country_x_TNC.csv'))
foreign_vlt_import <- read_csv(file.path(final_output_path, 'foreign_VLT_to_counties.csv'))

# Map of counties in AEA
county_map <- st_read(file.path(spatial_output_path, 'USA_county_2014_aea.gpkg'))
# Map of TNC ecoregions in AEA
tnc_map <- st_read(file.path(spatial_output_path, 'tnc_usa_aea.gpkg'))

# Plotting functions
source(file.path(code_path, 'final_output/figs/us_map_fxns.R'))

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

source(file.path(code_path, 'final_output/figs/load_data_lookup_tables.R'))

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