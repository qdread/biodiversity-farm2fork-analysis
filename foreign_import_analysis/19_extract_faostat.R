# Extract and process FAOSTAT data
# All data downloaded 31 Aug 2020

# Modified 30 June 2021. Clean up script, removing unused code.
# Modified 10 Sept 2020. Instead of using a single year, use 5 years of data.

library(tidyverse)
fp_fao <- file.path(data_path, 'FAOSTAT')

# We need the following data
# Production and yield data on crops, crops processed, livestock, livestock primary, and livestock processed.
# Land use inputs
# Trade: detailed trade matrix.

# Read the production CSVs
prod_crop_rawdata <- read_csv(file.path(fp_fao, 'Production_Crops_E_All_Data_(Normalized).csv'))
prod_cropsprocessed_rawdata <- read_csv(file.path(fp_fao, 'Production_CropsProcessed_E_All_Data_(Normalized).csv'))
prod_livestock_rawdata <- read_csv(file.path(fp_fao, 'Production_Livestock_E_All_Data_(Normalized).csv'))
prod_livestockprimary_rawdata <- read_csv(file.path(fp_fao, 'Production_LivestockPrimary_E_All_Data_(Normalized).csv'))
prod_livestockprocessed_rawdata <- read_csv(file.path(fp_fao, 'Production_LivestockProcessed_E_All_Data_(Normalized).csv'))

# Read the land use CSVs (incl. livestock patterns)
landuse_inputs_rawdata <- read_csv(file.path(fp_fao, 'Inputs_LandUse_E_All_Data_(Normalized).csv'))
livestockpatterns_rawdata <- read_csv(file.path(fp_fao, 'Environment_LivestockPatterns_E_All_Data_(Normalized).csv'))

# Read the trade CSV
trade_matrix_rawdata <- read_csv(file.path(fp_fao, 'Trade_DetailedTradeMatrix_E_All_Data_(Normalized).csv'))

# Write time averaged data to CSVs ----------------------------------------

# Modified 10 Sep: We can't assume the single year is representative so use 5 years.

timeavg <- function(dat, file, yr = 2014:2018) {
  datfilter <- dat %>%
    filter(Year %in% yr) %>%
    select(-`Year Code`)
  datavg <- datfilter %>%
    group_by(`Area Code`, Area, `Item Code`, Item, `Element Code`, Element, Unit) %>%
    summarize(value = mean(Value, na.rm = TRUE),
              n = sum(!is.na(Value)))
  names(datavg) <- c('area_code', 'area', 'item_code', 'item', 'element_code', 'element', 'unit', 'value', 'n')
  write_csv(datavg, file.path(intermediate_output_path, 'faostat_processed', paste0(file, '.csv')))
}



timeavg(prod_crop_rawdata, 'production_crops')
timeavg(prod_cropsprocessed_rawdata, 'production_cropsprocessed')
timeavg(prod_livestock_rawdata, 'production_livestock')
timeavg(prod_livestockprimary_rawdata, 'production_livestockprimary')
timeavg(prod_livestockprocessed_rawdata, 'production_livestockprocessed')
timeavg(landuse_inputs_rawdata, 'landuse_inputs')
timeavg(livestockpatterns_rawdata, 'livestock_patterns')

# Different function needed for trade matrix only.

datfilter <- trade_matrix_rawdata %>%
  filter(Year %in% yr) %>%
  select(-`Year Code`)
datavg <- datfilter %>%
  group_by(`Reporter Country Code`, `Reporter Countries`, `Partner Country Code`, `Partner Countries`, `Item Code`, Item, `Element Code`, Element, Unit) %>%
  summarize(Value = mean(Value, na.rm = TRUE))
names(datavg) <- c('reporter_country_code', 'reporter_country', 'partner_country_code', 'partner_country', 'item_code', 'item', 'element_code', 'element', 'unit', 'value')
write_csv(datavg, file.path(intermediate_output_path, 'faostat_processed', 'trade_matrix.csv'))
