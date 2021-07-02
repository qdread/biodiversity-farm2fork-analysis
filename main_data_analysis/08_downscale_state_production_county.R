# Downscale state level production by BEA to county level

# Load data ---------------------------------------------------------------

library(tidyverse)

# Production by state
receipts_bea_x_state <- read_csv(file.path(intermediate_output_path, 'susb_nass_workers_receipts_land_bea.csv')) 

# Read county level weighting data for downscaling state production to county
county_weightings <- read_csv(file.path(intermediate_output_path, 'county_weightings_for_downscale.csv'), col_types = c('cccciiii'))

# Downscale state level production to county ------------------------------

# Subset columns of receipts data and then join the county weightings to it

production_states <- receipts_bea_x_state %>%
  select(state_fips, state_name, BEA_code, receipts) %>%
  rename(production = receipts)

production_counties <- inner_join(county_weightings, production_states, by = c('state_fips', 'BEA_code')) %>%
  group_by(state_fips, state_name, BEA_code) %>%
  select(-n_employees, -q1_payroll, -annual_payroll) %>%
  mutate(production_county_downscaled = production * n_establishments/sum(n_establishments)) %>%
  replace_na(list(production_county_downscaled= 0)) %>%
  mutate(county_fips = paste0(state_fips, county_fips))

# Write county production to CSV
write_csv(production_counties, file.path(intermediate_output_path, 'county_production2012.csv'))
