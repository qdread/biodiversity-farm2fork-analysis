# Extract data from FAO food balance sheets to get proportions of each crop in each country used for animal feed.
# QDR / Virtualland / 30 Sep 2020

library(tidyverse)

fbs <- read_csv(file.path(data_path, "FAOSTAT", "FoodBalanceSheets_E_All_Data_(Normalized).csv"))

# Name repair of fbs
fbs <- fbs %>% rename_with(function(x) gsub(' ', '_', tolower(x)))

# There are data from 2014-2017 here.

# Average over time.
fbs_avg_time <- fbs %>%
  group_by(area_code, area, item_code, item, element_code, element, unit) %>%
  summarize(value = mean(value, na.rm = TRUE))

# Lookup table to see which ones are an "item group," or aggregated category, versus individual.
# Downloaded from http://www.fao.org/faostat/en/#data/BC/metadata (definitions sidebar > item group)
item_grp_lookup <- read_csv(file.path(fp_crosswalk, 'faostat_item_group_lookup.csv')) %>%
  rename_with(function(x) gsub(' ', '_', tolower(x)))

item_codes <- unique(fbs_avg_time[,c('item_code','item')])

# Separate into population, aggregate, and individual item categories
pop_codes <- c(2501)
agg_codes <- unique(fbs_avg_time$item_code[substr(fbs_avg_time$item_code,1,2) == '29'])

fbs_pop <- fbs_avg_time %>% filter(item_code %in% pop_codes)
fbs_agg <- fbs_avg_time %>% filter(item_code %in% agg_codes)
fbs_indiv <- fbs_avg_time %>% filter(!item_code %in% c(agg_codes, pop_codes))

# Widen to get a separate column for each of the weights

# Individual codes
fbs_weights_wide <- fbs_indiv %>% 
  ungroup %>%
  filter(unit == '1000 tonnes') %>%
  select(-element_code) %>%
  pivot_wider(names_from = element, values_from = value) %>% 
  rename_with(function(x) gsub(' ', '_', gsub('\\(|\\)|\\-', '', tolower(x))))

# Aggregated codes
fbs_agg_weights_wide <- fbs_agg %>% 
  ungroup %>%
  filter(unit == '1000 tonnes') %>%
  select(-element_code) %>%
  pivot_wider(names_from = element, values_from = value) %>% 
  rename_with(function(x) gsub(' ', '_', gsub('\\(|\\)|\\-', '', tolower(x))))

# Widen the per capita values as well

# Individual codes
fbs_percapita_wide <- fbs_indiv %>%
  ungroup %>%
  filter(element_code %in% c(645, 664, 674, 684)) %>%
  select(-element_code, -unit) %>%
  pivot_wider(names_from = element, values_from = value) %>% 
  rename_with(function(x) gsub(' ', '_', gsub('\\(|\\)|\\/', '', tolower(x))))

fbs_agg_percapita_wide <- fbs_agg %>%
  ungroup %>%
  filter(element_code %in% c(645, 664, 674, 684)) %>%
  select(-element_code, -unit) %>%
  pivot_wider(names_from = element, values_from = value) %>% 
  rename_with(function(x) gsub(' ', '_', gsub('\\(|\\)|\\/', '', tolower(x))))

# Save output

fp_out <- file.path(intermediate_output_path, 'faostat_processed')

write_csv(fbs_weights_wide, file.path(fp_out, 'fbs_indiv_weights_wide.csv'))
write_csv(fbs_agg_weights_wide, file.path(fp_out, 'fbs_agg_weights_wide.csv'))
write_csv(fbs_percapita_wide, file.path(fp_out, 'fbs_indiv_percapita_wide.csv'))
write_csv(fbs_agg_percapita_wide, file.path(fp_out, 'fbs_agg_percapita_wide.csv'))
