# Summary flows (inbound, outbound, and net) by scenario, for scenario analysis V2
# Sum goods flows by county, land flows by county, land flows by ecoregion, and species extinction flows by ecoregion
# QDR / Virtualland / 20 Jan 2021 (aka FDT Day)

# Modified 30 Jun 2021: combine all summary data into single script for archiving.
# Modified 24 Mar 2021: remove deprecated biodiversity flow summaries.

library(tidyverse)
library(vroom) # To speed up
library(furrr) # To parallelize

# Land flows by ecoregion -------------------------------------------------

tnc_landflows <- vroom(file.path(final_output_path, 'landflows_tnc_x_tnc_all_scenarios.csv'))

flows_outbound <- tnc_landflows %>%
  select(-TNC_to) %>%
  group_by(scenario, TNC_from) %>%
  summarize_all(sum) %>%
  ungroup %>%
  pivot_longer(-c(scenario, TNC_from), names_to = 'land_type', values_to = 'flow_outbound') %>%
  rename(TNC = TNC_from) %>%
  mutate(land_type = gsub('_flow', '', land_type, fixed = TRUE)) %>%
  separate(scenario, into = c('D', 'scenario_diet', 'W', 'scenario_waste'), sep = '_') %>%
  select(scenario_diet, scenario_waste, land_type, TNC, flow_outbound)

flows_inbound <- tnc_landflows %>%
  select(-TNC_from) %>%
  group_by(scenario, TNC_to) %>%
  summarize_all(sum) %>%
  ungroup %>%
  pivot_longer(-c(scenario, TNC_to), names_to = 'land_type', values_to = 'flow_inbound') %>%
  rename(TNC = TNC_to) %>%
  mutate(land_type = gsub('_flow', '', land_type, fixed = TRUE)) %>%
  separate(scenario, into = c('D', 'scenario_diet', 'W', 'scenario_waste'), sep = '_') %>%
  select(scenario_diet, scenario_waste, land_type, TNC, flow_inbound)

tnc_land_flows <- full_join(flows_outbound, flows_inbound)

write_csv(tnc_land_flows, file.path(final_output_path, 'landflows_tnc_sums_all_scenarios.csv'))

# Total demand sums -------------------------------------------------------

### County-level consumption and production
county_production <- vroom(file.path(final_output_path, 'county_production2012.csv'))
county_consumption <- vroom(file.path(final_output_path, 'county_consumption2012_allscenarios.csv')) # 320 MB
county_totaldemand <- vroom(file.path(final_output_path, 'county_totaldemand2012_allscenarios.csv')) # 430 MB

source(file.path(code_path, 'final_output/figs/load_data_lookup_tables.R'))

# Need to reshape county_totaldemand, sum across counties, and possibly calculate relative to baseline.
totaldemand_sums <- cbind(county_totaldemand[,c('BEA_code', 'scenario')], demand = rowSums(county_totaldemand[,-(1:2)])) %>%
  separate(scenario, into = c('d', 'scenario_diet', 'w', 'scenario_waste'), sep = '_') %>% 
  select(-d, -w) %>%
  mutate(scenario_diet = factor(scenario_diet, levels = diet_levels_ordered),
         scenario_waste = factor(scenario_waste, levels = waste_levels_ordered)) %>% 
  right_join(ag_names_lookup, by = c('BEA_code' = 'BEA_389_code'))

# Correct such that greenhouse crops are added to peanuts and sugar.
totaldemand_sums <- totaldemand_sums %>%
  mutate(BEA_code = if_else(BEA_code == '111400', '111900', BEA_code),
         short_name = fct_collapse(short_name, `peanuts, sugar, etc.` = c('greenhouse crops', 'peanuts, sugar, etc.'))) %>%
  group_by(BEA_code, short_name, kingdom, scenario_diet, scenario_waste) %>%
  summarize(demand = sum(demand))

write_csv(totaldemand_sums, file.path(final_output_path, 'totaldemand_sums_all_scenarios.csv'))

# Goods flows by county ---------------------------------------------------

plan(multicore(workers = 8))

fp_goods <- file.path(intermediate_output_path, 'county_consumption_csvs')

scenario_combos <- expand_grid(diet = c('baseline','planetaryhealth','medstyle','usstyle','vegetarian'),
                               waste = c('baseline','preconsumer','consumer','allavoidable'))

sum_goods_flows_county <- function(diet, waste, file_path) {
  flows <- read_csv(glue::glue('{file_path}/D_{diet}_WR_{waste}_wide.csv'))
  message(glue::glue('{diet} by {waste} read'))
  # Outbound: each row represents origin county, sum across all destination counties (columns)
  flows_outbound <- cbind(flows[, 1:3], flow_outbound = apply(flows[, -(1:3)], 1, sum)) %>%
    rename(county = county_fips)

  # Inbound: exclude column denoting origin county then sum by group and reshape
  flows_inbound <- flows %>%
    select(-county_fips) %>%
    group_by(BEA_code, scenario) %>%
    summarize_all(sum) %>%
    ungroup %>%
    pivot_longer(-c(BEA_code, scenario), names_to = 'county', values_to = 'flow_inbound')

    # Join and split scenario column up
  full_join(flows_inbound, flows_outbound, by = c('scenario', 'BEA_code', 'county')) %>%
    separate(scenario, into = c('D', 'scenario_diet', 'W', 'scenario_waste'), sep = '_') %>%
    select(scenario_diet, scenario_waste, BEA_code, county, flow_inbound, flow_outbound)

}

county_goods_flows <- future_pmap_dfr(scenario_combos, sum_goods_flows_county, file_path = fp_goods)

write_csv(county_goods_flows, file.path(final_output_path, 'goodsflows_county_sums_all_scenarios.csv'))

# Land flows by county ----------------------------------------------------

fp_landcounties <- file.path(intermediate_output_path, 'county_land_consumption_csvs')

scenario_combos <- expand_grid(diet = c('baseline','planetaryhealth','medstyle','usstyle','vegetarian'),
                               waste = c('baseline','preconsumer','consumer','allavoidable'))

sum_land_flows_county <- function(diet, waste, file_path) {
  flows <- read_csv(glue::glue('{file_path}/D_{diet}_WR_{waste}_landconsumption.csv'))
  message(glue::glue('{diet} by {waste} read'))
  # Longform data so outbound and inbound sums are grouped separately
  flows_outbound <- flows %>%
    group_by(scenario, land_type, county_from) %>%
    summarize(flow_outbound = sum(land_consumption, na.rm = TRUE)) %>%
    ungroup %>%
    rename(county = county_from)
  
  flows_inbound <- flows %>%
    group_by(scenario, land_type, county_to) %>%
    summarize(flow_inbound = sum(land_consumption, na.rm = TRUE)) %>%
    ungroup %>%
    rename(county = county_to)

  # Join and split scenario column up
  full_join(flows_inbound, flows_outbound, by = c('scenario', 'land_type', 'county')) %>%
    separate(scenario, into = c('D', 'scenario_diet', 'W', 'scenario_waste'), sep = '_') %>%
    mutate(land_type = gsub('_exchange', '', land_type, fixed = TRUE)) %>%
    select(scenario_diet, scenario_waste, land_type, county, flow_inbound, flow_outbound)
  
}

county_land_flows <- future_pmap_dfr(scenario_combos, sum_land_flows_county, file_path = fp_landcounties)

write_csv(county_land_flows, file.path(final_output_path, 'landflows_county_sums_all_scenarios.csv'))
