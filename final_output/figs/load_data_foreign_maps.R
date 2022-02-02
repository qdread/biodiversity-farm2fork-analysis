# Additionally, load foreign ecoregion map and country map
global_eco_map <- st_read(file.path(spatial_output_path, 'tnc_global_equalarea.gpkg'))
global_country_map <- st_read(file.path(spatial_output_path, 'countries_global_equalarea.gpkg')) %>%
  select(NAME_LONG, ISO_A3)

# Data preparation --------------------------------------

fill_fvsd <- scale_fill_manual(values = setNames(okabe_colors[c(3, 2)], c('domestic', 'foreign')))

library(data.table)
setDT(foreign_vlt_export)
setDT(county_land_flow_sums)
setnames(foreign_vlt_export, 
         old = c('ECO_CODE', 'ECO_NAME', 'NAME_LONG', 'REGION_UN', 'SUBREGION'),
         new = c('TNC', 'TNC_name', 'country_name', 'region_UN', 'subregion_UN'))

foreign_vlt_countries <- foreign_vlt_export[, lapply(.SD, sum, na.rm = TRUE), 
                                            by = .(scenario_diet, scenario_waste, country_name, ISO_A3, region_UN, subregion_UN), 
                                            .SDcols = patterns('^V')]

### Compare domestic and foreign VLT into the USA.

# Sum the foreign vlt across all countries.
foreign_vlt_sum <- foreign_vlt_export[, lapply(.SD, sum, na.rm = TRUE), by = .(scenario_diet, scenario_waste), .SDcols = patterns('_region')]
setnames(foreign_vlt_sum, gsub('(VLT_)|(_region)', '', names(foreign_vlt_sum)))
foreign_vlt_sum[, annual := annual + mixed/2]
foreign_vlt_sum[, permanent := permanent + mixed/2]
foreign_vlt_sum[, mixed := NULL]
foreign_vlt_sum <- melt(foreign_vlt_sum, id.vars = c('scenario_diet', 'scenario_waste'), variable.name = 'land_type', value.name = 'foreign')

# sum the domestic vlt across all counties. Convert to ha
domestic_vlt_sum <- county_land_flow_sums[, .(domestic = sum(flow_inbound)/1e4), by = .(scenario_diet, scenario_waste, land_type)]
domestic_vlt_sum[, land_type := gsub('(_cropland)|(land)', '', land_type)]

all_vlt_sum <- foreign_vlt_sum[domestic_vlt_sum, on = .NATURAL]
all_vlt_sum <- melt(all_vlt_sum, id.vars = c('scenario_diet', 'scenario_waste', 'land_type'), variable.name = 'origin', value.name = 'VLT')

# Reorder factor levels
all_vlt_sum[, scenario_diet := factor(scenario_diet, levels = diet_levels_ordered)]
all_vlt_sum[, scenario_waste := factor(scenario_waste, levels = waste_levels_ordered)]

setDT(foreign_extinction_import)
setDT(foreign_extinction_export)
setDT(county_extinction_flow_sums)

foreign_extinction_sum <- foreign_extinction_import[, .(foreign = sum(species_lost)), by = .(scenario_diet, scenario_waste, land_use, taxon)]

county_extinction_flow_sums <- tidyr::separate(county_extinction_flow_sums, scenario, into = c('D', 'scenario_diet', 'W', 'scenario_waste'), sep = '_')
county_extinction_flow_sums[, c('D','W') := NULL]

domestic_extinction_sum <- county_extinction_flow_sums[, .(domestic = sum(extinction_inbound)), by = .(scenario_diet, scenario_waste, land_use, taxon)]

all_extinction_sum <- foreign_extinction_sum[domestic_extinction_sum, on = .NATURAL]
all_extinction_sum <- melt(all_extinction_sum, id.vars = c('scenario_diet', 'scenario_waste', 'land_use', 'taxon'), variable.name = 'origin', value.name = 'extinctions')

all_extinction_sum[, scenario_diet := factor(scenario_diet, levels = diet_levels_ordered)]
all_extinction_sum[, scenario_waste := factor(scenario_waste, levels = waste_levels_ordered)]

all_extinction_sum[, kingdom := ifelse(taxon == 'plants', 'plants', 'animals')]


# Process data: foreign extinction exports to counties ---------------------------------------

# Threats shown as exports from the originating ecoregion and originating country, depending on how they are totaled.
foreign_extinction_export_tnc <- foreign_extinction_export[, .(species_lost = sum(species_lost, na.rm = TRUE)), by = .(scenario_diet, scenario_waste, TNC, TNC_name, land_use, taxon)]
foreign_extinction_export_country <- foreign_extinction_export[, .(species_lost = sum(species_lost, na.rm = TRUE)), by = .(scenario_diet, scenario_waste, country_name, ISO_A3, land_use, taxon)]

# For both TNC and country maps, calculate sums by land use and by taxon (animals and total)
foreign_extinction_export_tnc_animals <- foreign_extinction_export_tnc[!taxon %in% 'plants', .(species_lost = sum(species_lost)), by = .(scenario_diet, scenario_waste, TNC, TNC_name, land_use)][,
  taxon := 'animals'
]
foreign_extinction_export_country_animals <- foreign_extinction_export_country[!taxon %in% 'plants', .(species_lost = sum(species_lost)), by = .(scenario_diet, scenario_waste, country_name, ISO_A3, land_use)][,
  taxon := 'animals'
]

foreign_extinction_export_tnc_total <- foreign_extinction_export_tnc[, .(species_lost = sum(species_lost)), by = .(scenario_diet, scenario_waste, TNC, TNC_name, land_use)][,
  taxon := 'total'
]
foreign_extinction_export_country_total <- foreign_extinction_export_country[, .(species_lost = sum(species_lost)), by = .(scenario_diet, scenario_waste, country_name, ISO_A3, land_use)][,
  taxon := 'total'
]

foreign_extinction_export_tnc <- rbindlist(list(foreign_extinction_export_tnc, foreign_extinction_export_tnc_animals, foreign_extinction_export_tnc_total), use.names = TRUE)
foreign_extinction_export_country <- rbindlist(list(foreign_extinction_export_country, foreign_extinction_export_country_animals, foreign_extinction_export_country_total), use.names = TRUE)

foreign_extinction_export_tnc_totalland <- foreign_extinction_export_tnc[, .(species_lost = sum(species_lost)), by = .(scenario_diet, scenario_waste, TNC, TNC_name, taxon)][,
  land_use := 'total'
]

foreign_extinction_export_country_totalland <- foreign_extinction_export_country[, .(species_lost = sum(species_lost)), by = .(scenario_diet, scenario_waste, country_name, ISO_A3, taxon)][,
  land_use := 'total'
]

foreign_extinction_export_tnc <- rbindlist(list(foreign_extinction_export_tnc, foreign_extinction_export_tnc_totalland), use.names = TRUE)
foreign_extinction_export_country <- rbindlist(list(foreign_extinction_export_country, foreign_extinction_export_country_totalland), use.names = TRUE)

# Calculate the difference between each scenario and the baseline.
foreign_ext_tnc_base <- foreign_extinction_export_tnc[scenario_waste %in% 'baseline' & scenario_diet %in% 'baseline']
setnames(foreign_ext_tnc_base, old = 'species_lost', new = 'species_lost_baseline')
foreign_ext_tnc_base[, c('scenario_diet', 'scenario_waste') := NULL]

foreign_ext_country_base <- foreign_extinction_export_country[scenario_waste %in% 'baseline' & scenario_diet %in% 'baseline']
setnames(foreign_ext_country_base, old = 'species_lost', new = 'species_lost_baseline')
foreign_ext_country_base[, c('scenario_diet', 'scenario_waste') := NULL]

foreign_extinction_export_tnc <- foreign_ext_tnc_base[foreign_extinction_export_tnc, on = .NATURAL]
foreign_extinction_export_tnc[, species_vs_baseline := species_lost/species_lost_baseline - 1]

foreign_extinction_export_country <- foreign_ext_country_base[foreign_extinction_export_country, on = .NATURAL]
foreign_extinction_export_country[, species_vs_baseline := species_lost/species_lost_baseline - 1]

# Nest into panels
library(Rutilitybelt)

foreign_extinction_tnc_panels <- group_nest_dt(foreign_extinction_export_tnc, scenario_diet, scenario_waste, land_use, taxon)
foreign_extinction_country_panels <- group_nest_dt(foreign_extinction_export_country, scenario_diet, scenario_waste, land_use, taxon)


# Process data: foreign land exports to counties --------------------------

# Get rid of the columns that were not divided out by region weights
cols_remove <- names(foreign_vlt_export)[grepl('VLT', names(foreign_vlt_export)) & !grepl('region', names(foreign_vlt_export))]
foreign_vlt_export[, (cols_remove) := NULL]

# Melt to long form
foreign_vlt_export_long <- melt(foreign_vlt_export, measure.vars = patterns('VLT'), variable.name = 'land_use', value.name = 'VLT')
foreign_vlt_export_long <- foreign_vlt_export_long[, .(scenario_diet, scenario_waste, TNC, TNC_name, country_name, ISO_A3, land_use, VLT)]
foreign_vlt_export_long[, land_use := gsub('(VLT_)|(_region)', '', land_use)]

# Total VLT across land types and then bind.
foreign_vlt_export_long_total <- foreign_vlt_export_long[, .(VLT = sum(VLT, na.rm = TRUE)), by = .(scenario_diet, scenario_waste, TNC, TNC_name, country_name, ISO_A3)]
foreign_vlt_export_long_total[, land_use := 'total']

foreign_vlt_export_long <- rbindlist(list(foreign_vlt_export_long, foreign_vlt_export_long_total), use.names = TRUE)

# Calculate sums by TNC ecoregion and by country separately.
foreign_vlt_tnc <- foreign_vlt_export_long[, .(VLT = sum(VLT)), by = .(scenario_diet, scenario_waste, TNC, TNC_name, land_use)]
foreign_vlt_country <- foreign_vlt_export_long[, .(VLT = sum(VLT)), by = .(scenario_diet, scenario_waste, country_name, ISO_A3, land_use)]

# Calculate the difference between each scenario and the baseline.
foreign_vlt_tnc_base <- foreign_vlt_tnc[scenario_waste %in% 'baseline' & scenario_diet %in% 'baseline']
setnames(foreign_vlt_tnc_base, old = 'VLT', new = 'VLT_baseline')
foreign_vlt_tnc_base[, c('scenario_diet', 'scenario_waste') := NULL]

foreign_vlt_tnc <- foreign_vlt_tnc_base[foreign_vlt_tnc, on = .NATURAL]
foreign_vlt_tnc[, VLT_vs_baseline := VLT/VLT_baseline - 1]
foreign_vlt_tnc[is.nan(VLT_vs_baseline), VLT_vs_baseline := 0]

foreign_vlt_country_base <- foreign_vlt_country[scenario_waste %in% 'baseline' & scenario_diet %in% 'baseline']
setnames(foreign_vlt_country_base, old = 'VLT', new = 'VLT_baseline')
foreign_vlt_country_base[, c('scenario_diet', 'scenario_waste') := NULL]

foreign_vlt_country <- foreign_vlt_country_base[foreign_vlt_country, on = .NATURAL]
foreign_vlt_country[, VLT_vs_baseline := VLT/VLT_baseline - 1]
foreign_vlt_country[is.nan(VLT_vs_baseline), VLT_vs_baseline := 0]

# Nest to panels
foreign_vlt_tnc_panels <- group_nest_dt(foreign_vlt_tnc, scenario_diet, scenario_waste, land_use)
foreign_vlt_country_panels <- group_nest_dt(foreign_vlt_country, scenario_diet, scenario_waste, land_use)

country_map_toplot <- global_country_map %>%
  st_transform("+proj=robin") %>%
  rename(country_name = NAME_LONG)

tnc_map_toplot <- global_eco_map %>%
  st_transform("+proj=robin") %>%
  rename(TNC = ECO_CODE, TNC_name = ECO_NAME) %>%
  select(TNC, TNC_name)