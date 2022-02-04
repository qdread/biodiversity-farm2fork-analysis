# Draw 10 panel scenario maps for supplements. Note: One of these figures is also presented as Figure 4 in the MS.
# QDR / 30 June 2021

source(file.path(code_path, 'final_output/figs/load_data_domestic_maps.R'))
fp_fig <- file.path(fp_fig, 'paneled_maps')
if (!dir.exists(fp_fig)) dir.create(fp_fig)

# County land maps, 10 scenarios ------------------------------------------

# Total land, outbound vs. baseline
make_20panel_map_v2(map_panel_data = county_land_map_panels[land_type %in% 'total' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'outbound_vs_baseline',
                    file_name = '10scenarios_county_totalland_outbound_vs_baseline',
                    scale_name = 'Change vs.\nbaseline',
                    scale_factor = 1,
                    scale_trans = 'identity',
                    scale_type = 'divergent',
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)

# Annual crops, outbound vs. baseline
make_20panel_map_v2(map_panel_data = county_land_map_panels[land_type %in% 'annual_cropland' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'outbound_vs_baseline',
                    file_name = '10scenarios_county_annualcrop_outbound_vs_baseline',
                    scale_name = 'Change vs.\nbaseline',
                    scale_factor = 1,
                    scale_trans = 'identity',
                    scale_type = 'divergent',
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)

# Permanent crops, outbound vs. baseline
make_20panel_map_v2(map_panel_data = county_land_map_panels[land_type %in% 'permanent_cropland' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'outbound_vs_baseline',
                    file_name = '10scenarios_county_permanentcrop_outbound_vs_baseline',
                    scale_name = 'Change vs.\nbaseline',
                    scale_factor = 1,
                    scale_trans = 'identity',
                    scale_type = 'divergent',
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)

# Pastureland, outbound vs. baseline
make_20panel_map_v2(map_panel_data = county_land_map_panels[land_type %in% 'pastureland' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'outbound_vs_baseline',
                    file_name = '10scenarios_county_pasture_outbound_vs_baseline',
                    scale_name = 'Change vs.\nbaseline',
                    scale_factor = 1,
                    scale_trans = 'identity',
                    scale_type = 'divergent',
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)

###

# County outbound land, raw values

# Total land, outbound
make_20panel_map_v2(map_panel_data = county_land_map_panels[land_type %in% 'total' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'flow_outbound',
                    file_name = '10scenarios_county_totalland_outbound',
                    scale_name = 'Virtual land\nexport (ha)',
                    scale_factor = 10000,
                    scale_trans = 'log10',
                    scale_type = 'sequential',
                    percent_scale = FALSE,
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)

# Annual crops, outbound
make_20panel_map_v2(map_panel_data = county_land_map_panels[land_type %in% 'annual_cropland' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'flow_outbound',
                    file_name = '10scenarios_county_annualcrop_outbound',
                    scale_name = 'Virtual land\nexport (ha)',
                    scale_factor = 10000,
                    scale_trans = 'log10',
                    scale_type = 'sequential',
                    percent_scale = FALSE,
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)

# Permanent crops, outbound 
make_20panel_map_v2(map_panel_data = county_land_map_panels[land_type %in% 'permanent_cropland' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'flow_outbound',
                    file_name = '10scenarios_county_permanentcrop_outbound',
                    scale_name = 'Virtual land\nexport (ha)',
                    scale_factor = 10000,
                    scale_trans = 'log10',
                    scale_type = 'sequential',
                    percent_scale = FALSE,
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)

# Pastureland, outbound
make_20panel_map_v2(map_panel_data = county_land_map_panels[land_type %in% 'pastureland' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'flow_outbound',
                    file_name = '10scenarios_county_pasture_outbound',
                    scale_name = 'Virtual land\nexport (ha)',
                    scale_factor = 10000,
                    scale_trans = 'log10',
                    scale_type = 'sequential',
                    percent_scale = FALSE,
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)


# County extinction maps, 10 scenario version -----------------------------

# For each taxon, total land use, change vs. baseline.
# Amphibians, total land use
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'amphibians' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound_vs_baseline',
                    file_name = '10scenarios_county_totalland_amphibianextinction_outbound_vs_baseline',
                    scale_name = 'Change vs.\nbaseline',
                    scale_factor = 1,
                    scale_trans = 'identity',
                    scale_type = 'divergent',
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)

# Animals, total land use
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'animals' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound_vs_baseline',
                    file_name = '10scenarios_county_totalland_animalextinction_outbound_vs_baseline',
                    scale_name = 'Change vs.\nbaseline',
                    scale_factor = 1,
                    scale_trans = 'identity',
                    scale_type = 'divergent',
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)

# Birds, total land use
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'birds' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound_vs_baseline',
                    file_name = '10scenarios_county_totalland_birdextinction_outbound_vs_baseline',
                    scale_name = 'Change vs.\nbaseline',
                    scale_factor = 1,
                    scale_trans = 'identity',
                    scale_type = 'divergent',
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)

# Mammals, total land use
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'mammals' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound_vs_baseline',
                    file_name = '10scenarios_county_totalland_mammalextinction_outbound_vs_baseline',
                    scale_name = 'Change vs.\nbaseline',
                    scale_factor = 1,
                    scale_trans = 'identity',
                    scale_type = 'divergent',
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)

# Plants, total land use
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'plants' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound_vs_baseline',
                    file_name = '10scenarios_county_totalland_plantextinction_outbound_vs_baseline',
                    scale_name = 'Change vs.\nbaseline',
                    scale_factor = 1,
                    scale_trans = 'identity',
                    scale_type = 'divergent',
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)

# Reptiles, total land use
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'reptiles' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound_vs_baseline',
                    file_name = '10scenarios_county_totalland_reptileextinction_outbound_vs_baseline',
                    scale_name = 'Change vs.\nbaseline',
                    scale_factor = 1,
                    scale_trans = 'identity',
                    scale_type = 'divergent',
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)

# All taxa, total land use
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'total' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound_vs_baseline',
                    file_name = '10scenarios_county_totalland_totalextinction_outbound_vs_baseline',
                    scale_name = 'Change vs.\nbaseline',
                    scale_factor = 1,
                    scale_trans = 'identity',
                    scale_type = 'divergent',
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)

# All taxa, total land use relative to baseline, as PDF (fig 4)
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'total' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound_vs_baseline',
                    file_name = 'fig4',
                    scale_name = 'Change vs.\nbaseline',
                    scale_factor = 1,
                    scale_trans = 'identity',
                    scale_type = 'divergent',
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2,
                    output_type = 'pdf'
)

# Fig 4 as png
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'total' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound_vs_baseline',
                    file_name = 'fig4',
                    scale_name = 'Change vs.\nbaseline',
                    scale_factor = 1,
                    scale_trans = 'identity',
                    scale_type = 'divergent',
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)

###
# For each taxon, total land use, raw extinction outbound.

# Amphibians, total land use
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'amphibians' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound',
                    file_name = '10scenarios_county_totalland_amphibianextinction_outbound',
                    scale_name = 'Extinctions',
                    scale_factor = 1,
                    scale_trans = 'log10',
                    scale_type = 'sequential',
                    percent_scale = FALSE,
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)

# Animals, total land use
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'animals' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound',
                    file_name = '10scenarios_county_totalland_animalextinction_outbound',
                    scale_name = 'Extinctions',
                    scale_factor = 1,
                    scale_trans = 'log10',
                    scale_type = 'sequential',
                    percent_scale = FALSE,
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)

# Birds, total land use
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'birds' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound',
                    file_name = '10scenarios_county_totalland_birdextinction_outbound',
                    scale_name = 'Extinctions',
                    scale_factor = 1,
                    scale_trans = 'log10',
                    scale_type = 'sequential',
                    percent_scale = FALSE,
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)

# Mammals, total land use
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'mammals' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound',
                    file_name = '10scenarios_county_totalland_mammalextinction_outbound',
                    scale_name = 'Extinctions',
                    scale_factor = 1,
                    scale_trans = 'log10',
                    scale_type = 'sequential',
                    percent_scale = FALSE,
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)

# Plants, total land use
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'plants' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound',
                    file_name = '10scenarios_county_totalland_plantextinction_outbound',
                    scale_name = 'Extinctions',
                    scale_factor = 1,
                    scale_trans = 'log10',
                    scale_type = 'sequential',
                    percent_scale = FALSE,
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)

# Reptiles, total land use
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'reptiles' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound',
                    file_name = '10scenarios_county_totalland_reptileextinction_outbound',
                    scale_name = 'Extinctions',
                    scale_factor = 1,
                    scale_trans = 'log10',
                    scale_type = 'sequential',
                    percent_scale = FALSE,
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)

# All taxa, total land use
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'total' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound',
                    file_name = '10scenarios_county_totalland_totalextinction_outbound',
                    scale_name = 'Extinctions',
                    scale_factor = 1,
                    scale_trans = 'log10',
                    scale_type = 'sequential',
                    percent_scale = FALSE,
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)

# County goods flow maps --------------------------------------------------

# Do this one programmatically
# For each good produce four paneled map figures: flow outbound relative 20 panels, flow outbound absolute 20 panels, flow outbound relative 10 panels, flow outbound absolute 10 panels 

bea_codes <- unique(county_goods_map_panels$BEA_code)

for (code in bea_codes) {
  dat <- county_goods_map_panels[BEA_code %in% code]
  bea_name <- dat$BEA_name[1]
  message(paste('Drawing maps for', bea_name))
  
  # Outbound, relative, 10 panels
  make_20panel_map_v2(map_panel_data = dat[scenario_waste %in% c('baseline', 'allavoidable')],
                      base_map = county_map,
                      region_type = 'county',
                      variable = 'outbound_vs_baseline',
                      file_name = glue('10scenarios_county_{bea_name}_outbound_vs_baseline'),
                      scale_name = 'Change vs.\nbaseline',
                      scale_factor = 1,
                      scale_trans = 'identity',
                      scale_type = 'divergent',
                      ak_idx = county_ak_idx,
                      hi_idx = county_hi_idx,
                      add_theme = theme_void() + theme(legend.position = 'none'),
                      n_waste = 2
  )
  
  # Outbound, absolute, 10 panels
  make_20panel_map_v2(map_panel_data = dat[scenario_waste %in% c('baseline', 'allavoidable')],
                      base_map = county_map,
                      region_type = 'county',
                      variable = 'flow_outbound',
                      file_name = glue('10scenarios_county_{bea_name}_outbound'),
                      scale_name = 'Production\n(million USD)',
                      scale_factor = 1e6,
                      scale_trans = 'log10',
                      scale_type = 'sequential',
                      percent_scale = FALSE,
                      ak_idx = county_ak_idx,
                      hi_idx = county_hi_idx,
                      add_theme = theme_void() + theme(legend.position = 'none'),
                      n_waste = 2
  )

}
