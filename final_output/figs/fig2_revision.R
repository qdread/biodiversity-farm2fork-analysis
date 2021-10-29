# Modified version of Figure 2 for revision
# QDR / Virtualland / 15 Oct 2021


# Load data and subset baseline data for plotting -------------------------

library(data.table)
library(sf)
library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)

load(file.path(final_output_path, 'all_app_data.RData'))

domestic_land_toplot <- county_land_flow_sums[scenario_diet == 'baseline' & scenario_waste == 'baseline',
                                              .(VLT = sum(flow_outbound_domestic, na.rm = TRUE)),
                                              by = .(county)]
domestic_ext_toplot <- county_extinction_flow_sums[scenario_diet == 'baseline' & scenario_waste == 'baseline',
                                                   .(VBT = sum(flow_outbound_domestic, na.rm = TRUE)),
                                                   by = .(county)]
foreign_land_toplot <- foreign_land_flow_sums[scenario_diet == 'baseline' & scenario_waste == 'baseline',
                                              .(VLT = sum(flow_outbound_foreign, na.rm = TRUE)),
                                              by = .(ISO_A3)]
foreign_ext_toplot <- foreign_extinction_flow_sums[scenario_diet == 'baseline' & scenario_waste == 'baseline',
                                                   .(VBT = sum(flow_outbound_foreign, na.rm = TRUE)),
                                                   by = .(ISO_A3)]


# Set up world map projection ---------------------------------------------

# Filter country map to get rid of very southern latitudes
extent_countries <- st_bbox(c(xmin = -180, ymin = -58, xmax = 180, ymax = 84), crs = "+proj=longlat +ellps=WGS84") %>%
  st_as_sfc
poly_countries <- geosphere::makePoly(st_coordinates(extent_countries)[, c('X', 'Y')])
poly_countries <- st_polygon(list(poly_countries)) %>% st_sfc
st_crs(poly_countries) <- st_crs(extent_countries)
poly_countries <- st_transform(poly_countries, crs = "+proj=robin")

country_map_toplot <- global_country_map %>%
  st_transform(crs = "+proj=robin") %>%
  filter(!country_name %in% "Antarctica")

# Join data with maps and draw maps ---------------------------------------

# Note: land flows are already in units of ha for both.

scale_breaks = scales::trans_breaks("log10", function(x) 10^x)
scale_labels = scales::trans_format("log10", scales::math_format(10^.x))

leg_longbottom_theme <- theme_void() + theme(legend.position = 'bottom',
                                             legend.title = element_text(size = rel(1)),
                                             legend.key.width = unit(0.3, 'in'))

p_dland <- ggplot(left_join(county_map, domestic_land_toplot)) +
  geom_sf(aes(fill = VLT), color = NA) +
  scale_fill_viridis_c(name = 'Domestic land\nfootprint (ha)', trans = 'log10', breaks = scale_breaks, labels = scale_labels, na.value = 'gray80') +
  leg_longbottom_theme

p_fland <- ggplot(left_join(country_map_toplot, foreign_land_toplot)) +
  geom_sf(aes(fill = VLT), color = NA) +
  geom_sf(data = st_geometry(poly_countries), fill = NA) +
  scale_fill_viridis_c(name = 'Foreign land\nfootprint (ha)', trans = 'log10', breaks = scale_breaks, labels = scale_labels, na.value = 'gray80') +
  leg_longbottom_theme

p_dext <- ggplot(left_join(county_map, domestic_ext_toplot)) +
  geom_sf(aes(fill = VBT), color = NA) +
  scale_fill_viridis_c(name = 'Domestic biodiversity\nfootprint (extinctions)', trans = 'log10', breaks = scale_breaks, labels = scale_labels, na.value = 'gray80') +
  leg_longbottom_theme

p_fext <- ggplot(left_join(country_map_toplot, foreign_ext_toplot)) +
  geom_sf(aes(fill = VBT), color = NA) +
  geom_sf(data = st_geometry(poly_countries), fill = NA) +
  scale_fill_viridis_c(name = 'Foreign biodiversity\nfootprint (extinctions)', trans = 'log10', breaks = scale_breaks, labels = scale_labels, na.value = 'gray80') +
  leg_longbottom_theme

png(file.path(fp_fig, 'fig2.png'), height=8+90+90,width=8+120+120,res=400,units='mm')
grid.newpage()
grid.draw(gtable_cbind(
  gtable_rbind(ggplotGrob(p_dland), ggplotGrob(p_fland)),
  gtable_rbind(ggplotGrob(p_dext), ggplotGrob(p_fext))))
dev.off()

pdf(file.path(fp_fig, 'pdfs/fig3.pdf'), height=(8+90+90)/25.4,width=(8+120+120)/25.4)
grid.draw(gtable_cbind(
  gtable_rbind(ggplotGrob(p_dland), ggplotGrob(p_fland)),
  gtable_rbind(ggplotGrob(p_dext), ggplotGrob(p_fext))))
dev.off()
