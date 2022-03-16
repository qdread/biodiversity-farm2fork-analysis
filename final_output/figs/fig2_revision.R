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

# Redo transformation of AK and HI
county_map <- st_read(file.path(spatial_output_path, 'USA_county_2014_aea.gpkg')) %>% rename(county = fips) # High-res map

rot = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

crs_lambert <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"

county_map_trans <- county_map %>%
  st_transform(crs = crs_lambert)

alaska <- county_map_trans %>% filter(fips_state %in% '02')
alaska_g <- st_geometry(alaska)
alaska_centroid <- st_centroid(st_union(alaska_g))

alaska_trans <- (alaska_g - alaska_centroid) * rot(-39 * pi/180) / 2.3 + alaska_centroid + c(1000000, -5000000)
alaska <- st_set_geometry(alaska, alaska_trans) %>% st_set_crs(st_crs(county_map_trans))


hi_crs <- '+proj=aea +lat_1=8 +lat_2=18 +lat_0=3 +lon_0=-157 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs'
hi_box <- c(xmin = -400000, ymin = 1761000, xmax = 230000, ymax = 2130000)

hawaii <- county_map %>% 
  filter(fips_state %in% '15') %>%
  st_transform(crs = hi_crs) %>%
  st_crop(hi_box) %>%
  st_transform(crs = crs_lambert)

hawaii_g <- st_geometry(hawaii)
hawaii_centroid <- st_centroid(st_union(hawaii_g))

hawaii_trans <- (hawaii_g - hawaii_centroid) * rot(-35 * pi/180) + hawaii_centroid + c(5200000, -1400000)
hawaii <- st_set_geometry(hawaii, hawaii_trans) %>% st_set_crs(st_crs(county_map_trans))

county_map <- county_map_trans %>%
  filter(!fips_state %in% c('02', '15')) %>%
  rbind(alaska) %>%
  rbind(hawaii)

# Convert land from square km. back to hectares (multiply by 100)
domestic_land_toplot <- county_land_flow_sums[scenario_diet == 'baseline' & scenario_waste == 'baseline',
                                              .(VLT = sum(flow_outbound_domestic * 100, na.rm = TRUE)),
                                              by = .(county)]
domestic_ext_toplot <- county_extinction_flow_sums[scenario_diet == 'baseline' & scenario_waste == 'baseline',
                                                   .(VBT = sum(flow_outbound_domestic, na.rm = TRUE)),
                                                   by = .(county)]
foreign_land_toplot <- foreign_land_flow_sums[scenario_diet == 'baseline' & scenario_waste == 'baseline',
                                              .(VLT = sum(flow_outbound_foreign * 100, na.rm = TRUE)),
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
  scale_fill_viridis_c(name = 'Foreign biodiversity\nfootprint (extinctions) ', trans = 'log10', breaks = scale_breaks, labels = scale_labels, na.value = 'gray80') +
  leg_longbottom_theme

png(file.path(fp_fig, 'fig2.png'), height=8+90+90,width=8+120+120,res=400,units='mm')
grid.newpage()
grid.draw(gtable_cbind(
  gtable_rbind(ggplotGrob(p_dland), ggplotGrob(p_fland)),
  gtable_rbind(ggplotGrob(p_dext), ggplotGrob(p_fext))))
dev.off()

pdf(file.path(fp_fig, 'pdfs/fig2.pdf'), height=(8+90+90)/25.4,width=(8+120+120)/25.4)
grid.draw(gtable_cbind(
  gtable_rbind(ggplotGrob(p_dland), ggplotGrob(p_fland)),
  gtable_rbind(ggplotGrob(p_dext), ggplotGrob(p_fext))))
dev.off()
