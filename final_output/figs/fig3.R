# Figure 3, as well as some supplemental figures with bar plots comparing foreign and domestic land and biodiversity threat flows
# QDR / Virtualland / 19 Feb 2021

# Modified 30 June 2021: Cleaned up script and deleted unused code for clean repo.

source('figs/figs_v2_loaddata.R')
source('figs/load_data_foreign_maps.R')

# Summary figs comparing land and species lost -------------------------------------

# By scenario, show only baseline and all waste
p_vlt_fvsd_10 <- ggplot(all_vlt_sum[scenario_waste %in% c('baseline', 'allavoidable')], aes(x = land_type, y = VLT/100, fill = origin)) +
  facet_grid(scenario_waste ~ scenario_diet, labeller = scenario_labeller) +
  geom_col(position = 'dodge') +
  fill_fvsd +
  scale_y_continuous(name = parse(text = 'Virtual~land~consumption~(km^2/yr)'), expand = expansion(mult = c(0, 0.01))) +
  scale_x_discrete(name = 'Land use type') +
  theme(legend.position = c(0.95, 0.92), legend.background = element_blank())

p_vlt_fvsd_10 <- label_scenario_categories(p_vlt_fvsd_10)

# By scenario, only show baseline and all waste reduction.
p_plantextinction_fvsd_10 <- ggplot(all_extinction_sum[taxon %in% 'plants' & scenario_waste %in% c('baseline', 'allavoidable')], aes(x = land_use, y = extinctions, fill = origin)) +
  facet_grid(scenario_waste ~ scenario_diet, labeller = scenario_labeller) +
  geom_col(position = 'dodge') +
  fill_fvsd +
  scale_y_continuous(name = 'Plant extinctions imported to USA', expand = expansion(mult = c(0, 0.01))) +
  scale_x_discrete(name = 'Land use type') +
  theme(legend.position = c(0.95, 0.92), legend.background = element_blank())

p_plantextinction_fvsd_10 <- label_scenario_categories(p_plantextinction_fvsd_10)

p_animal_fvsd_10 <- ggplot(all_extinction_sum[kingdom %in% 'animals' & scenario_waste %in% c('baseline', 'allavoidable')], aes(x = land_use, y = extinctions, fill = origin)) +
  fill_fvsd +
  facet_grid(scenario_waste ~ scenario_diet, labeller = scenario_labeller) +
  scale_y_continuous(name = 'Animal extinctions imported to USA', expand = expansion(mult = c(0, 0.01))) +
  geom_bar(stat = 'sum', position = 'dodge', show.legend = c(fill = TRUE, size = FALSE)) +
  scale_x_discrete(name = 'Land use type') +
  theme(legend.position = c(0.95, 0.92), legend.background = element_blank())

p_animal_fvsd_10 <- label_scenario_categories(p_animal_fvsd_10)

ggsave(file.path(fp_fig, 'foreign_vs_domestic_vlt_by_scenario_10.png'), p_vlt_fvsd_10, height = 7, width = 12, dpi = 400)
ggsave(file.path(fp_fig, 'foreign_vs_domestic_plantextinctions_by_scenario_10.png'), p_plantextinction_fvsd_10, height = 7, width = 12, dpi = 400)
ggsave(file.path(fp_fig, 'foreign_vs_domestic_animalextinctions_by_scenario_10.png'), p_animal_fvsd_10, height = 7, width = 12, dpi = 400)


# Additional f vs d figs for supplement -----------------------------------

# Here show only 10 scenarios.
# All taxa summed, and each animal taxon separately.
extinction_alltaxa <- all_extinction_sum[scenario_waste %in% c('baseline', 'allavoidable'), .(extinctions = sum(extinctions)), by = .(scenario_diet, scenario_waste, land_use, origin)]

p_alltaxa_fvsd_10 <- ggplot(extinction_alltaxa, aes(x = land_use, y = extinctions, fill = origin)) +
  fill_fvsd +
  facet_grid(scenario_waste ~ scenario_diet, labeller = scenario_labeller) +
  scale_y_continuous(name = 'Total extinctions imported to USA', expand = expansion(mult = c(0, 0.01))) +
  geom_bar(stat = 'sum', position = 'dodge', show.legend = c(fill = TRUE, size = FALSE)) +
  scale_x_discrete(name = 'Land use type') +
  theme(legend.position = c(0.95, 0.92), legend.background = element_blank())

p_alltaxa_fvsd_10 <- label_scenario_categories(p_alltaxa_fvsd_10)

p_amphibian_fvsd_10 <- ggplot(all_extinction_sum[taxon %in% 'amphibians' & scenario_waste %in% c('baseline', 'allavoidable')], aes(x = land_use, y = extinctions, fill = origin)) +
  fill_fvsd +
  facet_grid(scenario_waste ~ scenario_diet, labeller = scenario_labeller) +
  scale_y_continuous(name = 'Amphibian extinctions imported to USA', expand = expansion(mult = c(0, 0.01))) +
  geom_bar(stat = 'sum', position = 'dodge', show.legend = c(fill = TRUE, size = FALSE)) +
  scale_x_discrete(name = 'Land use type') +
  theme(legend.position = c(0.95, 0.92), legend.background = element_blank())

p_amphibian_fvsd_10 <- label_scenario_categories(p_amphibian_fvsd_10)

p_bird_fvsd_10 <- ggplot(all_extinction_sum[taxon %in% 'birds' & scenario_waste %in% c('baseline', 'allavoidable')], aes(x = land_use, y = extinctions, fill = origin)) +
  fill_fvsd +
  facet_grid(scenario_waste ~ scenario_diet, labeller = scenario_labeller) +
  scale_y_continuous(name = 'Bird extinctions imported to USA', expand = expansion(mult = c(0, 0.01))) +
  geom_bar(stat = 'sum', position = 'dodge', show.legend = c(fill = TRUE, size = FALSE)) +
  scale_x_discrete(name = 'Land use type') +
  theme(legend.position = c(0.95, 0.92), legend.background = element_blank())

p_bird_fvsd_10 <- label_scenario_categories(p_bird_fvsd_10)

p_mammal_fvsd_10 <- ggplot(all_extinction_sum[taxon %in% 'mammals' & scenario_waste %in% c('baseline', 'allavoidable')], aes(x = land_use, y = extinctions, fill = origin)) +
  fill_fvsd +
  facet_grid(scenario_waste ~ scenario_diet, labeller = scenario_labeller) +
  scale_y_continuous(name = 'Mammal extinctions imported to USA', expand = expansion(mult = c(0, 0.01))) +
  geom_bar(stat = 'sum', position = 'dodge', show.legend = c(fill = TRUE, size = FALSE)) +
  scale_x_discrete(name = 'Land use type') +
  theme(legend.position = c(0.95, 0.92), legend.background = element_blank())

p_mammal_fvsd_10 <- label_scenario_categories(p_mammal_fvsd_10)

p_reptile_fvsd_10 <- ggplot(all_extinction_sum[taxon %in% 'reptiles' & scenario_waste %in% c('baseline', 'allavoidable')], aes(x = land_use, y = extinctions, fill = origin)) +
  fill_fvsd +
  facet_grid(scenario_waste ~ scenario_diet, labeller = scenario_labeller) +
  scale_y_continuous(name = 'Reptile extinctions imported to USA', expand = expansion(mult = c(0, 0.01))) +
  geom_bar(stat = 'sum', position = 'dodge', show.legend = c(fill = TRUE, size = FALSE)) +
  scale_x_discrete(name = 'Land use type') +
  theme(legend.position = c(0.95, 0.92), legend.background = element_blank())

p_reptile_fvsd_10 <- label_scenario_categories(p_reptile_fvsd_10)

ggsave(file.path(fp_fig, 'foreign_vs_domestic_alltaxaextinctions_by_scenario_10.png'), p_alltaxa_fvsd_10, height = 7, width = 12, dpi = 400)
ggsave(file.path(fp_fig, 'foreign_vs_domestic_amphibianextinctions_by_scenario_10.png'), p_amphibian_fvsd_10, height = 7, width = 12, dpi = 400)
ggsave(file.path(fp_fig, 'foreign_vs_domestic_birdextinctions_by_scenario_10.png'), p_bird_fvsd_10, height = 7, width = 12, dpi = 400)
ggsave(file.path(fp_fig, 'foreign_vs_domestic_mammalextinctions_by_scenario_10.png'), p_mammal_fvsd_10, height = 7, width = 12, dpi = 400)
ggsave(file.path(fp_fig, 'foreign_vs_domestic_reptileextinctions_by_scenario_10.png'), p_reptile_fvsd_10, height = 7, width = 12, dpi = 400)


# Grand totals of foreign and domestic extinctions with stacked bars --------------------

# 2x2 land x species, no waste reduction x all waste reduction
# Each one will have 5 bars.
# Use ggpattern package to fill in bars with patterns.

# animals + plants x foreign + domestic, ignore land use.
extinction_grandtotals <- all_extinction_sum[, .(extinctions = sum(extinctions)), by = .(scenario_diet, scenario_waste, origin, kingdom)]

library(ggpattern)


p_ext_grandtotals_10 <- ggplot(extinction_grandtotals[scenario_waste %in% c('baseline','allavoidable')], aes(y = extinctions, x = scenario_diet, pattern = origin, fill = kingdom)) +
  facet_grid(. ~ scenario_waste, labeller = scenario_labeller) +
  geom_bar_pattern(position = 'stack', stat = 'identity', pattern_fill = 'black', pattern_spacing = 0.02, color = 'black') +
  scale_x_discrete(name = 'diet scenario', labels = diet_long_names$medium_name) +
  scale_y_continuous(name = 'species committed to extinction', expand = expansion(mult = c(0, 0.01))) +
  scale_pattern_manual(values = c('circle', 'none')) +
  scale_fill_manual(values = as.character(okabe_colors[c('reddishpurple', 'bluishgreen')])) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        strip.text = element_text(size = rel(0.7)),
        legend.title = element_text(size = rel(0.8)),
        legend.position = c(0.93, 0.78),
        legend.background = element_blank(),
        legend.spacing.y = unit(0.1, 'cm')) +
  guides(fill = guide_legend(override.aes = list(pattern = 'none')))

p_land_grandtotals_10 <- ggplot(all_vlt_sum[!land_type %in% 'total' & scenario_waste %in% c('baseline','allavoidable')], aes(y = VLT/1e6, x = scenario_diet, pattern = origin, fill = land_type)) +
  facet_grid(. ~ scenario_waste, labeller = scenario_labeller) +
  geom_bar_pattern(position = 'stack', stat = 'identity', pattern_fill = 'black', pattern_spacing = 0.02, color = 'black') +
  scale_pattern_manual(values = c('circle', 'none')) +
  scale_x_discrete(name = 'diet scenario', labels = diet_long_names$medium_name) +
  scale_y_continuous(name = 'land footprint (million ha)', expand = expansion(mult = c(0, 0.01))) +
  scale_fill_manual(name = 'land use', values = as.character(okabe_colors[c('orange', 'blue', 'vermillion')])) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        strip.text = element_text(size = rel(0.7)),
        legend.title = element_text(size = rel(0.8)),
        legend.position = c(0.93, 0.71),
        legend.background = element_blank(),
        legend.spacing.y = unit(0.1, 'cm')) +
  guides(fill = guide_legend(override.aes = list(pattern = 'none')))

p_top <- p_land_grandtotals_10 +
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank(), 
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(0.8)),
        legend.key.size = unit(0.5, 'cm'),
        legend.position = c(0.93, 0.71),
        legend.background = element_blank(),
        legend.spacing.y = unit(0.05, 'cm')) 
p_bottom <- p_ext_grandtotals_10 +
  theme(legend.key.size = unit(0.5, 'cm'),
        strip.background = element_blank(), 
        strip.text = element_blank()) +
  guides(pattern = FALSE)

# Figure 3 in MS
png(file.path(fp_fig, 'foreign_vs_domestic_10scenarios_grandtotals.png'), height = 7, width = 7, res = 400, units = 'in')
  grid.draw(gridExtra::gtable_rbind(ggplotGrob(p_top), ggplotGrob(p_bottom)))
dev.off()

