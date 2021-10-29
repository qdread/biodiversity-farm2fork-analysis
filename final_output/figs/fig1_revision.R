# Figure 1
# QDR 30 June 2021

source(file.path(code_path, 'final_output/figs/load_data_all_figs.R'))

# Diet differences among scenarios ----------------------------------------

# Show by the "Group" column in lafa_joined
# Need to assign the missing ones to groups.
missing_groups <- data.frame(Category = c("White and whole wheat flour", "Durum flour", "Fresh brussels sprouts", "Other processed vegetables"),
                    Group = c('grain', 'grain', 'veg', 'veg'))
idx <- match(missing_groups$Category, lafa_joined$Category)
lafa_joined$Group[idx] <- missing_groups$Group

lafa_cal_by_diet <- lafa_joined %>%
  select(Category, Group, calories_available_cal_day, planetary_health, us_style, med_style, vegetarian) %>%
  mutate(baseline = rep(1, nrow(.))) %>%
  pivot_longer(planetary_health:baseline, names_to = 'diet', values_to = 'factor') %>%
  mutate(calories_available_cal_day = calories_available_cal_day * factor) %>%
  rename(food = Category, food_group = Group)

# Split up meat, fish, eggs, and nuts
fish_names <- c("Fresh and frozen fish", "Fresh and frozen shellfish", "Canned Salmon", "Canned Sardines", "Canned Tuna", "Canned shellfish", "Other canned fish", "Cured fish")
nut_names <- c("Peanuts", "Almonds", "Hazelnuts", "Pecans", "Walnuts", "Macadamia", "Pistachios", "Other tree nuts", "Coconut")

lafa_cal_summ <- lafa_cal_by_diet %>%
  mutate(food_group = case_when(food_group == 'veg' ~ 'vegetables',
                                food %in% nut_names ~ 'nuts',
                                food %in% fish_names ~ 'fish',
                                food_group == 'meat' ~ 'meat/eggs',
                                TRUE ~ food_group)) %>%
  group_by(diet, food_group) %>%
  summarize(calories_day = sum(calories_available_cal_day)) %>%
  ungroup %>%
  mutate(diet = factor(gsub('_', '', diet), levels = diet_levels_ordered))
  
# Goods consumption differences among scenarios ---------------------------

# This is by BEA code across diet change and waste change scenarios.
# It also accounts for the "footprint" of consumption, so for instance meat consumption would reflect the consumption of feed (need to check this)

# Reorder factors in loaded CSV.
totaldemand_sums <- totaldemand_sums %>%
  mutate(short_name = factor(short_name, levels = unique(short_name)),
         scenario_diet = factor(scenario_diet, levels = unique(scenario_diet)),
         scenario_waste = factor(scenario_waste, levels = unique(scenario_waste)))
		 
# Relative to baseline. Must reconstruct the scenarios again.
totaldemand_relative <- totaldemand_sums %>%
  pivot_wider(names_from = c(scenario_diet, scenario_waste), values_from = demand) %>%
  mutate(across(where(is.numeric), ~ . / baseline_baseline)) %>%
  pivot_longer(-c(BEA_code, short_name, kingdom), names_to = 'scenario', values_to = 'demand') %>%
  separate(scenario, into = c('scenario_diet', 'scenario_waste'), sep = '_') %>%
  mutate(scenario_diet = factor(scenario_diet, levels = diet_levels_ordered),
         scenario_waste = factor(scenario_waste, levels = waste_levels_ordered))
		 
# Draw figure -------------------------------------------------------------

lafa_cal_summ <- lafa_cal_summ %>% mutate(food_group = if_else(food_group == 'fat', 'added fats', food_group))
foodgroups_ordered <- c('grain', 'fruit', 'nuts', 'vegetables', 'sugar', 'meat/eggs', 'dairy', 'fish', 'added fats')
p_consprod_top <- ggplot(lafa_cal_summ %>% mutate(food_group = factor(food_group, levels = foodgroups_ordered)), aes(y = calories_day, x = food_group, color = diet, fill = diet, group = diet)) +
  geom_col(position = 'dodge', color = 'black', size = 0.1) +
  scale_fill_manual(values = unname(okabe_colors[c(1,7,3,4,5)]), labels = diet_long_names$medium_name) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.03)), name = 'calories per person per day') +
  scale_x_discrete(name = 'food group') +
  theme_classic() +
  theme(legend.position = c(0.65, 0.85),
        legend.text = element_text(size = rel(.65)),
        legend.background = element_blank(),
        legend.key.size = unit(0.5, 'cm'),
        legend.margin=margin(t=0, r=1, b=1, l=1, unit="mm")) +
  guides(fill = guide_legend(ncol = 3, title = NULL))

p_consprod_bottom <- ggplot(totaldemand_relative %>% filter(scenario_waste %in% c('baseline', 'allavoidable'))) +
  geom_col(aes(x = short_name, y = demand, color = scenario_diet, fill = scenario_diet, group = scenario_diet), position = 'dodge', color = 'black', size = 0.1) +
  geom_hline(yintercept = 1, linetype = 'dotted', color = 'black', size = 0.5) +
  geom_hline(yintercept = 0, color = 'black', size = 0.5) +
  geom_text(data = data.frame(scenario_waste = factor(c('baseline', 'allavoidable'), levels = c('baseline', 'allavoidable')), label = paste('waste scenario:', waste_long_names$long_name[c(1,4)])), aes(label = label), x = Inf, y = Inf, hjust = 1.01, vjust = 1.3) +
  scale_fill_manual(name = 'diet', values = unname(okabe_colors[c(1,7,3,4,5)]), labels = diet_long_names$medium_name) +
  facet_wrap(~ scenario_waste, nrow = 2) +
  scale_x_discrete(name = 'primary agricultural product') +
  scale_y_continuous(expand = expansion(mult = c(0, 0.03)), name = 'production relative to baseline') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = rel(0.9)),
        legend.position = 'none',
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  guides(fill=guide_legend(nrow = 2, byrow = FALSE))

png(file.path(fp_fig, 'fig1.png'), height = 9, width = 6, res = 400, units = 'in')
grid.arrange(p_consprod_top, p_consprod_bottom, nrow = 2, heights = c(1, 2.6))
dev.off()

pdf(file.path(fp_fig, 'pdfs/fig1.pdf'), height = 9, width = 6)
grid.arrange(p_consprod_top, p_consprod_bottom, nrow = 2, heights = c(1, 2.6))
dev.off()
