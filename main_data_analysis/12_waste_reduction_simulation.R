# Simulate waste reduction at different stages by 50%
# Add to existing "diet simulation" data frame
# QDR / Virtual land / 5 Jan 2021

# Modified 21 Jan 2021: Debug, correcting error in how production factors are gotten
# Modified 27 Jan 2022: Conservatively reduce primary-to-retail losses to zero for all foods; apply correct baseline.

library(tidyverse)

fp_diet <- file.path(data_path, 'dietary_guidelines')

lafa_df_joindiets <- read_csv(file.path(intermediate_output_path, 'lafa_joined_with_diet_proportions.csv'))

# Where are values missing? Looks like only in a few minor canned fruit categories. Replace zeroes where needed.
lafa_df_joindiets %>% select("Category", "Group", starts_with("loss")) %>% filter(!complete.cases(.))

# The canned and frozen fruits just have a NA in primary loss. It can be replaced with zero.
# Other processed vegetables has a total loss rate of 82%. Use the average rate for processed vegetables to assign the missing values there.
lafa_df_joindiets <- lafa_df_joindiets %>%
  mutate(loss_primary_to_retail_percent = if_else(is.na(loss_primary_to_retail_percent) & Group == 'fruit', 0, loss_primary_to_retail_percent))

# Subset processed vegetables and average the rates
processedveg <- lafa_df_joindiets %>% 
  filter(Group == 'veg' & !grepl("Fresh", Category, ignore.case = TRUE)) %>%
  select(starts_with("loss") & ends_with("percent"))
processedveg_meanloss <- apply(processedveg, 2, mean)

# Calculate mean rate for each stage for other processed, given the total loss rate.
# Apply it to the other processed vegetables category.
lafa_df_joindiets[lafa_df_joindiets$Category == 'Other processed vegetables', names(processedveg_meanloss)] <- as.list(processedveg_meanloss * lafa_df_joindiets$loss_total_percent[lafa_df_joindiets$Category == 'Other processed vegetables']/processedveg_meanloss['loss_total_percent'])

# Conservatively reduce primary-to-retail losses to zero for all foods 
# (we do not know how much of this is avoidable so it is most conservative to set it to zero)
lafa_df_joindiets <- lafa_df_joindiets %>% mutate(loss_primary_to_retail_percent = 0)

# Calculate avoidable pre-consumer waste (primary and retail combined) and avoidable consumer waste (consumer waste with unavoidable removed) rates
# Avoidable consumer waste already calculated as loss_consumer_other_percent
lafa_df_joindiets <- lafa_df_joindiets %>%
  mutate(loss_preconsumer_percent = (1 - (100 - loss_primary_to_retail_percent)/100 * (100 - loss_retail_to_consumer_percent)/100)*100,
         loss_allavoidable_percent = (1 - (100 - loss_preconsumer_percent)/100 * (100 - loss_consumer_other_percent)/100)*100)

# Use existing waste rates to calculate the production factor needed if waste is reduced by 50%
# Production factor is (1-base waste / (1-basewaste/2))

# function if baseline waste rate and counterfactual waste rate are expressed as a percentage
calc_prod_factor <- function(w_base, w_new) (1 - w_base/100) / (1 - w_new/100)

# Recalculate the total waste rate when modifying (1) preconsumer avoidable waste, (2) consumer avoidable waste, and (3) both
# Based on this, calculate the production factor.
lafa_df_joindiets <- lafa_df_joindiets %>%
  mutate(loss_total_percent_preconsumerreduced = 100 * (1 - (1 - loss_preconsumer_percent/200)*(1 - loss_consumer_other_percent/100)),
         loss_total_percent_consumerreduced = 100 * (1 - (1 - loss_preconsumer_percent/100)*(1 - loss_consumer_other_percent/200)),
         loss_total_percent_bothreduced = 100 * (1 - (1 - loss_preconsumer_percent/200)*(1 - loss_consumer_other_percent/200)),
         preconsumer_waste_reduction_prod_factor = calc_prod_factor(loss_allavoidable_percent, loss_total_percent_preconsumerreduced),
         consumer_waste_reduction_prod_factor = calc_prod_factor(loss_allavoidable_percent, loss_total_percent_consumerreduced),
         allavoidable_waste_reduction_prod_factor = calc_prod_factor(loss_allavoidable_percent, loss_total_percent_bothreduced))

# Calculate production factors for diet x waste
# Just multiply the appropriate factors by one another. This will result in 12 additional scenarios because we have 4 alternative diets x 3 waste reduction scenarios
diet_cols <- c('planetary_health', 'us_style', 'med_style', 'vegetarian')
waste_cols <- c('preconsumer_waste_reduction_prod_factor', 'consumer_waste_reduction_prod_factor', 'allavoidable_waste_reduction_prod_factor')

cols_df <- expand_grid(diet_cols, waste_cols) %>% mutate(newname = pmap_chr(., function(diet_cols, waste_cols) paste(diet_cols, gsub('_waste_reduction_prod_factor', '', waste_cols), sep = '_x_')))

for (i in 1:nrow(cols_df)) {
  name_i <- cols_df$newname[i]
  new_values <- lafa_df_joindiets[, cols_df$diet_cols[i]] * lafa_df_joindiets[, cols_df$waste_cols[i]]
  lafa_df_joindiets <- lafa_df_joindiets %>%
    mutate(!!name_i := deframe(new_values))
}

# Write the data frame with all production factors to CSV
write_csv(lafa_df_joindiets, file.path(intermediate_output_path, 'lafa_with_production_factors_diet_x_waste.csv'))

# Edit 27 January 2022: we no longer need separate factors for foreign imports because all use the same factors.
