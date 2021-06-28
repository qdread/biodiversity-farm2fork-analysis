# Biodiversity: Farm2Fork

Code to reproduce all analysis presented in the manuscript **Biodiversity effects of food system sustainability actions from farm to fork**, and the data for the associated Shiny app (put link to app here when complete).

**Citation (to be updated when final)**: Read, Quentin D., Kelly L. Hondula, and Mary K. Muth. Biodiversity effects of food system sustainability actions from farm to fork. *PNAS*, in preparation.

## Organization of repository

The code is organized into subdirectories containing numbered scripts. To reproduce all the analysis presented in the manuscript, the scripts should be run in order.

### Spatial data processing

The `spatial_data_processing` subdirectory contains an RMarkdown notebook, `01_spatial_processing.Rmd` that should be run first. There are also several Bash and Python scripts that are necessary to run the code in the notebook.

### Main data analysis pipeline

The main data analysis pipeline, for the USA-based domestic analysis, is in the `main_data_analysis` subdirectory, which contains R scripts numbered from `02` to `18`.

### Foreign imports analysis pipeline

The subdirectory `foreign_import_analysis` contains R scripts `19` through `24`, which supplement the main results with results for foreign-sourced virtual imports of land and biodiversity threats.

### Creation of visualizations and other output

The subdirectory `final_outputs` contains three additional subdirectories:

- `figs`: scripts to produce figures shown in manuscript. First, the three scripts called `figs_v2_summarydata*.R` should be run to produce the final CSV outputs from the raw results, which are needed to create the figures.
- `tables`: scripts to produce the tables shown in the supplements.
- `supplements`: RMarkdown notebooks for the supplementary figures and tables documents.

There is also an R script `compile_Shiny_data.R` which does additional processing for the data object used in the Shiny app.

*last modified by QDR, 28 Jun 2021*
