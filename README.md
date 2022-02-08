# Biodiversity: Farm2Fork

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5949590.svg)](https://doi.org/10.5281/zenodo.5949590)

This repository contains code to reproduce all analysis presented in the manuscript **Biodiversity effects of food system sustainability actions from farm to fork**, and the data for the associated [Shiny app](https://qdread.shinyapps.io/biodiversity-farm2fork).

**Citation (to be updated when final)**: Read, Quentin D., Kelly L. Hondula, and Mary K. Muth. Biodiversity effects of food system sustainability actions from farm to fork. *Proceedings of the National Academy of Sciences*, accepted.

## Organization of repository

The code is organized into subdirectories containing numbered scripts. To reproduce all the analysis presented in the manuscript, the scripts should be run in order.

### Setting file paths

The script `00_set_file_paths.R` in the root directory of this repository should be run first. This sets up file paths pointing to the code and raw data, and a path where the intermediate and final outputs will be saved.

### Spatial data processing

The `spatial_data_processing` subdirectory contains an RMarkdown notebook, `01_spatial_processing.Rmd`. Running the code in this notebook will do all the processing of spatial data required for the analysis. There are also several Bash and Python scripts that are necessary to run the code in the notebook.

### Main data analysis pipeline

The main data analysis pipeline, for the USA-based domestic analysis, is in the `main_data_analysis` subdirectory, which contains R scripts numbered from `02` to `18`.

### Foreign imports analysis pipeline

The subdirectory `foreign_import_analysis` contains R scripts `19` through `24`, which supplement the main results with results for foreign-sourced virtual imports of land and biodiversity threats.

### Creation of visualizations and other output

The subdirectory `final_outputs` contains two additional data processing scripts. `25_create_summary_data.R` produces final CSV outputs needed to create figures and tables from the raw results. `26_compile_Shiny_data.R` does additional processing for the data object used in the Shiny app.

After running those scripts, scripts to produce the visualizations are found in three additional subdirectories:

- `figs`: scripts to produce figures shown in manuscript. The scripts that create the figures are named `fig1.R`, etc., and `supplemental_figs_xxxx.R`, etc. These scripts source other scripts in the `figs` directory which load the needed data and functions.
- `tables`: scripts to produce the tables shown in the supplements.
- `supplements`: RMarkdown notebooks for the supplementary figures and tables documents.

## Availability of data

The raw data are archived on a [Figshare repository](https://doi.org/10.6084/m9.figshare.14892087).

All the data on the Figshare repository are included as compressed `.zip` files and will need to be decompressed before running the code. If all the `.zip` files are extracted into the same root directory, this will maintain the file tree structure used in the code.

The file names and paths to the data within the archive are listed in this repository `docs/data_filenames.csv`.

In addition, a few functions are called from an R package `Rutilitybelt` that I wrote. The source for this package is included in the root directory of
this repo, called `Rutilitybelt_0.0.0.9000.tar.gz`. It is installed from source as part of script `00`. It can also be installed from source using the
command `R CMD INSTALL Rutilitybelt_0.0.0.9000.tar.gz`.

*last modified by QDR, 04 February 2022*
