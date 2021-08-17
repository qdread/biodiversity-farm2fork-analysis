# INITIAL SCRIPT TO SET FILE PATHS
# To run all code in all pipelines in the biodiversity-farm2fork-analysis repo
# 02 July 2021

# All code should be run from the root directory of the GitHub repo.
# All data from the Figshare repo should be unzipped in a data directory.
# All final output is written to a separate directory.


# Define base paths -------------------------------------------------------

# Modify this section of the script as necessary.

code_path <- '~'

data_path <- '~/data/'

output_path <- '~/output/'


# Define subdirectories within base paths ---------------------------------

# Do not modify this section of the script.

# Subdirectories within data path
fp_crosswalk <- file.path(data_path, 'crossreference_tables')


# Subdirectories within output path
intermediate_output_path <- file.path(output_path, 'intermediate_output')
final_output_path <- file.path(output_path, 'final_output')
spatial_output_path <- file.path(output_path, 'spatial_output')

fp_fig <- file.path(final_output_path, 'figs')

# Create output paths if they do not exist --------------------------------

if (!dir.exists(output_path)) dir.create(output_path)
if (!dir.exists(intermediate_output_path)) dir.create(intermediate_output_path)
if (!dir.exists(final_output_path)) dir.create(final_output_path)
if (!dir.exists(spatial_output_path)) dir.create(spatial_output_path)
if (!dir.exists(fp_fig)) dir.create(fp_fig)
if (!dir.exists(file.path(intermediate_output_path, 'faostat_processed'))) dir.create(file.path(intermediate_output_path, 'faostat_processed'))
if (!dir.exists(file.path(intermediate_output_path, 'county_consumption_csvs'))) dir.create(file.path(intermediate_output_path, 'county_consumption_csvs'))
if (!dir.exists(file.path(intermediate_output_path, 'county_land_consumption_csvs'))) dir.create(file.path(intermediate_output_path, 'county_land_consumption_csvs'))
if (!dir.exists(file.path(intermediate_output_path, 'ecoregion_landflow_csvs'))) dir.create(file.path(intermediate_output_path, 'ecoregion_landflow_csvs'))
if (!dir.exists(file.path(intermediate_output_path, 'county_state_extinction_csvs'))) dir.create(file.path(intermediate_output_path, 'county_state_extinction_csvs'))


# Install Rutilitybelt package from source if needed ----------------------

# This package has a few utility functions that are used for manipulating data.tables.
if (!require(Rutilitybelt)) install.packages(file.path(code_path, 'Rutilitybelt_0.0.0.9000.tar.gz'), repos = NULL, type = 'source')
