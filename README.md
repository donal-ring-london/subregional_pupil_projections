# Sub-regional pupil projections

## Introduction
This repository contains the code to run the GLA's Sub-regional Pupil Projections. At present these projections are still in development, but are due for release soon. They provide 10-year projections for all National Curriculum Years from Reception up to Year 11, and are released at ITL Level 2 regions.  

## How to run this repository
1. Clone the repository
2. The only R packages required are `data.table` and `forecast`. Ensure you have these installed.
3. Amend the script `0_a_inputs.R` if you would like to run the projections over a different time period. The default, set by the maintainer, is that the 10-year projections will be created using the most recent input data available.
4. Run the script `0_b_creating_directories.R`. This will create the directories that were rejected by the `.gitignore file`. These folders will contain processed input data, the output projections, and output plots.
5. Run the script `0_c_running_the_modelling_scripts.R`. This will run the core scripts of the repository. The input data (held on the datastore) will be read in and processed, the modelling will be carried out, and output plots of the projections will be created.
