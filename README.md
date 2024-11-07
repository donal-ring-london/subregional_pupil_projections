# Sub-regional Pupil Projections

## Introduction

This repository contains the code to run the GLA's Sub-regional Pupil Projections. These projections are currently in development, with an expected release soon. They provide 10-year projections for all National Curriculum years, from Reception to Year 11, and cover ITL Level 2 regions.

## How to Run This Repository

To execute the full workflow, follow the steps below:

1. **Clone the Repository**
   Clone the repository to your local machine.

2. **Install Required Packages**
   The only R packages required are `data.table` and `forecast`. Ensure these are installed by running the `init.R` script:

```r
   source("init.R")
```

3. **Run the Project**
   You can run the entire project using the `run.R` script, which will sequentially execute all necessary scripts:

```r
   source("run.R")
```

4. **Customise the Input Parameters (Optional)**
   To adjust the time period or other parameters, modify the `0_a_inputs.R` script located in the scripts folder. By default, the script is set to create 10-year projections using the latest available input data.

5. **Generate Directories (Optional)**
   If running manually, start with `0_b_creating_directories.R` in the scripts folder. This will generate necessary directories excluded by the .gitignore file. These folders will store processed input data, projection outputs, and plots.

6. **Run Modelling Scripts (Optional)**
   Finally, execute `0_c_running_the_modelling_scripts.R` in the scripts folder to process input data, carry out modelling, and generate projection plots.
