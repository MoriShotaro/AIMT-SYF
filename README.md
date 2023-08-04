# Analysis of the role of synthetic fuels by AIM/Technology

## Introduction
- This repository includes source code for data analysis and figure production for the analysis of the role of synthetic fuels in net-zero emissions scenario by AIM/Technology.
- Datails on the analysis can be found in the following manuscript:

## How to use
- To run this script, scenario data file `scenario_data.csv` needs to be downloaded and copied to `./data/`. The instruction for the data file down loaded can be found in the Data Availability statement in the paper.
- Execute `./prog/plot.R` on the command line or main console on RStudio. The figures are generated in `./output/`.
- Following R packages are required: tidyverse, lemon, patchwork
