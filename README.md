# hybrid-geostat-ml-r

## Description
This repository provides R scripts supporting the study:
**Hybrid Geostatistics and Machine Learning in R for Petrophysical Mapping and Uncertainty**.

The scripts allow the reproduction of descriptive statistics, cross-validation performance tables, and stochastic Gaussian simulation results.

## Repository structure
hybrid-geostat-ml-r/
├── scripts/ # R scripts for tables and figures
├── data/ # Input data (not included)
├── outputs/ # Generated tables and figures (created automatically)
├── README.md
└── LICENSE


## How to run

1. Place the dataset file `TPgroupe2.xls` (or `.xlsx`) in the `data/` directory.
2. Open R or RStudio.
3. Set the working directory to the project root.
4. Run:

```r
source("scripts/run_tables.R")




