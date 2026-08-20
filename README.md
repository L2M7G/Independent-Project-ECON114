# ECON 114 Final Project

## Exploring the Role of Economic Integration in Inequality: A Fixed Effects and First Difference Analysis of Trade Openness and the Gini Index 

**Louisa Gallagher**  
University of California, Santa Cruz  
ECON 114 – Advanced Quantitative Methods  
March 2025


### Overview

This project examines the relationship between economic integration and income inequality using panel data for the period 1996–2019.

The analysis investigates the relationship between trade openness and the Gini index using Fixed Effects and First Difference models. The analysis also considers one-year and five-year lagged measures of trade openness.


### Repository Contents

- `ECON_114_Final_Project.pdf` – Final project paper.
- `Original_Analysis.R` – Original R script used for the analysis.
- `Bootstrap_Analysis_Refactored.R` – Refactored version of the bootstrap analysis using custom functions to reduce repeated code.
- `Data/` – Original raw and cleaned data.   


### Data

The analysis uses data from the World Bank DataBank. The primary variables include:

- Gini index
- Trade openness (imports and exports of goods and services as a percentage of GDP)
- Unemployment
- GDP per capita
- Inflation
- Tax revenue

The analysis covers the post-WTO and pre-COVID period from 1996 to 2019.


### Methods

The project uses:

- Two-way Fixed Effects models
- First Difference models
- One-year and five-year lagged specifications
- Models with and without control variables
- Bootstrap estimation with 10,000 replications
- Standard errors clustered at the country level


### Refactored Versions

The repository includes refactored versions of selected sections from the original project code. The original code is retained for transparency and reproducibility, while the refactored versions use custom functions to reduce repeated code and simplify the analysis.


#### Refactoring Notes

The refactored bootstrap analysis:
- Uses a custom function to run the `plm` models.
- Defines regression formulas separately to avoid repeatedly writing the same model specifications.
- Uses a custom function to calculate bootstrap confidence intervals.
- Uses a custom plotting function to generate the bootstrap distribution figures.
- Produces the same bootstrap analysis as the original code while reducing code repetition, making the code easier to modify and clearer to read.  

The refactored clustered standard errors analysis:
- Uses separate custom functions to calculate clustered standard errors for fixed-effects and first-difference models, accounting for differences in coefficient indexing.
- Uses a custom function to calculate confidence intervals from the estimated coefficients and clustered standard errors.
- Reuses these functions across the 1-year and 5-year lag specifications to reduce repeated calculations.
- Produces the same clustered standard errors and confidence intervals as the original code while reducing code repetition and making the analysis easier to modify and read.
