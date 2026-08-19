# ECON 114 Final Project

## Exploring the Role of Economic Integration in Inequality

**Louisa Gallagher**  
University of California, Santa Cruz  
ECON 114 – Econometrics  
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
...

