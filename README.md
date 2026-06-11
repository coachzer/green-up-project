# Waste Data Analysis in Slovenia

## Project Overview

This project focuses on analyzing statistical data available in Slovenia regarding companies producing specific types of waste and those responsible for the uptake and processing of this waste. The datasets span several years and include information on generated waste, collected waste, and how the waste is utilized. This data is broken down by companies (up to 2019) or regions (from 2019 onwards) within Slovenia.

## How to Run

### Dashboard (Shiny app)

From the repo root in R:

```r
shiny::runApp("Interface/Visualization")
```

or open `Interface/Visualization/server.R` in RStudio and click **Run App**. Required packages are listed in `Interface/Visualization/global.R`.

### Rebuilding the data

1. Place the raw SURS Excel files for the new year in a `<year> data/` directory (follow the structure of `2024 data/`).
2. Render `green-up-project.qmd` (cleans raw data into per-year filtered CSVs).
3. Render `analysis.qmd` (builds combined datasets and analysis outputs).
4. The dashboard reads the combined CSVs from `Interface/Visualization/data/`.

### Deployment

The app is deployed to shinyapps.io via `rsconnect` (app: `WoodWasteVisualization`).

## Project Structure

The data is organized into datasets for each year, categorized as follows:

1. **Generated Waste**
2. **Collected Waste**
3. **Waste Usage**

## Objective

The main objective is to understand the differences in data availability and structure across various years and to extract specific information about companies and regions involved in the production, collection, and processing of wood waste.

## Initial Questions

1. **Companies Producing Wood Waste (Up to 2019)**
   - Identify companies producing wood waste up to 2019.
   - Account for variations during COVID years.
   - Determine the location or municipality where the waste is produced.

2. **Regions Producing Wood Waste (From 2019 onwards)**
   - Identify regions producing wood waste from 2019 to the most recent year.
   - Account for variations during COVID years.
   - Determine the location or municipality where the waste is produced.

3. **Companies Collecting and Processing Waste**
   - Identify companies responsible for the collection and processing of the waste (up to 2019).
   
4. **Waste Processing and Usage**
   - Analyze how the waste is processed and utilized.

## Waste Codes

The analysis will focus on the following waste codes:

- **03 01 ??** (excluding 03 01 04\*)
- **15 01 03**: Wooden packaging
- **15 01 01**: Paper packaging
- **17 02 01**: Wood (construction waste)
- **20 01 38**: Wood not specified under other categories
- **20 01 37**: Wood containing hazardous substances

## Data Analysis Steps

1. **Data Collection and Cleaning**
   - Gather datasets for each year.
   - Clean the data to ensure consistency and accuracy.

2. **Data Comparison**
   - Compare data structures across different years to identify variations.
   - Normalize data where necessary to facilitate comparison.

3. **Company and Regional Identification**
   - Use waste codes to filter and identify companies producing and processing wood waste (up to 2019).
   - Identify regions producing wood waste (from 2019 onwards).
   - Determine the location of these companies and regions.

4. **Analysis of Waste Processing**
   - Study the methods and processes used by companies to handle and utilize the waste.

## Project Files

### green-up-project.qmd

This file focuses on cleaning the raw data obtained from the SURS website. It includes steps for data wrangling and preprocessing to ensure that the data is in a consistent and analyzable format.

### analysis.qmd

This file conducts further analysis on the cleaned data from the `green-up-project.qmd` file. It includes various statistical and graphical analyses to extract insights and trends from the data.

### Interface/Visualization (ui.R and server.R)

These files are part of a Shiny app designed for Wood Waste Management Simulation. The app provides an interactive interface for visualizing and simulating wood waste management scenarios, helping stakeholders to make informed decisions.

## Tools and Technologies

- **R and RStudio**: For data analysis and visualization.

- **tidyverse**: For data manipulation and analysis.
- **gt**: For creating beautiful tables.
- **gtExtras**: For enhancing gt tables.
- **readxl**: For reading Excel files.
- **ggplot2**: For data visualization.
- **purrr**: For functional programming and iteration.
- **openxlsx**: For writing Excel files.
- **scales**: For scaling and formatting data in visualizations.
- **rlang**: For enhanced programming capabilities in R.
- **shiny**: For building the interactive web dashboard.
- **shinydashboard**: For the dashboard layout and UI components.
- **plotly**: For interactive plots and charts.
- **DT**: For interactive data tables.
- **sf**: For spatial data handling and map visualizations.
- **simmer**: For discrete-event simulation of waste flows.
- **igraph**: For network graph computations.
- **bslib**: For theming and Bootstrap-based UI customization.

## Data Notes

### Unattributed-region rows (NEOPREDELJENO / NA region)

Some combined CSVs include rows where the statistical region is unattributed (`NEOPREDELJENO` or an empty/`NA` region value); others have these rows excluded at source:

- **Excluded at source:** `gnr_combined.csv` and the 2016 rows of `trt_treatment_combined.csv` (the 2016 treatment-of-waste source file omits 3,759.2 t of unattributed waste — roughly 0.58% of that year's treatment total).
- **Included in CSV, present at runtime:** `coll_received_combined.csv` carries 6 rows (2016) whose region is the literal string `NA`; `readr::read_csv` parses these as missing values and no filter removes them, so they are included in the collection tab's totals (~0.6% of 2016).
- **Included in CSV, dropped by `drop_na()`:** `trt_collected_combined.csv` carries 5 rows with the literal string `NA` as region for 2016; `readr::read_csv` parses these as `NA` and the `drop_na()` at the end of the read block silently removes them.

The asymmetry is a source-level artefact (some yearly SURS tables include an unattributed-region row, others do not). National totals on the Treatment tab undercount 2016 by ~0.58% relative to the raw SURS export (rows excluded at source); the Collection tab's 2016 totals include the 6 unattributed rows from `coll_received_combined.csv` (~0.6%). No code change has been made to enforce uniform inclusion; the data files retain the rows as delivered by the source.

## Conclusion

This project aims to provide a comprehensive understanding of waste production and management in Slovenia, focusing on specific waste types and how they are handled by various companies and regions over the years. The insights gained from this analysis will help in identifying trends, inefficiencies, and potential areas for improvement in waste management practices.

## Contact Information

For any queries or further information, please contact:

- **Name**: [Nikola Kovačević]
- **Email**: [89232043@student.upr.si]
