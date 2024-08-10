# Waste Data Analysis in Slovenia

## Project Overview

This project focuses on analyzing statistical data available in Slovenia regarding companies producing specific types of waste and those responsible for the uptake and processing of this waste. The datasets span several years and include information on generated waste, collected waste, and how the waste is utilized. This data is broken down by companies (up to 2019) or regions (from 2019 onwards) within Slovenia.

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

## Conclusion

This project aims to provide a comprehensive understanding of waste production and management in Slovenia, focusing on specific waste types and how they are handled by various companies and regions over the years. The insights gained from this analysis will help in identifying trends, inefficiencies, and potential areas for improvement in waste management practices.

## Contact

For any queries or further information, please contact:

- **Name**: [Your Name]
- **Email**: [Your Email]
- **Institution/Organization**: [Your Institution/Organization]
