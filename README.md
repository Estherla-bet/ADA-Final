# ADA Final Project Analysis


## Project Title
Diabetes and Functional Limitation among adults 40 years and older in Rural South Africa: Evidence 
from Health and Aging in Africa Longitudinal Studies INDEPTH Communities Study (2018 – 2019)


## Project Description
This repository contains all code and accompanying materials for my final project in the Advanced Data Analysis class
Using the Health and Aging in Africa (HAALSI): Longitudinal Studies INDEPTH communities study data, this project investigates 
the association between diabetes and functional among older adults living in rural South Africa from 2018 to 2019.

The analysis comprises of
- Multiple imputation of missing covariates and outcomes
-	Construction of analytic variables 
- Logistic regression models (unadjusted and adjusted)
-	Interaction models to assess effect measure modification by sex
-	APA-formatted tables and figures

## Files Included
- `1ADA_DATA.csv`: Cleaned version of the Class 1 survey dataset
- `ADA.Rmd`: R script used to summarize and visualize the data
- `README.md`: This file summarizes the analysis conducted

## Summary of What the Code Does
Package Loading
- Uses a comprehensive package suite including: tidyverse, mice, dplyr, gtsummary, flextable, officer, DiagrammeR, DiagrammeRsvg, rsvg


Importing of data and selection of study variables
	
Variable Creation and recoding
Exposure variabale is diabetes defined as:
- self-reported diagnosis, OR  
- current treatment 
Categorized as Yes or No

Outcome Variable is functional limitation, defined as difficulty in ≥1 Activity of Daily Living (ADL).
ADLs include bathing, walking, eating, going in and out of bed, using the toilet and dressing. 
Categorized as:
- 1 = Has functional limitation  
- 0 = No functional limitation

Covariates
- Age group  
- Sex  
- Education  
- Marital status  
- Wealth index  
- BMI  
- Smoking  
- Alcohol use  


Survey Design Multiple Imputation
- mice used to impute missing exposures, outcomes, and covariates
	

## Background
Functional limitation refers to difficulty performing Activities of Daily Living (ADLs),
including bathing, dressing, walking, eating, and toileting. In sub-Saharan Africa, 
the burden is high, evidence suggest that about 40% of older adults in sub-Saharan Africa experience some 
impairment in physical functioning.Diabetes mellitus (DM), characterized by persistent hyperglycemia due to impaired glucose metabolism, is rising across Africa.
DM-related complications—neuropathy, retinopathy, cardiovascular disease—can significantly worsen physical functioning and independence.
Although high-income countries have studied diabetes–function relationships, limited evidence exists for African aging populations, 
particularly in rural, resource-limited settings. 

## Objectives
### Primary Objectives:
1) To examine the association between diabetes and functional limitation among adults ≥40 years in rural South Africa.
2) To assess effect modification of the association between diabetes and functional limitation among adults ≥40 years 
in rural South Africa by sex, age group, and educational level on the diabetes–functional limitation association.

##Data Source
This study used the wave 2 data (2018 to 2019) data from the Health and Aging in Africa (HAALSI): Longitudinal Studies 
INDEPTH communities study.
The study was based in the Agincourt HDSS site, a sub-district of rural Mpumalanga Province in South Africa. 
Link: https://www.icpsr.umich.edu/web/ICPSR/studies/36633/datadocumentation

##Study Population
The study population comprises adults 40 years and older who participated in the Health 
and Aging in Africa (HAALSI): Longitudinal Studies INDEPTH communities study.
Study Design: Will utilize a cross-sectional study design to analyze data from the wave 2 HAALSI data from 2018 – 2019.


## Statistical Analysis

### Descriptive Analysis
- Descriptive Statistics:  summarized the characteristics of study participants by diabetes status

### Regression Modeling
A logistic regression model estimates odds of functional limitation by diabetes status.  
Sequential models include:
1. **Unadjusted model**  
2. **Adjusted model** based on DAG-informed confounders.

Adjusted confounders include:
- Age  
- Sex  
- Marital status  
- Wealth index  
- BMI  
- Smoking  
- Alcohol use  


### Effect Measure Modification (Interaction) by sex
Interactions tested for:
- Diabetes × Sex  

### Model Diagnostics
- Independence of observations  
- Hosmer–Lemeshow goodness-of-fit test  
- Influence diagnostics for outliers

## How to Run the Code
1. Download or clone this repository to your computer
2. Open `ADA.Rmd' in RStudio
3. Make sure your working directory is set to the folder where the files are saved
4. Make sure to import and load the data set 1ADA_DATA.csv
4. Run the script

## Author
- Name: Estherla Twene 
- Course: Advanced Data Analysis 
- Date: December 2025






