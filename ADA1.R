#Estherla Twene
#Advanced Data Analysis
#Loading libraries
library(haven)    
library(tidyverse) 
library(mice)     
library(broom)
library(dplyr)
library(gtsummary)
library(flextable)
library(officer)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(car)
install.packages("ResourceSelection")
library(ResourceSelection)

#Setting working directory
getwd()
setwd("/Users/estherlatwene/Desktop/class1_survey_project")

#Loading dataset
haalsi <- read_csv("1ADA_DATA.csv")


#Checking variables
dim(haalsi)           
names(haalsi)[1:40] 

#Checking variable names
names(haalsi)


#Creating the outcome variable
haalsi <- haalsi %>%
  mutate(
    W2C_PF_CADL_num = as.numeric(W2C_PF_CADL),
    func_limit = case_when(
      W2C_PF_CADL_num == 1 ~ 1L,   # has limitation
      W2C_PF_CADL_num == 0 ~ 0L,   # no limitation
      W2C_PF_CADL_num %in% c(-99, -98, -97) ~ NA_integer_,
      TRUE ~ NA_integer_
    ),
    
    func_limit = factor(
      func_limit,
      levels = c(0, 1),
      labels = c("No limitation", "Has limitation")
    )
  )

#Checking the outcome variable
table(haalsi$func_limit, useNA = "ifany")

# 419 study participants have functional limitation,3717 no functional limitation and 923 with missing values(NA)

#Creating the exposure variables and defining diabetes
haalsi <- haalsi %>%
  mutate(
    diag_num = as.numeric(W2C_CM_DIABETES_SRDIAG),
    trt_num  = as.numeric(W2C_CM_DIABETES_SRTRT),
    
    diag_num = ifelse(diag_num %in% c(-99, -98, -97), NA, diag_num),
    trt_num  = ifelse(trt_num %in% c(-99, -98, -97), NA, trt_num),
    
    # Diabetes definition:
    # YES if ever diagnosed OR treated
    diabetes_num = case_when(
      diag_num == 1 | trt_num == 1 ~ 1L,  # any YES → Diabetes
      diag_num == 2 & trt_num == 2 ~ 0L,  # both NO → No diabetes
      TRUE ~ NA_integer_             # everything else → missing
    ),
    
    diabetes = factor(
      diabetes_num,
      levels = c(0, 1),
      labels = c("No diabetes", "Diabetes")
    )
  )

#Assessing the distribution of the diabetes variable
table(haalsi$diabetes, useNA = "ifany")
#There are 2,695 participants with no diabetes, 581 with diabetes and 1783 NA

#Covariates
##Renaming the covariates
haalsi <- haalsi %>%
  rename(
    sex        = W2C_RSEX,
    educ       = W1C_BD_EDUC4,
    wealth     = W2C_WEALTHINDEX,
    bmi_cat    = W2C_BS_BMICAT,
    marital    = W2C_BD_MAR,
    smoke      = W2_C_CURRENT_SMOKE,
    hyper      = W2_C_EVER_HYPER,
    stroke     = W2_C_STROKE,
    angina     = W3C_EVER_ANGINA
  )


haalsi <- haalsi %>%
  mutate(
    sex     = as_factor(sex),
    bmi_cat = as_factor(bmi_cat),
    marital = as_factor(marital),
    smoke   = as_factor(smoke),
  )


# Adding age variable
haalsi <- haalsi %>%
  mutate(
    age_years = as.numeric(W2C_RAGE_CALC)
  )

# Age distribution
summary(haalsi$age_years)

# Recoding age into groups 10-year gaps, 40-49, 40-59, 60-69, 70-79 and 80+
haalsi <- haalsi %>%
  mutate(
    age_group = cut(
      age_years,
      breaks = c(40, 50, 60, 70, 80, Inf),   # 40–49, 50–59, 60–69, 70–79, 80+
      labels = c("40–49", "50–59", "60–69", "70–79", "80+"),
      right = FALSE
    )
  )

# Checking age group distribution
table(haalsi$age_group, useNA = "ifany")

# Wealth quintiles
haalsi <- haalsi %>%
  mutate(
    wealth = factor(wealth,
                             levels = 1:5,
                             labels = c("Q1 (Poorest)", "Q2", "Q3", "Q4", "Q5 (Richest)"))
  )

# Recode alcohol (W2CM069: 1 = Yes, 2 = No, -97/-98/-99 = missing)
haalsi <- haalsi %>%
  mutate(
    alcohol_num = as.numeric(W2CM069),
    alcohol_num = ifelse(alcohol_num %in% c(-99, -98, -97), NA, alcohol_num),
    alcohol = factor(
      alcohol_num,
      levels = c(2, 1),          # 2 = No, 1 = Yes
      labels = c("No", "Yes")
    )
  )

table(haalsi$alcohol, useNA = "ifany")

#Recoding educational
haalsi <- haalsi %>%
  mutate(
    # numeric versions (keep these for models if you want)
    hyper_num  = as.numeric(hyper),
    stroke_num = as.numeric(stroke),
    angina_num = as.numeric(angina),
    
    # set special missing codes to NA
    hyper_num  = ifelse(hyper_num  %in% c(-97, -98, -99), NA, hyper_num),
    stroke_num = ifelse(stroke_num %in% c(-97, -98, -99), NA, stroke_num),
    angina_num = ifelse(angina_num %in% c(-97, -98, -99), NA, angina_num),
    
    # recode to 0/1: here I’m assuming 1 = Yes, 2 = No
    hyper_num  = dplyr::case_when(
      hyper_num == 1 ~ 1L,
      hyper_num == 2 ~ 0L,
      TRUE ~ NA_integer_
    ),
    stroke_num = dplyr::case_when(
      stroke_num == 1 ~ 1L,
      stroke_num == 2 ~ 0L,
      TRUE ~ NA_integer_
    ),
    angina_num = dplyr::case_when(
      angina_num == 1 ~ 1L,
      angina_num == 2 ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    # labelled factors for descriptives and models
    hyper  = factor(hyper_num,
                    levels = c(0, 1),
                    labels = c("No hypertension", "Hypertension")),
    stroke = factor(stroke_num,
                    levels = c(0, 1),
                    labels = c("No stroke", "Stroke")),
    angina = factor(angina_num,
                    levels = c(0, 1),
                    labels = c("No angina", "Angina"))
  )

table(haalsi$hyper, useNA="ifany")
table(haalsi$stroke, useNA="ifany")
table(haalsi$angina, useNA="ifany")


# Recoding variable labels in dataset
# Sex
haalsi$sex <- fct_recode(
  haalsi$sex,
  "Male"   = "1",
  "Female" = "2"
)

# BMI
haalsi$bmi_cat <- fct_recode(
  haalsi$bmi_cat,
  "Underweight" = "0",
  "Normal"      = "1",
  "Overweight"  = "2",
  "Obese"       = "3"
)

# Marital Status
haalsi$marital <- fct_recode(
  haalsi$marital,
  "Never married"      = "0",
  "Married"            = "1",
  "Separated/Divorced" = "2",
  "Widowed"            = "3"
)

# Smoking
haalsi$smoke <- fct_recode(
  haalsi$smoke,
  "Yes" = "1",
  "No"  = "2"
)

# Alcohol
haalsi$alcohol <- fct_recode(
  haalsi$alcohol,
  "No"  = "No",
  "Yes" = "Yes"
)


#Re-coding educational variable
haalsi <- haalsi %>%
  mutate(
    educ_num = as.numeric(educ),
    educ = case_when(
      educ_num == 1 ~ "No formal education",
      educ_num == 2 ~ "Some primary (1–7 years)",
      educ_num == 3 ~ "Some secondary (8–11 years)",
      educ_num == 4 ~ "Secondary or more (12+ years)",
      educ_num %in% c(-97, -98, -99) ~ NA_character_,
      TRUE ~ NA_character_
    ),
    educ = factor(
      educ,
      levels = c(
        "No formal education",
        "Some primary (1–7 years)",
        "Some secondary (8–11 years)",
        "Secondary or more (12+ years)"))
  )

table(haalsi$educ, useNA="ifany")
table(haalsi$educ, haalsi$diabetes, useNA="ifany")


#Variables to be included in MICE
log_vars <- c(
  "func_limit", "diabetes",
  "sex", "educ", "wealth",
  "bmi_cat", "marital",
  "smoke", "alcohol",
  "hyper", "stroke", "angina", "age_group"
)


mice_vars <- c(
  "func_limit", "diabetes",
  "sex", "wealth",
  "bmi_cat", "marital",
  "smoke", "alcohol", "age_group"
)

#Assessing missingness
# Create data set for MICE
haalsi_mice <- haalsi[, mice_vars]

#Running multiple imputation
imp <- mice(haalsi_mice, m = 10, seed = 123)

# Descriptive statistics
table1_diabetes <- haalsi %>%
  filter(!is.na(diabetes)) %>% 
  tbl_summary(
    by = diabetes,
    include = c(
      func_limit,
      age_group,
      sex, educ, wealth,
      bmi_cat, marital,
      smoke, alcohol,
      hyper, stroke, angina
    ),
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing = "no",
    label = list(
      func_limit ~ "Functional limitation",
      age_group ~ "Age group",
      sex ~ "Sex",
      educ ~ "Education level",
      wealth ~ "Wealth quintile",
      bmi_cat ~ "BMI category",
      marital ~ "Marital status",
      smoke ~ "Smoking status",
      alcohol ~ "Alcohol use"
    )
  ) %>%
  modify_caption("Table 1. Participant Characteristics by Diabetes Status") %>%
  modify_header(label ~ "**Variable**") %>%
  bold_labels()

table1_diabetes

table1_flex <- as_flex_table(table1_diabetes)

doc <- read_docx()
doc <- body_add_par(doc, "Table 1. Participant Characteristics by Diabetes Status", style = "heading 1")
doc <- body_add_flextable(doc, table1_flex)
print(doc, target = "Table1_Descriptives_By_Diabetes.docx")

#Checking assumptions
#Multicolinearity
cc <- haalsi %>%
  select(func_limit, diabetes, sex, wealth, bmi_cat, marital,
         smoke, alcohol, age_group) %>%
  na.omit()

model_check <- glm(
  func_limit ~ diabetes + sex + wealth + bmi_cat +
    marital + smoke + alcohol + age_group,
  data = cc,
  family = binomial
)


vif(model_check)
## Variance inflation factors (VIFs) were examined to assess multi-collinearity among predictors.No multi-collinearity

#Goodness of fit
cc$func_limit_num <- ifelse(cc$func_limit == "Has limitation", 1, 0)
model_check <- glm(
  func_limit_num ~ diabetes + sex + wealth + bmi_cat +
    marital + smoke + alcohol + age_group,
  data = cc,
  family = binomial
)

hoslem.test(cc$func_limit_num, fitted(model_check), g = 10)
##p-value >0.05, indication good model fit


#Un-adjusted model with only diabetes and functional limitation
logistic_unadj <- with(
  imp,
  glm(func_limit ~ diabetes, family = binomial)
)

pooled_unadj <- pool(logistic_unadj)

unadj_sum <- summary(pooled_unadj, conf.int = TRUE, small = TRUE)

APA_unadj <- unadj_sum %>%
  mutate(
    OR       = round(exp(estimate), 2),
    lower_CI = round(exp(`2.5 %`), 2),
    upper_CI = round(exp(`97.5 %`), 2),
    p_value  = p.value
  ) %>%
  select(term, OR, lower_CI, upper_CI, p_value)

APA_unadj

# Logistic regression, adjusting for confounders
logistic_mi <- with(
  imp,
  glm(
    func_limit ~ diabetes + sex + wealth +
      bmi_cat + marital + smoke + alcohol + age_group ,
    family = binomial
  )
)

pooled_results <- pool(logistic_mi)


# Pooled summary with CIs and p-values
adj_sum <- summary(pooled_results, conf.int = TRUE, small = TRUE)

# APA-style adjusted results
APA_adj <- adj_sum %>%
  mutate(
    OR       = round(exp(estimate), 2),
    lower_CI = round(exp(`2.5 %`), 2),
    upper_CI = round(exp(`97.5 %`), 2),
    p_value  = p.value
  ) %>%
  select(term, OR, lower_CI, upper_CI, p_value)

APA_adj


#Assessing EMM by sex (Male and Female)
# Diabetes × Sex Interaction
logistic_int_sex <- with(
  imp,
  glm(
    func_limit ~ diabetes * sex +
      age_group + wealth + bmi_cat + marital +
      smoke + alcohol,
    family = binomial
  )
)

pooled_int_sex <- pool(logistic_int_sex)

int_sex_sum <- summary(pooled_int_sex, conf.int = TRUE, small = TRUE)

APA_int_sex <- int_sex_sum %>%
  mutate(
    OR       = round(exp(estimate), 2),
    lower_CI = round(exp(`2.5 %`), 2),
    upper_CI = round(exp(`97.5 %`), 2),
    p_value  = p.value
  ) %>%
  select(term, OR, lower_CI, upper_CI, p_value)

APA_int_sex


#Want clean labels 
clean_labels <- function(table) {
  table %>%
    mutate(
      term_chr = as.character(term),
      
      Variable = dplyr::case_when(
        # Diabetes
        term_chr == "diabetesDiabetes" ~ "Diabetes (yes)",
        
        # Sex
        term_chr == "sex2" ~ "Female",
        
        # BMI
        term_chr == "bmi_cat0" ~ "Underweight (BMI)",
        term_chr == "bmi_cat1" ~ "Normal BMI",
        term_chr == "bmi_cat2" ~ "Overweight (BMI)",
        term_chr == "bmi_cat3" ~ "Obese (BMI)",
        
        # Marital status
        term_chr == "marital0" ~ "Never married",
        term_chr == "marital1" ~ "Separated/divorced",
        term_chr == "marital2" ~ "Widowed",
        term_chr == "marital3" ~ "Currently married",
        
        # Smoking
        term_chr == "smoke1" ~ "Current smoker",
        
        # Alcohol
        term_chr == "alcoholYes" ~ "Alcohol use (yes)",
        
        # Age groups
        term_chr == "age_group40–49" ~ "Age 40–49",
        term_chr == "age_group50–59" ~ "Age 50–59",
        term_chr == "age_group60–69" ~ "Age 60–69",
        term_chr == "age_group70–79" ~ "Age 70–79",
        term_chr == "age_group80+"   ~ "Age 80+",
        
        # Interaction terms (Sex)
        term_chr == "diabetesDiabetes:sex2"           ~ "Diabetes × Female",
        
        TRUE ~ term_chr
      )
    ) %>%
    select(Variable, OR, lower_CI, upper_CI, p_value)
}



##Re-running my models after cleaning labels
clean_APA_adj <- clean_labels(APA_adj)
clean_APA_adj

clean_APA_sex <- clean_labels(APA_int_sex)
clean_APA_sex


#Creating table 2
table2_logistic <- clean_APA_adj %>%
  mutate(
    OR = sprintf("%.2f", OR),
    CI = sprintf("%.2f–%.2f", lower_CI, upper_CI),
    p_value = ifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value))
  ) %>%
  select(Variable, OR, CI, p_value)


table2_flex <- flextable(table2_logistic) %>%
  autofit() %>%
  bold(part = "header") %>%        # bold header
  align(align = "center", part = "all") %>% 
  set_caption("Table 2. Adjusted Logistic Regression of Diabetes and Functional Limitation")

doc <- read_docx()
doc <- body_add_par(doc, "Table 2. Adjusted Logistic Regression of Diabetes and Functional Limitation", style = "heading 1")
doc <- body_add_flextable(doc, table2_flex)
print(doc, target = "Table2_Logistic_Regression.docx")


# UNADJUSTED
# Men
logistic_unadj_male <- with(
  imp,
  glm(
    func_limit ~ diabetes,
    family = binomial,
    subset = sex == "Male"
  )
)
pooled_unadj_male <- pool(logistic_unadj_male)

unadj_male_sum <- summary(pooled_unadj_male, conf.int = TRUE, small = TRUE) %>%
  dplyr::filter(term == "diabetesDiabetes") %>%
  dplyr::mutate(
    Sex      = "Male",
    Model    = "Unadjusted",
    OR       = exp(estimate),
    lower_CI = exp(`2.5 %`),
    upper_CI = exp(`97.5 %`),
    p_value  = p.value
  ) %>%
  dplyr::select(Sex, Model, OR, lower_CI, upper_CI, p_value)

# Women
logistic_unadj_female <- with(
  imp,
  glm(
    func_limit ~ diabetes,
    family = binomial,
    subset = sex == "Female"
  )
)
pooled_unadj_female <- pool(logistic_unadj_female)

unadj_female_sum <- summary(pooled_unadj_female, conf.int = TRUE, small = TRUE) %>%
  dplyr::filter(term == "diabetesDiabetes") %>%
  dplyr::mutate(
    Sex      = "Female",
    Model    = "Unadjusted",
    OR       = exp(estimate),
    lower_CI = exp(`2.5 %`),
    upper_CI = exp(`97.5 %`),
    p_value  = p.value
  ) %>%
  dplyr::select(Sex, Model, OR, lower_CI, upper_CI, p_value)

# ADJUSTED

# Men
logistic_adj_male <- with(
  imp,
  glm(
    func_limit ~ diabetes + wealth + bmi_cat + marital +
      smoke + alcohol + age_group,
    family = binomial,
    subset = sex == "Male"
  )
)
pooled_adj_male <- pool(logistic_adj_male)

adj_male_sum <- summary(pooled_adj_male, conf.int = TRUE, small = TRUE) %>%
  dplyr::filter(term == "diabetesDiabetes") %>%
  dplyr::mutate(
    Sex      = "Male",
    Model    = "Adjusted",
    OR       = exp(estimate),
    lower_CI = exp(`2.5 %`),
    upper_CI = exp(`97.5 %`),
    p_value  = p.value
  ) %>%
  dplyr::select(Sex, Model, OR, lower_CI, upper_CI, p_value)

# Women
logistic_adj_female <- with(
  imp,
  glm(
    func_limit ~ diabetes + wealth + bmi_cat + marital +
      smoke + alcohol + age_group,
    family = binomial,
    subset = sex == "Female"
  )
)
pooled_adj_female <- pool(logistic_adj_female)

adj_female_sum <- summary(pooled_adj_female, conf.int = TRUE, small = TRUE) %>%
  dplyr::filter(term == "diabetesDiabetes") %>%
  dplyr::mutate(
    Sex      = "Female",
    Model    = "Adjusted",
    OR       = exp(estimate),
    lower_CI = exp(`2.5 %`),
    upper_CI = exp(`97.5 %`),
    p_value  = p.value
  ) %>%
  dplyr::select(Sex, Model, OR, lower_CI, upper_CI, p_value)

# Combine all four rows
emm_by_sex <- bind_rows(
  unadj_male_sum,
  unadj_female_sum,
  adj_male_sum,
  adj_female_sum
)

emm_by_sex_formatted <- emm_by_sex %>%
  dplyr::mutate(
    OR      = sprintf("%.2f", OR),
    CI      = sprintf("%.2f–%.2f", lower_CI, upper_CI),
    p_value = ifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value))
  ) %>%
  dplyr::select(Sex, Model, OR, CI, p_value)

emm_by_sex_formatted

emm_sex_OR_flex <- flextable(emm_by_sex_formatted) %>%
  autofit() %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  set_caption("Table X. Unadjusted and Adjusted Association Between Diabetes and Functional Limitation, Stratified by Sex")

doc <- read_docx()
doc <- body_add_par(doc, "Table X. Diabetes and Functional Limitation by Sex", style = "heading 1")
doc <- body_add_flextable(doc, emm_sex_OR_flex)

print(doc, target = "TableX_Diabetes_Sex_Stratified.docx")


# Assessing Effect Measure Modification (EMM) by Sex using interaction terms
emm_flex <- flextable(emm_sex_formatted) %>%
  autofit() %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  set_caption("Table 3. Effect Measure Modification (EMM) by Sex for the Association Between Diabetes and Functional Limitation")

doc <- read_docx()
doc <- body_add_par(doc, "Table 3. Effect Measure Modification (EMM) by Sex", style = "heading 1")
doc <- body_add_flextable(doc, emm_flex)

print(doc, target = "Table3_EMM_Sex.docx")

#Flow chart. CONSORT-style flowchart for HAALSI analytic sample
figure1 <- grViz("
digraph flowchart {

  graph [layout = dot, rankdir = TB]

  node [fontname = Helvetica, shape = rectangle, fontsize = 15]

  # nodes
  node1 [label = '@@1']
  node2 [label = '@@2']
  node3 [label = '@@3']

  # flow
  node1 -> node2 -> node3
}

[1]: 'HAALSI Wave 2 participants aged ≥40 years\\n n = 5,059'
[2]: 'Excluding 923 participants with missing data on functional limitation\\nAnalytic sample with functional limitation information\\n n = 4,136'
[3]: 'Excluding 860 participants with missing diabetes status\\nFinal analytic sample for descriptive and regression analyses\\n n = 3,276'
")

figure1

# Export to PDF as Figure 1
figure1 %>%
  DiagrammeRsvg::export_svg() %>%  # convert to SVG text
  charToRaw() %>%                  # convert to raw vector
  rsvg::rsvg_pdf("Figure1_Flowchart.pdf")  # save as PDF
