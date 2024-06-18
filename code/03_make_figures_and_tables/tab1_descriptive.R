source("code/dependencies.R")

library(writexl)

# Select covariates to include
covs <- c("state", "result", "tested", "sex", "age_idade", "age_cat", 
          "health_unit", "hiv_status", "tratamento", "pop_rua", "pop_liber", 
          "agravtabac", "agravdroga", "agravdroga", "agravdiabe", "cs_escol_n", 
          "pop_imig", "race")


# Create table  -----------------------------------------------------------
dat <- sinan_tmp %>% 
  filter(tratamento %in% c("1", "2", "3") & situa_ence != "06" & diag_yr >= "2017-01-01" & diag_yr < "2024-01-01") %>% 
  select(all_of(covs)) %>% 
  mutate(case_type = if_else(tratamento == "1", "New", "Previous"))

# By testing status (with variation by state)
dat_type <- dat %>% 
  group_by(case_type, tested) %>% 
  summarize(n = n(), 
            result = round(mean(result == "positive"), 2),
            male_mean = round(mean(sex == "male"), 2), 
            age_mean = round(mean(age_idade, na.rm = TRUE), 2), 
            age_sd = round(sd(age_idade, na.rm = TRUE), 2),
            hiv_mean = round(mean(hiv_status == "positive"), 2),
            hiv_miss_mean = round(mean(hiv_status == "missing"), 2),
            hu_low = round(mean(health_unit == "low complexity"), 2),
            hu_med = round(mean(health_unit == "medium complexity"), 2),
            hu_high = round(mean(health_unit == "high complexity"), 2),
            rua_mean = round(mean(pop_rua == "yes"), 2),
            rua_miss_mean = round(mean(pop_rua == "missing"), 2),
            liber_mean = round(mean(pop_liber == "yes"), 2),
            liber_miss_mean = round(mean(pop_liber == "missing"), 2),
            imig_mean = round(mean(pop_imig == "yes"), 2),
            imig_miss_mean = round(mean(pop_imig == "missing"), 2),
            agravtabac_mean = round(mean(agravtabac == "yes"), 2),
            agravtabac_miss_mean = round(mean(agravtabac == "missing"), 2),
            agravdroga_mean = round(mean(agravdroga == "yes"), 2),
            agravdroga_miss_mean = round(mean(agravdroga == "missing"), 2),
            agravdiabe_mean = round(mean(agravdiabe == "yes"), 2),
            agravdiabe_miss_mean = round(mean(agravdiabe == "missing"), 2),
            "brown" = round(mean(race == "brown", na.rm = TRUE), 2),
            "asian" = round(mean(race == "asian", na.rm = TRUE), 2),
            "black" = round(mean(race == "black", na.rm = TRUE), 2),
            "indigenous" = round(mean(race == "indigenous", na.rm = TRUE), 2),
            "missing" = round(mean(race == "missing", na.rm = TRUE), 2),
            "white" = round(mean(race == "white", na.rm = TRUE), 2)
            ) %>% 
  t() %>% 
  as.data.frame(., stringsAsFactors = FALSE)


# Convert transposed matrix to data frame
dat_type_df <- as.data.frame(dat_type, stringsAsFactors = FALSE)

# Create dat for overall 
dat_overall <- dat %>% 
  group_by(case_type) %>% 
  summarize(tested = round(mean(tested == "tested"), 2),
            n = n(), 
            result = round(mean(result == "positive"), 2),
            male_mean = round(mean(sex == "male"), 2), 
            age_mean = round(mean(age_idade, na.rm = TRUE), 2), 
            age_sd = round(sd(age_idade, na.rm = TRUE), 2),
            hiv_mean = round(mean(hiv_status == "positive"), 2),
            hiv_miss_mean = round(mean(hiv_status == "missing"), 2),
            hu_low = round(mean(health_unit == "low complexity"), 2),
            hu_med = round(mean(health_unit == "medium complexity"), 2),
            hu_high = round(mean(health_unit == "high complexity"), 2),
            rua_mean = round(mean(pop_rua == "yes"), 2),
            rua_miss_mean = round(mean(pop_rua == "missing"), 2),
            liber_mean = round(mean(pop_liber == "yes"), 2),
            liber_miss_mean = round(mean(pop_liber == "missing"), 2),
            imig_mean = round(mean(pop_imig == "yes"), 2),
            imig_miss_mean = round(mean(pop_imig == "missing"), 2),
            agravtabac_mean = round(mean(agravtabac == "yes"), 2),
            agravtabac_miss_mean = round(mean(agravtabac == "missing"), 2),
            agravdroga_mean = round(mean(agravdroga == "yes"), 2),
            agravdroga_miss_mean = round(mean(agravdroga == "missing"), 2),
            agravdiabe_mean = round(mean(agravdiabe == "yes"), 2),
            agravdiabe_miss_mean = round(mean(agravdiabe == "missing"), 2),
            "brown" = round(mean(race == "brown", na.rm = TRUE), 2),
            "asian" = round(mean(race == "asian", na.rm = TRUE), 2),
            "black" = round(mean(race == "black", na.rm = TRUE), 2),
            "indigenous" = round(mean(race == "indigenous", na.rm = TRUE), 2),
            "missing" = round(mean(race == "missing", na.rm = TRUE), 2),
            "white" = round(mean(race == "white", na.rm = TRUE), 2)
            ) %>% 
  t() %>% 
  as.data.frame(., stringsAsFactors = FALSE)

# Convert transposed matrix to data frame
dat_overall_df <- as.data.frame(dat_overall, stringsAsFactors = FALSE)

# Combine overall and type
dat_df <- cbind(dat_type_df, dat_overall_df)


# Tidy up table -----------------------------------------------------------
# Fix rownames
rownames(dat_df)[rownames(dat_df) == "case_type"] <- ""
rownames(dat_df)[rownames(dat_df) == "tested"] <- "Conclusive Xpert resistance result"
rownames(dat_df)[rownames(dat_df) == "result"]<- "Rifampicin resistance indicated"
rownames(dat_df)[rownames(dat_df) == "male_mean"]<- "Male"
rownames(dat_df)[rownames(dat_df) == "age_mean"]<- "Age"
rownames(dat_df)[rownames(dat_df) == "age_sd"]<- "Age_sd"
rownames(dat_df)[rownames(dat_df) == "brown"]<- "Brown"
rownames(dat_df)[rownames(dat_df) == "white"]<- "White"
rownames(dat_df)[rownames(dat_df) == "black"]<- "Black"
rownames(dat_df)[rownames(dat_df) == "asian"]<- "Asian"
rownames(dat_df)[rownames(dat_df) == "indigenous"]<- "Indigenous"
rownames(dat_df)[rownames(dat_df) == "missing"]<- "Other"
rownames(dat_df)[rownames(dat_df) == "hiv_mean"]<- "HIV - Positive"
rownames(dat_df)[rownames(dat_df) == "hiv_miss_mean"]<- "HIV - Other"
rownames(dat_df)[rownames(dat_df) == "agravdiabe_mean"]<- "Has diabetes"
rownames(dat_df)[rownames(dat_df) == "agravdiabe_miss_mean"]<- "Has diabetes - Other"
rownames(dat_df)[rownames(dat_df) == "hu_low"]<- "Health Unit - Low complexity"
rownames(dat_df)[rownames(dat_df) == "hu_med"]<- "Health Unit - Medium complexity"
rownames(dat_df)[rownames(dat_df) == "hu_high"]<- "Health Unit - High complexity"
rownames(dat_df)[rownames(dat_df) == "rua_mean"]<- "Homeless"
rownames(dat_df)[rownames(dat_df) == "rua_miss_mean"]<- "Homeless - Other"
rownames(dat_df)[rownames(dat_df) == "liber_mean"]<- "Incarcerated"
rownames(dat_df)[rownames(dat_df) == "liber_miss_mean"]<- "Incarcerated - Other"
rownames(dat_df)[rownames(dat_df) == "imig_mean"]<- "Immigrant"
rownames(dat_df)[rownames(dat_df) == "imig_miss_mean"]<- "Immigrant - Other"
rownames(dat_df)[rownames(dat_df) == "agravtabac_mean"]<- "Uses tobacco"
rownames(dat_df)[rownames(dat_df) == "agravtabac_miss_mean"]<- "Uses tobacco - Other"
rownames(dat_df)[rownames(dat_df) == "agravdroga_mean"]<- "Uses illicit drugs"
rownames(dat_df)[rownames(dat_df) == "agravdroga_miss_mean"]<- "Uses illicit drugs - Other"

# Combine mean and sd for age
dat_df["Age", ] <- paste(dat_df["Age", ], " (", dat_df["Age_sd", ], ")", sep = "")
dat_df <- dat_df[rownames(dat_df) != "Age_sd", ] # remove sge_ad

# Make rownames a columns
dat_df <- cbind(rowname = rownames(dat_df), dat_df)

# Reorder columns (Overall, Tested, Not tested)
dat_reordered <- dat_df[, c(1, 6, 3, 2, 7, 5, 4)]

# Add column header
dat_reordered["", ] <- c("Variable", "Overall", "Tested", "Not tested",  "Overall", "Tested", "Not tested")
dat_reordered["Conclusive Xpert resistance result", ] <- c("Conclusive Xpert resistance result", dat_reordered["Conclusive Xpert resistance result", 2], "-", "-",  dat_reordered["Conclusive Xpert resistance result", 5], "-", "-")

# Combine n and category
dat_reordered[1,] <- paste(dat_reordered[1, ], " (N = ", dat_reordered["n", ], ")", sep = "")
dat_reordered <- dat_reordered[rownames(dat_reordered) != "n", ]


# Output table ------------------------------------------------------------
write_xlsx(dat_reordered,"output/figures_and_tables/tab1_descriptive.xlsx")


