# Description: Loads sinan data and cleans

source("code/dependencies.R")

load_and_clean_sinan <- function() {
  # Load sinan --------------------------------------------------------------
  names(sinan) <- tolower(names(sinan))

  sinan_tmp <- sinan %>%
    mutate(
      # Create quarter and year of diagnosis based on date when date of TB
      # diagnosis
      dt_diag = as.Date(as.character(dt_diag), format = "%Y%m%d"),
      diag_yr = floor_date(as_date(dt_diag), "year"),
      diag_qrt = floor_date(as_date(dt_diag), "quarter"),
      tratamento = as.factor(tratamento),
      # Clean up location parameters
      sg_uf = if_else(sg_uf == "" | sg_uf == "9", NA, sg_uf) %>% as.factor(), # State of residence
      id_mn_resi = if_else(id_mn_resi == "" | id_mn_resi == "9", NA, id_mn_resi) %>% as.factor(), # Municipality of residence
      sg_uf_not = if_else(sg_uf_not == "" | sg_uf_not == "9", NA, sg_uf_not) %>% as.factor(), # State of notifying health unit
      id_municip = if_else(id_municip == "" | id_municip == "9", NA, id_municip) %>% as.factor() # Municipality of notifying health unit
    ) %>%
    rename(id_mn_not = id_municip)

  # Filter to selected years
  sinan_tmp$diag_yr <- as.numeric(year(sinan_tmp$diag_yr))

  sinan_tmp <- sinan_tmp %>% filter(as.numeric(diag_yr) %in% years_to_pull)


# Add level of health unit data --------------------------------------------
sinan_tmp <- sinan_tmp %>%
  mutate(health_unit_not = case_when(
    first_level_not == 1 ~ "low complexity",
    second_level_not == 1 ~ "medium complexity",
    second_level_tb_not == 1 ~ "medium complexity",
    second_level_not == 1 & second_level_tb_not == 1 ~ "medium complexity",
    third_level_not == 1 ~ "high complexity",
    third_level_tb_not == 1 ~ "high complexity",
    third_level_not == 1 & third_level_tb_not == 1 ~ "high complexity",
    second_level_tb_not == 1 & third_level_tb_not == 1 ~ "high complexity",
    others_not == 1 ~ "other"
  ))

sinan_tmp$health_unit <- factor(sinan_tmp$health_unit_not, levels = c("low complexity", "medium complexity", "high complexity", "other"))


# Impute missing patient state/mun of residence  ------------------------------------------------
  ## Replace missing municipality/state of residence with either municipality/state
  ## of notifying health unit. If notifying health unit is also missing, replace with location
  ## of treating health unit.
  sinan_tmp <- sinan_tmp %>%
    mutate(
      id_mn_resi_clean = if_else(is.na(as.factor(id_mn_resi)) & !is.na(as.factor(id_mn_not)), as.factor(id_mn_not),
                                 if_else(is.na(as.factor(id_mn_resi)) & is.na(as.factor(id_mn_not)), as.factor(id_munic_a), as.factor(id_mn_resi))
                                 ),
      sg_uf_clean = if_else(is.na(sg_uf) & !is.na(sg_uf_not), sg_uf_not,
                            if_else(is.na(sg_uf) & is.na(sg_uf_not), sg_uf_at, sg_uf)
                            ),
      # Identify where state of residence is being sourced from
      sg_uf_clean_flag = if_else(is.na(sg_uf) & !is.na(sg_uf_not), "sg_uf_not",
                                 if_else(is.na(sg_uf) & is.na(sg_uf_not), "sg_uf_at", "sg_uf")
                                 )
      )


  # Add location characteristics ----------------------------------------

  ## Latitude and longitude based on centroid of municipality of residence
  source(here::here("code/01_data_processing/get_municipality_centroid.R"))

  centroid <- get_municipality_centroid()
  
  municip <- as.numeric(as.character(centroid$CD_MUN_merge))
  
  # Clean up municipality codes prior to merge
  sinan_tmp <- sinan_tmp %>%
    mutate(
      # Identify where municip is being sourced from from
      mn_to_merge_flag = if_else(id_mn_resi_clean %in% municip, "id_mn_resi",
                                 if_else(id_mn_not %in% municip, "id_mn_not", NA)
                                 ),
      # Replace incorrect municipality of residence codes in SINAN with municipality of notifying health unit (id_mn_not)
      mn_to_merge = if_else(id_mn_resi_clean %in% municip, id_mn_resi_clean,
                            if_else(id_mn_not %in% municip, id_mn_not, NA)
                            )
      )

  # Merge lat and long based on municipality of residence
  sinan_tmp <- left_join(sinan_tmp, centroid %>% 
                           select(CD_MUN_merge, NM_MUN, lat, lon), 
                         by = c("mn_to_merge" = "CD_MUN_merge")) %>%
    rename(mn_resi_nm = NM_MUN)

  # Update id_mn_resi_clean with correct id_mn
  sinan_tmp <- sinan_tmp %>% select(-c(id_mn_resi_clean)) %>% rename(id_mn_resi_clean = mn_to_merge)


  ## Add state name ----------------------------------------------------------
  state_name <- read_xlsx("data/StateCodes.xlsx") %>% mutate(sg_uf = as.factor(sg_uf))

  sinan_tmp <- left_join(sinan_tmp, state_name %>% 
                           select(sg_uf, NAME_1, NAME_2), 
                         by = c("sg_uf_clean" = "sg_uf")) %>%
    rename(state_nm = NAME_1, region_nm = NAME_2)


# Clean covariates --------------------------------------------------------

  # Sex
  sinan_tmp <- sinan_tmp %>% mutate(sex = if_else(cs_sexo == "M", "male",
                                                  if_else(cs_sexo == "F", "female", "missing")
  ) %>% 
    as.factor() %>% 
    relevel(ref = "female")
  )
  
  # HIV status
  # Defined first based on HIV test result, and then self-report
  sinan_tmp <- sinan_tmp %>% mutate(hiv_status = if_else(hiv == "1", "positive",
                                                         if_else(hiv == "2", "negative",
                                                                 if_else(agravaids == "1", "positive",
                                                                         if_else(agravaids == "2", "negative", NA)
                                                                 )
                                                         )
  ),
  hiv_status = if_else(is.na(hiv_status), "missing", hiv_status) %>% 
    as.factor() %>% 
    relevel(ref = "negative")
  )
  
  # Health Unit
  sinan_tmp <- sinan_tmp %>% mutate(health_unit = if_else(is.na(health_unit), "missing", health_unit) %>% 
                                      as.factor()
  )
  
  
  # Race
  sinan_tmp <- sinan_tmp %>% mutate(race = if_else(cs_raca == "1", "white",
                                                   if_else(cs_raca == "2", "black",
                                                           if_else(cs_raca == "3", "asian",
                                                                   if_else(cs_raca == "4", "brown",
                                                                           if_else(cs_raca == "5", "indigenous", "missing"
                                                                           )
                                                                   )
                                                           )
                                                   )
  ), 
  race = ifelse(is.na(race), "missing", race) %>% 
    as.factor() %>% 
    relevel(ref = "brown")
  )
  # Received Xpert tested - Tested defined as having a conclusive resistance result (either resistant or sensitive)
  sinan_tmp <- sinan_tmp %>% mutate(tested = if_else(test_molec %in% c("1", "2"), "tested", "not tested") %>% as.factor()) 
  
  # Xpert result
  sinan_tmp <- sinan_tmp %>% mutate(result = if_else(test_molec %in% c("2"), "positive",
                                                     if_else(test_molec %in% c("1"), "negative", "missing")) %>% 
                                      as.factor()
  )
  # Homelessness
  sinan_tmp <- sinan_tmp %>% mutate(pop_rua = if_else(pop_rua == 1, "yes",
                                                      if_else(pop_rua == 2, "no", NA)
                                                      ), 
  pop_rua = ifelse(is.na(pop_rua), "missing", pop_rua) %>% 
    as.factor() %>% 
    relevel(ref = "no")
  ) 
  
  
  
  # Incarcerated
  sinan_tmp <- sinan_tmp %>% mutate(pop_liber = if_else(pop_liber == 1, "yes",
                                                        if_else(pop_liber == 2, "no", NA)
                                                        ),
  pop_liber = ifelse(is.na(pop_liber), "missing", pop_liber) %>% 
    as.factor() %>% 
    relevel(ref = "no"))
  
  
  # Smoking
  sinan_tmp <- sinan_tmp %>% mutate(agravtabac = if_else(agravtabac == 1, "yes",
                                                         if_else(agravtabac == 2, "no", NA)
  ),
  agravtabac = ifelse(is.na(agravtabac), "missing", agravtabac) %>% 
    as.factor() %>% 
    relevel(ref = "no"))
  
  # Alcohol
  sinan_tmp <- sinan_tmp %>% mutate(agravalcoo = if_else(agravalcoo == 1, "yes",
                                                         if_else(agravalcoo == 2, "no", NA)
  ),
  agravalcoo = ifelse(is.na(agravalcoo), "missing", agravalcoo) %>% 
    as.factor() %>% 
    relevel(ref = "no"))
  
  # Ilicit drug use
  sinan_tmp <- sinan_tmp %>% mutate(agravdroga = if_else(agravdroga == 1, "yes",
                                                         if_else(agravdroga == 2, "no", NA)
  ),
  agravdroga = ifelse(is.na(agravdroga), "missing", agravdroga) %>% 
    as.factor() %>% 
    relevel(ref = "no"))
  
  
  # Diabetes
  sinan_tmp <- sinan_tmp %>% mutate(agravdiabe = if_else(agravdiabe == 1, "yes",
                                                         if_else(agravdiabe == 2, "no", NA)
  ),
  agravdiabe = ifelse(is.na(agravdiabe), "missing", agravdiabe) %>% 
    as.factor() %>% 
    relevel(ref = "no"))
  
  # Immigration status
  sinan_tmp <- sinan_tmp %>% mutate(
    pop_imig = if_else(pop_imig == 1, "yes",
                       if_else(pop_imig == 2, "no", NA)
    ),
    pop_imig = ifelse(is.na(pop_imig), "missing", pop_imig) %>% 
      as.factor() %>% 
      relevel(ref = "no"))
  
  
  
  
  # Age 
  # Create age based on date of birth and nu_idade_n
  sinan_tmp <- sinan_tmp %>% mutate(dt_nasc = as.Date(as.character(dt_nasc), format = "%Y%m%d"))
  sinan_tmp <- sinan_tmp %>% mutate(age_nasc = floor(as.numeric((dt_diag - dt_nasc) / 365)))
  sinan_tmp <- sinan_tmp %>% mutate(age_idade = if_else(nu_idade_n > 4000, nu_idade_n - 4000,
                                                        if_else(nu_idade_n < 4000, 1,
                                                                # NTP says these
                                                                # should mostly
                                                                # be children -
                                                                # Double check
                                                                # because a
                                                                # number are
                                                                # recorded as
                                                                # being in
                                                                # prison; and
                                                                # dt_diag equals
                                                                # or is close to
                                                                # dt_nasc
                                                                if_else(nu_idade_n %in% c(4000, 3000, 2000, 1000), 1, NA)
                                                                )
                                                        )
                                    )
  
  
  # Create flag for age: 
  # Identifies discrepancies in nu_idade_n (i.e. adults coded as children)
  # Create preliminary age flag for children 
  sinan_tmp <- sinan_tmp %>% mutate(age_flag = 
                            # If there are children in prison, with education, etc., double check age nasc --> Missing age (Most cases are missing DOB anyway so can't compare)
                            if_else(age_idade >= 1 & age_idade <= 4 & (pop_liber == "yes" | agravdroga == "yes" | agravalcoo == "yes" | agravtabac == "yes" | cs_escol_n %in% !c(0,1,2,3,9,10,NA)), "0-4 flag", 
                                    # --> Missing age
                                    if_else(age_idade >= 5 & age_idade <= 14 & pop_liber == "yes", "5-14 flag",  
                                            # Identify full matches --> idade 
                                            if_else(abs(age_idade - age_nasc) <= 5, "Match - Use age_idade", 
                                                    # Other NA --> idade
                                                    NA))))
# Update age flag
sinan_tmp <- sinan_tmp %>% 
  # If last two digits of age_idade match dt-nasc -> age nasc
  mutate(age_flag = if_else(age_nasc > 15 & age_nasc == nu_idade_n %% 100, "Match - Use age_nasc", age_flag), 
         # Make sure all 0-4 YOs are correct 
         age_flag = if_else(age_flag == "0-4 flag" & cs_escol_n %in% c(0,1,2,3,9,10,NA) & pop_liber != "yes" & agravdroga != "yes" & agravalcoo != "yes" & agravtabac != "yes", "Match - Use age_idade", age_flag), 
         # If DOB isn't missing, use DOB
         age_flag = if_else(age_flag == "0-4 flag" & (age_nasc > 4 & !is.na(dt_nasc)), "Match - Use age_nasc", age_flag), 
         # If DOB is missing and not in either flags, use age_idade
         age_flag = if_else(is.na(age_flag) & (is.na(dt_nasc) | age_idade > 4), "Match - Use age_idade", age_flag))



  # Create age variable based on age flag 
  sinan_tmp <- sinan_tmp %>% mutate(age = if_else(age_flag %in% c("0-4 flag", "5-14 flag"), NA, 
                                                  if_else(age_flag == "Match - Use age_nasc", age_nasc, 
                                                          if_else(age_flag == "Match - Use age_idade", age_idade, NA))))
  
  # Finalize age categories
  sinan_tmp$age_cat <- case_when(
    sinan_tmp$age >= 0 & sinan_tmp$age < 5 ~ "0-4",
    sinan_tmp$age >= 5 & sinan_tmp$age < 15 ~ "5-14",
    sinan_tmp$age >= 15 & sinan_tmp$age < 25 ~ "15-24",
    sinan_tmp$age >= 25 & sinan_tmp$age < 35 ~ "25-34",
    sinan_tmp$age >= 35 & sinan_tmp$age < 45 ~ "35-44",
    sinan_tmp$age >= 45 & sinan_tmp$age < 55 ~ "45-54",
    sinan_tmp$age >= 55 & sinan_tmp$age < 65 ~ "55-64",
    sinan_tmp$age >= 65 ~ "65+", 
    is.na(sinan_tmp$age) ~ "missing" 
  )
  
  # Compare age category based on other age variables
  ## Create age category based on date of birth
  # sinan_tmp$age_cat_nasc <- case_when(
  #   sinan_tmp$age_nasc >= 0 & sinan_tmp$age_nasc < 5 ~ "0-4",
  #   sinan_tmp$age_nasc >= 5 & sinan_tmp$age_nasc < 15 ~ "5-14",
  #   sinan_tmp$age_nasc >= 15 & sinan_tmp$age_nasc < 25 ~ "15-24",
  #   sinan_tmp$age_nasc >= 25 & sinan_tmp$age_nasc < 35 ~ "25-34",
  #   sinan_tmp$age_nasc >= 35 & sinan_tmp$age_nasc < 45 ~ "35-44",
  #   sinan_tmp$age_nasc >= 45 & sinan_tmp$age_nasc < 55 ~ "45-54",
  #   sinan_tmp$age_nasc >= 55 & sinan_tmp$age_nasc < 65 ~ "55-64",
  #   sinan_tmp$age_nasc >= 65 ~ "65+"
  # )


  ## Create age category based on observed age variable (What NTP says is more reliable)
  # sinan_tmp$age_cat_idade <- case_when(
  #   sinan_tmp$age_idade >= 0 & sinan_tmp$age_idade < 5 ~ "0-4",
  #   sinan_tmp$age_idade >= 5 & sinan_tmp$age_idade < 15 ~ "5-14",
  #   sinan_tmp$age_idade >= 15 & sinan_tmp$age_idade < 25 ~ "15-24",
  #   sinan_tmp$age_idade >= 25 & sinan_tmp$age_idade < 35 ~ "25-34",
  #   sinan_tmp$age_idade >= 35 & sinan_tmp$age_idade < 45 ~ "35-44",
  #   sinan_tmp$age_idade >= 45 & sinan_tmp$age_idade < 55 ~ "45-54",
  #   sinan_tmp$age_idade >= 55 & sinan_tmp$age_idade < 65 ~ "55-64",
  #   sinan_tmp$age_idade >= 65 ~ "65+"
  # )

  # Compare age_cat to age_idade
  # tabyl(sinan_tmp, age_cat, age_cat_idade)

  sinan_tmp$age_cat <- factor(sinan_tmp$age_cat, levels = c("0-4", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65+", "missing")) %>% relevel(ref = "25-34")

  
  # Education level
  sinan_tmp <- sinan_tmp %>% 
    mutate(cs_escol_n = case_when(
      cs_escol_n == 0 ~ "illiterate",
      cs_escol_n >= 1 & cs_escol_n <= 3 ~ "some primary school",
      cs_escol_n == 4 ~ "completed 8th grade",
      cs_escol_n == 5 ~ "some secondary school",
      cs_escol_n == 6 ~ "completed secondary school",
      cs_escol_n == 7 ~ "some university",
      cs_escol_n == 8 ~ "completed university",
      cs_escol_n == 10 & age_cat == "0-4" ~ "NA - child",
      cs_escol_n == 10 & age_cat != "0-4" ~ "missing",
      cs_escol_n == 9 ~ "missing",
      is.na(cs_escol_n) ~ "missing") %>%
        as.factor())
    


  # Clean closure type
  sinan_tmp <- sinan_tmp %>% 
    mutate(situa_ence = case_when(
      situa_ence %in% c(" 1", "01", "1") ~ "01",
      situa_ence %in% c(" 2", "02", "2", "-2") ~ "02",
      situa_ence %in% c(" 3", "03", "3", "-3") ~ "03",
      situa_ence %in% c(" 4", "04", "4") ~ "04",
      situa_ence %in% c(" 5", "05", "5") ~ "05",
      situa_ence %in% c(" 6", "06", "6") ~ "06",
      situa_ence %in% c(" 7", "07", "7") ~ "07",
      situa_ence %in% c(" 8", "08", "8") ~ "08",
      situa_ence %in% c(" 9", "09", "9") ~ "09",
      situa_ence %in% c("10") ~ "10",
      situa_ence %in% c("", "NA") ~ "missing"
    ) %>%
    as.factor())
  
  sinan_tmp <- sinan_tmp %>% mutate(
    state = as.factor(sg_uf_clean),
    tratamento = as.factor(tratamento)
  )
  

  return(sinan_tmp)
}
