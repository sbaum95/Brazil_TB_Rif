# Author: Sarah Baum
# Created: 2024-04-03
# Updated: 2024-05-26
# Description: Loads sinan data and cleans

source("code/dependencies.R")

load_and_clean_sinan <- function() {

# Load sinan --------------------------------------------------------------
  names(sinan) <- tolower(names(sinan))

  sinan_tmp <- sinan %>%
    mutate(
      # Create quarter and years based on date when patient was diagnosed with TB
      dt_diag = as.Date(as.character(dt_diag), format = "%Y%m%d"),
      diag_yr = floor_date(as_date(dt_diag), "year"),
      diag_qrt = floor_date(as_date(dt_diag), "quarter"),
      tratamento = as.factor(tratamento),
      # Clean up location parameters
      sg_uf = if_else(sg_uf == "" | sg_uf == "9", NA, sg_uf) %>% as.factor(),
      id_mn_resi = if_else(id_mn_resi == "" | id_mn_resi == "9", NA, id_mn_resi) %>% as.factor(),
      sg_uf_not = if_else(sg_uf_not == "" | sg_uf_not == "9", NA, sg_uf_not) %>% as.factor(),
      id_municip = if_else(id_municip == "" | id_municip == "9", NA, id_municip) %>% as.factor()
    ) %>%
    rename(id_mn_not = id_municip)

  # Filter to selected years
  sinan_tmp$diag_yr <- as.numeric(year(sinan_tmp$diag_yr))

  sinan_tmp <- sinan_tmp %>% filter(as.numeric(diag_yr) %in% years_to_pull)

# Add type of health unit data --------------------------------------------
source(here::here("code/01_data_processing/clean_health_unit_type.R"))

sinan_tmp <- load_health_unit_type(sinan_tmp)

# Impute missing patient state/mun of residence  ------------------------------------------------
## Note: Impute missing municipality or state of residence with either municipality of treating health unit or notifying health unit
sinan_tmp <- sinan_tmp %>%
  mutate(
    id_mn_resi_clean = if_else(is.na(as.factor(id_mn_resi)) & !is.na(as.factor(id_mn_not)), as.factor(id_mn_not),
      if_else(is.na(as.factor(id_mn_resi)) & is.na(as.factor(id_mn_not)), as.factor(id_munic_a), as.factor(id_mn_resi))
    ),
    sg_uf_clean = if_else(is.na(sg_uf) & !is.na(sg_uf_not), sg_uf_not,
      if_else(is.na(sg_uf) & is.na(sg_uf_not), sg_uf_at, sg_uf)
    ), 
    # Identify what sg_uf is being replaced by
    sg_uf_clean_flag = if_else(is.na(sg_uf) & !is.na(sg_uf_not), "sg_uf_not",
                          if_else(is.na(sg_uf) & is.na(sg_uf_not), "sg_uf_at", "sg_uf")
    )
  )


# Add location characteristics ----------------------------------------

## Add municipality latitude and longitude ----------------------------------------------
source(here::here("code/01_data_processing/get_municipality_centroid.R"))

centroid <- get_municipality_centroid()

# Identify which municipalities of residence have corresponding municip in centroid
## Either merge based on id_mn_resi_clean (if correct) or based on id_mn_not
municip <- as.numeric(as.character(centroid$CD_MUN_merge))

sinan_tmp <- sinan_tmp %>%
  mutate(
    # Identify where municip is coming from
    mn_to_merge_flag = if_else(id_mn_resi_clean %in% municip, "id_mn_resi",
      if_else(id_mn_not %in% municip, "id_mn_not", NA)
    ),
    # Impute municip
    mn_to_merge = if_else(id_mn_resi_clean %in% municip, id_mn_resi_clean,
      if_else(id_mn_not %in% municip, id_mn_not, NA)
    )
  )

# Perform merge
sinan_tmp <- left_join(sinan_tmp, centroid %>% select(CD_MUN_merge, NM_MUN, lat, lon), by = c("mn_to_merge" = "CD_MUN_merge")) %>%
  rename(mn_resi_nm = NM_MUN)

# Update id_mn_resi_clean with correct id_mn
sinan_tmp <- sinan_tmp %>% select(-c(id_mn_resi_clean))
sinan_tmp <- sinan_tmp %>% rename(id_mn_resi_clean = mn_to_merge)


## Add state name ----------------------------------------------------------
state_name <- read_xlsx("data/StateCodes.xlsx") %>% mutate(sg_uf = as.factor(sg_uf))

sinan_tmp <- left_join(sinan_tmp, state_name %>% select(sg_uf, NAME_1, NAME_2), by = c("sg_uf_clean" = "sg_uf")) %>%
  rename(state_nm = NAME_1, region_nm = NAME_2)


# Add state and municipality population -----------------------------------
mun_population <- read_excel("data/MunicipalCodes.xlsx") %>%
  rename(id_mn_resi = cod.6.2010, sg_uf = cod.sg.2010, mn_resi_nm = "Município 2010") %>%
  mutate(
    id_mn_resi = as.factor(id_mn_resi),
    sg_uf = as.factor(sg_uf),
    mun_pop_2010 = as.numeric(gsub("\\.", "", pop.tot.2010))
  )

sinan_tmp <- left_join(sinan_tmp, mun_population %>% select(id_mn_resi, sg_uf, mun_pop_2010),
  by = c("id_mn_resi_clean" = "id_mn_resi", "sg_uf_clean" = "sg_uf")
)
  



# Clean age categories ----------------------------------------------------
  sinan_tmp <- sinan_tmp %>%
    # Create age two ways use dt_nasc and nu_idade
    mutate(
      age_idade = if_else(nu_idade_n > 4000, nu_idade_n - 4000,
        if_else(nu_idade_n < 4000, 1,
          if_else(nu_idade_n %in% c(4000, 3000, 2000, 1000), 1, -1)
        )
      )
    )


  # Clean age: 0-4 YOs who have schooling, Remove individuals who are over 100
  sinan_tmp <- sinan_tmp %>%
    mutate(
      age = if_else((age_idade <= 4 & cs_escol_n %in% c("4", "5", "6", "7", "8")) | age_idade > 100, NA, age_idade),
      age_flag = if_else((age_idade <= 4 & cs_escol_n %in% c("4", "5", "6", "7", "8")), "0-4 w/ school",
        if_else(age_idade > 100, "> 100", "age_idade")
      )
    )


  # Finalize age categories
  sinan_tmp$age_cat <- case_when(
    sinan_tmp$age >= 0 & sinan_tmp$age < 5 ~ "0-4",
    sinan_tmp$age >= 5 & sinan_tmp$age < 15 ~ "5-14",
    sinan_tmp$age >= 15 & sinan_tmp$age < 25 ~ "15-24",
    sinan_tmp$age >= 25 & sinan_tmp$age < 35 ~ "25-34",
    sinan_tmp$age >= 35 & sinan_tmp$age < 45 ~ "35-44",
    sinan_tmp$age >= 45 & sinan_tmp$age < 55 ~ "45-54",
    sinan_tmp$age >= 55 & sinan_tmp$age < 65 ~ "55-64",
    sinan_tmp$age >= 65 ~ "65+"
  )

  sinan_tmp$age_cat <- factor(sinan_tmp$age_cat, levels = c("0-4", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65+"))

  # Clean covariates --------------------------------------------------------
sinan_tmp <- sinan_tmp %>%
  mutate(
    
    # Sex
    sex = if_else(cs_sexo == "M", "male",
                  if_else(cs_sexo == "F", "female", "missing")
    ) %>% as.factor() %>% relevel(ref = "female"),
    
    # HIV status
    hiv_status = if_else(hiv == "1", "positive",
                         if_else(hiv == "2", "negative",
                                 if_else(agravaids == "1", "positive",
                                         if_else(agravaids == "2", "negative", NA)
                                 ))), 
    hiv_status = if_else(is.na(hiv_status), "missing", hiv_status) %>% as.factor() %>% relevel(ref = "negative"),
  
    
    # Age category
    age_cat = if_else(is.na(age_cat), "missing", age_cat) %>% as.factor() %>% relevel(ref = "25-34"),
    
    # Health Unit
    health_unit = if_else(is.na(health_unit), "missing", health_unit) %>% as.factor(),
    
    # Received Xpert tested
    tested = if_else(test_molec %in% c("1", "2"), "tested", "not tested") %>% as.factor(),
    
    # Xpert result
    result = if_else(test_molec %in% c("2"), "positive",
                     if_else(test_molec %in% c("1"), "negative", "missing")
    ) %>% as.factor(),
    
    
    
    # Homelessness
    pop_rua = if_else(pop_rua == 1, "yes",
                      if_else(pop_rua == 2, "no", NA)), 
    
    pop_rua = ifelse(is.na(pop_rua), "missing", pop_rua) %>% as.factor() %>% relevel(ref = "no"),
  
    
  
    # Incarcerated
    pop_liber = if_else(pop_liber == 1, "yes",
                        if_else(pop_liber == 2, "no", NA)), 
  
    pop_liber = ifelse(is.na(pop_liber), "missing", pop_liber) %>% as.factor() %>% relevel(ref = "no"), 
    
    

    # Smoking
    agravtabac = if_else(agravtabac == 1, "yes",
                         if_else(agravtabac == 2, "no", NA)), 
    
    agravtabac = ifelse(is.na(agravtabac), "missing", agravtabac) %>% as.factor() %>% relevel(ref = "no"), 

    
    # Alcohol
    agravalcoo = if_else(agravalcoo == 1, "yes",
                         if_else(agravalcoo == 2, "no", NA)), 
    
    agravalcoo = ifelse(is.na(agravalcoo), "missing", agravalcoo) %>% as.factor() %>% relevel(ref = "no"), 

    
    # Ilicit drug use
    agravdroga = if_else(agravdroga == 1, "yes",
                         if_else(agravdroga == 2, "no", NA)), 
    
    agravdroga = ifelse(is.na(agravdroga), "missing", agravdroga) %>% as.factor() %>% relevel(ref = "no"), 
    
    # Diabetes
    agravdiabe = if_else(agravdiabe == 1, "yes",
                         if_else(agravdiabe == 2, "no", NA)), 
    
    agravdiabe = ifelse(is.na(agravdiabe), "missing", agravdiabe) %>% as.factor() %>% relevel(ref = "no"), 

    # Immigration status
    pop_imig = if_else(pop_imig == 1, "yes",
                       if_else(pop_imig == 2, "no", NA)),  # Includes values coded as 3?
    
    pop_imig  = ifelse(is.na(pop_imig ), "missing", pop_imig ) %>% as.factor() %>% relevel(ref = "no")) %>%  

    mutate(state = as.factor(state), 
           tratamento = as.factor(tratamento))
  


  ## Education - Already cleaned
  # sinan_tmp$cs_escol_n <- case_when(
  #   sinan_tmp$cs_escol_n %in% c(" 0", "0", "00") ~ "0",
  #   sinan_tmp$cs_escol_n %in% c(" 1", " 2", " 3", "01", "02", "03", "1", "2", "3") ~ "1-3",
  #   sinan_tmp$cs_escol_n %in% c(" 4", "04", "4") ~ "4",
  #   sinan_tmp$cs_escol_n %in% c(" 5", "05", "5") ~ "5",
  #   sinan_tmp$cs_escol_n %in% c(" 6", "06", "6") ~ "6",
  #   sinan_tmp$cs_escol_n %in% c(" 7", "07", "7") ~ "7",
  #   sinan_tmp$cs_escol_n %in% c(" 8", "08", "8") ~ "8",
  #   sinan_tmp$cs_escol_n %in% c("10") ~ "10",
  #   sinan_tmp$cs_escol_n %in% c(" 9", "09", "9", "") ~ "missing"
  # ) %>%
  #   as.factor()



  ## Closure type
  sinan_tmp$situa_ence <- case_when(
    sinan_tmp$situa_ence %in% c(" 1", "01", "1") ~ "01",
    sinan_tmp$situa_ence %in% c(" 2", "02", "2", "-2") ~ "02",
    sinan_tmp$situa_ence %in% c(" 3", "03", "3", "-3") ~ "03",
    sinan_tmp$situa_ence %in% c(" 4", "04", "4") ~ "04",
    sinan_tmp$situa_ence %in% c(" 5", "05", "5") ~ "05",
    sinan_tmp$situa_ence %in% c(" 6", "06", "6") ~ "06",
    sinan_tmp$situa_ence %in% c(" 7", "07", "7") ~ "07",
    sinan_tmp$situa_ence %in% c(" 8", "08", "8") ~ "08",
    sinan_tmp$situa_ence %in% c(" 9", "09", "9") ~ "09",
    sinan_tmp$situa_ence %in% c("10") ~ "10",
    sinan_tmp$situa_ence %in% c("", "NA") ~ "missing"
  ) %>%
    as.factor()


  return(sinan_tmp)
}
