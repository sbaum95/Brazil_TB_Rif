# Author: Sarah Baum
# Created: 2024-04-03
# Updated: 2024-04-05
# Description: Loads sinan data and cleans



load_and_clean_sinan <- function() {
  # Load sinan --------------------------------------------------------------
  sinan <- read_dta("data/fim_abr_2023_no name.dta")


  sinan_tmp <- sinan %>%
    # Create quarter and years based on date when patient was diagnosed with TB
    mutate(
      not_yr = as.Date(nu_ano, format = "%Y"),
      dt_nasc = as.Date(dt_nasc, format = "%Y-%m-%d"),
      dt_diag = as.Date(dt_diag, format = "%Y-%m-%d"),
      diag_yr = floor_date(as_date(dt_diag), "year"),
      diag_qrt = floor_date(as_date(dt_diag), "quarter"),
      tratamento = as.factor(tratamento),
      sg_uf = if_else(sg_uf == "" | sg_uf == "9", NA, sg_uf) %>% as.factor(),
      id_mn_resi = if_else(id_mn_resi == "" | id_mn_resi == "9", NA, id_mn_resi) %>% as.factor(),
      sg_uf_not = if_else(sg_uf_not == "" | sg_uf_not == "9", NA, sg_uf_not) %>% as.factor(),
      id_municip = if_else(id_municip == "" | id_municip == "9", NA, id_municip) %>% as.factor(),
      pop_imig = as.factor(pop_imig)
    ) %>%
    rename(id_mn_not = id_municip)



  # Add type of health unit data --------------------------------------------
  source(here::here("code/01_data_processing/clean_health_unit_type.R"))
  
  sinan_tmp$diag_yr <- as.numeric(year(sinan_tmp$diag_yr)) # only run this line once - otw origin error
  
  sinan_tmp <- load_health_unit_type(years = c("2019", "2018", "2017", "2016", "2015", "2014"), sinan_tmp = sinan_tmp)
  
  
  
  # Impute missing state/mun ------------------------------------------------
  # Impute missing state and municipality with location of health unit (117 patients have a missing  state, 119 missing municip)
 sinan_tmp <- sinan_tmp %>%
  mutate(
    id_mn_resi_clean = if_else(is.na(id_mn_resi) & !is.na(id_mn_unidade), id_mn_unidade,
      if_else(is.na(id_mn_resi) & is.na(id_mn_unidade), id_mn_not, id_mn_resi)
    ),
    sg_uf_clean = if_else(is.na(sg_uf) & !is.na(sg_uf_unidade), sg_uf_unidade,
      if_else(is.na(sg_uf) & is.na(sg_uf_unidade), sg_uf_not, sg_uf)
    )
  )

  
  # Add municipality name and population ------------------------------------

  # add state name and region
  state_name <- read_xlsx("data/StateCodes.xlsx") %>%
    mutate(sg_uf = as.factor(sg_uf))

  sinan_tmp <- left_join(sinan_tmp, state_name %>% select(sg_uf, NAME_1, NAME_2), by = c("sg_uf_clean" = "sg_uf")) %>% 
    rename(state_nm = NAME_1, region_nm = NAME_2)




  # add municipality names and population data
  mun_population <- read_excel("data/MunicipalCodes.xlsx") %>%
    rename(id_mn_resi = cod.6.2010, sg_uf = cod.sg.2010, municip_name = "Município 2010") %>%
    mutate(
      id_mn_resi = as.factor(id_mn_resi),
      sg_uf = as.factor(sg_uf),
      mun_pop_2010 = as.numeric(gsub("\\.", "", pop.tot.2010))
    )

  sinan_tmp <- left_join(sinan_tmp, mun_population %>% select(id_mn_resi, sg_uf, mun_pop_2010), 
                         by = c("id_mn_resi_clean" = "id_mn_resi", "sg_uf_clean" = "sg_uf"))




  
  
  
  # Add latitude and longitude ----------------------------------------------
  source(here::here("code/01_data_processing/get_municipality_centroid.R"))
  
  centroid <- get_municipality_centroid()
  
  sinan_tmp <- left_join(sinan_tmp, centroid %>% select(CD_MUN_merge, lat, lon), by = c("id_mn_resi" = "CD_MUN_merge"))
  



  # Clean age categories ----------------------------------------------------
  sinan_tmp <- sinan_tmp %>%
    # Create age two ways use dt_nasc and nu_idade
    mutate(
      age_nasc = floor(as.numeric((dt_diag - dt_nasc) / 365)),
      age_idade = if_else(nu_idade_n > 4000, nu_idade_n - 4000,
        if_else(nu_idade_n < 4000, 1,
          if_else(nu_idade_n %in% c(4000, 3000, 2000, 100), 1, -1)
        )
      )
    )



  # Create age category based on date of birth
  sinan_tmp$age_cat_nasc <- case_when(
    sinan_tmp$age_nasc >= 0 & sinan_tmp$age_nasc < 5 ~ "0-4",
    sinan_tmp$age_nasc >= 5 & sinan_tmp$age_nasc < 15 ~ "5-14",
    sinan_tmp$age_nasc >= 15 & sinan_tmp$age_nasc < 25 ~ "15-24",
    sinan_tmp$age_nasc >= 25 & sinan_tmp$age_nasc < 35 ~ "25-34",
    sinan_tmp$age_nasc >= 35 & sinan_tmp$age_nasc < 45 ~ "35-44",
    sinan_tmp$age_nasc >= 45 & sinan_tmp$age_nasc < 55 ~ "45-54",
    sinan_tmp$age_nasc >= 55 & sinan_tmp$age_nasc < 65 ~ "55-64",
    sinan_tmp$age_nasc >= 65 ~ "65+"
  )


  # Create age category based on observed age variable (What NTP says is more reliable)
  sinan_tmp$age_cat_idade <- case_when(
    sinan_tmp$age_idade >= 0 & sinan_tmp$age_idade < 5 ~ "0-4",
    sinan_tmp$age_idade >= 5 & sinan_tmp$age_idade < 15 ~ "5-14",
    sinan_tmp$age_idade >= 15 & sinan_tmp$age_idade < 25 ~ "15-24",
    sinan_tmp$age_idade >= 25 & sinan_tmp$age_idade < 35 ~ "25-34",
    sinan_tmp$age_idade >= 35 & sinan_tmp$age_idade < 45 ~ "35-44",
    sinan_tmp$age_idade >= 45 & sinan_tmp$age_idade < 55 ~ "45-54",
    sinan_tmp$age_idade >= 55 & sinan_tmp$age_idade < 65 ~ "55-64",
    sinan_tmp$age_idade >= 65 ~ "65+"
  )


  sinan_tmp <- sinan_tmp %>%
    mutate(
      age =
      # Nasc and idade are in agreement or they are off by a little, then go with idade
        if_else(abs(age_nasc - age_idade) <= 5, age_idade,
          # NTP said these idade codes are most likely children, make sure that is the case and then go with idade
          if_else(nu_idade_n %in% c(1000, 2000, 3000, 4000) & (cs_escol_n %in% c("0", "1-3") & agravalcoo != 1 & agravtabac != 1 & pop_liber != 1), age_idade,
            # If the last two digits of nu_idade == age_nasc, then use age_nasc (maybe they messed up 4 vs. 3)
            if_else(age_nasc == nu_idade_n %% 100, age_nasc,
              # If idade is not 4 digits, and it == age_nasc, then use age_nasc
              if_else(nchar(as.character(nu_idade_n)) == 2, nu_idade_n,
                if_else(dt_nasc > dt_diag, NA, age_idade)
              )
            )
          )
        )
    )


  # Remove 0-4 YOs who have schooling
  sinan_tmp <- sinan_tmp %>% mutate(age = if_else(age <= 4 & cs_escol_n %in% c("4", "5", "6", "7", "8"), NA, age)) #| agravalcoo == 1 | agravtabac == 1 | pop_liber == 1), NA,


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





  # Look into incorrectly coded age categories and 0-4 YOs
  # test <- sinan_tmp %>%
  #   select(tratamento, age_nasc, nu_idade_n, age_idade, age_cat_nasc, age_cat_idade, cs_escol_n, agravalcoo, agravtabac, pop_liber, age) %>%
  #   filter((age_cat_nasc != age_cat_idade & age_nasc-age_idade > 1) | (age_cat_nasc == "0-4" & is.na(age_cat_idade)| age_cat_idade == "0-4" & is.na(age_cat_nasc))) %>%
  #   filter(is.na(age))



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
            if_else(agravaids == "2", "negative", "missing")
          )
        )
      ) %>% as.factor() %>% relevel(ref = "negative"),

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
        if_else(pop_rua == 2, "no", "missing")
      ) %>% as.factor() %>% relevel(ref = "no"),

      # Incarcerated
      pop_liber = if_else(pop_liber == 1, "yes",
        if_else(pop_liber == 2, "no", "missing")
      ) %>% as.factor() %>% relevel(ref = "no"),

      # Smoking
      agravtabac = if_else(agravtabac == 1, "yes",
        if_else(agravtabac == 2, "no", "missing")
      ) %>% as.factor() %>% relevel(ref = "no"),

      # Alcohol
      agravalcoo = if_else(agravalcoo == 1, "yes",
        if_else(agravalcoo == 2, "no", "missing")
      ) %>% as.factor() %>% relevel(ref = "no"),

      # Ilicit drug use
      agravdroga = if_else(agravdroga == 1, "yes",
        if_else(agravdroga == 2, "no", "missing")
      ) %>% as.factor() %>% relevel(ref = "no"),

      # Diabetes
      agravdiabe = if_else(agravdiabe == 1, "yes",
        if_else(agravdiabe == 2, "no", "missing")
      ) %>% as.factor() %>% relevel(ref = "no"),

      # Immigration status
      pop_imig = if_else(pop_imig == 1, "yes",
        if_else(pop_imig == 2, "no", "missing") # Includes values coded as 3?
      ) %>% as.factor() %>% relevel(ref = "no")
    ) %>% 
    
    rename(state = sg_uf_clean)




  ## Education
  sinan_tmp$cs_escol_n <- case_when(
    sinan_tmp$cs_escol_n %in% c(" 0", "0", "00") ~ "0",
    sinan_tmp$cs_escol_n %in% c(" 1", " 2", " 3", "01", "02", "03", "1", "2", "3") ~ "1-3",
    sinan_tmp$cs_escol_n %in% c(" 4", "04", "4") ~ "4",
    sinan_tmp$cs_escol_n %in% c(" 5", "05", "5") ~ "5",
    sinan_tmp$cs_escol_n %in% c(" 6", "06", "6") ~ "6",
    sinan_tmp$cs_escol_n %in% c(" 7", "07", "7") ~ "7",
    sinan_tmp$cs_escol_n %in% c(" 8", "08", "8") ~ "8",
    sinan_tmp$cs_escol_n %in% c("10") ~ "10",
    sinan_tmp$cs_escol_n %in% c(" 9", "09", "9", "") ~ "missing"
  ) %>%
    as.factor()



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
