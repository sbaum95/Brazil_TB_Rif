# Author: Sarah Baum
# Created: 2023-09-20
# Updated: 2023-11-11
# Description: 
# -- Creates binary variable for whether there is a prison in municipality in a given year
# -- Issues with merging based on municipality
# -- Source: https://www.gov.br/senappen/pt-br/servicos/sisdepen/bases-de-dados


files <- list.files(path = "data/prison", pattern = ".xlsx", full.names = TRUE)

data_list <- list()

# load xlsx files from folder
for (name in files) {
  # Load the CSV file and store it in the list
  data <- read_excel(name)
  data_list[[basename(name)]] <- data
}




# Loop through the lists and rename the files
new_file_names <- c("2019.2", "2019.1", "2018.2", "2018.1", "2017.2", "2017.1", "2016", "2015", "2014")

data_list_clean<- list()



# Create data frames with count of prisons per year
# for (i in 1:length(data_list)) {
  
for (i in 1:11) { # the only thing that changes is the NA file, which doesn't have IBGE codes
  
  new_name <- new_file_names[i]
  df <- data_list[[i]] 

# create function that replaces "`" in municipio with "'"
replace_backticks <- function(text) {
  if ("`" %in% text) {
    text <- gsub("`", "'", text)
  }
  return(text)
}



# clean files based on how old they are 
if ("UF" %in% names(df) || "UF:" %in% names(df)) {
  if ("UF" %in% names(df)) {
    
    df <- df %>% 
  mutate(Município = replace_backticks(Município)) %>%
    group_by (UF, Município, `Código IBGE`) %>%
      # group_by (UF, Município) %>% 
      summarize(is_prison = if_else(n() == 0 , 0, 1)) %>% 
      rename(!! paste0('is_prison', '_', new_name) := is_prison) %>% 
  mutate(UF = tolower(UF), 
         Município = tolower(Município)) %>% 
      rename(IBGE = `Código IBGE`)

  } else {
    
df <- df %>% 
  mutate(Cidade = replace_backticks(Cidade)) %>%
  # one df had full state name and then acronym in ()
  mutate(`UF:` = ifelse(str_detect(`UF:`, "\\([^()]+\\)"), sub(".*\\(([^)]+)\\).*", "\\1", `UF:`), `UF:`)) %>% 
    group_by (`UF:`, Cidade, `Código IBGE`) %>%
  # group_by (`UF:`, Cidade) %>% 
      summarize(
        is_prison = if_else(n() ==  0, 0, 1)) %>% 
  mutate(`UF:` = tolower(`UF:`), 
         Cidade = tolower(Cidade)) %>% 
      rename(!! paste0('is_prison', '_', new_name) := is_prison, 
             UF = `UF:`, 
             Município = Cidade, 
             IBGE = `Código IBGE`) 
  }
  
# new_col <- paste0("IBGE_", new_name)
# df <- df %>% rename(new_col = `Código IBGE`)
}

df_name <- paste0("dataset_", new_name)

data_list_clean[[df_name]] <- df

}




# merge dataframes in the list
merged_df <- data_list_clean %>%
  reduce(function(df1, df2) full_join(df1, df2, by = c("UF", "Município", "IBGE")))


# Note: There is a dataframe "is_prison_NA" - which is there is a prison there, but not sure on what year

# collapse into a single df
prison_merge <- merged_df %>% 
  mutate(id_municip = as.numeric(substr(as.character(IBGE), 1, nchar(IBGE)-1))) %>% 
  filter(!is.na(id_municip)) %>% 
  mutate_at(vars("is_prison_2019.2":"is_prison_2014"), ~ifelse(is.na(.), 0, .)) %>% 
  # combine columns for a single year
  mutate(
    # `2021` = if_else(is_prison_2021.2 + is_prison_2021.1 > 0, 1, 0), 
    #      `2020` = if_else(is_prison_2020.2 + is_prison_2020.1 > 0, 1, 0),
         `2019` = if_else(is_prison_2019.2 + is_prison_2019.1 > 0, 1, 0),
         `2018` = if_else(is_prison_2018.2 + is_prison_2018.1 > 0, 1, 0),
         `2017` = if_else(is_prison_2017.2 + is_prison_2017.1 > 0, 1, 0), 
         `Unknown` = if_else(is_prison_NA > 0, 1, 0)) %>% 
  rename(`2016` = "is_prison_2016",
         `2015` = "is_prison_2015", 
         `2014` = "is_prison_2014")  %>% 
  #exclude those where the year is not known
  select(Município, id_municip, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`
        
         # `2020`, `2021`,  `Unknown`
         ) %>% 
  pivot_longer(cols = c(`2014`, `2015`, `2016`, `2017`, `2018`, `2019`
                        # `2020`, `2021`
                        ), names_to = "year") %>% 
  rename(name_mun = Município, 
         mun_has_prison = value)
  
  # to remove municip with only one prison during period 
  # group_by(id_municip) %>% 
  # mutate(count = sum(mun_has_prison)) %>% 
  # filter(count > 1)

# # check out merged data 
# # number of years with prisons
count <- prison_merge %>%
  group_by(id_municip) %>%
  mutate(count = sum(mun_has_prison))


## Look to see how many municips only have prisons in a handful of years (for
## all municips)
count %>% 
  group_by(id_municip) %>% 
  summarize(years_with_prison = sum(mun_has_prison)) %>% 
  tabyl(years_with_prison)



## Now, look to see how many of the municips in SINAN only have a prison for a
## handful of years. Identify how many municips in sinan have a prison. Also
## count up how many cases this represents.
sinan_municips <- sinan_xpert %>% 
  group_by(id_municip) %>% 
  summarize(cases = n()) %>% 
  select(id_municip, cases) %>% 
  unique()

aggregated_count <- count %>% 
  group_by(id_municip) %>% 
  summarize(years_with_prison = sum(mun_has_prison))

test <- merge(aggregated_count, sinan_municips, by = "id_municip")

#### How many cases are occuring in municips by years_with_prison? 
test %>% 
  mutate(total_cases = sum(cases)) %>% 
  group_by(years_with_prison) %>% 
  summarize(share_of_cases = sum(cases)/total_cases) %>% 
  unique()
  tabyl(test, years_with_prison, cases)



## Now, look to see among those without a prison for the full period but who
## have a prison for > 1 year, have a prison in one-off years

  
  




# # which years are there only one prison
<- count %>%
  group_by(id_municip) %>%
  filter(count==1 & mun_has_prison ==1) %>%
  tabyl(year)

# count %>% 
#   group_by(id_municip) %>% 
#   filter(count==2 & mun_has_prison ==1) %>% 
#   tabyl(year)



# collapse to merge with sinan
prison_final <- prison_merge %>% 
  filter(!is.na(mun_has_prison)) %>% 
  group_by(name_mun, id_municip) %>% 
  summarize(mun_has_prison = if_else(sum(mun_has_prison) > 0, 1, 0))





# merge to sinan
sinan_tmp <-  left_join(sinan_tmp, prison_final %>% mutate(id_municip = as.factor(id_municip)), by = c("id_mn_resi" = "id_municip"))

sinan_tmp <- sinan_tmp %>% mutate(mun_has_prison = if_else(is.na(mun_has_prison), 0, 1))

tabyl(sinan_tmp, mun_has_prison)
# mic_has_prison <- sinan_tmp %>% 
#   select(name_mun, id_micro, mun_has_prison) %>%
#   unique() %>%
#   group_by(id_micro) %>% 
#   mutate(mic_has_prison = if_else(sum(mun_has_prison) > 0, 1, 0)) %>% 
#   select(-c(municip_name, mun_has_prison)) %>% 
#   unique() 

# sinan_xpert <- left_join(sinan_xpert, mic_has_prison , by = c("id_micro")) 

  
