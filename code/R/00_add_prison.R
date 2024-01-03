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
for (i in 1:length(data_list)) {
  
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
    group_by (UF, Município) %>% 
      summarize(is_prison = if_else(n() == 0 , 0, 1)) %>% 
      rename(!! paste0('is_prison', '_', new_name) := is_prison) %>% 
  mutate(UF = tolower(UF), 
         Município = tolower(Município))

  } else {
    
df <- df %>% 
  mutate(Cidade = replace_backticks(Cidade)) %>%
  # one df had full state name and then acronym in ()
  mutate(`UF:` = ifelse(str_detect(`UF:`, "\\([^()]+\\)"), sub(".*\\(([^)]+)\\).*", "\\1", `UF:`), `UF:`)) %>% 
    group_by (`UF:`, Cidade) %>% 
      summarize(
        is_prison = if_else(n() ==  0, 0, 1)) %>% 
  mutate(`UF:` = tolower(`UF:`), 
         Cidade = tolower(Cidade)) %>% 
      rename(!! paste0('is_prison', '_', new_name) := is_prison, 
             UF = `UF:`, 
             Município = Cidade) 
  }

}

df_name <- paste0("dataset_", new_name)

data_list_clean[[df_name]] <- df

}







## Merge all of the files into one data frame
merge_dataframes <- function(df1, df2) {
  merged_df <- merge(df1, df2, by = c("UF", "Município"), all = TRUE)
  return(merged_df)
  
}

# merge dataframes in the list
merged_df <- Reduce(merge_dataframes, data_list_clean) 

# Note: There is a dataframe "is_prison_NA" - which is there is a prison there, but not sure on what year

# collapse into a single df
prison_merge <- merged_df %>% 
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
  select(-UF) %>% 
  select(Município, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `Unknown`
         # `2020`, `2021`
         ) %>% 
  group_by(Município) %>% 
  summarize_all(sum) %>%
  mutate(
    # `2021` = if_else(`2021` > 0, 1, 0), 
    #      `2020` = if_else(`2020` > 0, 1, 0),
         `2019` = if_else(`2019` > 0, 1, 0),
         `2018` = if_else(`2018` > 0, 1, 0),
         `2017` = if_else(`2017` > 0, 1, 0),
         `2016` = if_else(`2016` > 0, 1, 0),
         `2015` = if_else(`2015` > 0, 1, 0),
         `2014` = if_else(`2014` > 0, 1, 0),
         `Unknown` = if_else(`Unknown` > 0, 1, 0),
         ) %>% 
  pivot_longer(cols = c(`2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `Unknown`
                        # `2020`, `2021`
                        ), names_to = "year") %>% 
  rename(name_mun = Município, 
         mun_has_prison = value) %>% 
  filter(!is.na(mun_has_prison)) %>% 
  group_by(name_mun) %>% 
  summarize(mun_has_prison = if_else(sum(mun_has_prison) > 0, 1, 0))


# merge to sinan
sinan_tmp <- left_join(sinan_tmp, prison_merge, by = c("name_mun"))

sinan_tmp <- sinan_tmp %>% mutate(mun_has_prison = if_else(is.na(mun_has_prison), 0, 1))

# mic_has_prison <- sinan_tmp %>% 
#   select(name_mun, id_micro, mun_has_prison) %>%
#   unique() %>%
#   group_by(id_micro) %>% 
#   mutate(mic_has_prison = if_else(sum(mun_has_prison) > 0, 1, 0)) %>% 
#   select(-c(municip_name, mun_has_prison)) %>% 
#   unique() 

# sinan_xpert <- left_join(sinan_xpert, mic_has_prison , by = c("id_micro")) 

  
