# Author: Sarah Baum
# Created: 2023-09-22
# Updated: 2024-03-22
# Description:
# -- Creates bins for avg percentage of individuals in municipality in 2018 receiving BF
# -- Source: https://aplicacoes.cidadania.gov.br/vis/data3/v.php?q[]=oNOclsLerpibuKep3bV%2BgW5g05Kv2rmg2a19ZXR1ZWumaX6JaV2JlGCadGCNrMmim7iareyYsK%2BbjMfDmaWjlMnusm%2BwuqqftHSzr6OgvJxu3bKggOmspKG7qJmpcH2tmo67wKPMo27D3LmnoYOprO6eiImdjsPUmKV4ptLocKKbuJqt7Jh9hWeMiJVjyaCUwdq9lqKDm5vlrLKJq5%2FMxm6lo5TJ7rJvd7uqp5ynrL6coMrAZJ5tko%2Byf5OfqZmZ6ZuziZ2Ow9SYpbGl0uCIb6Kpoa3edIjBrJp63FSqq5LN4MCnm6uWntipr7RXWnehocmtmNDurGdteZSd2p2svpmTeN5u0J6f0OCIqK69mnW0n666qpKSnKbfqq%2Bu8K6isLGZm92ebcKmobjNU86iU83gwKerqaha3qZttJiaGg6f056mfd2yoqGunp3i%2FO7AoI7KgZfZXYPP6rSmnbWWWruoucGYTZ3CoC3qn8bccIWxqaOu4p2uspxNu8ZT2qKm0Oqup1ytolrfmroR5JnAwqaKn5jL4LOdn7H42%2BuirsFXkcaBg9ysms%2FcupVciqSm7JptlJiaGg6f055TwOq6VK6to57aWb2zqU26wqPTsZR96LKir6mhWueabZR4dq%2BiU5tnVq7wrqKwsZmb3Z5tspxNx8am3ayU0JuyoVyulqc85rm3mKB3w5jYopnG3rb33bqem%2BxZsb1XfcnQmtyeoL6bj6Oou5Zav5q6EeSZwMJTzaygfe2yoqCpVareq22xmJ3A1ZSKqpjL7q6gXLaWWr96lqZ4TYmLXY2OqL7pwZ2gqZmfmZ2ybqeSytSiy7BTwuhtmp21%2BOfloq7BV4%2B8z5jQppbGPu6mpamoWt2obZ6pnL7TlNeeU5%2FquaedaHub5vz6uqCOd8Si112lwumxlVy4mqyZnK6%2BoKG4gaDPq6a%2B522inWh7e8KRjm5qV4GLVruylMvvtpidrJpa3Z5tvpygytCU3V2YypuzlakL4qbimsBumZLFxpnToJwgHL%2BdnbtVnuhZncCmlMnCoMtddcznwJVcjpanPOa5t5hNutCgiq%2BYy9%2BuVKytp1rcmr23q453zpjYsJTJm7uVXI52g9F6bYJhV4GLr9p4r9n3iA%3D%3D&ag=m&dt1=2018-01-01&dt2=2019-12-01



load_bolsa_familia <- function() {
  
  # load bf data
  bf <- list.files(path = "data/bf_coverage", pattern = ".xlsx", full.names = TRUE) %>% read_excel()

  # load population data and merge
  source("code/R/load_location_and_population.R")

  # clean bf data
  bf_merge <- bf %>%
    mutate(
      year = floor_date(as_date(month), "year"),
      id_mun = as.factor(id_municip)
    ) %>%
    left_join(mun_pop, by = "id_mun") %>%
    group_by(year, UF, id_mun) %>%
    summarize(
      # calculate avg number of families receiving BF per month by year
      mun_avg_persons_bf = mean(persons), 
      mun_pop_2010 = mun_pop_2010) %>% 
    unique() %>% 
    ungroup() %>% 
    mutate(
      mun_pct_bf = mun_avg_persons_bf / mun_pop_2010,
      mun_bf_cat = cut(mun_pct_bf,
        breaks = quantile(mun_pct_bf, probs = 0:5 / 5, na.rm = TRUE),
        labels = FALSE,
        include.lowest = TRUE
      )
    ) %>%
    filter(year == "2018-01-01")


  return(bf_merge)
}



# merge
# sinan_tmp <- left_join(sinan_tmp, bf_merge %>%
#   ungroup() %>%
#   select(id_municip, mun_avg_persons_bf),
# by = c("id_mn_resi" = "id_municip")
# ) %>%
#   mutate(
#     mun_pct_bf = mun_avg_persons_bf / mun_pop_2010,
#     mun_bf_cat = cut(mun_pct_bf,
#       breaks = quantile(mun_pct_bf, probs = 0:5 / 5, na.rm = TRUE),
#       labels = FALSE,
#       include.lowest = TRUE
#     )
#   )


# ## by microregion
# mic_bf <- sinan_xpert %>%
#   select(id_micro, mun_avg_persons_bf, mic_pop_2010) %>%
#   unique() %>%
#   group_by(id_micro) %>%
#   mutate(mic_avg_person_bf = sum(mun_avg_persons_bf, na.rm = TRUE),
#          mic_pct_bf = mic_avg_person_bf/mic_pop_2010) %>%
#   select(-mun_avg_persons_bf) %>%
#   unique()
#
#
# mic_bf$mic_bf_cat <- cut(mic_bf$mic_pct_bf,
#                          breaks = quantile(mic_bf$mic_pct_bf, probs = 0:5/5, na.rm = TRUE),
#                          labels = FALSE,
#                          include.lowest = TRUE)


# sinan_xpert <- left_join(sinan_xpert, mic_bf %>% select(id_micro, mic_pct_bf, mic_bf_cat),  by = c("id_micro"))
