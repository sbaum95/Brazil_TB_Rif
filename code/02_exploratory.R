# Author: Sarah Baum
# Created: 2023-06-15
# Updated: 
# Description: Exploratory analysis of: 
#   -- Xpert coverage over geography and time 
#   -- determinants of testing 


source(here::here("code/dependencies.R"))

load("data/sinan_xpert.Rdata")

## Questions ##:
# - Do facilities that don't have access to Xpert put "Not Performed" vs. NA

sinan_xpert$tb_hist <- if_else(sinan_xpert$tratamento %in% c("2", "3"), 1, 
                               if_else(sinan_xpert$tratamento %in% c("1"), 0, 
                                       if_else(sinan_xpert$tratamento %in% c("5"), 2, 
                                               if_else(sinan_xpert$tratamento %in% c("6"), 3, NA))))

sinan_xpert$hiv_status <- if_else(sinan_xpert$agravaids == 1, 1, # 1 - HIV positive
                                  if_else(sinan_xpert$agravaids == 2, 0, NA)) # 0 - HIV negative


sinan_xpert$sex <- if_else(sinan_xpert$cs_sexo == "M", 1, 
                           if_else(sinan_xpert$cs_sexo == "F", 0, NA))


# Xpert coverage by year of implementation --------------------------------
start_imp_yr <- xpert_access %>% 
  mutate(implementation_yr = floor_date(as_date(dt_install), "year")) %>% 
  group_by(id_municip) %>% 
  summarize(start_imp_yr = min(implementation_yr))

# Number of municipalities implementing each year 
start_imp_yr %>% 
  group_by(start_imp_yr) %>% 
  count()


fig_xpert_cov <- sinan_xpert %>% 
  filter(id_municip %in% muni_with_xpert) %>% 
  left_join(start_imp_yr, by = "id_municip") %>% 
  filter(start_imp_yr >= "2014-01-01") %>% 
  group_by(start_imp_yr, diag_yr, id_municip) %>% 
  summarize(diagnoses = n(), 
            number_performed = sum(if_else(test_molec %in% c("1", "2", "3"), 1, 0))/diagnoses,
            not_performed = sum(if_else(test_molec %in% c("5"), 1, 0))/diagnoses,
            missing = sum(if_else(is.na(test_molec) | test_molec == "", 1, 0))/diagnoses) %>% 
  group_by(start_imp_yr, diag_yr) %>% 
  summarize(avg = mean(number_performed)) %>% 
  ggplot(aes(x = diag_yr, y = avg, group = factor(start_imp_yr))) + 
  geom_line(aes(color = factor(start_imp_yr))) + 
  xlab("Year") + 
  ylab("Fraction of diagnoses receiving Xpert") + 
  ggtitle("Average share of diagnoses in which Xpert was performed among municipalities by year of implementation") + 
  theme_bw() +
  scale_color_brewer(palette = "Dark2", name = "Year of implementation")

ggsave("figures/fig_xpert_cov.png", plot = fig_xpert_cov, width = 10, height = 6, dpi = 300)









# TB and RR Outcomes ------------------------------------------------------
## Table by year - How does RR-TB change overtime with implmentation
diag <- sinan_xpert %>% 
  filter(id_municip %in% muni_with_xpert) %>% 
  filter(diag_yr < "2023-01-01") %>% 
  left_join(start_imp_yr, by = "id_municip") %>% 
  filter(start_imp_yr >= "2014-01-01") %>% 
  group_by(start_imp_yr, diag_yr) %>% 
  summarize(performed = sum(test_molec %in% c("1", "2", "3", "4")), 
            results_conclusive = sum(test_molec %in% c("1", "2", "3")),
            TB_pos = sum(test_molec %in% c("1", "2")), 
            pct_TB_pos = sum(test_molec %in% c("1", "2"))/sum(test_molec %in% c("1", "2", "3")),
            pct_RR_pos = sum(test_molec %in% c("2"))/sum(test_molec %in% c("1", "2")),
            TB_neg = sum(test_molec %in% c("3")), 
            results_inconclusive = sum(test_molec %in% c("4")), 
            not_performed = sum(test_molec %in% c("5", ""))) %>% 
  select(start_imp_yr, diag_yr, performed, pct_TB_pos, pct_RR_pos) %>% 
  filter(diag_yr >= start_imp_yr)

# TB positivity
fig_TB_pos <- sinan_xpert %>% 
  filter(id_municip %in% muni_with_xpert) %>% 
  left_join(start_imp_yr, by = "id_municip") %>% 
  filter(start_imp_yr >= "2014-01-01") %>% 
  group_by(start_imp_yr, diag_yr) %>% 
  summarize(performed = sum(test_molec %in% c("1", "2", "3", "4")), 
            results_conclusive = sum(test_molec %in% c("1", "2", "3")),
            TB_pos = sum(test_molec %in% c("1", "2")), 
            pct_TB_pos = sum(test_molec %in% c("1", "2"))/sum(test_molec %in% c("1", "2", "3")),
            pct_RR_pos = sum(test_molec %in% c("2"))/sum(test_molec %in% c("1", "2")),
            TB_neg = sum(test_molec %in% c("3")), 
            results_inconclusive = sum(test_molec %in% c("4")), 
            not_performed = sum(test_molec %in% c("5", ""))) %>% 
  ggplot(aes(x = diag_yr, y = pct_TB_pos, group = factor(start_imp_yr))) + 
  geom_line(aes(color = factor(start_imp_yr))) + 
  xlab("Year") + 
  ylab("Fraction of diagnoses - TB+") + 
  ggtitle("Fraction of Xpert diagnoses with a conclusive result that are TB+") + 
  theme_bw() + 
  scale_color_brewer(palette = "Dark2", name = "Year of implementation")

ggsave("figures/fig_TB_pos.png", plot = fig_TB_pos, width = 10, height = 6, dpi = 300)

# RR-TB positivity
fig_RR_pos <- sinan_xpert %>% 
  filter(id_municip %in% muni_with_xpert) %>% 
  left_join(start_imp_yr, by = "id_municip") %>% 
  filter(start_imp_yr >= "2014-01-01") %>% 
  group_by(start_imp_yr, diag_yr) %>% 
  summarize(performed = sum(test_molec %in% c("1", "2", "3", "4")), 
            results_conclusive = sum(test_molec %in% c("1", "2", "3")),
            TB_pos = sum(test_molec %in% c("1", "2")), 
            pct_TB_pos = sum(test_molec %in% c("1", "2"))/sum(test_molec %in% c("1", "2", "3")),
            pct_RR_pos = sum(test_molec %in% c("2"))/sum(test_molec %in% c("1", "2")),
            TB_neg = sum(test_molec %in% c("3")), 
            results_inconclusive = sum(test_molec %in% c("4")), 
            not_performed = sum(test_molec %in% c("5", ""))) %>% 
  ggplot(aes(x = diag_yr, y = pct_RR_pos, group = factor(start_imp_yr))) + 
  geom_line(aes(color = factor(start_imp_yr))) + 
  xlab("Year") + 
  ylab("Fraction of diagnoses - RR-TB+") + 
  ggtitle("Fraction of Xpert diagnoses with a conclusive result that are RR-TB+") + 
  theme_bw() + 
  scale_color_brewer(palette = "Dark2", name = "Year of implementation")

ggsave("figures/fig_RR_pos.png", plot = fig_RR_pos, width = 10, height = 6, dpi = 300)


# by state 
# sinan_xpert %>% 
#   filter(diag_yr < "2023-01-01") %>% 
#   group_by(sg_uf) %>% 
#   summarize(cases = n(), 
#             performed = sum(test_molec %in% c("1", "2", "3", "4")),
#             pct_performed = performed/sum(cases),
#             pct_TB_pos = sum(test_molec %in% c("1", "2"))/sum(test_molec %in% c("1", "2", "3")), 
#             pct_RR_pos = sum(test_molec %in% c("2"))/sum(test_molec %in% c("1", "2")))




## RR pos seems very high?!?



# Patient characteristics -------------------------------------------------

## Age category ------------------------------------------------------------
fig_age_gen <- sinan_xpert %>% 
  filter(id_municip %in% muni_with_xpert) %>% 
  left_join(start_imp_yr, by = "id_municip") %>% 
  filter(start_imp_yr >= "2014-01-01") %>% 
  filter(diag_yr < "2023-01-01" & age_cat !="NA") %>% 
  group_by(age_cat, start_imp_yr, diag_yr) %>% 
  summarize(diagnoses = n()) %>% 
  # group_by(age_cat, start_imp_yr, diag_yr) %>% 
  # summarize(age_cat = age_cat,
  #           diagnoses = diagnoses,
  #           pct = performed/sum(diagnoses)) %>% 
  ggplot(aes(x = diag_yr, y = diagnoses, fill= age_cat)) + 
  geom_bar(position = "fill", stat = "identity") + 
  facet_wrap(~start_imp_yr) + 
  scale_fill_brewer(palette = "Dark2", name = "Age category")+
  xlab("Year") + 
  ylab("Fraction of diagnoses")+
  ggtitle("Fraction of diagnoses by age category")+
  theme_bw()

ggsave("figures/fig_age_gen.png", plot = fig_age_gen, width = 10, height = 6, dpi = 300)



fig_age <- sinan_xpert %>% 
  filter(id_municip %in% muni_with_xpert) %>% 
  left_join(start_imp_yr, by = "id_municip") %>% 
  filter(start_imp_yr >= "2014-01-01") %>% 
  filter(diag_yr < "2023-01-01" & age_cat !="NA") %>% 
  group_by(age_cat, start_imp_yr, diag_yr) %>% 
  summarize(diagnoses = n(), 
            performed = sum(test_molec %in% c("1", "2", "3", "4"))) %>% 
  group_by(age_cat, start_imp_yr, diag_yr) %>% 
  summarize(age_cat = age_cat,
            performed = performed,
            diagnoses = diagnoses,
            pct = performed/sum(diagnoses)) %>% 
  ggplot(aes(x = diag_yr, y = pct, fill= age_cat)) + 
  geom_bar(position = "fill", stat = "identity") + 
  facet_wrap(~start_imp_yr) + 
  scale_fill_brewer(palette = "Dark2", name = "Age category")+
  xlab("Year") + 
  ylab("Fraction of diagnoses with Xpert")+
  ggtitle("Fraction of diagnoses in which Xpert performed by age category")+
  theme_bw()
  

ggsave("figures/fig_age.png", plot = fig_age, width = 10, height = 6, dpi = 300)

fig_age_month <- sinan_xpert %>% 
  filter(id_municip %in% muni_with_xpert) %>% 
  left_join(start_imp_yr, by = "id_municip") %>% 
  filter(start_imp_yr >= "2014-01-01") %>% 
  filter(diag_yrmo < "2023-01-01" & age_cat !="NA") %>% 
  group_by(age_cat, start_imp_yr, diag_yrmo) %>% 
  summarize(diagnoses = n(), 
            performed = sum(test_molec %in% c("1", "2", "3", "4"))) %>% 
  group_by(age_cat, start_imp_yr, diag_yrmo) %>% 
  summarize(age_cat = age_cat,
            performed = performed,
            diagnoses = diagnoses,
            pct = performed/sum(diagnoses)) %>% 
  ggplot(aes(x = diag_yrmo, y = pct, fill= age_cat)) + 
  geom_bar(position = "fill", stat = "identity") + 
  facet_wrap(~start_imp_yr) + 
  scale_fill_brewer(palette = "Dark2", name = "Age category")+
  xlab("Month") + 
  ylab("Fraction of diagnoses with Xpert")+
  ggtitle("Fraction of diagnoses in which Xpert performed by age category")+
  theme_bw()

ggsave("figures/fig_age_month.png", plot = fig_age_month, width = 10, height = 6, dpi = 300)



## Sex ---------------------------------------------------------------------
fig_sex_gen <- sinan_xpert %>% 
  filter(id_municip %in% muni_with_xpert) %>% 
  left_join(start_imp_yr, by = "id_municip") %>% 
  filter(start_imp_yr >= "2014-01-01") %>% 
  filter(diag_yr < "2023-01-01" & cs_sexo !="NA") %>% 
  group_by(cs_sexo, start_imp_yr, diag_yr) %>% 
  summarize(diagnoses = n()) %>% 
  ggplot(aes(x = diag_yr, y = diagnoses, fill= cs_sexo)) + 
  geom_bar(position = "fill", stat = "identity") + 
  facet_wrap(~start_imp_yr) + 
  xlab("Year") + 
  ylab("Fraction of diagnoses")+
  scale_fill_brewer(palette = "Dark2", name = "Sex")+
  ggtitle("Fraction of diagnoses by sex")+
  theme_bw()

ggsave("figures/fig_sex_gen.png", plot = fig_sex_gen, width = 10, height = 6, dpi = 300)

fig_sex <- sinan_xpert %>% 
  filter(id_municip %in% muni_with_xpert) %>% 
  left_join(start_imp_yr, by = "id_municip") %>% 
  filter(start_imp_yr >= "2014-01-01") %>% 
  filter(diag_yr < "2023-01-01" & cs_sexo !="NA") %>% 
  group_by(cs_sexo, start_imp_yr, diag_yr) %>% 
  summarize(diagnoses = n(), 
            performed = sum(test_molec %in% c("1", "2", "3", "4"))) %>% 
  group_by(cs_sexo, start_imp_yr, diag_yr) %>% 
  summarize(cs_sexo = cs_sexo,
            performed = performed,
            diagnoses = diagnoses,
            pct = performed/sum(diagnoses)) %>% 
  ggplot(aes(x = diag_yr, y = pct, fill= cs_sexo)) + 
  geom_bar(position = "fill", stat = "identity") + 
  facet_wrap(~start_imp_yr) + 
  xlab("Year") + 
  ylab("Fraction of diagnoses with Xpert")+
  scale_fill_brewer(palette = "Dark2", name = "Sex")+
  ggtitle("Fraction of diagnoses in which Xpert performed by sex")+
  theme_bw()

ggsave("figures/fig_sex.png", plot = fig_sex, width = 10, height = 6, dpi = 300)

fig_sex_month <- sinan_xpert %>% 
  filter(id_municip %in% muni_with_xpert) %>% 
  left_join(start_imp_yr, by = "id_municip") %>% 
  filter(start_imp_yr >= "2014-01-01") %>% 
  filter(diag_yrmo < "2023-01-01" & cs_sexo !="NA") %>% 
  group_by(cs_sexo, start_imp_yr, diag_yrmo) %>% 
  summarize(diagnoses = n(), 
            performed = sum(test_molec %in% c("1", "2", "3", "4"))) %>% 
  group_by(cs_sexo, start_imp_yr, diag_yrmo) %>% 
  summarize(cs_sexo = cs_sexo,
            performed = performed,
            diagnoses = diagnoses,
            pct = performed/sum(diagnoses)) %>% 
  ggplot(aes(x = diag_yrmo, y = pct, fill= cs_sexo)) + 
  geom_bar(position = "fill", stat = "identity") + 
  facet_wrap(~start_imp_yr) + 
  xlab("Month") + 
  ylab("Fraction of diagnoses with Xpert")+
  scale_fill_brewer(palette = "Dark2", name = "Sex")+
  ggtitle("Fraction of diagnoses in which Xpert performed by sex")+
  theme_bw()

ggsave("figures/fig_sex_month.png", plot = fig_sex_month, width = 10, height = 6, dpi = 300)

 

## TB History --------------------------------------------------------------
fig_hist_gen <- sinan_xpert %>% 
  filter(id_municip %in% muni_with_xpert) %>% 
  left_join(start_imp_yr, by = "id_municip") %>% 
  filter(start_imp_yr >= "2014-01-01") %>% 
  filter(diag_yr < "2023-01-01") %>% 
  group_by(start_imp_yr, diag_yrmo, tratamento) %>% 
  summarize(diagnoses = n()) %>% 
  ggplot(aes(x = diag_yrmo, y = diagnoses, fill= tratamento)) + 
  geom_bar(position = "fill", stat = "identity") + 
  scale_fill_brewer(palette = "Dark2", name = "Entry type", labels = c("New case", "Relapse", "Re-entry after abandonment", "Unknown", "Transfer", "Postmortem"))+
  facet_wrap(~start_imp_yr, scales = "free") +
  ylab("Fraction of diagnoses") +
  xlab("Month") + 
  ggtitle("Diagnoses by entry type") + 
  theme_bw()

ggsave("figures/fig_hist_gen.png", plot = fig_hist_gen, width = 10, height = 6, dpi = 300)

## By Year 
fig_hist_year <- sinan_xpert %>% 
  filter(id_municip %in% muni_with_xpert) %>% 
  left_join(start_imp_yr, by = "id_municip") %>% 
  filter(start_imp_yr >= "2014-01-01") %>% 
  filter(diag_yr < "2023-01-01") %>% 
  group_by(start_imp_yr, diag_yr, tratamento) %>% 
  summarize(performed = sum(test_molec %in% c("1", "2", "3", "4"))) %>% 
  # group_by(start_imp_yr, diag_yr, tratamento) %>% 
  # summarize(tratamento = tratamento,
  #           performed = performed,
  #           pct = performed/sum(diagnoses)) %>% 
  ggplot(aes(x = diag_yr, y = performed, fill= tratamento)) + 
  geom_bar(position = "stack", stat = "identity") + 
  scale_fill_brewer(palette = "Dark2", name = "Entry type", labels = c("New case", "Relapse", "Re-entry after abandonment", "Unknown", "Transfer", "Postmortem"))+
  facet_wrap(~start_imp_yr, scales = "free") +
  ylab("Number diagnoses with Xpert") +
  xlab("Year") + 
  ggtitle("Patients receiving Xpert by entry type and implementation year") + 
  theme_bw()

ggsave("figures/fig_hist_year.png", plot = fig_hist_year, width = 10, height = 6, dpi = 300)


## By Month
fig_hist_month <- sinan_xpert %>% 
  filter(id_municip %in% muni_with_xpert) %>% 
  left_join(start_imp_yr, by = "id_municip") %>% 
  filter(start_imp_yr >= "2014-01-01") %>% 
  filter(diag_yrmo < "2023-01-01") %>% 
  group_by(start_imp_yr, diag_yrmo, tratamento) %>% 
  summarize(performed = sum(test_molec %in% c("1", "2", "3", "4"))) %>% 
  # group_by(start_imp_yr, diag_yr, tratamento) %>% 
  # summarize(tratamento = tratamento,
  #           performed = performed,
  #           pct = performed/sum(diagnoses)) %>% 
  ggplot(aes(x = diag_yrmo, y = performed, fill= tratamento)) + 
  geom_bar(position = "fill", stat = "identity") + 
  scale_fill_brewer(palette = "Dark2", name = "Entry type", labels = c("New case", "Relapse", "Re-entry after abandonment", "Unknown", "Transfer", "Postmortem"))+
  facet_wrap(~start_imp_yr) +
  ylab("Fraction of diagnoses with Xpert") +
  xlab("Month")+
  ggtitle("Patients receiving Xpert by entry type and implementation year") + 
  theme_bw()

ggsave("figures/fig_hist_month.png", plot = fig_hist_month, width = 10, height = 6, dpi = 300)



## HIV status --------------------------------------------------------------
fig_hiv_gen <- sinan_xpert %>% 
  filter(id_municip %in% muni_with_xpert) %>% 
  left_join(start_imp_yr, by = "id_municip") %>% 
  filter(start_imp_yr >= "2014-01-01" & !agravaids %in% c("", "NA")) %>% 
  filter(diag_yr < "2023-01-01") %>% 
  group_by(start_imp_yr, diag_yrmo, agravaids) %>% 
  summarize(diagnoses = n()) %>% 
  ggplot(aes(x = diag_yrmo, y = diagnoses, fill= agravaids)) + 
  geom_bar(position = "fill", stat = "identity") + 
  scale_fill_brewer(palette = "Dark2", name = "HIV Status", labels = c("Positive", "Negative", "Ignored"))+
  facet_wrap(~start_imp_yr) +
  ylab("Fraction of Xpert performed") +
  xlab("Year") + 
  ggtitle("Fraction of diagnoses by HIV status") + 
  theme_bw()

ggsave("figures/fig_hiv_gen.png", plot = fig_hiv_gen, width = 10, height = 6, dpi = 300)


## By Year
fig_hiv <- sinan_xpert %>% 
  filter(id_municip %in% muni_with_xpert) %>% 
  left_join(start_imp_yr, by = "id_municip") %>% 
  filter(start_imp_yr >= "2014-01-01" & !agravaids %in% c("", "NA")) %>% 
  filter(diag_yr < "2023-01-01") %>% 
  group_by(start_imp_yr, diag_yr, agravaids) %>% 
  summarize(performed = sum(test_molec %in% c("1", "2", "3", "4"))) %>% 
  ggplot(aes(x = diag_yr, y = performed, fill= agravaids)) + 
  geom_bar(position = "fill", stat = "identity") + 
  scale_fill_brewer(palette = "Dark2", name = "HIV Status", labels = c("Positive", "Negative", "Ignored"))+
  facet_wrap(~start_imp_yr) +
  ylab("Fraction of Xpert performed") +
  xlab("Year") + 
  ggtitle("Fraction of diagnoses in which Xpert performed by HIV status") + 
  theme_bw()

ggsave("figures/fig_hiv.png", plot = fig_hiv, width = 10, height = 6, dpi = 300)

fig_hiv_month <- sinan_xpert %>% 
  filter(id_municip %in% muni_with_xpert) %>% 
  left_join(start_imp_yr, by = "id_municip") %>% 
  filter(start_imp_yr >= "2014-01-01" & !agravaids %in% c("", "NA")) %>% 
  filter(diag_yrmo < "2023-01-01") %>% 
  group_by(start_imp_yr, diag_yrmo, agravaids) %>% 
  summarize(performed = sum(test_molec %in% c("1", "2", "3", "4"))) %>% 
  ggplot(aes(x = diag_yrmo, y = performed, fill= agravaids)) + 
  geom_bar(position = "fill", stat = "identity") + 
  scale_fill_brewer(palette = "Dark2", name = "HIV Status", labels = c("Positive", "Negative", "Ignored"))+
  facet_wrap(~start_imp_yr) +
  ylab("Fraction of Xpert performed") +
  xlab("Month") + 
  ggtitle("Fraction of diagnoses in which Xpert performed by HIV status") + 
  theme_bw()

ggsave("figures/fig_hiv_month.png", plot = fig_hiv_month, width = 10, height = 6, dpi = 300)


tabyl(sinan_xpert %>% filter(diag_yrmo < "2023-01-01" & id_municip %in% muni_with_xpert),agravaids)

  

# pct_TB_pos = sum(test_molec %in% c("1", "2"))/sum(test_molec %in% c("1", "2", "3")), 
# pct_RR_pos = sum(test_molec %in% c("2"))/sum(test_molec %in% c("1", "2"))

















# Old code ----------------------------------------------------------------
## Number and share of diagnoses receiving Xpert per month
### Note: Removed miscodes (test_molec = 9)
fig_xpert_count <- sinan_xpert %>% 
  filter(diag_yr >= "2014-01-01" & diag_yr < "2023-01-01") %>% 
  group_by(diag_yrmo, test_molec) %>% 
  # filter(test_molec != "9") %>%
  summarize(count = n()) %>% 
  group_by(diag_yrmo) %>% 
  summarize(performed = sum(count[test_molec %in% c("1", "2", "3", "4")]),
            not_performed = sum(count[test_molec %in% c("5", "NA")]), 
            pct = performed/(performed + not_performed)) %>% 
  ggplot(aes(x = diag_yrmo, y = performed)) + 
  geom_line()

fig_xpert_share <- sinan_xpert %>% 
  filter(diag_yr >= "2014-01-01" & diag_yr < "2023-01-01") %>% 
  group_by(diag_yrmo, test_molec) %>% 
  # filter(test_molec != "9") %>%
  summarize(count = n()) %>% 
  group_by(diag_yrmo) %>% 
  summarize(performed = sum(count[test_molec %in% c("1", "2", "3", "4")]),
            not_performed = sum(count[test_molec %in% c("5", "NA")]), 
            pct = performed/(performed + not_performed)) %>% 
  ggplot(aes(x = diag_yrmo, y = pct)) + 
  geom_line()








## Identify municipalities that have implemented Xpert
munic_perform_xpert <- sinan_xpert %>%
  # filter(diag_yr == "2017-01-01") %>%
  group_by(diag_yr, id_municip) %>% 
  summarize(diagnoses = n(), 
            number_performed = sum(if_else(test_molec %in% c("1", "2", "3"), 1, 0))) %>% 
  group_by(diag_yr, id_municip) %>% 
  summarize(diagnoses = diagnoses, 
            number_performed = number_performed, 
            ever_performed = if_else(number_performed > 0, 1, 0)) %>% 
  group_by(diag_yr) %>% 
  summarize(facilities_ever_performed = sum(ever_performed))


munic <-  sinan_xpert %>% 
  group_by(diag_yr, id_municip, id_unidade) %>% 
  summarize(performed = if_else(test_molec %in% c("1", "2", "3"), 1, 0),
            not_performed = if_else(test_molec %in% c("5"), 1, 0),
            missing = if_else(is.na(test_molec), 1, 0)) %>%
  select(diag_yr, test_molec, performed, not_performed, id_municip, id_unidade, missing) %>%
  mutate(id_unidade = as.character(id_unidade)) %>%
  group_by(diag_yr, id_municip) %>%
  summarize(total_performed = sum(performed),
            total_not_performed = sum(not_performed),
            total_missing = sum(missing)) %>%
  group_by(diag_yr,id_municip) %>%
  summarize(facility_in_munic = n(),
            total_performed = sum(total_performed)) %>%
  filter(total_performed >= 1)
# 
# ,
#             total_not_performed = first(total_not_performed),
#             total_missing = first(total_missing)) %>%
#   filter(total_performed > 0)
# 
# 
# 
#   filter(total_performed == 0 & total_not_performed == 0) %>%
#   group_by(diag_yr, id_municip) %>%
#   summarize(fac_w_out_access = n(),
#             pct_w_out_access = round(n()/facility_in_munic, 2))


test <- tabyl(munic_perf, performed, diag_yr)


## Coverage of Xpert among facilities overtime (2014-2022)
### Number of states implementing Xpert by year
### Number of municipalities implementing Xpert by year
### Number of facilities implementing Xpert by year 



# sinan_xpert %>% 
#   filter(diag_yr < "2023-01-01" & !cs_sexo %in% c("NA","I")) %>% # I - very few 
#   filter(id_municip %in% muni_with_xpert) %>% 
#   group_by(diag_yr, cs_sexo) %>% 
#   summarize(performed = sum(test_molec %in% c("1", "2", "3", "4")),
#             not_performed = sum(test_molec %in% c("5")),
#             pct_TB_pos = sum(test_molec %in% c("1", "2"))/sum(test_molec %in% c("1", "2", "3")), 
#             pct_RR_pos = sum(test_molec %in% c("2"))/sum(test_molec %in% c("1", "2"))) %>% 
#   group_by(diag_yr) %>% 
#   summarize(cs_sexo = cs_sexo,
#             performed = performed,
#             not_performed = not_performed,
#             pct = performed/sum(performed),
#             pct_TB_pos = pct_TB_pos,
#             pct_RR_pos = pct_RR_pos)



