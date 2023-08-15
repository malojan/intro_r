
needs(tidyverse)

pr2022 <- openxlsx::read.xlsx("https://www.data.gouv.fr/fr/datasets/r/6d9b33e5-667d-4c3e-9a0b-5fdf5baac708")

pr_long <- pr2022 |> 
  janitor::clean_names() |> 
  mutate_all(as.character) |> 
  pivot_longer(20:ncol(pr2022))

pr_wide <- pr_long |> 
  mutate(number = rep(seq(1, 7), length.out = nrow(pr_long)) |> as.character(), 
         number = case_match(
           number,
           "1"  ~ "n_panneau",
           "2"  ~ "sexe",
           "3"  ~ "nom",
           "4"  ~ "prenom",
           "5"  ~ "voix",
           "6"  ~ "percent_voix_ins",
           "7"  ~ "percent_voix_exp"
         )) |> 
  select(- name) |> 
  pivot_wider(names_from = number, values_from = value) |> 
  unnest()

pr_wide |> slice_head(n = 100) |> view()


pr2022 <- pr_wide |> 
  mutate(code_du_departement = case_when(
    str_length(code_du_departement) == 1 ~ paste0("0", code_du_departement),
    TRUE ~ as.character(code_du_departement)
  ), 
  code_de_la_commune = case_when(
    str_length(code_de_la_commune) == 2 ~paste0("0", code_de_la_commune),
    str_length(code_de_la_commune) == 1 ~paste0("00", code_de_la_commune),
    TRUE ~ as.character(code_de_la_commune)
  ), 
  CODGEO = paste0(code_du_departement, code_de_la_commune)) 


pop  <- readxl::read_excel("data/base-cc-evol-struct-pop-2019.xlsx", skip = 5)

pop <- pop |> 
  select(CODGEO, REG, DEP, LIBGEO, P19_POP, C19_POP15P, C19_POP15P_CS1, C19_POP15P_CS6) |> 
  rename(population_nb = P19_POP,
         population15_nb = C19_POP15P,
         agri15_nb = C19_POP15P_CS1,
         ouv15_nb = C19_POP15P_CS6) |> 
  mutate(agri_share = agri15_nb/population15_nb*100,
         ouvri_share = ouv15_nb/population15_nb*100)

## Employment

unemploy <-  readxl::read_excel("data/base-cc-emploi-pop-active-2019.xlsx", skip = 5)

unemploy <- unemploy |> 
  select(CODGEO, LIBGEO, P19_POP1564, P19_CHOMEUR1564) |> 
  rename(pop1564 = P19_POP1564,
         unemp1574_nb = P19_CHOMEUR1564) |> 
  mutate(unemp_share = unemp1574_nb/pop1564*100)

## Immigration data from recensement 2019

immig  <- readxl::read_excel("data/BTX_TD_IMG1A_2019.xlsx", skip = 9)

immig <- immig |> 
  pivot_longer(cols = contains("IMMI")) |> 
  mutate(name = case_when(
    str_detect(name, "IMMI1") ~ "Immigrés",
    str_detect(name, "IMMI2")  ~ "Non-immigrés"
  )) |> 
  group_by(CODGEO, LIBGEO, name) |> 
  summarise(immig_nb = sum(value)) |> 
  group_by(CODGEO, LIBGEO) |> 
  mutate(immig_share = immig_nb/sum(immig_nb)*100) |> 
  filter(name == "Immigrés")  |> 
  select(- name)

pop <- pop |> 
  left_join(immig) |> 
  left_join(unemploy)

pr <- pr2022 |> left_join(pop) |> 
  mutate(vote_share = as.numeric(percent_voix_exp), 
         ouvri_share = as.numeric(ouvri_share))

pr |> head() |> view()

pr_legi <- pr |> 
  select(code_du_departement, libelle_du_departement, libelle_de_la_commune, percent_exp_ins, nom, vote_share, dplyr::contains("share")) |> 
  rename(candidate = nom, 
         departement = libelle_du_departement, 
         departement_code = code_du_departement, 
         commune = libelle_de_la_commune, 
         turnout = percent_exp_ins) |> 
  mutate(candidate = str_to_lower(candidate) |> str_replace_all("é", "e") |> str_replace_all(" |-", "_"))

pr_legi |> count(candidate)
write_csv(pr_legi, here::here("data", "data_pr.csv"))
