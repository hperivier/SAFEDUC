#import packages 
source("packages.R")

#import fonctions
source("fonctions.R")

#import bases
d_all <- readRDS("safeduc_all.rds")
d_f <- readRDS("safeduc_finished.rds")


d_f<- reverse_one_hot_encoding(
  d_f, 
  c(
    "I_NATIONALITE", "I_NATIO_HORS_UE", "I_S_ANNEE",
    "I_S_AUTRE_ETAB_TYPE", "I_U_FACULTE", "I_U_FACULTE_SANTE", 
    "I_U_FACULTE_SCIENCES", "I_U_FACULTE_SOCIETES", "I_U_AUTRE_ETAB_TYPE",
    "C_ACTI_UNI", "C_ACTI_HORS"
  )
)


d_f<- reverse_one_hot_encoding(
  d_f, 
  c(
    "V_PSY_DOUZE", "V_PSY_ETAB","V_PSY_LIEU", "V_PSY_INTERNET", "V_PSY_AUTEUR_GENRE",
    "V_PSY_AUTEUR_STATUT", "V_PSY_AUTEUR_REL", "V_PSY_AUTEUR_ALCOOL", "V_PSY_MOTIF"
  )
)

d_f<- reverse_one_hot_encoding(
  d_f, 
  c(
    "V_PHY_ETAB","V_PHY_LIEU", "V_PHY_AUTEUR_STATUT", 
    "V_PHY_AUTEUR_REL", "V_PHY_AUTEUR_ALCOOL", "V_PHY_MOTIF"
  )
)

d_f<- reverse_one_hot_encoding(
  d_f, 
  c(
    "V_SEX_DOUZE", "V_SEX_ETAB","V_SEX_LIEU", "V_SEX_INTERNET", "V_SEX_AUTEUR_GENRE",
    "V_SEX_AUTEUR_STATUT", "V_SEX_AUTEUR_REL", "V_SEX_AUTEUR_ALCOOL", "V_SEX_MOTIF"
  )
)

d_f<- reverse_one_hot_encoding(
  d_f, 
  c(
    "Q_CONSQ", "Q_PARLE_PROFIL", "Q_PARLE_ETAB","Q_PARLE_RAIS_NO_ETA",
    "Q_PARLE_EXT", "Q_PARLE_RAIS_PLAINT", "Q_PARLE_RAIS_NO_EXT"
  )
)

#####Variables de comptage des faits v�cus

variables_faits_psy <- names(d_f)[grepl("^P_FAITS_PSY", names(d_f))]
d_f <- compter_faits(d_f, variables_faits_psy, "P_FAITS_PSY_SUM")

variables_faits_phys <- names(d_f)[grepl("^P_FAITS_PHYS", names(d_f))]
d_f <- compter_faits(d_f, variables_faits_phys, "P_FAITS_PHYS_SUM")


variables_faits_sex <- names(d_f)[grepl("^P_FAITS_SEX", names(d_f))]
d_f <- compter_faits(d_f, variables_faits_sex, "P_FAITS_SEX_SUM")

d_f <- d_f %>% 
  mutate(
    P_FAITS_SUM = rowSums(across(c("P_FAITS_PSY_SUM", "P_FAITS_PHYS_SUM", "P_FAITS_SEX_SUM"), na.rm=T))
  )


#### Analyses sur la pr�valence 

d_f %>% 
  tbl_summary(
    include = c(P_FAITS_PSY_SUM,P_FAITS_PHYS_SUM,P_FAITS_SEX_SUM),
    by = I_IDGENRE_RC
  )

d_f %>%
  select(P_FAITS_PSY_SUM,P_FAITS_PHYS_SUM,P_FAITS_SEX_SUM, I_IDGENRE_RC, I_ETAB_RC) %>%
  tbl_strata(
    strata = I_ETAB_RC,
    .tbl_fun =
      ~ .x %>%
      tbl_summary(by = I_IDGENRE_RC) %>%
      add_n()
  )

#### Cr�ation d'une variable binaire pour chaque question de pr�valence (on remplace 1,2,3,98,99 par 1 ou 0) 

d_f <- d_f %>%
  mutate(across(
    .cols = starts_with("P_FAITS") & !ends_with("RC"),
    .fns = ~ ifelse(. %in% c(1, 2), 1, 0),
    .names = "bin_{col}"
  ))

#### Recodage de la variable de genre 

d_f <- d_f %>%
  mutate(GENRE_RC = case_when(
    I_IDGENRE_RC == "Une femme" ~ "Une femme",
    I_IDGENRE_RC == "Un homme" ~ "Un homme",
    TRUE ~ "Autre ou Refus"  # Cette condition s'applique � toutes les autres modalit�s
  ))

d_f %>%
  select(starts_with("bin_"), GENRE_RC, I_ETAB_RC) %>%
  tbl_strata(
    strata = I_ETAB_RC,
    .tbl_fun =
      ~ .x %>%
      tbl_summary(by = GENRE_RC) %>%
      add_n() %>% 
      add_overall() 
  ) 



#dataframe qui ne prend que les personnes d'upcite 
#et qui d�double les etudiant.es qui sont inscrit.es dans 2 facult�s
# par ex, quelqu'un inscrit en fac de sant� et de sciences apparaitra une fois avec la modalit� de fac sant� et une autre fois avec la modalit� fac sciences

df_upcite_faculte <- d_f %>%
  pivot_longer(
    cols = c(I_U_FACULTE_1, I_U_FACULTE_2, I_U_FACULTE_3, I_U_FACULTE_4),
    names_to = "faculte",
    values_to = "presence"
  ) %>%
  filter(presence == 1)

  
df_upcite_faculte %>%
  select(starts_with("bin_"), GENRE_RC, faculte) %>%
  tbl_strata(
    strata = faculte,
    .tbl_fun =
      ~ .x %>%
      tbl_summary(by = GENRE_RC) %>%
      add_n() %>% 
      add_overall() 
  )



