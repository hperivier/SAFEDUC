#import packages 
source("packages.R")

#import fonctions
source("fonctions.R")

#import bases
d_all <- readRDS("safeduc_all.rds")
d_f <- readRDS("safeduc_finished.rds")

### Ce code permet de créer des tables descriptives de certaines populations de sciences po
# on affiche le taux de réponse et le nombre moyen de faits vécus


d_f<- reverse_one_hot_encoding(
  d_f, 
  c(
    "I_NATIONALITE", "I_NATIO_HORS_UE", "I_S_ANNEE",
    "I_S_AUTRE_ETAB_TYPE", "I_U_FACULTE", "I_U_FACULTE_SANTE", 
    "I_U_FACULTE_SCIENCES", "I_U_FACULTE_SOCIETES", "I_U_AUTRE_ETAB_TYPE",
    "C_ACTI_UNI", "C_ACTI_HORS"
  )
)

d_f <- d_f %>%
  mutate(GENRE_RC = case_when(
    I_IDGENRE_RC == "Une femme" ~ "Une femme",
    I_IDGENRE_RC == "Un homme" ~ "Un homme",
    TRUE ~ "Autre ou Refus"  # Cette condition s'applique à toutes les autres modalités
  ))


#####Variables de comptage des faits vécus

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


#### Analyse des caractéristiques de la population de Sciences Po ####

##### Analyse par Genre/année d'étude


# Préparer les données de l'échantillon
echantillon_genre_annee <- subset(d_f, I_ETAB == 1) %>%
  count(I_S_ANNEE_liste, GENRE_RC) %>%
  group_by(GENRE_RC) %>%
  mutate(freq_echantillon = (n / 2556)) %>%
  ungroup()

echantillon_genre_annee$I_S_ANNEE_liste <- premier_element(echantillon_genre_annee$I_S_ANNEE_liste)

# Regrouper les catégories 1, 2, 3 ensemble, 4, 5 ensemble, et laisser 6 et 7 séparées
echantillon_genre_annee <- echantillon_genre_annee %>%
  mutate(I_S_ANNEE_liste = case_when(
    I_S_ANNEE_liste %in% c("5", "7") ~ "5&7",
    TRUE ~ as.character(I_S_ANNEE_liste)
  ))

# Filtrer pour supprimer la modalité "98"
echantillon_genre_annee <- echantillon_genre_annee %>%
  filter(I_S_ANNEE_liste != "98" & GENRE_RC != "Autre ou Refus")

# Recalculer les effectifs après regroupement
echantillon_genre_annee <- echantillon_genre_annee %>%
  group_by(GENRE_RC, I_S_ANNEE_liste) %>%
  summarise(n = sum(n), .groups = 'drop')

# Calculer l'effectif total
total_effectif <- sum(echantillon_genre_annee$n)

# Calculer les fréquences sur l'effectif total
echantillon_genre_annee <- echantillon_genre_annee %>%
  mutate(freq_echantillon = (n / total_effectif))

# Calculer les moyennes des faits vécus
echantillon_genre_annee <- echantillon_genre_annee %>%
  left_join(d_f %>%
              filter(I_ETAB == 1 & GENRE_RC != "Autre ou Refus" & I_S_ANNEE_liste != "98") %>%
              mutate(I_S_ANNEE_liste = case_when(
                I_S_ANNEE_liste %in% c("5", "7") ~ "5&7",
                TRUE ~ as.character(I_S_ANNEE_liste)
              )) %>%
              group_by(GENRE_RC, I_S_ANNEE_liste) %>%
              summarise(
                mean_faits_vecus = mean(P_FAITS_SUM, na.rm = TRUE),
                mean_faits_phys = mean(P_FAITS_PHYS_SUM, na.rm = TRUE),
                mean_faits_psy = mean(P_FAITS_PSY_SUM, na.rm = TRUE),
                mean_faits_sex = mean(P_FAITS_SEX_SUM, na.rm = TRUE),
                .groups = 'drop'
              ),
            by = c("GENRE_RC", "I_S_ANNEE_liste"))

# Données de la population
population_genre_annee <- data.frame(
  GENRE_RC = c(rep("Une femme", 7), rep("Un homme", 7)),
  I_S_ANNEE_liste = rep(c("1", "2", "3", "4", "5", "6", "7"), 2),
  effectif_pop = c(1201, 2557, 1216, 2096, 2341, 146, 118, 533, 1079, 583, 1180, 1517, 155, 115)
)

# Regrouper les catégories dans les données de la population de la même manière
population_genre_annee <- population_genre_annee %>%
  mutate(I_S_ANNEE_liste = case_when(
    I_S_ANNEE_liste %in% c("5", "7") ~ "5&7",
    TRUE ~ as.character(I_S_ANNEE_liste)
  )) %>%
  group_by(GENRE_RC, I_S_ANNEE_liste) %>%
  summarise(effectif_pop = sum(effectif_pop), .groups = 'drop')

# Calculer les proportions de la population
population_genre_annee <- population_genre_annee %>%
  mutate(freq_pop = (effectif_pop / sum(effectif_pop)))

# Joindre les données de l'échantillon et de la population
comp_genre_annee <- merge(echantillon_genre_annee, population_genre_annee, by = c("GENRE_RC", "I_S_ANNEE_liste"))

# Calculer les effectifs attendus pour l'échantillon
comp_genre_annee <- comp_genre_annee %>%
  mutate(expected_n_echantillon = freq_pop * total_effectif)

# Calculer les taux de réponse
comp_genre_annee$taux_reponse <- comp_genre_annee$n / comp_genre_annee$effectif_pop

#reorganiser les colonnes

comp_genre_annee <- comp_genre_annee %>%
  select(GENRE_RC, I_S_ANNEE_liste, n, taux_reponse, mean_faits_vecus, mean_faits_phys, mean_faits_psy, mean_faits_sex, effectif_pop, freq_pop, expected_n_echantillon, freq_echantillon
         )

#### Analyse par Campus x Genre ####

echantillon_genre_campus <- subset(d_f, I_ETAB == 1) %>%
  count(I_S_CAMPUS, GENRE_RC) %>%
  group_by(GENRE_RC) %>%
  mutate(freq_echantillon = (n / 2556)) %>%
  ungroup()

# Filtrer pour supprimer la modalité "98"
echantillon_genre_campus <- echantillon_genre_campus %>%
  filter(I_S_CAMPUS != "98" & GENRE_RC != "Autre ou Refus")

# Recalculer les effectifs après regroupement
echantillon_genre_campus <- echantillon_genre_campus %>%
  group_by(GENRE_RC, I_S_CAMPUS) %>%
  summarise(n = sum(n), .groups = 'drop')

# Calculer l'effectif total
total_effectif <- sum(echantillon_genre_campus$n)

# Calculer les fréquences sur l'effectif total
echantillon_genre_campus <- echantillon_genre_campus %>%
  mutate(freq_echantillon = (n / total_effectif))

# Calculer les moyennes des faits vécus
echantillon_genre_campus <- echantillon_genre_campus %>%
  left_join(d_f %>%
              filter(I_ETAB == 1 & GENRE_RC != "Autre ou Refus" & I_S_CAMPUS != "98") %>%
              group_by(GENRE_RC, I_S_CAMPUS) %>%
              summarise(
                mean_faits_vecus = mean(P_FAITS_SUM, na.rm = TRUE),
                mean_faits_phys = mean(P_FAITS_PHYS_SUM, na.rm = TRUE),
                mean_faits_psy = mean(P_FAITS_PSY_SUM, na.rm = TRUE),
                mean_faits_sex = mean(P_FAITS_SEX_SUM, na.rm = TRUE),
                .groups = 'drop'
              ),
            by = c("GENRE_RC", "I_S_CAMPUS"))

# Données de la population
population_genre_campus <- data.frame(
  GENRE_RC = c(rep("Une femme", 7), rep("Un homme", 7)),
  I_S_CAMPUS = rep(c("1", "2", "3", "4", "5", "6", "7"), 2),
  effectif_pop = c(174, 344, 389, 337, 6704, 288, 1439, 101, 175, 177, 160, 3862, 107, 580)
)

# Calculer les proportions de la population
population_genre_campus <- population_genre_campus %>%
  mutate(freq_pop = (effectif_pop / sum(effectif_pop)))

# Joindre les données de l'échantillon et de la population
comp_genre_campus <- merge(echantillon_genre_campus, population_genre_campus, by = c("GENRE_RC", "I_S_CAMPUS"))

# Calculer les effectifs attendus pour l'échantillon
comp_genre_campus <- comp_genre_campus %>%
  mutate(expected_n_echantillon = freq_pop * total_effectif)

# Calculer les taux de réponse
comp_genre_campus$taux_reponse <- comp_genre_campus$n / comp_genre_campus$effectif_pop

#reorganiser les colonnes

comp_genre_campus <- comp_genre_campus %>%
  select(GENRE_RC, I_S_CAMPUS, n, taux_reponse, mean_faits_vecus, mean_faits_phys, mean_faits_psy, mean_faits_sex, effectif_pop, freq_pop, freq_echantillon, expected_n_echantillon)
 

## Analyse par Genre

echantillon_genre <- subset(d_f, I_ETAB == 1) %>%
  count(GENRE_RC) %>%
  filter(GENRE_RC != "Autre ou Refus") %>%
  group_by(GENRE_RC) %>%
  summarise(n = sum(n), .groups = 'drop')

# Calculer l'effectif total de l'échantillon
total_effectif <- sum(echantillon_genre$n)

# Calculer les fréquences sur l'effectif total
echantillon_genre <- echantillon_genre %>%
  mutate(freq_echantillon = (n / total_effectif))

# Calculer les moyennes des faits vécus
echantillon_genre <- echantillon_genre %>%
  left_join(d_f %>%
              filter(I_ETAB == 1 & GENRE_RC != "Autre ou Refus") %>%
              group_by(GENRE_RC) %>%
              summarise(
                mean_faits_vecus = mean(P_FAITS_SUM, na.rm = TRUE),
                mean_faits_phys = mean(P_FAITS_PHYS_SUM, na.rm = TRUE),
                mean_faits_psy = mean(P_FAITS_PSY_SUM, na.rm = TRUE),
                mean_faits_sex = mean(P_FAITS_SEX_SUM, na.rm = TRUE),
                .groups = 'drop'
              ),
            by = "GENRE_RC")

# Données de la population (effectifs)
population_genre <- data.frame(
  GENRE_RC = c("Une femme", "Un homme"),
  effectif_pop = c(9675, 5162)  # Exemple d'effectifs dans la population
)

# Calculer les proportions de la population
population_genre <- population_genre %>%
  mutate(freq_pop = (effectif_pop / sum(effectif_pop)))

# Joindre les données de l'échantillon et de la population
comp_genre <- merge(echantillon_genre, population_genre, by = "GENRE_RC")

# Calculer les effectifs attendus pour l'échantillon
comp_genre <- comp_genre %>%
  mutate(expected_n_echantillon = freq_pop * total_effectif)

# Calculer les taux de réponse
comp_genre$taux_reponse <- comp_genre$n / comp_genre$effectif_pop

#reorganiser les colonnes
comp_genre <- comp_genre %>%
  select(GENRE_RC, n, taux_reponse, mean_faits_vecus, mean_faits_phys, mean_faits_psy, mean_faits_sex, effectif_pop, freq_pop, freq_echantillon, expected_n_echantillon)


