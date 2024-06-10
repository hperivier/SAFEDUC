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

d_f <- d_f %>%
  mutate(GENRE_RC = case_when(
    I_IDGENRE_RC == "Une femme" ~ "Une femme",
    I_IDGENRE_RC == "Un homme" ~ "Un homme",
    TRUE ~ "Autre ou Refus"  # Cette condition s'applique à toutes les autres modalités
  ))

#### Analyse des caractéristiques de la population de Sciences Po ####

##### Analyse par Genre/année d'étude

freq(d_f$I_S_ANNEE_liste)

echantillon_genre_annee <- subset(d_f, I_ETAB==1) %>%
  count(I_S_ANNEE_liste, GENRE_RC) %>%
  group_by(GENRE_RC) %>%
  mutate(freq_echantillon = (n /2556)) %>%
  ungroup()

echantillon_genre_annee$I_S_ANNEE_liste <- premier_element(echantillon_genre_annee$I_S_ANNEE_liste)

# Regrouper les catégories 1, 2, 3 ensemble, 4, 5 ensemble, et laisser 6 et 7 séparées
echantillon_genre_annee <- echantillon_genre_annee %>%
  mutate(I_S_ANNEE_liste = case_when(
    I_S_ANNEE_liste %in% c("1", "2", "3") ~ "1-3",
    I_S_ANNEE_liste %in% c("4", "5") ~ "4-5",
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

# Données de la population
population_genre_annee <- data.frame(
  GENRE_RC = c(rep("Une femme", 7), rep("Un homme", 7)),
  I_S_ANNEE_liste = rep(c("1", "2", "3", "4", "5", "6", "7"), 2),
  effectif_pop = c(1201, 2557, 1216, 2096, 2341, 146, 118, 533, 1079, 583, 1180, 1517, 155, 115)
)

# Regrouper les catégories dans les données de la population de la même manière
population_genre_annee <- population_genre_annee %>%
  mutate(I_S_ANNEE_liste = case_when(
    I_S_ANNEE_liste %in% c("1", "2", "3") ~ "1-3",
    I_S_ANNEE_liste %in% c("4", "5") ~ "4-5",
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


# Effectuer le test du chi carré d'adéquation
chisq_test <- chisq.test(comp_genre_annee$n, p = comp_genre_annee$expected_n_echantillon / sum(comp_genre_annee$expected_n_echantillon))

# Afficher les résultats du test
print("Résultats du test du chi carré d'adéquation :")
print(chisq_test)

## Analyse par Campus x Genre

# Préparer les données de l'échantillon

echantillon_genre_campus <- subset(d_f, I_ETAB == 1) %>%
  count(I_S_CAMPUS, GENRE_RC) %>%
  group_by(GENRE_RC) %>%
  mutate(freq_echantillon = (n / 2556)) %>%
  ungroup()

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

# Données de la population
population_genre_campus <- data.frame(
  GENRE_RC = c(rep("Une femme", 7), rep("Un homme", 7)),
  I_S_CAMPUS = rep(c("1", "2", "3", "4", "5", "6", "7"), 2),
  effectif_pop = c(174,344,389,337,6704,288,1439,101,175,177,160,3862,107,580)
)

# Regrouper les catégories dans les données de la population de la même manière
population_genre_campus <- population_genre_campus %>%
  mutate(I_S_CAMPUS = case_when(
    I_S_CAMPUS %in% c("1", "2", "3","4","6","7") ~ "Autre Campus",
    TRUE ~ as.character(I_S_CAMPUS)
  )) %>%
  group_by(GENRE_RC, I_S_CAMPUS) %>%
  summarise(effectif_pop = sum(effectif_pop), .groups = 'drop')

# Calculer les proportions de la population
population_genre_campus <- population_genre_campus %>%
  mutate(freq_pop = (effectif_pop / sum(effectif_pop)))

# Joindre les données de l'échantillon et de la population
comp_genre_campus <- merge(echantillon_genre_campus, population_genre_campus, by = c("GENRE_RC", "I_S_CAMPUS"))

# Calculer les effectifs attendus pour l'échantillon
comp_genre_campus <- comp_genre_campus %>%
  mutate(expected_n_echantillon = freq_pop * total_effectif)

# Effectuer le test du chi carré d'adéquation
chisq_test <- chisq.test(comp_genre_campus$n, p = comp_genre_campus$expected_n_echantillon / sum(comp_genre_campus$expected_n_echantillon))

# Afficher les résultats du test
print("Résultats du test du chi carré d'adéquation :")
print(chisq_test)

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

chisq_test <- chisq.test(comp_genre$n, p = comp_genre$expected_n_echantillon / sum(comp_genre$expected_n_echantillon))

# Afficher les résultats du test
print("Résultats du test du chi carré d'adéquation :")
print(chisq_test)


