#import packages 
source("packages.R")

#import fonctions
source("fonctions.R")

#import bases
d_all <- readRDS("safeduc_all.rds")
d_f <- readRDS("safeduc_finished.rds")


#### Analyse du taux de completion ####

questionnaire_variables <- names(d_all)[1:(which(names(d_all) == "I_ETAB_RC") - 1)]
d_all$LastQuestion[d_all$Progress<100] <- apply(d_all[d_all$Progress<100,], 1, find_last_question)
d_all$Abandon[d_all$Progress<100] <- apply(d_all[d_all$Progress<100,], 1, find_first_unanswered)

freq_last_question <- freq(d_all$LastQuestion, total = T, sort = "dec")

# Convertir les frequences en DataFrame et ajouter une colonne pour les modalites
freq_last_question <- as.data.frame(freq_last_question)
freq_last_question <- rownames_to_column(freq_last_question, "LastQuestion")

# Calculer la moyenne de Progress pour chaque modalite de LastQuestion
mean_progress <- d_all %>%
  group_by(LastQuestion) %>%
  summarise(Progression = mean(Progress, na.rm = TRUE))



# Joindre la moyenne de Progress au DataFrame freq_last_question
freq_last_question <- freq_last_question %>%
  left_join(mean_progress, by = "LastQuestion")

freq_last_question$Progression <- round(freq_last_question$Progression,0)

# Ajouter la colonne Abandon
freq_last_question <- freq_last_question %>%
  mutate(Abandon = d_all$Abandon[match(LastQuestion, d_all$LastQuestion)])

freq_last_question <- head(freq_last_question,15)

labels_lastquestion <- sapply(freq_last_question$LastQuestion, function(x) {
  if (x %in% colnames(d_all)) {
    var_label(d_all[[x]])
  } else {
    NA
  }
})

# Ajouter la colonne Question avec les labels correspondants
freq_last_question <- freq_last_question %>%
  mutate("Dernière Question" = labels_lastquestion)


## idem pour la question abandonnee
labels_abandon <- sapply(freq_last_question$Abandon, function(x) {
  if (x %in% colnames(d_all)) {
    var_label(d_all[[x]])
  } else {
    NA
  }
})

# Ajouter la colonne Question avec les labels correspondants
freq_last_question <- freq_last_question %>%
  mutate("Question d'abandon" = labels_abandon)

freq_last_question <- freq_last_question %>% rename("Effectif" = "n")
freq_last_question <- freq_last_question %>% rename("Proportion" = "val%")


#### Evolution de la passation et des relances a Sciences Po ####



# Creer une sequence de dates entre le 25 mars et le 20 mai (jours seulement)
date_seq <- seq(from = as.Date("2024-03-25"), to = as.Date("2024-05-20"), by = "day")

# Calculer l'effectif simple pour chaque jour
daily_counts <- d_f %>%
  mutate(open_day = as.Date(StartDate)) %>%
  group_by(open_day) %>%
  summarise(daily_count = n(), .groups = 'drop')

# Joindre la sequence de dates avec les Resultats pour inclure les jours sans reponses
daily_counts <- data.frame(open_day = date_seq) %>%
  left_join(daily_counts, by = "open_day") %>%
  replace_na(list(daily_count = 0))

# Calculer l'Effectif cumule
daily_counts <- daily_counts %>%
  mutate(cumulative_count = cumsum(daily_count))

# Calculer le pourcentage simple et cumule du taux de reponses
total_responses <- sum(daily_counts$daily_count)

daily_counts <- daily_counts %>%
  mutate(
    daily_percentage = daily_count / total_responses * 100,
    cumulative_percentage = cumulative_count / total_responses * 100
  )

# Creation des groupes de dates correspondant a differents relais de relances
relances_mail <- as.Date(c("2024-03-25", "2024-04-04", "2024-05-06", "2024-05-16", "2024-04-22", "2024-05-15"))
relances_campus <- as.Date(c("2024-04-19", "2024-04-24", "2024-04-26"))
relances_flyers <- as.Date(c("2024-04-11", "2024-04-15", "2024-04-18", "2024-04-22", "2024-05-13", "2024-05-14", "2024-05-16",
                             "2024-04-22", "2024-04-23", "2024-04-24", "2024-04-25", "2024-04-29", "2024-04-30",
                             "2024-05-03", "2024-05-13", "2024-05-14", "2024-05-15", "2024-05-16", "2024-05-17"))

# Creer un dataframe pour les lignes verticales
vertical_lines <- data.frame(
  date = c(relances_mail, relances_campus, relances_flyers),
  group = factor(rep(c("Relances Mails", "Relances Campus (Sciences Po)", "Relances Flyers"),
                     times = c(length(relances_mail), length(relances_campus), length(relances_flyers))))
)

# Creer un dataframe pour la periode de vacances
vacances <- data.frame(
  xmin = as.Date("2024-04-14"),
  xmax = as.Date("2024-04-21"),
  ymin = -Inf,
  ymax = Inf,
  label = "Période de vacances"
)

plot_passation <- ggplot(daily_counts, aes(x = open_day)) +
  geom_rect(data = vacances, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "blue", color = NA, alpha = 0.2, inherit.aes = FALSE) +
  geom_line(aes(y = daily_count, color = "Effectif journalier"), size = 1) +
  geom_vline(data = vertical_lines, aes(xintercept = as.numeric(date), color = group, linetype = group),
             size = 0.5, show.legend = FALSE) + 
  scale_y_continuous(name = "Effectif") +
  labs(title = "Évolution de la passation dans les deux établissements",
       x = "Date",
       y = "Effectif") +
  theme_minimal(base_size = 15) +  # Appliquer le thème minimal avec un fond blanc
  scale_color_manual(name = "NULL",
                     values = c("Effectif journalier" = "#0072B2","Effectif cumulé" = "#D55E00",
                                "Relances Mails" = "#009E73", "Relances Campus (Sciences Po)" = "#E69F00", "Relances Flyers" = "#999999")) +
  scale_fill_manual(name = NULL, values = c("Période de vacances" = "blue")) +
  scale_linetype_manual(name = NULL,
                        values = c("Relances Mails" = "solid", "Relances Flyers" = "dotted", "Relances Campus (Sciences Po)" = "dashed")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    legend.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(hjust = 0.5)
  ) +
  guides(color = guide_legend(ncol = 2, byrow = TRUE))  # Diviser la légende en plusieurs colonnes


#print(plot_passation)




#### Evolution de la passation et des relances a Sciences Po ####



# Creer une sequence de dates entre le 25 mars et le 20 mai (jours seulement)
date_seq <- seq(from = as.Date("2024-03-25"), to = as.Date("2024-05-20"), by = "day")

# Calculer l'effectif simple pour chaque jour
daily_counts <- d_all %>%
  filter(I_ETAB == 1) %>%
  mutate(open_day = as.Date(StartDate)) %>%
  group_by(open_day) %>%
  summarise(daily_count = n(), .groups = 'drop')

# Joindre la sequence de dates avec les Resultats pour inclure les jours sans reponses
daily_counts <- data.frame(open_day = date_seq) %>%
  left_join(daily_counts, by = "open_day") %>%
  replace_na(list(daily_count = 0))

# Calculer l'Effectif cumule
daily_counts <- daily_counts %>%
  mutate(cumulative_count = cumsum(daily_count))

# Calculer le pourcentage simple et cumule du taux de reponses
total_responses <- sum(daily_counts$daily_count)

daily_counts <- daily_counts %>%
  mutate(
    daily_percentage = daily_count / total_responses * 100,
    cumulative_percentage = cumulative_count / total_responses * 100
  )

# Creation des groupes de dates correspondant a differents relais de relances
relances_mail <- as.Date(c("2024-04-04", "2024-05-06", "2024-05-16"))
relances_campus <- as.Date(c("2024-04-19", "2024-04-24", "2024-04-26"))
relances_flyers <- as.Date(c("2024-04-11", "2024-04-15", "2024-04-18", "2024-04-22", "2024-05-13", "2024-05-14", "2024-05-16"))

# Creer un dataframe pour les lignes verticales
vertical_lines <- data.frame(
  date = c(relances_mail, relances_campus, relances_flyers),
  group = factor(rep(c("Relances Mails", "Relances Campus", "Relances Flyers"),
                     times = c(length(relances_mail), length(relances_campus), length(relances_flyers))))
)

# Creer le plot avec ggplot2
plot_scpo <- ggplot(daily_counts, aes(x = open_day)) +
  geom_line(aes(y = daily_count, color = "Effectif journalier"), size = 1) +
  scale_y_continuous(name = "Effectif") +
  labs(title = "Évolution de la passation à Sciences Po",
       x = "Date",
       y = "Effectif") +
  theme_minimal(base_size = 15) +
  scale_color_manual(values = c("Effectif journalier" = "#0072B2", "Effectif cumule" = "#D55E00")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 8),  # Reduire la taille des etiquettes de la legende
    plot.title = element_text(hjust = 0.5)
  ) +
  geom_vline(data = vertical_lines, aes(xintercept = as.numeric(date), color = group),
             linetype = c(rep("solid", length(relances_mail)),
                          rep("dashed", length(relances_campus)),
                          rep("dotted", length(relances_flyers))),
             show.legend = TRUE) +
  scale_color_manual(name = "legende",
                     values = c("Effectif journalier" = "#0072B2", "Effectif cumule" = "#D55E00",
                                "Relances Mails" = "#009E73", "Relances Campus" = "#E69F00", "Relances Flyers" = "#999999")) +
  guides(color = guide_legend(ncol = 2, byrow = TRUE))  # Diviser la legende en plusieurs colonnes

# Afficher le plot
#print(plot_scpo)

#ggsave("plot_passation_scpo.png", plot = plot_scpo, width = 10, height = 6, dpi = 300, bg = "white")


#### Evolution de la passation et des relances a Sciences Po en fonction des campus Paris/autres####

# Creer une sequence de dates entre le 25 mars et le 20 mai (jours seulement)
date_seq <- seq(from = as.Date("2024-03-25"), to = as.Date("2024-05-20"), by = "day")

# Calculer l'effectif simple pour chaque jour et par campus
daily_counts_campus <- d_all %>%
  filter(I_ETAB == 1, !is.na(I_S_CAMPUS)) %>%  # Filtrer les NA dans I_S_CAMPUS
  mutate(open_day = as.Date(StartDate)) %>%
  group_by(open_day, I_S_CAMPUS) %>%
  summarise(daily_count = n(), .groups = 'drop')

# Joindre la sequence de dates avec les Resultats pour inclure les jours sans reponses
daily_counts_campus <- expand.grid(open_day = date_seq, I_S_CAMPUS = unique(daily_counts_campus$I_S_CAMPUS)) %>%
  left_join(daily_counts_campus, by = c("open_day", "I_S_CAMPUS")) %>%
  replace_na(list(daily_count = 0))

# Ajouter une colonne pour differencier les deux parties du graphique
daily_counts_campus <- daily_counts_campus %>%
  mutate(Campus_Group = ifelse(I_S_CAMPUS == 5, "Campus de Paris", "Autres Campus"))

# Creation des groupes de dates correspondant a differents relais de relances
relances_mail <- as.Date(c("2024-04-04", "2024-05-06", "2024-05-16"))
relances_campus <- as.Date(c("2024-04-19", "2024-04-24", "2024-04-26"))
relances_flyers <- as.Date(c("2024-04-11", "2024-04-15", "2024-04-18", "2024-04-22", "2024-05-13", "2024-05-14", "2024-05-16"))

# Creer un dataframe pour les lignes verticales avec linetype approprie
vertical_lines <- data.frame(
  date = c(relances_mail, relances_campus, relances_flyers),
  group = factor(rep(c("Relances Mails", "Relances Campus", "Relances Flyers"),
                     times = c(length(relances_mail), length(relances_campus), length(relances_flyers))))
)

# Creer le plot avec ggplot2 pour les campus sans courbes lissees
plot_scpo_campus <- ggplot(daily_counts_campus, aes(x = open_day, y = daily_count)) +
  geom_line(aes(color = Campus_Group), size = 1) +  # Lignes originales
  scale_y_continuous(name = "Effectif") +
  labs(title = "Évolution de la passation à Sciences Po par campus",
       x = "Date",
       y = "Effectif") +
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 8),  # Reduire la taille des etiquettes de la legende
    plot.title = element_text(hjust = 0.5)
  ) +
  geom_vline(data = vertical_lines, aes(xintercept = as.numeric(date), color = group, linetype = group),
             size = 0.5, show.legend = F) +
  scale_color_manual(name = "Legende",
                     values = c("Campus de Paris" = "#0072B2", "Autres Campus" = "#D55E00",
                                "Relances Mails" = "#009E73", "Relances Campus" = "#E69F00", "Relances Flyers" = "#999999")) +
  scale_linetype_manual(name = "Legende",
                        values = c("Relances Mails" = "solid", "Relances Campus" = "dashed", "Relances Flyers" = "dotted")) +
  guides(color = guide_legend(ncol = 2, byrow = TRUE)) +
  facet_wrap(~ Campus_Group, ncol = 1, scales = "free_y")  # Diviser horizontalement

# Afficher le plot
#print(plot_scpo_campus)


#### Meme travail pour UPCite ####

#### Evolution de la passation et des relances a UPcite ####

# Creer une sequence de dates entre le 25 mars et le 20 mai (jours seulement)
date_seq <- seq(from = as.Date("2024-03-25"), to = as.Date("2024-05-20"), by = "day")

# Calculer l'effectif simple pour chaque jour
daily_counts <- d_all %>%
  filter(I_ETAB == 2) %>%
  mutate(open_day = as.Date(StartDate)) %>%
  group_by(open_day) %>%
  summarise(daily_count = n(), .groups = 'drop')

# Joindre la sequence de dates avec les resultats pour inclure les jours sans reponses
daily_counts <- data.frame(open_day = date_seq) %>%
  left_join(daily_counts, by = "open_day") %>%
  replace_na(list(daily_count = 0))

# Calculer l'effectif cumule
daily_counts <- daily_counts %>%
  mutate(cumulative_count = cumsum(daily_count))

# Calculer le pourcentage simple et cumule du taux de reponses
total_responses <- sum(daily_counts$daily_count)

daily_counts <- daily_counts %>%
  mutate(
    daily_percentage = daily_count / total_responses * 100,
    cumulative_percentage = cumulative_count / total_responses * 100
  )

# Creation des groupes de dates correspondant a differents relais de relances
relances_mail <- as.Date(c("2024-03-25", "2024-04-22", "2024-05-15"))
relances_flyers <- as.Date(c("2024-04-22", "2024-04-23", "2024-04-24", "2024-04-25", "2024-04-29", "2024-04-30",
                             "2024-05-03", "2024-05-13", "2024-05-14", "2024-05-15", "2024-05-16", "2024-05-17"))

# Creer un dataframe pour les lignes verticales
vertical_lines <- data.frame(
  date = c(relances_mail, relances_flyers),
  group = factor(rep(c("Relances Mails", "Relances Flyers"),
                     times = c(length(relances_mail), length(relances_flyers))))
)

# Creer le plot avec ggplot2
plot_upcite <- ggplot(daily_counts, aes(x = open_day)) +
  geom_line(aes(y = daily_count, color = "Effectif journalier"), size = 1) +
  scale_y_continuous(name = "Effectif") +
  labs(title = "Évolution de la passation à Université Paris Cité",
       x = "Date",
       y = "Effectif") +
  theme_minimal(base_size = 15) +
  scale_color_manual(values = c("Effectif journalier" = "#0072B2", "Effectif cumule" = "#D55E00")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 8),  # Reduire la taille des etiquettes de la legende
    plot.title = element_text(hjust = 0.5)
  ) +
  geom_vline(data = vertical_lines, aes(xintercept = as.numeric(date), color = group),
             linetype = c(rep("solid", length(relances_mail)),
                          rep("dotted", length(relances_flyers))),
             show.legend = TRUE) +
  scale_color_manual(name = "legende",
                     values = c("Effectif journalier" = "#0072B2", "Effectif cumule" = "#D55E00",
                                "Relances Mails" = "#009E73", "Relances Flyers" = "#999999")) +
  guides(color = guide_legend(ncol = 2, byrow = TRUE))  # Diviser la legende en plusieurs colonnes

# Afficher le plot
#print(plot_upcite)



#### Passation a UPCite par faculte ####

# Creer la sequence de dates entre le 25 mars et le 20 mai (jours seulement)
date_seq <- seq(from = as.Date("2024-03-25"), to = as.Date("2024-05-20"), by = "day")

# Filtrer et pivoter les donnees uniquement pour les facultes 1, 2 et 3
df_combined <- d_all %>%
  pivot_longer(
    cols = c(I_U_FACULTE_1, I_U_FACULTE_2, I_U_FACULTE_3),
    names_to = "faculte",
    values_to = "presence"
  ) %>%
  filter(presence == 1) %>%
  filter(I_ETAB == 2) %>%
  select(-presence)

# Calculer l'effectif simple pour chaque jour et chaque faculte
daily_counts <- df_combined %>%
  mutate(open_day = as.Date(StartDate)) %>%
  group_by(open_day, faculte) %>%
  summarise(daily_count = n(), .groups = 'drop')

# Joindre la sequence de dates avec les resultats pour inclure les jours sans reponses
daily_counts <- expand.grid(open_day = date_seq, faculte = unique(df_combined$faculte)) %>%
  left_join(daily_counts, by = c("open_day", "faculte")) %>%
  replace_na(list(daily_count = 0))

# Calculer le pourcentage simple du taux de reponses
total_responses <- daily_counts %>%
  group_by(faculte) %>%
  summarise(total_responses = sum(daily_count))

daily_counts <- daily_counts %>%
  left_join(total_responses, by = "faculte") %>%
  mutate(daily_percentage = daily_count / total_responses * 100) %>%
  select(-total_responses)

# Creation des groupes de dates correspondant a differents relais de relances
relances_mail <- as.Date(c("2024-03-25", "2024-04-22", "2024-05-15"))
relances_flyers <- as.Date(c("2024-04-22", "2024-04-23", "2024-04-24", "2024-04-25", "2024-04-29", "2024-04-30",
                             "2024-05-03", "2024-05-13", "2024-05-14", "2024-05-15", "2024-05-16", "2024-05-17"))

# Creer un dataframe pour les lignes verticales avec linetype approprie
vertical_lines <- data.frame(
  date = c(relances_mail, relances_flyers),
  group = rep(c("Relances Mails", "Relances Flyers"), times = c(length(relances_mail), length(relances_flyers))),
  linetype = rep(c("solid", "dotted"), times = c(length(relances_mail), length(relances_flyers)))
)

# Nombre de repondants par faculte
echantillon <- df_combined %>%
  group_by(faculte) %>%
  summarise(repondants = n_distinct(ResponseId))

# Population par faculte (exemple fictif)
population <- data.frame(
  faculte = c("I_U_FACULTE_1", "I_U_FACULTE_2", "I_U_FACULTE_3"),
  population = c(21772, 8280, 20362)  # Remplacer par les valeurs reelles
)

# Fusionner les informations sur les repondants et la population
faculte_info <- echantillon %>%
  left_join(population, by = "faculte") %>%
  mutate(taux_reponse = repondants / population * 100,
         info_label = paste0("Échantillon: ", repondants, "\nPopulation: ", population, "\nTaux de réponse: ", round(taux_reponse, 2), "%"))


# Custom labels for facets
facet_labels <- c(
  I_U_FACULTE_1 = "Faculté de Santé",
  I_U_FACULTE_2 = "Faculté des Sciences",
  I_U_FACULTE_3 = "Faculté Sociétés et Humanités"
)

# Creer un dataframe pour la periode de vacances
vacances <- data.frame(
  xmin = as.Date("2024-04-14"),
  xmax = as.Date("2024-04-21"),
  ymin = -Inf,
  ymax = Inf,
  label = "Période de vacances"
)


# Creer le plot avec ggplot2
plot_facultes <- ggplot(daily_counts, aes(x = open_day)) +
  geom_rect(data = vacances, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = label),
            color = NA, alpha = 0.2, inherit.aes = FALSE) +
  geom_line(aes(y = daily_count, color = "Effectif journalier"), size = 1) +
  geom_vline(data = vertical_lines, aes(xintercept = as.numeric(date), color = group, linetype = group),
             size = 0.5, show.legend = FALSE) +  # Ne pas montrer les lignes verticales dans la legende
  scale_y_continuous(name = "Effectif") +
  labs(title = "Evolution de la passation à Université Paris Cite par faculté",
       x = "Date",
       y = "Effectif") +
  theme_minimal(base_size = 15) +
  scale_color_manual(name = NULL,
                     values = c("Effectif journalier" = "#0072B2",
                                "Relances Mails" = "#009E73", "Relances Flyers" = "#999999")) +
  scale_fill_manual(name = NULL, values = c("Période de vacances" = "blue")) +
  scale_linetype_manual(name = NULL,
                        values = c("Relances Mails" = "solid", "Relances Flyers" = "dotted")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.text = element_text(size = 8),  # Reduire la taille des etiquettes de la legende
    plot.title = element_text(hjust = 0.5),
    legend.background = element_rect(fill = "white", color = NA)  # Fond blanc pour la legende
  ) +
  guides(color = guide_legend(override.aes = list(linetype = c(1))),
         fill = guide_legend(override.aes = list(alpha = 0.2))) +
  facet_wrap(~ faculte, scales = "free_y", ncol = 1, labeller = as_labeller(facet_labels)) +  # Diviser les graphiques par faculte
  geom_text(data = faculte_info, aes(x = min(daily_counts$open_day), y = Inf, label = info_label),
            vjust = 1, hjust = -0.2, size = 3, inherit.aes = FALSE)  # Ajouter les annotations avec un decalage

# Afficher le plot
#print(plot_facultes)
