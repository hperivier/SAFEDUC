#import packages 
source("packages.R")

#import fonctions
source("fonctions.R")

#import bases
d_all <- readRDS("safeduc_all.rds")
d_f <- readRDS("safeduc_finished.rds")


#### Analyse du taux de completion ####

hist(d_all[d_all$Progress<100,]$Progress, main="Histogramme du taux de progression", xlab="Taux de progression", ylab="Fréquence")

questionnaire_variables <- names(d_all)[1:(which(names(d_all) == "I_ETAB_RC") - 1)]
d_all$LastQuestion[d_all$Progress<100] <- apply(d_all[d_all$Progress<100,], 1, find_last_question)
freq(d_all$LastQuestion, total = T, sort = "dec")


#### Evolution de la passation et des relances à Sciences Po ####

# Créer une séquence de dates entre le 25 mars et le 20 mai (jours seulement)
date_seq <- seq(from = as.Date("2024-03-25 00:00:00"), to = as.Date("2024-05-20 23:59:59"), by = "day")

# Calculer l'effectif simple pour chaque jour
daily_counts <- d_all %>%
  filter(I_ETAB == 1) %>%
  mutate(open_day = as.Date(StartDate)) %>%
  group_by(open_day) %>%
  summarise(daily_count = n())

# Joindre la séquence de dates avec les résultats pour inclure les jours sans réponses
daily_counts <- data.frame(open_day = date_seq) %>%
    left_join(daily_counts, by = "open_day") %>%
    replace_na(list(daily_count = 0))  

# Calculer l'effectif cumulé
daily_counts <- daily_counts %>%
  mutate(cumulative_count = cumsum(daily_count))

# Calculer le pourcentage simple et cumulé du taux de réponses
total_responses <- sum(daily_counts$daily_count)

daily_counts <- daily_counts %>%
  mutate(
    daily_percentage = daily_count / total_responses * 100,
    cumulative_percentage = cumulative_count / total_responses * 100
  )

# Création des groupes de dates correspondant à différents relais de relances
relances_mail <- as.Date(c("2024-04-04", "2024-05-06", "2024-05-16"))
relances_campus <- as.Date(c("2024-04-19", "2024-04-24", "2024-04-26"))
relances_flyers <- as.Date(c("2024-04-11", "2024-04-15", "2024-04-18", "2024-04-22", "2024-05-13", "2024-05-14", "2024-05-16"))

# Créer un dataframe pour les lignes verticales
vertical_lines <- data.frame(
  date = c(relances_mail, relances_campus, relances_flyers),
  group = factor(rep(c("Relances Mail", "Relances Campus", "Relances Flyers"),
                     times = c(length(relances_mail), length(relances_campus), length(relances_flyers))))
)

# Créer le plot avec ggplot2
plot_scpo <- ggplot(daily_counts, aes(x = open_day)) +
  geom_line(aes(y = daily_count, color = "Effectif journalier"), size = 1) +
  geom_line(aes(y = cumulative_count, color = "Effectif cumulé"), size = 1) +
  scale_y_continuous(name = "Effectif") +
  labs(title = "Evolution de la passation à Sciences Po",
       x = "Date",
       y = "Effectif") +
  theme_minimal() +
  scale_color_manual(values = c("Effectif journalier" = "blue", "Effectif cumulé" = "red")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank()
  ) +
  geom_vline(data = vertical_lines, aes(xintercept = as.numeric(date), color = group),
             linetype = c(rep("solid", length(relances_mail)), 
                          rep("dashed", length(relances_campus)), 
                          rep("dotted", length(relances_flyers))), 
             show.legend = TRUE) +
  scale_color_manual(name = "Légende",
                     values = c("Effectif journalier" = "blue", "Effectif cumulé" = "red", 
                                "Relances Mail" = "green", "Relances Campus" = "orange", "Relances Flyers" = "gray"))

# Afficher le plot
print(plot_scpo)

#ggsave("plot_passation_scpo.png", plot = plot_scpo, width = 10, height = 6, dpi = 300, bg = "white")


#### Evolution de la passation et des relances à Sciences Po en fonction des campus Paris/autres####

# Créer une séquence de dates entre le 25 mars et le 20 mai (jours seulement)
date_seq <- seq(from = as.Date("2024-03-25"), to = as.Date("2024-05-20"), by = "day")

# Calculer l'effectif simple pour chaque jour et par campus
daily_counts_campus <- d_all %>%
  filter(I_ETAB == 1, !is.na(I_S_CAMPUS)) %>%  # Filtrer les NA dans I_S_CAMPUS
  mutate(open_day = as.Date(StartDate)) %>%
  group_by(open_day, I_S_CAMPUS) %>%
  summarise(daily_count = n(), .groups = 'drop')

# Joindre la séquence de dates avec les résultats pour inclure les jours sans réponses
daily_counts_campus <- expand.grid(open_day = date_seq, I_S_CAMPUS = unique(daily_counts_campus$I_S_CAMPUS)) %>%
  left_join(daily_counts_campus, by = c("open_day", "I_S_CAMPUS")) %>%
  replace_na(list(daily_count = 0))

# Ajouter une colonne pour différencier les deux parties du graphique
daily_counts_campus <- daily_counts_campus %>%
  mutate(Campus_Group = ifelse(I_S_CAMPUS == 5, "Campus 5", "Autres Campus"))

# Création des groupes de dates correspondant à différents relais de relances
relances_mail <- as.Date(c("2024-04-04", "2024-05-06", "2024-05-16"))
relances_campus <- as.Date(c("2024-04-19", "2024-04-24", "2024-04-26"))
relances_flyers <- as.Date(c("2024-04-11", "2024-04-15", "2024-04-18", "2024-04-22", "2024-05-13", "2024-05-14", "2024-05-16"))

# Créer un dataframe pour les lignes verticales avec linetype approprié
vertical_lines <- data.frame(
  date = c(relances_mail, relances_campus, relances_flyers),
  group = factor(rep(c("Relances Mail", "Relances Campus", "Relances Flyers"),
                     times = c(length(relances_mail), length(relances_campus), length(relances_flyers)))),
  linetype = factor(c(rep("solid", length(relances_mail)), 
                      rep("dashed", length(relances_campus)), 
                      rep("dotted", length(relances_flyers))))
)

# Créer le plot avec ggplot2 pour les campus
plot_scpo_campus <- ggplot(daily_counts_campus, aes(x = open_day, y = daily_count)) +
  geom_line(aes(color = Campus_Group), size = 1) +
  scale_y_continuous(name = "Effectif") +
  labs(title = "Évolution de la passation à Sciences Po par campus",
       x = "Date",
       y = "Effectif") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank()
  ) +
  geom_vline(data = vertical_lines, aes(xintercept = as.numeric(date), linetype = linetype, color = group),
             size = 0.5, show.legend = F) +
  scale_color_manual(name = "Légende",
                     values = c("Campus 5" = "blue", "Autres Campus" = "red", 
                                "Relances Mail" = "green", "Relances Campus" = "orange", "Relances Flyers" = "gray")) +
  facet_wrap(~ Campus_Group, ncol = 1, scales = "free_y")  # Diviser horizontalement

# Afficher le plot
print(plot_scpo_campus)


