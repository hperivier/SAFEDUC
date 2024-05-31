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
daily_counts <- d_f %>%
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

# Créer le plot avec ggplot2
plot_scpo <- ggplot(daily_counts, aes(x = open_day)) +
  geom_line(aes(y = daily_count, color = "Effectif journalier"), size = 1) +
  geom_line(aes(y = cumulative_count, color = "Effectif cumulé"), size = 1) +
  scale_y_continuous(
    name = "Effectif"
  ) +
  labs(title = "Evolution du taux de réponse à Sciences Po",
       x = "Date",
       y = "Effectif") +
  theme_minimal() +
  scale_color_manual(values = c("Effectif journalier" = "blue", "Effectif cumulé" = "red")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank()
  )

# Ajouter les lignes verticales pour chaque groupe
for (date in relances_mail) {
  plot_scpo <- plot_scpo + geom_vline(xintercept = as.numeric(date), color = "green", linetype = "solid")
}

for (date in relances_campus) {
  plot_scpo <- plot_scpo + geom_vline(xintercept = as.numeric(date), color = "orange", linetype = "dashed")
}

for (date in relances_flyers) {
  plot_scpo <- plot_scpo + geom_vline(xintercept = as.numeric(date), color = "gray", linetype = "dotted")
}

# Afficher le plot
print(plot_scpo)


