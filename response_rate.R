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

