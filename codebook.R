#import packages 
source("packages.R")

#import fonctions
source("fonctions.R")

# import des bases
safeduc_num <- read_csv("safeduc_num.csv")
safeduc_let <- read_csv("safeduc_let.csv")

safeduc_num <- safeduc_num %>% rename(Duration_seconds = `Duration (in seconds)`)
safeduc_let <- safeduc_let %>% rename(Duration_seconds = `Duration (in seconds)`)

dv <- c(safeduc_num[1,]) #on cree un vecteur texte, qui comprend la premiere ligne de notre base, c-a-d leur description
safeduc_num <- safeduc_num[c(-1,-2),] # on supprime les deux premieres lignes de la base qui contiennent des infos sur les variables, mais qui ne doivent pas etre traitees comme des reponses
safeduc_num <- set_variable_labels(safeduc_num,.labels=dv) #On indique à R d'utiliser le vecteur comprenant les descriptions des variables pour les inclure dans notre nouvelle base


dv <- c(safeduc_let[1,]) #on cree un vecteur texte, qui comprend la premiere ligne de notre base, c-a-d leur description
safeduc_let <- safeduc_let[c(-1,-2),] # on supprime les deux premieres lignes de la base qui contiennent des infos sur les variables, mais qui ne doivent pas etre traitees comme des reponses
safeduc_let <- set_variable_labels(safeduc_let,.labels=dv) #On indique à R d'utiliser le vecteur comprenant les descriptions des variables pour les inclure dans notre nouvelle base

##########     DEBUT DES RECODAGES    ##########


#Pour aller au niveau du dernier recodage verifie, chercher [REPRISE]

d <- safeduc_num

#Appliquer la transformation sur les colonnes qui peuvent être converties en integer
### Attention, cela enlève les labels des variables

d[,-(1:10)] <- as.data.frame(lapply(d[,-(1:10)], function(x) {
  if (is.character(x) && is_convertible_to_integer(x)) {
    as.integer(x)  # Convertir en integer
  } else {
    x  # Laisser la colonne telle quelle
  }
}))

#Récupération des labels 
var_label(d) <- dv

#LABELISATION DES MODALITES PAR VARIABLES

#Variable I_ETAB, pour l'etablissement de rattachement (Sciences Po/Upcite)

d$I_ETAB_RC[d$I_ETAB==1]="Sciences Po"
d$I_ETAB_RC[d$I_ETAB==2]="UPCite"

d$I_ETAB_RC <- factor(d$I_ETAB_RC, levels=c("Sciences Po","UPCite"))

var_label(d$I_ETAB_RC) <- var_label(d$I_ETAB)

#Variable I_ECHANGE, pour le fait ou non d'etre en echange dans l'etablissement

d$I_ECHANGE_RC[d$I_ECHANGE==1]="Oui"
d$I_ECHANGE_RC[d$I_ECHANGE==2]="Non"
d$I_ECHANGE_RC[d$I_ECHANGE==98]="Refus"
d$I_ECHANGE_RC[d$I_ECHANGE==99]="NSP"

d$I_ECHANGE_RC <- factor(d$I_ECHANGE_RC, levels= c("Oui","Non","Refus","NSP"))

var_label(d$I_ECHANGE_RC) <- var_label(d$I_ECHANGE)

#Variable I_IDGENRE, pour l'identite de genre declaree

d$I_IDGENRE_RC[d$I_IDGENRE==1]="Une femme"
d$I_IDGENRE_RC[d$I_IDGENRE==2]="Un homme"
d$I_IDGENRE_RC[d$I_IDGENRE==3]="Une personne non-binaire"
d$I_IDGENRE_RC[d$I_IDGENRE==4]="Vous utilisez un autre terme"
d$I_IDGENRE_RC[d$I_IDGENRE==98]="Refus"

d$I_IDGENRE_RC <- factor(d$I_IDGENRE_RC, levels=c("Une femme","Un homme","Une personne non-binaire","Vous utilisez un autre terme", "Refus"))
var_label(d$I_IDGENRE_RC) <- var_label(d$I_IDGENRE) 

table(d$I_IDGENRE)
table(d$I_IDGENRE_RC)

#Variable I_SEX_NAIS, pour les repondants ayant choisi "homme" ou "femme" ==> demande si c'etait la categorie a la naissance

d$I_SEX_NAIS_RC[d$I_SEX_NAIS==1]="Oui"
d$I_SEX_NAIS_RC[d$I_SEX_NAIS==2]="Non"
d$I_SEX_NAIS_RC[d$I_SEX_NAIS==98]="Refus"

d$I_SEX_NAIS_RC <- factor(d$I_SEX_NAIS_RC, levels=c("Oui","Non","Refus"))
var_label(d$I_SEX_NAIS_RC) <- var_label(d$I_SEX_NAIS)

#Variable I_NAIS_FR, pour oui/non naissance en France

d$I_NAIS_FR_RC[d$I_NAIS_FR==1]="Oui, en France (metropolitaine ou departement d'Outre-mer)"
d$I_NAIS_FR_RC[d$I_NAIS_FR==2]="Non"
d$I_NAIS_FR_RC[d$I_NAIS_FR==98]="Refus"

d$I_NAIS_FR_RC <- factor(d$I_NAIS_FR_RC, levels=c("Oui, en France (metropolitaine ou departement d'Outre-mer)","Non", "Refus"))
var_label(d$I_NAIS_FR_RC) <- var_label(d$I_NAIS_FR)

# Recodage de d$I_CSP_PARENTS_1 en d$I_CSP_PARENTS_1_RC

d$I_CSP_PARENTS_1_RC[d$I_CSP_PARENTS_1 == "1"] <- "Exploitant agricole"
d$I_CSP_PARENTS_1_RC[d$I_CSP_PARENTS_1 == "2"] <- "Artisanat, commerce, gestion d'entreprise"
d$I_CSP_PARENTS_1_RC[d$I_CSP_PARENTS_1 == "3"] <- "Cadres et professions intellectuelles superieures"
d$I_CSP_PARENTS_1_RC[d$I_CSP_PARENTS_1 == "4"] <- "Professions intermediaires"
d$I_CSP_PARENTS_1_RC[d$I_CSP_PARENTS_1 == "5"] <- "Employe ou employee"
d$I_CSP_PARENTS_1_RC[d$I_CSP_PARENTS_1 == "6"] <- "Ouvrier ou ouvriere"
d$I_CSP_PARENTS_1_RC[d$I_CSP_PARENTS_1 == "7"] <- "Retraite ou retraitee"
d$I_CSP_PARENTS_1_RC[d$I_CSP_PARENTS_1 == "8"] <- "Sans activite remuneree"
d$I_CSP_PARENTS_1_RC[d$I_CSP_PARENTS_1 == "98"] <- "Refus"
d$I_CSP_PARENTS_1_RC[d$I_CSP_PARENTS_1 == "99"] <- "NSP"

d$I_CSP_PARENTS_1_RC <-
  factor(d$I_CSP_PARENTS_1_RC,levels = c(
    "Exploitant agricole",
    "Artisanat, commerce, gestion d'entreprise",
    "Cadres et professions intellectuelles superieures",
    "Professions intermediaires",
    "Employe ou employee",
    "Ouvrier ou ouvriere",
    "Retraite ou retraitee",
    "Sans activite remuneree",
    "Refus",
    "NSP"))

var_label(d$I_CSP_PARENTS_1_RC) <- var_label(d$I_CSP_PARENTS_1)

# Recodage de d$I_CSP_PARENTS_2 en d$I_CSP_PARENTS_2_RC

d$I_CSP_PARENTS_2_RC[d$I_CSP_PARENTS_2 == "1"] <- "Exploitant agricole"
d$I_CSP_PARENTS_2_RC[d$I_CSP_PARENTS_2 == "2"] <- "Artisanat, commerce, gestion d'entreprise"
d$I_CSP_PARENTS_2_RC[d$I_CSP_PARENTS_2 == "3"] <- "Cadres et professions intellectuelles superieures"
d$I_CSP_PARENTS_2_RC[d$I_CSP_PARENTS_2 == "4"] <- "Professions intermediaires"
d$I_CSP_PARENTS_2_RC[d$I_CSP_PARENTS_2 == "5"] <- "Employe ou employee"
d$I_CSP_PARENTS_2_RC[d$I_CSP_PARENTS_2 == "6"] <- "Ouvrier ou ouvriere"
d$I_CSP_PARENTS_2_RC[d$I_CSP_PARENTS_2 == "7"] <- "Retraite ou retraitee"
d$I_CSP_PARENTS_2_RC[d$I_CSP_PARENTS_2 == "8"] <- "Sans activite remuneree"
d$I_CSP_PARENTS_2_RC[d$I_CSP_PARENTS_2 == "98"] <- "Refus"
d$I_CSP_PARENTS_2_RC[d$I_CSP_PARENTS_2 == "99"] <- "NSP"

d$I_CSP_PARENTS_2_RC <-
  factor(d$I_CSP_PARENTS_2_RC,levels = c(
    "Exploitant agricole",
    "Artisanat, commerce, gestion d'entreprise",
    "Cadres et professions intellectuelles superieures",
    "Professions intermediaires",
    "Employe ou employee",
    "Ouvrier ou ouvriere",
    "Retraite ou retraitee",
    "Sans activite remuneree",
    "Refus",
    "NSP"))

var_label(d$I_CSP_PARENTS_2_RC) <- var_label(d$I_CSP_PARENTS_2)

#RECODAGE DU MODULE SCIENCES PO

#Recodage de d$I_S_AGE en d$I_S_AGE_RC

d$I_S_AGE_RC[d$I_S_AGE==1]="Moins de 18 ans"
d$I_S_AGE_RC[d$I_S_AGE==2]="Entre 18 et 20 ans inclus"
d$I_S_AGE_RC[d$I_S_AGE==3]="Entre 21 et 25 ans inclus"
d$I_S_AGE_RC[d$I_S_AGE==4]="Entre 26 et 30 ans inclus"
d$I_S_AGE_RC[d$I_S_AGE==5]="Plus de 30 ans"
d$I_S_AGE_RC[d$I_S_AGE==98]="Refus"

d$I_S_AGE_RC <-factor(
  d$I_S_AGE_RC,levels = c(
    "Moins de 18 ans",
    "Entre 18 et 20 ans inclus",
    "Entre 21 et 25 ans inclus",
    "Entre 26 et 30 ans inclus",
    "Plus de 30 ans",
    "Refus"))

var_label(d$I_S_AGE_RC) <- var_label(d$I_S_AGE)


#Recodage de d$I_S_CAMPUS en d$I_S_CAMPUS_RC "Vous etudiez actuellement sur le campus de..."

d$I_S_CAMPUS_RC[d$I_S_CAMPUS==1]="Dijon"
d$I_S_CAMPUS_RC[d$I_S_CAMPUS==2]="Le Havre"
d$I_S_CAMPUS_RC[d$I_S_CAMPUS==3]="Menton"
d$I_S_CAMPUS_RC[d$I_S_CAMPUS==4]="Nancy"
d$I_S_CAMPUS_RC[d$I_S_CAMPUS==5]="Paris"
d$I_S_CAMPUS_RC[d$I_S_CAMPUS==6]="Poitiers"
d$I_S_CAMPUS_RC[d$I_S_CAMPUS==7]="Reims"
d$I_S_CAMPUS_RC[d$I_S_CAMPUS==98]="Refus"

d$I_S_CAMPUS_RC <- factor(d$I_S_CAMPUS_RC, levels = c(
  "Dijon",
  "Le Havre",
  "Menton",
  "Nancy",
  "Paris",
  "Poitiers",
  "Reims",
  "Refus"))
var_label(d$I_S_CAMPUS_RC) <- var_label(d$I_S_CAMPUS)

#On recode la variable I_S_APPREN

d$I_S_APPREN_RC[d$I_S_APPREN==1]="Oui, j'ai un contrat d'alternance ou d'apprentissage cette annee"
d$I_S_APPREN_RC[d$I_S_APPREN==2]="Non"
d$I_S_APPREN_RC[d$I_S_APPREN==98]="Refus"

d$I_S_APPREN_RC <- factor(
  d$I_S_APPREN_RC,
  levels = c(
    "Oui, j'ai un contrat d'alternance ou d'apprentissage cette annee",
    "Non",
    "Refus"))

var_label(d$I_S_APPREN_RC) <- var_label(d$I_S_APPREN)

#On recode la variable I_S_ECOLE

d$I_S_ECOLE_RC[d$I_S_ECOLE==1]="Ecole d'affaires publiques"
d$I_S_ECOLE_RC[d$I_S_ECOLE==2]="Ecole des affaires internationales"
d$I_S_ECOLE_RC[d$I_S_ECOLE==3]="Ecole de droit"
d$I_S_ECOLE_RC[d$I_S_ECOLE==4]="Ecole de journalisme"
d$I_S_ECOLE_RC[d$I_S_ECOLE==5]="Ecole du management et de l'impact"
d$I_S_ECOLE_RC[d$I_S_ECOLE==6]="Ecole urbaine" 
d$I_S_ECOLE_RC[d$I_S_ECOLE==7]="Ecole de la recherche" 
d$I_S_ECOLE_RC[d$I_S_ECOLE==98]="Refus"
d$I_S_ECOLE_RC[d$I_S_ECOLE==99]="NSP"

d$I_S_ECOLE_RC <- factor(d$I_S_ECOLE_RC,levels=c(
  "Ecole d'affaires publiques",
  "Ecole des affaires internationales",
  "Ecole de droit",
  "Ecole de journalisme",
  "Ecole du management et de l'impact",
  "Ecole urbaine" ,
  "Ecole de la recherche",
  "Refus",
  "NSP"))

var_label(d$I_S_ECOLE_RC) <- var_label(d$I_S_ECOLE)

#On recode la variable I_S_Bourse

d$I_S_BOURSE_RC[d$I_S_BOURSE==1]="Oui, d'une bourse du Crous"
d$I_S_BOURSE_RC[d$I_S_BOURSE==2]="Oui, d'une autre bourse (hors Crous)"
d$I_S_BOURSE_RC[d$I_S_BOURSE==3]="Non"
d$I_S_BOURSE_RC[d$I_S_BOURSE==98]="Refus"
d$I_S_BOURSE_RC[d$I_S_BOURSE==99]="NSP"

## Reordonnancement de d$I_S_BOURSE_RC

d$I_S_BOURSE_RC <- factor(d$I_S_BOURSE_RC,
                          levels = c("Oui, d'une bourse du Crous",
                                     "Oui, d'une autre bourse (hors Crous)",
                                     "Non",
                                     "Refus",
                                     "NSP"))

var_label(d$I_S_BOURSE_RC) <- var_label(d$I_S_BOURSE)

#On recode la variable I_S_ECHELON_BOURSE

d$I_S_ECHELON_BOURSE_RC[d$I_S_ECHELON_BOURSE==1]="Echelon 0 bis"
d$I_S_ECHELON_BOURSE_RC[d$I_S_ECHELON_BOURSE==2]="Echelon 1 a 7 (inclus)"
d$I_S_ECHELON_BOURSE_RC[d$I_S_ECHELON_BOURSE==98]="Refus"
d$I_S_ECHELON_BOURSE_RC[d$I_S_ECHELON_BOURSE==99]="NSP"

d$I_S_ECHELON_BOURSE_RC <- factor(d$I_S_ECHELON_BOURSE_RC,
                                  levels=c("Echelon 0 bis",
                                           "Echelon 1 a 7 (inclus)",
                                           "Refus",
                                           "NSP"))

var_label(d$I_S_ECHELON_BOURSE_RC) <- var_label(d$I_S_ECHELON_BOURSE)

#On recode la variable I_S_AUTRE_ETAB

d$I_S_AUTRE_ETAB_RC[d$I_S_AUTRE_ETAB==1]="Oui"
d$I_S_AUTRE_ETAB_RC[d$I_S_AUTRE_ETAB==2]="Non"
d$I_S_AUTRE_ETAB_RC[d$I_S_AUTRE_ETAB==98]="Refus"

d$I_S_AUTRE_ETAB_RC <- factor(d$I_S_AUTRE_ETAB_RC,
                              levels=c("Oui",
                                       "Non",
                                       "Refus"))

var_label(d$I_S_AUTRE_ETAB_RC) <- var_label(d$I_S_AUTRE_ETAB)

#On recode la variable I_U_AGE

d$I_U_AGE_RC <- d$I_U_AGE
d$I_U_AGE_RC[d$I_U_AGE==1]="Moins de 19 ans"
d$I_U_AGE_RC[d$I_U_AGE==2]="19 ou 20 ans"
d$I_U_AGE_RC[d$I_U_AGE==3]="Entre 21 et 25 ans inclus"
d$I_U_AGE_RC[d$I_U_AGE==4]="Entre 26 et 30 ans inclus"
d$I_U_AGE_RC[d$I_U_AGE==5]="Plus de 30 ans"
d$I_U_AGE_RC[d$I_U_AGE==98]="Refus"

d$I_U_AGE_RC <- factor(d$I_U_AGE_RC,
                       levels=c("Moins de 19 ans",
                                "19 ou 20 ans",
                                "Entre 21 et 25 ans inclus",
                                "Entre 26 et 30 ans inclus",
                                "Plus de 30 ans",
                                "Refus"))

var_label(d$I_U_AGE_RC) <- var_label(d$I_U_AGE)

#On recode la variable I_U_ANNEE

d$I_U_ANNEE_RC[d$I_U_ANNEE==1]="Bac + 1 (L1 ou equivalent)"
d$I_U_ANNEE_RC[d$I_U_ANNEE==2]="Bac + 2 (L2 ou equivalent)"
d$I_U_ANNEE_RC[d$I_U_ANNEE==3]="Bac + 3 (L3 ou equivalent)"
d$I_U_ANNEE_RC[d$I_U_ANNEE==4]="Bac + 4 (M1 ou equivalent)"
d$I_U_ANNEE_RC[d$I_U_ANNEE==5]="Bac + 5 (M2 ou equivalent)"
d$I_U_ANNEE_RC[d$I_U_ANNEE==6]="Doctorat"
d$I_U_ANNEE_RC[d$I_U_ANNEE==7]="Autre situation"
d$I_U_ANNEE_RC[d$I_U_ANNEE==98]="Refus"


d$I_U_ANNEE_RC <- factor(d$I_U_ANNEE_RC,levels=c("Bac + 1 (L1 ou equivalent)",
                                                 "Bac + 2 (L2 ou equivalent)",
                                                 "Bac + 3 (L3 ou equivalent)", 
                                                 "Bac + 4 (M1 ou equivalent)", 
                                                 "Bac + 5 (M2 ou equivalent)",
                                                 "Doctorat",
                                                 "Autre situation",
                                                 "Refus"
                                                  ))

var_label(d$I_U_ANNEE_RC) <- var_label(d$I_U_ANNEE)

#On recode la variable I_U_APPREN 

d$I_U_APPREN_RC[d$I_U_APPREN==1]="Oui, j'ai un contrat d'alternance ou d'apprentissage cette annee"
d$I_U_APPREN_RC[d$I_U_APPREN==2]="Non"
d$I_U_APPREN_RC[d$I_U_APPREN==98]="Refus"

d$I_U_APPREN_RC <- factor(d$I_U_APPREN_RC,levels=c("Oui, j'ai un contrat d'alternance ou d'apprentissage cette annee",
                                                   "Non",
                                                   "Refus"))

var_label(d$I_U_APPREN_RC) <- var_label(d$I_U_APPREN)

#On recode la variable I_U_BOURSE

d$I_U_BOURSE_RC[d$I_U_BOURSE==1]="Oui, d'une bourse du Crous"
d$I_U_BOURSE_RC[d$I_U_BOURSE==2]="Oui, d'une autre bourse (hors Crous)"
d$I_U_BOURSE_RC[d$I_U_BOURSE==3]="Non"
d$I_U_BOURSE_RC[d$I_U_BOURSE==98]="Refus"
d$I_U_BOURSE_RC[d$I_U_BOURSE==99]="NSP"


d$I_U_BOURSE_RC <- factor(d$I_U_BOURSE_RC,
                          levels = c("Oui, d'une bourse du Crous",
                                     "Oui, d'une autre bourse (hors Crous)",
                                     "Non",
                                     "Refus",
                                     "NSP"))

var_label(d$I_U_BOURSE_RC) <- var_label(d$I_U_BOURSE)


#On recode la variable I_U_ECHELON_BOURSE

d$I_U_BOURSE_ECHELON_RC[d$I_U_BOURSE_ECHELON==1]="Echelon 0 bis"
d$I_U_BOURSE_ECHELON_RC[d$I_U_BOURSE_ECHELON==2]="Echelon 1 a 7 (inclus)"
d$I_U_BOURSE_ECHELON_RC[d$I_U_BOURSE_ECHELON==98]="Refus"
d$I_U_BOURSE_ECHELON_RC[d$I_U_BOURSE_ECHELON==99]="NSP"

d$I_U_BOURSE_ECHELON_RC <- factor(d$I_U_BOURSE_ECHELON_RC,
                                  levels=c("Echelon 0 bis",
                                           "Echelon 1 a 7 (inclus)",
                                           "Refus",
                                           "NSP"))

var_label(d$I_U_BOURSE_ECHELON_RC) <- var_label(d$I_U_BOURSE_ECHELON_RC)


#On recode la variable I_U_AUTRE_ETAB

d$I_U_AUTRE_ETAB_RC[d$I_U_AUTRE_ETAB==1]="Oui"
d$I_U_AUTRE_ETAB_RC[d$I_U_AUTRE_ETAB==2]="Non"
d$I_U_AUTRE_ETAB_RC[d$I_U_AUTRE_ETAB==98]="Refus"

d$I_U_AUTRE_ETAB_RC <- factor(d$I_U_AUTRE_ETAB_RC,
                              levels=c("Oui",
                                       "Non",
                                       "Refus"))

var_label(d$I_U_AUTRE_ETAB_RC) <- var_label(d$I_U_AUTRE_ETAB)

#On cree la variable C_HABIT_RC

d$C_HABIT_RC[d$C_HABIT==1]="Chez un de vos parents ou chez vos deux parents"
d$C_HABIT_RC[d$C_HABIT==2]="Chez quelqu'un de votre entourage"
d$C_HABIT_RC[d$C_HABIT==3]="Dans un logement independant seul ou seule"
d$C_HABIT_RC[d$C_HABIT==4]="Dans un logement independant en couple"
d$C_HABIT_RC[d$C_HABIT==5]="Dans un logement independant en colocation"
d$C_HABIT_RC[d$C_HABIT==6]="Dans une residence collective (foyer, internat, residence universitaire...)"
d$C_HABIT_RC[d$C_HABIT==7]="Dans une chambre louee chez un particulier"
d$C_HABIT_RC[d$C_HABIT==8]="Vous n'avez pas de domicile fixe"
d$C_HABIT_RC[d$C_HABIT==9]="Autre"
d$C_HABIT_RC[d$C_HABIT==98]="Refus"


d$C_HABIT_RC <- factor(d$C_HABIT_RC, levels=c("Chez un de vos parents ou chez vos deux parents",
                                           "Chez quelqu'un de votre entourage",
                                           "Dans un logement independant seul ou seule",
                                           "Dans un logement independant en couple",
                                           "Dans un logement independant en colocation",
                                           "Dans une residence collective (foyer, internat, residence universitaire...)",
                                           "Dans une chambre louee chez un particulier",
                                           "Vous n'avez pas de domicile fixe",
                                           "Autre",
                                           "Refus"))

var_label(d$C_HABIT_RC) <- var_label(d$C_HABIT)

#On cree la variable C_FREQ_FAMILLE_RC

d$C_FREQ_FAMILLE_RC[d$C_FREQ_FAMILLE==1]="Je rentre tous les week-ends ou plus"
d$C_FREQ_FAMILLE_RC[d$C_FREQ_FAMILLE==2]="Je rentre au moins une fois par mois"
d$C_FREQ_FAMILLE_RC[d$C_FREQ_FAMILLE==3]="Je rentre a chaque vacances ou presque"
d$C_FREQ_FAMILLE_RC[d$C_FREQ_FAMILLE==4]="Je rentre environ une ou deux fois par an"
d$C_FREQ_FAMILLE_RC[d$C_FREQ_FAMILLE==5]="Plus rarement"
d$C_FREQ_FAMILLE_RC[d$C_FREQ_FAMILLE==6]="Non, jamais"
d$C_FREQ_FAMILLE_RC[d$C_FREQ_FAMILLE==98]="Refus"

d$C_FREQ_FAMILLE_RC <- factor(d$C_FREQ_FAMILLE_RC, levels = c("Je rentre tous les week-ends ou plus",
                                                              "Je rentre au moins une fois par mois",
                                                              "Je rentre a chaque vacances ou presque",
                                                              "Je rentre environ une ou deux fois par an",
                                                              "Plus rarement",
                                                              "Non, jamais",
                                                              "Refus"))

var_label(d$C_FREQ_FAMILLE_RC) <- var_label(d$C_FREQ_FAMILLE)

#On cree une variable C_FREQ_PRESETA_RC

d$C_FREQ_PRESETA_RC[d$C_FREQ_PRESETA==1]="Tous les jours ouvres ou presque"
d$C_FREQ_PRESETA_RC[d$C_FREQ_PRESETA==2]="Au moins une fois par semaine"
d$C_FREQ_PRESETA_RC[d$C_FREQ_PRESETA==3]="Moins frequemment"
d$C_FREQ_PRESETA_RC[d$C_FREQ_PRESETA==98]="Refus"

d$C_FREQ_PRESETA_RC <- factor(d$C_FREQ_PRESETA_RC,levels=c("Tous les jours ouvres ou presque",
                                                           "Au moins une fois par semaine",
                                                           "Moins frequemment",
                                                           "Refus"))

var_label(d$C_FREQ_PRESETA_RC) <- var_label(d$C_FREQ_PRESETA)


#On cree la base C_FREQ_EVETU_RC


d$C_FREQ_EVETU_RC[d$C_FREQ_EVETU==1]="Plusieurs fois par semaine"
d$C_FREQ_EVETU_RC[d$C_FREQ_EVETU==2]="Environ une fois par semaine"
d$C_FREQ_EVETU_RC[d$C_FREQ_EVETU==3]="Une a deux fois pendant le mois"
d$C_FREQ_EVETU_RC[d$C_FREQ_EVETU==4]="Moins frequemment"
d$C_FREQ_EVETU_RC[d$C_FREQ_EVETU==98]="Refus"


d$C_FREQ_EVETU_RC <- factor(d$C_FREQ_EVETU_RC, levels=c("Plusieurs fois par semaine",
                                                        "Environ une fois par semaine",
                                                        "Une a deux fois pendant le mois",
                                                        "Moins frequemment",
                                                        "Refus"))

var_label(d$C_FREQ_EVETU_RC) <- var_label(d$C_FREQ_EVETU)

# Création de C_COUPLE_RC

d$C_COUPLE_RC[d$C_COUPLE==1]='Celibataire'
d$C_COUPLE_RC[d$C_COUPLE==2]='En couple'
d$C_COUPLE_RC[d$C_COUPLE==3]='Vous fréquentez une/des personnes mais vous ne vous considérez pas engage, engagee'
d$C_COUPLE_RC[d$C_COUPLE==4]='Autre'
d$C_COUPLE_RC[d$C_COUPLE==98]='Refus'
d$C_COUPLE_RC[d$C_COUPLE==99]='NSP'

d$C_COUPLE_RC <- factor(d$C_COUPLE_RC, levels=c("Celibataire",                                                                       
                                                "En couple",                                                                         
                                                "Vous fréquentez une/des personnes mais vous ne vous considérez pas engage, engagee",
                                                "Autre",
                                                "Refus",
                                                "NSP"                                                                              
                                                ))

var_label(d$C_COUPLE_RC) <- var_label(d$C_COUPLE)

table(d$C_COUPLE_RC)
table(d$C_COUPLE)

#On cree la variable C_OS

d$C_OS_RC[d$C_OS==1]="Bisexuel/bisexuelle"
d$C_OS_RC[d$C_OS==2]="Heterosexuel/heterosexuelle"
d$C_OS_RC[d$C_OS==3]="Gay/Lesbienne/Homosexuel/homosexuelle"
d$C_OS_RC[d$C_OS==4]="Vous utilisez un autre terme"
d$C_OS_RC[d$C_OS==98]="Refus"
d$C_OS_RC[d$C_OS==99]="NSP"

d$C_OS_RC <- factor(d$C_OS_RC,levels=c("Bisexuel/bisexuelle",
                                       "Heterosexuel/heterosexuelle",
                                       "Gay/Lesbienne/Homosexuel/homosexuelle",
                                       "Vous utilisez un autre terme",
                                       "Refus",
                                       "NSP"))

var_label(d$C_OS_RC) <- var_label(d$C_OS) 

#On cree la variable C_ACTI_SEX_RC

d$C_ACTI_SEX_RC[d$C_ACTI_SEX==1]="Oui, de maniere reguliere"
d$C_ACTI_SEX_RC[d$C_ACTI_SEX==2]="Oui, quelques fois"
d$C_ACTI_SEX_RC[d$C_ACTI_SEX==3]="Oui, une fois"
d$C_ACTI_SEX_RC[d$C_ACTI_SEX==4]="Non"
d$C_ACTI_SEX_RC[d$C_ACTI_SEX==98]="Refus"
d$C_ACTI_SEX_RC[d$C_ACTI_SEX==99]="NSP"

d$C_ACTI_SEX_RC <- factor(d$C_ACTI_SEX_RC, levels=c("Oui, de maniere reguliere",
                                                 "Oui, quelques fois",
                                                 "Oui, une fois",
                                                 "Non",
                                                 "Refus",
                                                 "NSP"))

var_label(d$C_ACTI_SEX_RC) <- var_label(d$C_ACTI_SEX)

#On cree la variable C_ACTI_NCR_RC

d$C_ACTI_NCR_RC[d$C_ACTI_NCR==1]="Tout a fait indispensables"
d$C_ACTI_NCR_RC[d$C_ACTI_NCR==2]="Plutot indispensables"
d$C_ACTI_NCR_RC[d$C_ACTI_NCR==3]="Plutot pas indispensables"
d$C_ACTI_NCR_RC[d$C_ACTI_NCR==4]="Pas du tout indispensables"
d$C_ACTI_NCR_RC[d$C_ACTI_NCR==98]="Refus"
d$C_ACTI_NCR_RC[d$C_ACTI_NCR==99]="NSP"

d$C_ACTI_NCR_RC <- factor(d$C_ACTI_NCR_RC,levels = c(
                                                      "Tout a fait indispensables",
                                                      "Plutot indispensables",
                                                      "Plutot pas indispensables",
                                                      "Pas du tout indispensables",
                                                      "Refus",
                                                      "NSP"))

var_label(d$C_ACTI_NCR_RC) <- var_label(d$C_ACTI_NCR)

#On cree la serie de variables pour C_INDIC_EGA_1_RC

d$C_INDIC_EGA_1_RC[d$C_INDIC_EGA_1==1]="Tout a fait vrai"
d$C_INDIC_EGA_1_RC[d$C_INDIC_EGA_1==2]="Plutot vrai"
d$C_INDIC_EGA_1_RC[d$C_INDIC_EGA_1==3]="Ni vrai, ni faux"
d$C_INDIC_EGA_1_RC[d$C_INDIC_EGA_1==4]="Plutot faux"
d$C_INDIC_EGA_1_RC[d$C_INDIC_EGA_1==5]="Tout a fait faux"
d$C_INDIC_EGA_1_RC[d$C_INDIC_EGA_1==98]="Refus"
d$C_INDIC_EGA_1_RC[d$C_INDIC_EGA_1==99]="NSP"

d$C_INDIC_EGA_1_RC <- factor(d$C_INDIC_EGA_1_RC, levels=c("Tout a fait vrai",
                                                          "Plutot vrai",
                                                          "Ni vrai, ni faux",
                                                          "Plutot faux",
                                                          "Tout a fait faux",
                                                          "Refus",
                                                          "NSP"))

var_label(d$C_INDIC_EGA_1_RC) <- var_label(d$C_INDIC_EGA_1)

#On cree la serie de variables pour C_INDIC_EGA_2_RC

d$C_INDIC_EGA_2_RC[d$C_INDIC_EGA_2==1]="Tout a fait vrai"
d$C_INDIC_EGA_2_RC[d$C_INDIC_EGA_2==2]="Plutot vrai"
d$C_INDIC_EGA_2_RC[d$C_INDIC_EGA_2==3]="Ni vrai, ni faux"
d$C_INDIC_EGA_2_RC[d$C_INDIC_EGA_2==4]="Plutot faux"
d$C_INDIC_EGA_2_RC[d$C_INDIC_EGA_2==5]="Tout a fait faux"
d$C_INDIC_EGA_2_RC[d$C_INDIC_EGA_2==98]="Refus"
d$C_INDIC_EGA_2_RC[d$C_INDIC_EGA_2==99]="NSP"

d$C_INDIC_EGA_2_RC <- factor(d$C_INDIC_EGA_2_RC, levels=c("Tout a fait vrai",
                                                          "Plutot vrai",
                                                          "Ni vrai, ni faux",
                                                          "Plutot faux",
                                                          "Tout a fait faux",
                                                          "Refus",
                                                          "NSP"))

var_label(d$C_INDIC_EGA_2_RC) <- var_label(d$C_INDIC_EGA_2)


#On cree la serie de variables pour C_INDIC_EGA_3_RC

d$C_INDIC_EGA_3_RC[d$C_INDIC_EGA_3==1]="Tout a fait vrai"
d$C_INDIC_EGA_3_RC[d$C_INDIC_EGA_3==2]="Plutot vrai"
d$C_INDIC_EGA_3_RC[d$C_INDIC_EGA_3==3]="Ni vrai, ni faux"
d$C_INDIC_EGA_3_RC[d$C_INDIC_EGA_3==4]="Plutot faux"
d$C_INDIC_EGA_3_RC[d$C_INDIC_EGA_3==5]="Tout a fait faux"
d$C_INDIC_EGA_3_RC[d$C_INDIC_EGA_3==98]="Refus"
d$C_INDIC_EGA_3_RC[d$C_INDIC_EGA_3==99]="NSP"

d$C_INDIC_EGA_3_RC <- factor(d$C_INDIC_EGA_3_RC, levels=c("Tout a fait vrai",
                                                          "Plutot vrai",
                                                          "Ni vrai, ni faux",
                                                          "Plutot faux",
                                                          "Tout a fait faux",
                                                          "Refus",
                                                          "NSP"))

var_label(d$C_INDIC_EGA_3_RC) <- var_label(d$C_INDIC_EGA_3)

#On cree la serie de variables pour C_INDIC_EGA_4_RC

d$C_INDIC_EGA_4_RC[d$C_INDIC_EGA_4==1]="Tout a fait vrai"
d$C_INDIC_EGA_4_RC[d$C_INDIC_EGA_4==2]="Plutot vrai"
d$C_INDIC_EGA_4_RC[d$C_INDIC_EGA_4==3]="Ni vrai, ni faux"
d$C_INDIC_EGA_4_RC[d$C_INDIC_EGA_4==4]="Plutot faux"
d$C_INDIC_EGA_4_RC[d$C_INDIC_EGA_4==5]="Tout a fait faux"
d$C_INDIC_EGA_4_RC[d$C_INDIC_EGA_4==98]="Refus"
d$C_INDIC_EGA_4_RC[d$C_INDIC_EGA_4==99]="NSP"

d$C_INDIC_EGA_4_RC <- factor(d$C_INDIC_EGA_4_RC, levels=c("Tout a fait vrai",
                                                          "Plutot vrai",
                                                          "Ni vrai, ni faux",
                                                          "Plutot faux",
                                                          "Tout a fait faux",
                                                          "Refus",
                                                          "NSP"))

var_label(d$C_INDIC_EGA_4_RC) <- var_label(d$C_INDIC_EGA_4)


#On cree la serie de variables pour C_INDIC_EGA_5_RC

d$C_INDIC_EGA_5_RC[d$C_INDIC_EGA_5==1]="Tout a fait vrai"
d$C_INDIC_EGA_5_RC[d$C_INDIC_EGA_5==2]="Plutot vrai"
d$C_INDIC_EGA_5_RC[d$C_INDIC_EGA_5==3]="Ni vrai, ni faux"
d$C_INDIC_EGA_5_RC[d$C_INDIC_EGA_5==4]="Plutot faux"
d$C_INDIC_EGA_5_RC[d$C_INDIC_EGA_5==5]="Tout a fait faux"
d$C_INDIC_EGA_5_RC[d$C_INDIC_EGA_5==98]="Refus"
d$C_INDIC_EGA_5_RC[d$C_INDIC_EGA_5==99]="NSP"

d$C_INDIC_EGA_5_RC <- factor(d$C_INDIC_EGA_5_RC, levels=c("Tout a fait vrai",
                                                          "Plutot vrai",
                                                          "Ni vrai, ni faux",
                                                          "Plutot faux",
                                                          "Tout a fait faux",
                                                          "Refus",
                                                          "NSP"))

var_label(d$C_INDIC_EGA_5_RC) <- var_label(d$C_INDIC_EGA_5)

#On cree la serie de variables pour C_INDIC_EGA_6_RC

d$C_INDIC_EGA_6_RC[d$C_INDIC_EGA_6==1]="Tout a fait vrai"
d$C_INDIC_EGA_6_RC[d$C_INDIC_EGA_6==2]="Plutot vrai"
d$C_INDIC_EGA_6_RC[d$C_INDIC_EGA_6==3]="Ni vrai, ni faux"
d$C_INDIC_EGA_6_RC[d$C_INDIC_EGA_6==4]="Plutot faux"
d$C_INDIC_EGA_6_RC[d$C_INDIC_EGA_6==5]="Tout a fait faux"
d$C_INDIC_EGA_6_RC[d$C_INDIC_EGA_6==98]="Refus"
d$C_INDIC_EGA_6_RC[d$C_INDIC_EGA_6==99]="NSP"

d$C_INDIC_EGA_6_RC <- factor(d$C_INDIC_EGA_6_RC, levels=c("Tout a fait vrai",
                                                          "Plutot vrai",
                                                          "Ni vrai, ni faux",
                                                          "Plutot faux",
                                                          "Tout a fait faux",
                                                          "Refus",
                                                          "NSP"))

var_label(d$C_INDIC_EGA_6_RC) <- var_label(d$C_INDIC_EGA_6)

#On cree la serie de variables C_INDIC_BIEN

#C_INDIC_BIEN_1_RC

d$C_INDIC_BIEN_1_RC[d$C_INDIC_BIEN_1==1]="Tres souvent"
d$C_INDIC_BIEN_1_RC[d$C_INDIC_BIEN_1==2]="Souvent"
d$C_INDIC_BIEN_1_RC[d$C_INDIC_BIEN_1==3]="Quelques fois"
d$C_INDIC_BIEN_1_RC[d$C_INDIC_BIEN_1==4]="Jamais"
d$C_INDIC_BIEN_1_RC[d$C_INDIC_BIEN_1==98]="Refus"
d$C_INDIC_BIEN_1_RC[d$C_INDIC_BIEN_1==99]="NSP"

d$C_INDIC_BIEN_1_RC <- factor(d$C_INDIC_BIEN_1_RC, levels=c("Tres souvent",
                                                            "Souvent",
                                                            "Quelques fois",
                                                            "Jamais",
                                                            "Refus",
                                                            "NSP"))

var_label(d$C_INDIC_BIEN_1_RC) <- var_label(d$C_INDIC_BIEN_1)

#C_INDIC_BIEN_2_RC

d$C_INDIC_BIEN_2_RC[d$C_INDIC_BIEN_2==1]="Tres souvent"
d$C_INDIC_BIEN_2_RC[d$C_INDIC_BIEN_2==2]="Souvent"
d$C_INDIC_BIEN_2_RC[d$C_INDIC_BIEN_2==3]="Quelques fois"
d$C_INDIC_BIEN_2_RC[d$C_INDIC_BIEN_2==4]="Jamais"
d$C_INDIC_BIEN_2_RC[d$C_INDIC_BIEN_2==98]="Refus"
d$C_INDIC_BIEN_2_RC[d$C_INDIC_BIEN_2==99]="NSP"

d$C_INDIC_BIEN_2_RC <- factor(d$C_INDIC_BIEN_2_RC, levels=c("Tres souvent",
                                                            "Souvent",
                                                            "Quelques fois",
                                                            "Jamais",
                                                            "Refus",
                                                            "NSP"))

var_label(d$C_INDIC_BIEN_2_RC) <- var_label(d$C_INDIC_BIEN_2)

#C_INDIC_BIEN_3_RC

d$C_INDIC_BIEN_3_RC[d$C_INDIC_BIEN_3==1]="Tres souvent"
d$C_INDIC_BIEN_3_RC[d$C_INDIC_BIEN_3==2]="Souvent"
d$C_INDIC_BIEN_3_RC[d$C_INDIC_BIEN_3==3]="Quelques fois"
d$C_INDIC_BIEN_3_RC[d$C_INDIC_BIEN_3==4]="Jamais"
d$C_INDIC_BIEN_3_RC[d$C_INDIC_BIEN_3==98]="Refus"
d$C_INDIC_BIEN_3_RC[d$C_INDIC_BIEN_3==99]="NSP"

d$C_INDIC_BIEN_3_RC <- factor(d$C_INDIC_BIEN_3_RC, levels=c("Tres souvent",
                                                            "Souvent",
                                                            "Quelques fois",
                                                            "Jamais",
                                                            "Refus",
                                                            "NSP"))

var_label(d$C_INDIC_BIEN_3_RC) <- var_label(d$C_INDIC_BIEN_3)

#C_INDIC_BIEN_4_RC

d$C_INDIC_BIEN_4_RC[d$C_INDIC_BIEN_4==1]="Tres souvent"
d$C_INDIC_BIEN_4_RC[d$C_INDIC_BIEN_4==2]="Souvent"
d$C_INDIC_BIEN_4_RC[d$C_INDIC_BIEN_4==3]="Quelques fois"
d$C_INDIC_BIEN_4_RC[d$C_INDIC_BIEN_4==4]="Jamais"
d$C_INDIC_BIEN_4_RC[d$C_INDIC_BIEN_4==98]="Refus"
d$C_INDIC_BIEN_4_RC[d$C_INDIC_BIEN_4==99]="NSP"

d$C_INDIC_BIEN_4_RC <- factor(d$C_INDIC_BIEN_4_RC, levels=c("Tres souvent",
                                                            "Souvent",
                                                            "Quelques fois",
                                                            "Jamais",
                                                            "Refus",
                                                            "NSP"))

var_label(d$C_INDIC_BIEN_4_RC) <- var_label(d$C_INDIC_BIEN_4)

#C_INDIC_BIEN_5_RC

d$C_INDIC_BIEN_5_RC[d$C_INDIC_BIEN_5==1]="Tres souvent"
d$C_INDIC_BIEN_5_RC[d$C_INDIC_BIEN_5==2]="Souvent"
d$C_INDIC_BIEN_5_RC[d$C_INDIC_BIEN_5==3]="Quelques fois"
d$C_INDIC_BIEN_5_RC[d$C_INDIC_BIEN_5==4]="Jamais"
d$C_INDIC_BIEN_5_RC[d$C_INDIC_BIEN_5==98]="Refus"
d$C_INDIC_BIEN_5_RC[d$C_INDIC_BIEN_5==99]="NSP"

d$C_INDIC_BIEN_5_RC <- factor(d$C_INDIC_BIEN_5_RC, levels=c("Tres souvent",
                                                            "Souvent",
                                                            "Quelques fois",
                                                            "Jamais",
                                                            "Refus",
                                                            "NSP"))

var_label(d$C_INDIC_BIEN_5_RC) <- var_label(d$C_INDIC_BIEN_5)

#C_INDIC_BIEN_6_RC

d$C_INDIC_BIEN_6_RC[d$C_INDIC_BIEN_6==1]="Tres souvent"
d$C_INDIC_BIEN_6_RC[d$C_INDIC_BIEN_6==2]="Souvent"
d$C_INDIC_BIEN_6_RC[d$C_INDIC_BIEN_6==3]="Quelques fois"
d$C_INDIC_BIEN_6_RC[d$C_INDIC_BIEN_6==4]="Jamais"
d$C_INDIC_BIEN_6_RC[d$C_INDIC_BIEN_6==98]="Refus"
d$C_INDIC_BIEN_6_RC[d$C_INDIC_BIEN_6==99]="NSP"

d$C_INDIC_BIEN_6_RC <- factor(d$C_INDIC_BIEN_6_RC, levels=c("Tres souvent",
                                                            "Souvent",
                                                            "Quelques fois",
                                                            "Jamais",
                                                            "Refus",
                                                            "NSP"))

var_label(d$C_INDIC_BIEN_6_RC) <- var_label(d$C_INDIC_BIEN_6)

#C_INDIC_BIEN_7_RC

d$C_INDIC_BIEN_7_RC[d$C_INDIC_BIEN_7==1]="Tres souvent"
d$C_INDIC_BIEN_7_RC[d$C_INDIC_BIEN_7==2]="Souvent"
d$C_INDIC_BIEN_7_RC[d$C_INDIC_BIEN_7==3]="Quelques fois"
d$C_INDIC_BIEN_7_RC[d$C_INDIC_BIEN_7==4]="Jamais"
d$C_INDIC_BIEN_7_RC[d$C_INDIC_BIEN_7==98]="Refus"
d$C_INDIC_BIEN_7_RC[d$C_INDIC_BIEN_7==99]="NSP"

d$C_INDIC_BIEN_7_RC <- factor(d$C_INDIC_BIEN_7_RC, levels=c("Tres souvent",
                                                            "Souvent",
                                                            "Quelques fois",
                                                            "Jamais",
                                                            "Refus",
                                                            "NSP"))

var_label(d$C_INDIC_BIEN_7_RC) <- var_label(d$C_INDIC_BIEN_7)

### [REPRISE]



###LIGNES DE FIN
#Appliquer la transformation sur les colonnes qui peuvent être converties en factor
### Attention, cela enlève les labels des variables

d[,-(1:10)] <- as.data.frame(lapply(d[,-(1:10)], function(x) {
  if (is.integer(x) || is.character(x)) {
    as.factor(x)  # Convertir en factor
  } else {
    x  # Laisser la colonne telle quelle
  }
}))


#### Transformation en datetime#### 

d$StartDate <- as.POSIXct(d$StartDate, format="%Y-%m-%d %H:%M:%S")
d$EndDate <- as.POSIXct(d$EndDate, format="%Y-%m-%d %H:%M:%S")
d$RecordedDate <- as.POSIXct(d$RecordedDate, format="%Y-%m-%d %H:%M:%S")

#### Transformation en integer ####

d$Progress <- as.integer(d$Progress)
d$Duration_seconds <- as.integer(d$Duration_seconds)

### ATTENTION : VERIFIER LE FORMAT SOUHAITE POUR LES 10 PREMIERES VAR

#Récupération des labels 
var_label(d) <- dv


