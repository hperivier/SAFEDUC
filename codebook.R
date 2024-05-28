#import packages 
source("packages.R")

#import fonctions
source("fonctions.R")

# import des bases
safeduc_num <- read_csv("safeduc_num.csv")
safeduc_let <- read_csv("safeduc_let.csv")


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

d <- as.data.frame(lapply(d, function(x) {
  if (is.character(x) && is_convertible_to_integer(x)) {
    as.integer(x)  # Convertir en integer
  } else {
    x  # Laisser la colonne telle quelle
  }
}))

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

###LIGNES DE FIN
#Appliquer la transformation sur les colonnes qui peuvent être converties en factor

d <- as.data.frame(lapply(d, function(x) {
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
d$Duration..in.seconds. <- as.integer(d$Duration..in.seconds.)