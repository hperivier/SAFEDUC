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

#Initialement, toutes les variables sont de type character

#### Transformation en integer ####

#Appliquer la transformation sur les colonnes qui peuvent être converties en integer

d <- as.data.frame(lapply(d, function(x) {
  if (is.character(x) && is_convertible_to_integer(x)) {
    as.integer(x)  # Convertir en factor
  } else {
    x  # Laisser la colonne telle quelle
  }
}))

#### Fusionner safeduc_num et safeduc_let afin de labelliser les variables #### 

#d_label <- merge_with_labels(safeduc_num, safeduc_let)

#### Création de variables regroupant les modalités à choix multiples (reverse one hot encoding#### 

d<- reverse_one_hot_encoding(
  d, 
  c(
    "I_NATIONALITE", "I_NATIO_HORS_UE", "I_S_ANNEE",
    "I_S_AUTRE_ETAB_TYPE", "I_U_FACULTE", "I_U_FACULTE_SANTE", 
    "I_U_FACULTE_SCIENCES", "I_U_FACULTE_SOCIETES", "I_U_AUTRE_ETAB_TYPE",
    "C_ACTI_UNI", "C_ACTI_HORS"
  )
)

d<- reverse_one_hot_encoding(
  d, 
  c(
    "V_PSY_DOUZE", "V_PSY_ETAB","V_PSY_LIEU", "V_PSY_INTERNET", "V_PSY_AUTEUR_GENRE",
    "V_PSY_AUTEUR_STATUT", "V_PSY_AUTEUR_REL", "V_PSY_AUTEUR_ALCOOL", "V_PSY_MOTIF"
  )
)

d<- reverse_one_hot_encoding(
  d, 
  c(
    "V_PHY_ETAB","V_PHY_LIEU", "V_PHY_AUTEUR_STATUT", 
    "V_PHY_AUTEUR_REL", "V_PHY_AUTEUR_ALCOOL", "V_PHY_MOTIF"
  )
)

d<- reverse_one_hot_encoding(
  d, 
  c(
    "V_SEX_DOUZE", "V_SEX_ETAB","V_SEX_LIEU", "V_SEX_INTERNET", "V_SEX_AUTEUR_GENRE",
    "V_SEX_AUTEUR_STATUT", "V_SEX_AUTEUR_REL", "V_SEX_AUTEUR_ALCOOL", "V_SEX_MOTIF"
  )
)

d<- reverse_one_hot_encoding(
  d, 
  c(
    "Q_CONSQ", "Q_PARLE_PROFIL", "Q_PARLE_ETAB","Q_PARLE_RAIS_NO_ETA",
    "Q_PARLE_EXT", "Q_PARLE_RAIS_PLAINT", "Q_PARLE_RAIS_NO_EXT"
  )
)


###### Création de nouvelles variables recodées ######


#####Variables de comptage des faits vécus


variables_faits_psy <- names(d)[grepl("^P_FAITS_PSY", names(d))]
d <- compter_faits(d, variables_faits_psy, "P_FAITS_PSY_SUM")

variables_faits_phys <- names(d)[grepl("^P_FAITS_PHYS", names(d))]
d <- compter_faits(d, variables_faits_phys, "P_FAITS_PHYS_SUM")


variables_faits_sex <- names(d)[grepl("^P_FAITS_SEX", names(d))]
d <- compter_faits(d, variables_faits_sex, "P_FAITS_SEX_SUM")

d <- d %>% 
  mutate(
    P_FAITS_SUM = rowSums(across(c("P_FAITS_PSY_SUM", "P_FAITS_PHYS_SUM", "P_FAITS_SEX_SUM"), na.rm=T))
  )


#### Transformation en factor ####

#Appliquer la transformation sur les colonnes qui peuvent être converties en factor

d <- as.data.frame(lapply(d, function(x) {
  if (is.integer(x) && is_convertible_to_integer(x)) {
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

##### CODE VICTOR #####

#Variable I_ETAB, pour l'etablissement de rattachement (Sciences Po/Upcite)

d$I_ETAB_RC[d$I_ETAB==1]="Sciences Po"
d$I_ETAB_RC[d$I_ETAB==2]="UPCite"

d$I_ETAB_RC <- factor(d$I_ETAB_RC, levels=c("Sciences Po","UPCite"))

var_label(d$I_ETAB_RC) <- var_label(d$I_ETAB)
var_label(d$I_ETAB) <- "Etablissement de rattachement"

#Variable I_ECHANGE, pour le fait ou non d'etre en echange dans l'etablissement

d$I_ECHANGE_RC[d$I_ECHANGE==1]="Oui"
d$I_ECHANGE_RC[d$I_ECHANGE==2]="Non"
d$I_ECHANGE_RC[d$I_ECHANGE==99]="NSP"
d$I_ECHANGE_RC[d$I_ECHANGE==98]="Refus"

d$I_ECHANGE_RC <- factor(d$I_ECHANGE_RC, levels= c("Oui","Non",NA))

var_label(d$I_ECHANGE_RC) <- "En echange (ou non) au sein de l'etablissement"
var_label(d$I_ECHANGE) <- "En echange (ou non) au sein de l'etablissement"

#Variable I_IDGENRE, pour l'identite de genre declaree

d$I_IDGENRE_RC[d$I_IDGENRE==1]="Une femme"
d$I_IDGENRE_RC[d$I_IDGENRE==2]="Un homme"
d$I_IDGENRE_RC[d$I_IDGENRE==3]="Une personne non-binaire"
d$I_IDGENRE_RC[d$I_IDGENRE==4]="Vous utilisez un autre terme"
d$I_IDGENRE_RC[d$I_IDGENRE==98]="Refus"

d$I_IDGENRE_RC <- factor(d$I_IDGENRE_RC, levels=c("Une femme","Un homme","Une personne non-binaire","Vous utilisez un autre terme", NA))
var_label(d$I_IDGENRE_RC) <- "Identite de genre declaree"
var_label(d$I_IDGENRE) <- "Identite de genre declaree"


#Variable I_SEX_NAIS, pour les repondants ayant choisi "homme" ou "femme" ==> demande si c'etait la categorie a la naissance

d$I_SEX_NAIS_RC[d$I_SEX_NAIS==1]="Oui"
d$I_SEX_NAIS_RC[d$I_SEX_NAIS==2]="Non"
d$I_SEX_NAIS_RC[d$I_SEX_NAIS==98]="Refus"

d$I_SEX_NAIS_RC <- factor(d$I_SEX_NAIS_RC, levels=c("Oui","Non",NA))
var_label(d$I_SEX_NAIS_RC) <- "Sexe a la naissance identique a identite de genre"
var_label(d$I_SEX_NAIS) <- "Sexe a la naissance identique a identite de genre"

#Variable I_NAIS_FR, pour oui/non naissance en France

d$I_NAIS_FR_RC[d$I_NAIS_FR==1]="Oui, en France (metropolitaine ou departement d'Outre-mer)"
d$I_NAIS_FR_RC[d$I_NAIS_FR==2]="Non"
d$I_NAIS_FR_RC[d$I_NAIS_FR==98]="Refus"

d$I_NAIS_FR_RC <- factor(d$I_NAIS_FR_RC, levels=c("Oui, en France (metropolitaine ou departement d'Outre-mer)","Non", NA))
var_label(d$I_NAIS_FR_RC) <- "Naissance (ou non) en France"
var_label(d$I_NAIS_FR) <- "Naissance (ou non) en France"

#Variables I_NATIONALITE pour nationalite francaise ou autre (choix multiple)
#Variable I_NATIONALITE_FR pour nationalite francaise en binaire

d$I_NATIONALITE_FR[d$I_NATIONALITE_1==1]="Oui"
d$I_NATIONALITE_FR[d$I_NATIONALITE_1==0]="Non"

d$I_NATIONALITE_FR <- factor(d$I_NATIONALITE_FR,levels=c("Oui","Non"))
var_label(d$I_NATIONALITE_FR) <- "Nationalite francaise"
var_label(d$I_NATIONALITE_1) <- "Nationalite francaise"

#Variable I_NATIONALITE_UE pour nationalite dans l'union europeenne

d$I_NATIONALITE_UE[d$I_NATIONALITE_2==1]="Oui"
d$I_NATIONALITE_UE[d$I_NATIONALITE_2==0]="Non"

d$I_NATIONALITE_UE <- factor(d$I_NATIONALITE_UE,levels=c("Oui","Non"))
var_label(d$I_NATIONALITE_UE) <- "Autre nationalite (que francaise) dans l'Union Europenne"
var_label(d$I_NATIONALITE_2) <- "Autre nationalite (que francaise) dans l'Union Europenne"

#Variable I_NATIONALITE_HUE pour nationalite hors union europeenne

d$I_NATIONALITE_HUE[d$I_NATIONALITE_3==1]="Oui"
d$I_NATIONALITE_HUE[d$I_NATIONALITE_3==0]="Non" 

d$I_NATIONALITE_HUE <- factor(d$I_NATIONALITE_HUE,levels=c("Oui","Non"))
var_label(d$I_NATIONALITE_HUE) <- "Autre nationalite hors Union Europenne"
var_label(d$I_NATIONALITE_3) <- "Autre nationalite hors Union Europenne"

#A verifier avec nouveau recodage

d$I_NATIONALITE_REFUS[d$I_NATIONALITE_98==1]="Refus"
d$I_NATIONALITE_REFUS[d$I_NATIONALITE_98==0]="Pas de refus"

d$I_NATIONALITE_REFUS <- factor(d$I_NATIONALITE_HUE,levels=c("Refus","Pas de refus de répondre"))
var_label(d$I_NATIONALITE_REFUS) <- "Nationalite(s) - Refus de repondre"
var_label(d$I_NATIONALITE_98) <- "Nationalite(s) - Refus de repondre"

#Variable I_NATIO_HORS_UE, pour la zone de nationalite hors UE (choix multiples)
#Variable I_NATIO_HUE_EUR pour nationalite en europe non UE
d$I_NATIONALITE_EUR_HORS_UE[d$I_NATIO_HORS_UE_1==1]="Oui, nationalite Europe hors UE"
d$I_NATIONALITE_EUR_HORS_UE[d$I_NATIO_HORS_UE_1==0]="Non, pas de nationalite Europe hors UE"

d$I_NATIONALITE_EUR_HORS_UE <- factor(d$I_NATIONALITE_EUR_HORS_UE,levels=c("Oui, nationalite Europe hors UE","Non, pas de nationalite Europe hors UE"))
var_label(d$I_NATIONALITE_EUR_HORS_UE) <- "Nationalite europeenne hors UE"
var_label(d$I_NATIO_HORS_UE_1) <- "Nationalite europeenne hors UE"

#Variable I_NATIO_HUE_AMN pour nationalite en amerique du Nord
d$I_NATIONALITE_AMN[d$I_NATIO_HORS_UE_2==1]="Oui, nationalite Amerique du Nord"
d$I_NATIONALITE_AMN[d$I_NATIO_HORS_UE_2==0]="Non, pas de nationalite Amerique du Nord"

d$I_NATIONALITE_AMN <- factor(d$I_NATIONALITE_AMN,levels=c("Oui, nationalite Amerique du Nord","Non, pas de nationalite Amerique du Nord"))
var_label(d$I_NATIONALITE_AMN) <- "Nationalite nord-americaine"
var_label(d$I_NATIO_HORS_UE_2) <- "Nationalite nord-americaine"

#Variable I_NATIO_HUE_AMSC pour nationalite en amerique du sud ou Centrale
d$I_NATIONALITE_AMSC[d$I_NATIO_HORS_UE_3==1]="Oui, nationalite Amerique du Sud ou Centrale"
d$I_NATIONALITE_AMSC[d$I_NATIO_HORS_UE_3==0]="Non, pas de nationalite Amerique du Sud ou Centrale"

d$I_NATIONALITE_AMSC <- factor(d$I_NATIONALITE_AMSC,levels=c("Oui, nationalite Amerique du Sud ou Centrale","Non, pas de nationalite Amerique du Sud ou Centrale"))
var_label(d$I_NATIONALITE_AMSC) <- "Nationalite d'amerique du sud ou centrale"
var_label(d$I_NATIO_HORS_UE_3) <- "Nationalite d'amerique du sud ou centrale"

#Variable I_NATIO_HUE_ASSE pour nationalite en asie du sud et sud-est
d$I_NATIONALITE_ASSE[d$I_NATIO_HORS_UE_4==1]="Oui, nationalite Asie du Sud et Sud-Est"
d$I_NATIONALITE_ASSE[d$I_NATIO_HORS_UE_4==0]="Non, pas de nationalite Asie du Sud et Sud-Est"

d$I_NATIONALITE_ASSE <- factor(d$I_NATIONALITE_ASSE,levels=c("Oui, nationalite Asie du Sud et Sud-Est","Non, pas de nationalite Asie du Sud et Sud-Est"))
var_label(d$I_NATIONALITE_ASSE) <- "Nationalite d'amerique du sud ou centrale"
var_label(d$I_NATIO_HORS_UE_4) <- "Nationalite d'amerique du sud ou centrale"

#Variable I_NATIO_HUE_EAC pour nationalite en Eurasie ou asie centrale
d$I_NATIONALITE_EAC[d$I_NATIO_HORS_UE_5==1]="Oui, nationalite Eurasie et Asie Centrale"
d$I_NATIONALITE_EAC[d$I_NATIO_HORS_UE_5==0]="Non, pas de nationalite Eurasie et Asie Centrale"

d$I_NATIONALITE_EAC <- factor(d$I_NATIONALITE_EAC,levels=c("Oui, nationalite Eurasie et Asie Centrale","Non, pas de nationalite Eurasie et Asie Centrale"))
var_label(d$I_NATIONALITE_EAC) <- "Nationalite d'Eurasie ou asie centrale"
var_label(d$I_NATIO_HORS_UE_5) <- "Nationalite d'Eurasie ou asie centrale"

#Variable I_NATIO_HUE_MOAN pour nationalite au Moyen Orien et Afrique du Nord
d$I_NATIONALITE_MOAN[d$I_NATIO_HORS_UE_6==1]="Oui, nationalite Moyen Orient et Afrique du Nord"
d$I_NATIONALITE_MOAN[d$I_NATIO_HORS_UE_6==0]="Non, pas de nationalite Moyen Orient et Afrique du Nord"

d$I_NATIONALITE_MOAN <- factor(d$I_NATIONALITE_MOAN,levels=c("Oui, nationalite Moyen Orient et Afrique du Nord","Non, pas de nationalite Moyen Orient et Afrique du Nord"))
var_label(d$I_NATIONALITE_MOAN) <- "Nationalite du Moyen-Orient ou Afrique du Nord"
var_label(d$I_NATIO_HORS_UE_6) <- "Nationalite du Moyen-Orient ou Afrique du Nord"

#Variable I_NATIO_HUE_AFSS pour nationalite en Afrique Subsaharienne
d$I_NATIONALITE_AFSS[d$I_NATIO_HORS_UE_7==1]="Oui, nationalite Afrique Subsaharienne"
d$I_NATIONALITE_AFSS[d$I_NATIO_HORS_UE_7==0]="Non, pas de nationalite Afrique Subsaharienne"

d$I_NATIONALITE_AFSS <- factor(d$I_NATIONALITE_AFSS,levels=c("Oui, nationalite Afrique Subsaharienne","Non, pas de nationalite Afrique Subsaharienne"))
var_label(d$I_NATIONALITE_AFSS) <- "Nationalite d'Afrique Subsaharienne"
var_label(d$I_NATIO_HORS_UE_7) <- "Nationalite d'Afrique Subsaharienne"

#Variable I_NATIONALITE_OC pour nationalite en Oceanie 
d$I_NATIONALITE_OC[d$I_NATIO_HORS_UE_8==1]="Oui, nationalite Oceanie"
d$I_NATIONALITE_OC[d$I_NATIO_HORS_UE_8==0]="Non, pas de nationalite Oceanie"

d$I_NATIONALITE_OC <- factor(d$I_NATIONALITE_OC,levels=c("Oui, nationalite Oceanie","Non, pas de nationalite Oceanie"))
var_label(d$I_NATIONALITE_OC) <- "Nationalite d'Oceanie"
var_label(d$I_NATIO_HORS_UE_8) <- "Nationalite d'Oceanie"

#Variable I_NATIO_HUE_AUTRE pour autre nationalite hors UE
d$I_NATIONALITE_AUTRE[d$I_NATIO_HORS_UE_9==1]="Autre nationalite hors UE"
d$I_NATIONALITE_AUTRE[d$I_NATIO_HORS_UE_9==0]="Non, pas d'autre Nationalite hors UE"

d$I_NATIONALITE_AUTRE <- factor(d$I_NATIONALITE_AUTRE,levels=c("Autre nationalite hors UE","Non, pas d'autre Nationalite hors UE"))
var_label(d$I_NATIONALITE_AUTRE) <- "Autre nationalite (hors UE)"
var_label(d$I_NATIO_HORS_UE_9) <- "Autre nationalite (hors UE)"

#Variable I_NATIO_HUE_REFUS pour refus de donner la nationalite
d$I_NATIONALITE_HORS_EU_REFUS[d$I_NATIO_HORS_UE_98==1]="Refus"
d$I_NATIONALITE_HORS_EU_REFUS[d$I_NATIO_HORS_UE_98==0]="Non, pas de refus"

d$I_NATIONALITE_HORS_EU_REFUS <- factor(d$I_NATIONALITE_HORS_EU_REFUS,levels=c("Refus","Non, pas de refus"))
var_label(d$I_NATIONALITE_REFUS) <- "Nationalite (hors UE) - Refus"
var_label(d$I_NATIO_HORS_UE_98) <- "Nationalite (hors UE) - Refus"

# #On reprend ces variables, et on en fait des binaires pour les traitements
# #Variable I_NATIO_HUE_EUR_BIN pour nationalite en europe non UE
# d$I_NATIO_HUE_EUR_BIN[d$I_NATIO_HORS_UE_1==1]=1
# d$I_NATIO_HUE_EUR_BIN[d$I_NATIO_HORS_UE_1==0]=0
# #Variable I_NATIO_HUE_AMN pour nationalite en amerique du Nord
# d$I_NATIO_HUE_AMN_BIN[d$I_NATIO_HORS_UE_2==1]=1
# d$I_NATIO_HUE_AMN_BIN[d$I_NATIO_HORS_UE_2==0]=0
# #Variable I_NATIO_HUE_AMSC pour nationalite en amerique du sud ou Centrale
# d$I_NATIO_HUE_AMSC_BIN[d$I_NATIO_HORS_UE_3==1]=1
# d$I_NATIO_HUE_AMSC_BIN[d$I_NATIO_HORS_UE_3==0]=0
# #Variable I_NATIO_HUE_ASSE pour nationalite en asie du sud et sud-est
# d$I_NATIO_HUE_ASSE_BIN[d$I_NATIO_HORS_UE_4==1]=1
# d$I_NATIO_HUE_ASSE_BIN[d$I_NATIO_HORS_UE_4==0]=0
# #Variable I_NATIO_HUE_EAC pour nationalite en Eurase ou asie centrale
# d$I_NATIO_HUE_EAC_BIN[d$I_NATIO_HORS_UE_5==1]=1
# d$I_NATIO_HUE_EAC_BIN[d$I_NATIO_HORS_UE_5==0]=0
# #Variable I_NATIO_HUE_MOAN pour nationalite au Moyen Orien et Afrique du Nord
# d$I_NATIO_HUE_MOAN_BIN[d$I_NATIO_HORS_UE_6==1]=1
# d$I_NATIO_HUE_MOAN_BIN[d$I_NATIO_HORS_UE_6==0]=0
# #Variable I_NATIO_HUE_AFSS pour nationalite en Afrique Subsaharienne
# d$I_NATIO_HUE_AFSS_BIN[d$I_NATIO_HORS_UE_7==1]=1
# d$I_NATIO_HUE_AFSS_BIN[d$I_NATIO_HORS_UE_7==0]=0
# #Variable I_NATIO_HUE_OC pour nationalite en Oceanie
# d$I_NATIO_HUE_OC_BIN[d$I_NATIO_HORS_UE_8==1]=1
# d$I_NATIO_HUE_OC_BIN[d$I_NATIO_HORS_UE_8==0]=0
# #Variable I_NATIO_HUE_AUTRE pour autre nationalite hors UE
# d$I_NATIO_HUE_AUTRE_BIN[d$I_NATIO_HORS_UE_9==1]=1
# d$I_NATIO_HUE_AUTRE_BIN[d$I_NATIO_HORS_UE_9==0]=0
# #Variable I_NATIO_HUE_REFUS pour refus de donner la nationalite
# d$I_NATIO_HUE_REFUS_BIN[d$I_NATIO_HORS_UE_98==1]=1
# d$I_NATIO_HUE_REFUS_BIN[d$I_NATIO_HORS_UE_98==0]=0

# Recodage de d$I_CSP_PARENTS_1 en d$I_CSP_PARENTS_1_RC

d$I_CSP_PARENTS_1_RC <- d$I_CSP_PARENTS_1
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
    NA))

# Recodage de d$I_CSP_PARENTS_2 en d$I_CSP_PARENTS_2_RC

d$I_CSP_PARENTS_2_RC <- d$I_CSP_PARENTS_2
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
    NA))

#RECODAGE DU MODULE SCIENCES PO

#Recodage de d$I_S_AGE en d$I_S_AGE_RC

d$I_S_AGE_RC <- d$I_S_AGE
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
    NA))

#Recodage de d$I_S_CAMPUS en d$I_S_CAMPUS_RC "Vous etudiez actuellement sur le campus de..."

d$I_S_CAMPUS_RC <- d$I_S_CAMPUS
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
  NA))

#Recodage des variable d$I_S_ANNEE

d$I_S_ANNEE_RC[d$I_S_ANNEE_1==1]="Bac + 1 (L1 ou equivalent)"
d$I_S_ANNEE_RC[d$I_S_ANNEE_2==1]="Bac + 2 (L2 ou equivalent)"
d$I_S_ANNEE_RC[d$I_S_ANNEE_3==1]="Bac + 3 (L3 ou equivalent)"
d$I_S_ANNEE_RC[d$I_S_ANNEE_4==1]="Bac + 4 (M1 ou equivalent)"
d$I_S_ANNEE_RC[d$I_S_ANNEE_5==1]="Bac + 5 (M2 ou equivalent)"
d$I_S_ANNEE_RC[d$I_S_ANNEE_5==1 & d$I_S_ANNEE_7==1 ]="Bac + 5 (M2 ou equivalent) ET preparation au concours"
d$I_S_ANNEE_RC[d$I_S_ANNEE_5==0 & d$I_S_ANNEE_7==1 ]="Uniquement la preparation aux concours administratifs"
d$I_S_ANNEE_RC[d$I_S_ANNEE_6==1]="Doctorat"

d$I_S_ANNEE_RC <- factor(d$I_S_ANNEE_RC,levels=c("Bac + 1 (L1 ou equivalent)",
                                                 "Bac + 2 (L2 ou equivalent)",
                                                 "Bac + 3 (L3 ou equivalent)", 
                                                 "Bac + 4 (M1 ou equivalent)", 
                                                 "Bac + 5 (M2 ou equivalent)",
                                                 "Bac + 5 (M2 ou equivalent) ET preparation au concours",
                                                 "Doctorat",
                                                 "Uniquement la preparation aux concours administratifs",NA))
#On recode la variable I_S_APPREN

d$I_S_APPREN_RC[d$I_S_APPREN==1]="Oui, j'ai un contrat d'alternance ou d'apprentissage cette annee"
d$I_S_APPREN_RC[d$I_S_APPREN==2]="Non"
d$I_S_APPREN_RC[d$I_S_APPREN==98]="Refus"

d$I_S_APPREN_RC <- factor(d$I_S_APPREN_RC,levels=c("Oui, j'ai un contrat d'alternance ou d'apprentissage cette annee","Non",NA))

#On recode la variable I_S_ECOLE

d$I_S_ECOLE_RC[d$I_S_ECOLE==1]="Ecole d'affaire publiques"
d$I_S_ECOLE_RC[d$I_S_ECOLE==2]="Ecole des affaires internationales"
d$I_S_ECOLE_RC[d$I_S_ECOLE==3]="Ecole de droit"
d$I_S_ECOLE_RC[d$I_S_ECOLE==4]="Ecole de journalisme"
d$I_S_ECOLE_RC[d$I_S_ECOLE==5]="Ecole du management et de l'impact"
d$I_S_ECOLE_RC[d$I_S_ECOLE==6]="Ecole urbaine" 
d$I_S_ECOLE_RC[d$I_S_ECOLE==7]="Ecole de la recherche" 
d$I_S_ECOLE_RC[d$I_S_ECOLE==99]="Ne sait pas"
d$I_S_ECOLE_RC[d$I_S_ECOLE==98]="Refus"

d$I_S_ECOLE_RC <- factor(d$I_S_ECOLE_RC,levels=c(
  "Ecole d'affaire publiques",
  "Ecole des affaires internationales",
  "Ecole de droit",
  "Ecole de journalisme",
  "Ecole du management et de l'impact",
  "Ecole urbaine" ,
  "Ecole de la recherche",
  NA))

#[REPRISE]
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
                                     NA))

#On recode la variable I_S_ECHELON_BOURSE

d$I_S_ECHELON_BOURSE_RC <- d$I_S_ECHELON_BOURSE
d$I_S_ECHELON_BOURSE_RC[d$I_S_ECHELON_BOURSE==1]="Echelon 0 bis"
d$I_S_ECHELON_BOURSE_RC[d$I_S_ECHELON_BOURSE==2]="Echelon 1 a 7 (inclus)"
d$I_S_ECHELON_BOURSE_RC[d$I_S_ECHELON_BOURSE==98]="Refus"
d$I_S_ECHELON_BOURSE_RC[d$I_S_ECHELON_BOURSE==99]="NSP"

d$I_S_ECHELON_BOURSE_RC <- factor(d$I_S_ECHELON_BOURSE,
                                  levels=c("Echelon 0 bis",
                                           "Echelon 1 a 7 (inclus)",
                                           NA))

#On recode la variable I_S_AUTRE_ETAB

d$I_S_AUTRE_ETAB_RC <- d$I_S_AUTRE_ETAB
d$I_S_AUTRE_ETAB_RC[d$I_S_AUTRE_ETAB==1]="Oui"
d$I_S_AUTRE_ETAB_RC[d$I_S_AUTRE_ETAB==2]="Non"
d$I_S_AUTRE_ETAB_RC[d$I_S_AUTRE_ETAB==98]="Refus"

d$I_S_AUTRE_ETAB_RC <- factor(d$I_S_AUTRE_ETAB_RC,
                              levels=c("Oui",
                                       "Non",
                                       NA))

#On recode la variable I_S_AUTRE_ETAB_TYPE

d$I_S_AUTRE_ETAB_UNIV_FR <- d$I_S_AUTRE_ETAB_TYPE_1
d$I_S_AUTRE_ETAB_IUT <- d$I_S_AUTRE_ETAB_TYPE2
d$I_S_AUTRE_ETAB_UNIV_ETR <- d$I_S_AUTRE_ETAB_TYPE_3
d$I_S_AUTRE_ETAB_PREP <- d$I_S_AUTRE_ETAB_TYPE_4
d$I_S_AUTRE_ETAB_ECOM <- d$I_S_AUTRE_ETAB_TYPE_5
d$I_S_AUTRE_ETAB_EING <- d$I_S_AUTRE_ETAB_TYPE_6
d$I_S_AUTRE_ETAB_BTS <- d$I_S_AUTRE_ETAB_TYPE_7
d$I_S_AUTRE_ETAB_AUTRE <- d$I_S_AUTRE_ETAB_TYPE_8
d$I_S_AUTRE_ETAB_NSP <- d$I_S_AUTRE_ETAB_TYPE_99
d$I_S_AUTRE_ETAB_REFUS <- d$I_S_AUTRE_ETAB_TYPE_98

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
                                NA))

#On recode la variable I_U_ANNEE

d$I_U_ANNEE_RC[d$I_U_ANNEE==1]="Bac + 1 (L1 ou equivalent)"
d$I_U_ANNEE_RC[d$I_U_ANNEE==2]="Bac + 2 (L2 ou equivalent)"
d$I_U_ANNEE_RC[d$I_U_ANNEE==3]="Bac + 3 (L3 ou equivalent)"
d$I_U_ANNEE_RC[d$I_U_ANNEE==4]="Bac + 4 (M1 ou equivalent)"
d$I_U_ANNEE_RC[d$I_U_ANNEE==5]="Bac + 5 (M2 ou equivalent)"
d$I_U_ANNEE_RC[d$I_U_ANNEE==6]="Doctorat"
d$I_U_ANNEE_RC[d$I_U_ANNEE==7]="Autre situation"
d$I_U_ANNEE_RC[d$I_U_ANNEE==98]="Refus"
d$I_U_ANNEE_RC[d$I_U_ANNEE==99]="NSP"


d$I_U_ANNEE_RC <- factor(d$I_U_ANNEE_RC,levels=c("Bac + 1 (L1 ou equivalent)",
                                                 "Bac + 2 (L2 ou equivalent)",
                                                 "Bac + 3 (L3 ou equivalent)", 
                                                 "Bac + 4 (M1 ou equivalent)", 
                                                 "Bac + 5 (M2 ou equivalent)",
                                                 "Doctorat",
                                                 "Autre situation",NA))

#On recode la variable I_U_APPREN 


d$I_U_APPREN_RC[d$I_U_APPREN==1]="Oui, j'ai un contrat d'alternance ou d'apprentissage cette annee"
d$I_U_APPREN_RC[d$I_U_APPREN==2]="Non"
d$I_U_APPREN_RC[d$I_U_APPREN==98]="Refus"

d$I_U_APPREN_RC <- factor(d$I_U_APPREN_RC,levels=c("Oui, j'ai un contrat d'alternance ou d'apprentissage cette annee","Non",NA))


#On recode la variable I_U_FACULTE

d$I_U_FACULTE_SANTE<- d$I_U_FACULTE_1
d$I_U_FACULTE_SCIENCES <- d$I_U_FACULTE_2
d$I_U_FACULTE_SHS <- d$I_U_FACULTE_3
d$I_U_FACULTE_IPGP <- d$I_U_FACULTE_4
d$I_U_FACULTE_REFUS <- d$I_U_FACULTE_98

#On cree la variable I_U_FACULTE_NB pour le nombre de facultes cochees

table(d$I_U_FACULTE_1)
d$I_U_FACULTE_GEN <- rowSums(d[,c("I_U_FACULTE_1","I_U_FACULTE_2","I_U_FACULTE_3","I_U_FACULTE_4")], na.rm=T)

#On recode la variable d$I_U_FACULTE_SANTE

d$I_UFR_MEDECINE <- d$I_U_FACULTE_SANTE_1
d$I_UFR_ODONTO <- d$I_U_FACULTE_SANTE_2
d$I_UFR_PHARMA <- d$I_U_FACULTE_SANTE_3
d$I_UFR_DEP_MAIEUTIQUE<- d$I_U_FACULTE_SANTE_4
d$I_UFR_DEP_SCINFIRMIERE<- d$I_U_FACULTE_SANTE_5
d$I_UFR_DEP_REEDUC_READP <- d$I_U_FACULTE_SANTE_6
d$I_UFR_SANTE_AUTRE <- d$I_UFR_FACULTE_SANTE_7
d$I_UFR_SANTE_NSP <- d$I_U_FACULTE_SANTE_99
d$I_UFR_SANTE_REFUS <- d$I_U_FACULTE_SANTE_98

#On cree la variable I_U_UFR_SANTE_NB pour le nombre d'UFR a la faculte de sante
d$I_UFR_SANTE_GEN <- rowSums(d[,c("I_U_FACULTE_SANTE_1","I_U_FACULTE_SANTE_2","I_U_FACULTE_SANTE_3","I_U_FACULTE_SANTE_4","I_U_FACULTE_SANTE_5","I_U_FACULTE_SANTE_6","I_UFR_FACULTE_SANTE_7")], na.rm=T)

#On recode la variable d$I_U_FACULTE_SCIENCES

d$I_UFR_EIDD <- d$I_U_FACULTE_SCIENCES_1
d$I_UFR_SCFOND_BIOMEDIC<- d$I_U_FACULTE_SCIENCES_2
d$I_UFR_IUTPAJOL <- d$I_U_FACULTE_SCIENCES_3
d$I_UFR_CHIMIE <- d$I_U_FACULTE_SCIENCES_4
d$I_UFR_INFORMATIQUE <- d$I_U_FACULTE_SCIENCES_5
d$I_UFR_MATH <- d$I_U_FACULTE_SCIENCES_6
d$I_UFR_MATH_ET_INFORMATIQUE <- d$I_U_FACULTE_SCIENCES_7
d$I_UFR_PHYSIQUE <- d$I_U_FACULTE_SCIENCES_8
d$I_UFR_SDV <- d$I_U_FACULTE_SCIENCES_9
d$I_UFR_SCIENCES_AUTRE <- d$I_U_FACULTE_SCIENCES_10
d$I_UFR_SCIENCES_NSP <- d$I_U_FACULTE_SCIENCES_99
d$I_UFR_SCIENCES_REFUS <- d$I_U_FACULTE_SCIENCES_98

#On cree la variable I_UFR_SCIENCES_NB pour le nombre d'UFR a la faculte des Sciences
d$I_UFR_SCIENCES_GEN <-rowSums(d[, c("I_U_FACULTE_SCIENCES_1",
                                    "I_U_FACULTE_SCIENCES_2",
                                    "I_U_FACULTE_SCIENCES_3",
                                    "I_U_FACULTE_SCIENCES_4",
                                    "I_U_FACULTE_SCIENCES_5",
                                    "I_U_FACULTE_SCIENCES_6",
                                    "I_U_FACULTE_SCIENCES_7",
                                    "I_U_FACULTE_SCIENCES_8",
                                    "I_U_FACULTE_SCIENCES_9",
                                    "I_U_FACULTE_SCIENCES_10")],na.rm=T)

#On recode la variable I_U_FACULTE_SOCIETES

d$I_UFR_DR_ECO_GEST <- d$I_U_FACULTE_SOCIETES_1
d$I_UFR_ETU_ANGLO<- d$I_U_FACULTE_SOCIETES_2
d$I_UFR_LANG_APPLIQ <- d$I_U_FACULTE_SOCIETES_3
d$I_UFR_GEO_HIST_ECO_SOC<- d$I_U_FACULTE_SOCIETES_4
d$I_UFR_PSYCHO <- d$I_U_FACULTE_SOCIETES_5
d$I_UFR_HUMA_SC_ET_SOC <- d$I_U_FACULTE_SOCIETES_6
d$I_UFR_IUT_RIV2SEINE <- d$I_U_FACULTE_SOCIETES_7
d$I_UFR_LANG_CIV_ASIE_ORIENT <- d$I_U_FACULTE_SOCIETES_8
d$I_UFR_LETTR_ART_CINE <- d$I_U_FACULTE_SOCIETES_9
d$I_UFR_LINGUI<- d$I_U_FACULTE_SOCIETES_10
d$I_UFR_SHS <-  d$I_U_FACULTE_SOCIETES_11
d$I_UFR_STAPS <- d$I_U_FACULTE_SOCIETES_12
d$I_UFR_SOCIETES_AUTRE <- d$I_U_FACULTE_SOCIETES_13

d$I_UFR_SOCIETES_REFUS <- d$I_U_FACULTE_SOCIETES_98
d$I_UFR_SOCIETES_NSP <- d$I_U_FACULTE_SOCIETES_99

#On cree la variable I_UFR_SCIENCES_NB pour le nombre d'UFR a la faculte des Sciences
d$I_UFR_SOCIETES_GEN <-rowSums(d[, c("I_U_FACULTE_SOCIETES_1",
                                    "I_U_FACULTE_SOCIETES_2",
                                    "I_U_FACULTE_SOCIETES_3",
                                    "I_U_FACULTE_SOCIETES_4",
                                    "I_U_FACULTE_SOCIETES_5",
                                    "I_U_FACULTE_SOCIETES_6",
                                    "I_U_FACULTE_SOCIETES_7",
                                    "I_U_FACULTE_SOCIETES_8",
                                    "I_U_FACULTE_SOCIETES_9",
                                    "I_U_FACULTE_SOCIETES_10",
                                    "I_U_FACULTE_SOCIETES_11",
                                    "I_U_FACULTE_SOCIETES_12",
                                    "I_U_FACULTE_SOCIETES_13")],na.rm=T)

#On cree la variable I_UFR_NB pour le nombre d'UFR dans l'ensemble d'UPCITE

d$I_UFR_GEN <- rowSums((d[,c("I_UFR_SANTE_NB","I_UFR_SCIENCES_NB","I_UFR_SOCIETES_NB")]))

#On recode la variable I_U_BOURSE

d$I_U_BOURSE_RC[d$I_U_BOURSE==1]="Oui, d'une bourse du Crous (Centre regional des oeuvres universitaires et scolaires)"
d$I_U_BOURSE_RC[d$I_U_BOURSE==2]="Oui, d'une autre bourse (hors Crous)"
d$I_U_BOURSE_RC[d$I_U_BOURSE==3]="Non"
d$I_U_BOURSE_RC[d$I_U_BOURSE==98]="Refus"
d$I_U_BOURSE_RC[d$I_U_BOURSE==99]="NSP"

d$I_U_BOURSE_RC <- factor(d$I_U_BOURSE_RC,
                          levels = c("Oui, d'une bourse du Crous (Centre regional des oeuvres universitaires et scolaires)",
                                     "Oui, d'une autre bourse (hors Crous)",
                                     "Non",
                                     NA))

#On recode la variable I_U_ECHELON_BOURSE

d$I_U_ECHELON_BOURSE_RC <- d$I_U_ECHELON_BOURSE
d$I_U_ECHELON_BOURSE_RC[d$I_U_ECHELON_BOURSE==1]="Echelon 0 bis"
d$I_U_ECHELON_BOURSE_RC[d$I_U_ECHELON_BOURSE==2]="Echelon 1 a 7 (inclus)"
d$I_U_ECHELON_BOURSE_RC[d$I_U_ECHELON_BOURSE==98]="Refus"
d$I_U_ECHELON_BOURSE_RC[d$I_U_ECHELON_BOURSE==99]="NSP"

d$I_U_ECHELON_BOURSE_RC <- factor(d$I_U_ECHELON_BOURSE,
                                  levels=c("Echelon 0 bis",
                                           "Echelon 1 a 7 (inclus)",
                                           NA))

#On recode la variable I_U_AUTRE_ETAB

d$I_U_AUTRE_ETAB_RC <- d$I_U_AUTRE_ETAB
d$I_U_AUTRE_ETAB_RC[d$I_U_AUTRE_ETAB==1]="Oui"
d$I_U_AUTRE_ETAB_RC[d$I_U_AUTRE_ETAB==2]="Non"
d$I_U_AUTRE_ETAB_RC[d$I_U_AUTRE_ETAB==98]="Refus"

d$I_U_AUTRE_ETAB_RC <- factor(d$I_U_AUTRE_ETAB_RC,
                              levels=c("Oui",
                                       "Non",
                                       NA))

#On recode la variable I_U_AUTRE_ETAB_TYPE

d$I_U_AUTRE_ETAB_UNIV_FR <- d$I_U_AUTRE_ETAB_TYPE_1
d$I_U_AUTRE_ETAB_IUT <- d$I_U_AUTRE_ETAB_TYPE2
d$I_U_AUTRE_ETAB_UNIV_ETR <- d$I_U_AUTRE_ETAB_TYPE_3
d$I_U_AUTRE_ETAB_PREP <- d$I_U_AUTRE_ETAB_TYPE_4
d$I_U_AUTRE_ETAB_ECOM <- d$I_U_AUTRE_ETAB_TYPE_5
d$I_U_AUTRE_ETAB_EING <- d$I_U_AUTRE_ETAB_TYPE_6
d$I_U_AUTRE_ETAB_BTS <- d$I_U_AUTRE_ETAB_TYPE_7
d$I_U_AUTRE_ETAB_AUTRE <- d$I_U_AUTRE_ETAB_TYPE_8
d$I_U_AUTRE_ETAB_NSP <- d$I_U_AUTRE_ETAB_TYPE_99
d$I_U_AUTRE_ETAB_REFUS <- d$I_U_AUTRE_ETAB_TYPE_98

#On cree une serie de variable, commune entre UPCite et Sciences Po

table(d$I_U_AGE_RC)
d$I_AGE_RC <- NA

d$I_AGE_RC[d$I_S_AGE_RC=="Moins de 18 ans"] <-  "20 ans ou moins"
d$I_AGE_RC[d$I_S_AGE_RC=="Entre 18 et 20 ans inclus"] <-  "20 ans ou moins"
d$I_AGE_RC[d$I_S_AGE_RC=="Entre 21 et 25 ans inclus"] <-  "Entre 21 et 25 ans inclus"
d$I_AGE_RC[d$I_S_AGE_RC=="Entre 26 et 30 ans inclus"] <-  "Entre 26 et 30 ans inclus"
d$I_AGE_RC[d$I_S_AGE_RC=="Plus de 30 ans"] <-  "Plus de 30 ans"

d$I_AGE_RC[d$I_U_AGE_RC=="Moins de 19 ans"] <-  "20 ans ou moins"
d$I_AGE_RC[d$I_U_AGE_RC=="19 ou 20 ans"] <-  "20 ans ou moins"
d$I_AGE_RC[d$I_U_AGE_RC=="Entre 21 et 25 ans inclus"] <-  "Entre 21 et 25 ans inclus"
d$I_AGE_RC[d$I_U_AGE_RC=="Entre 26 et 30 ans inclus"] <-  "Entre 26 et 30 ans inclus"
d$I_AGE_RC[d$I_U_AGE_RC=="Plus de 30 ans"] <-  "Plus de 30 ans"

d$I_AGE_RC <- factor(d$I_AGE_RC, levels=c("20 ans ou moins",
                                          "Entre 21 et 25 ans inclus",
                                          "Entre 26 et 30 ans inclus",
                                          "Plus de 30 ans"))
#On cree la variable d'annee d'etude

d$I_ANNEE_RC <- NA

d$I_ANNEE_RC[d$I_U_ANNEE_RC=="Bac + 1 (L1 ou equivalent)"]="Bac + 1 (L1 ou equivalent)"
d$I_ANNEE_RC[d$I_U_ANNEE_RC=="Bac + 2 (L2 ou equivalent)"]="Bac + 2 (L2 ou equivalent)"
d$I_ANNEE_RC[d$I_U_ANNEE_RC=="Bac + 3 (L3 ou equivalent)"]="Bac + 3 (L3 ou equivalent)"
d$I_ANNEE_RC[d$I_U_ANNEE_RC=="Bac + 4 (M1 ou equivalent)"]="Bac + 4 (M1 ou equivalent)"
d$I_ANNEE_RC[d$I_U_ANNEE_RC=="Bac + 5 (M2 ou equivalent)"]="Bac + 5 (M2 ou equivalent)"
d$I_ANNEE_RC[d$I_U_ANNEE_RC=="Doctorat"]="Doctorat"
d$I_ANNEE_RC[d$I_U_ANNEE_RC=="Autre situation"]="Autre situation"
d$I_ANNEE_RC[d$I_U_ANNEE_RC=="Refus"]="Refus"
d$I_ANNEE_RC[d$I_U_ANNEE_RC=="NSP"]="NSP"
table(d$I_ANNEE_RC)

d$I_ANNEE_RC[d$I_S_ANNEE_RC=="Bac + 1 (L1 ou equivalent)"]="Bac + 1 (L1 ou equivalent)"
d$I_ANNEE_RC[d$I_S_ANNEE_RC=="Bac + 2 (L2 ou equivalent)"]="Bac + 2 (L2 ou equivalent)"
d$I_ANNEE_RC[d$I_S_ANNEE_RC=="Bac + 3 (L3 ou equivalent)"]="Bac + 3 (L3 ou equivalent)"
d$I_ANNEE_RC[d$I_S_ANNEE_RC=="Bac + 4 (M1 ou equivalent)"]="Bac + 4 (M1 ou equivalent)"
d$I_ANNEE_RC[d$I_S_ANNEE_RC=="Bac + 5 (M2 ou equivalent)"]="Bac + 5 (M2 ou equivalent)"
d$I_ANNEE_RC[d$I_S_ANNEE_RC=="Bac + 5 (M2 ou equivalent) ET preparation au concours"]="Bac + 5 (M2 ou equivalent)"
d$I_ANNEE_RC[d$I_S_ANNEE_RC=="Uniquement la preparation aux concours administratifs"]="Autre situation"
d$I_ANNEE_RC[d$I_S_ANNEE_RC=="Doctorat"]="Doctorat"

d$I_ANNEE_RC <- factor(d$I_ANNEE_RC,levels=c("Bac + 1 (L1 ou equivalent)",
                                             "Bac + 2 (L2 ou equivalent)",
                                             "Bac + 3 (L3 ou equivalent)", 
                                             "Bac + 4 (M1 ou equivalent)", 
                                             "Bac + 5 (M2 ou equivalent)",
                                             "Doctorat",
                                             "Autre situation",NA))

#On cree la variable I_APPREN_RC

d$I_APPREN_RC <- NA
d$I_APPREN_RC <- factor(d$I_APPREN_RC,levels=c("Oui, j'ai un contrat d'alternance ou d'apprentissage cette annee","Non",NA))
d$I_APPREN_RC[d$I_S_APPREN_RC=="Oui, j'ai un contrat d'alternance ou d'apprentissage cette annee" | d$I_U_APPREN_RC=="Oui, j'ai un contrat d'alternance ou d'apprentissage cette annee"]="Oui, j'ai un contrat d'alternance ou d'apprentissage cette annee"
d$I_APPREN_RC[d$I_S_APPREN_RC=="Non" | d$I_U_APPREN_RC=="Non"]="Non"

#On cree la variable I_BOURSE_RC

d$I_BOURSE_RC <- NA
d$I_BOURSE_RC <- factor(d$I_BOURSE_RC,levels = c("Oui, d'une bourse du Crous (Centre regional des oeuvres universitaires et scolaires)",
                                                 "Oui, d'une autre bourse (hors Crous)",
                                                 "Non",
                                                 NA))

d$I_BOURSE_RC[d$I_S_BOURSE_RC=="Oui, d'une bourse du Crous (Centre regional des oeuvres universitaires et scolaires)" | d$I_U_BOURSE_RC=="Oui, d'une bourse du Crous (Centre regional des oeuvres universitaires et scolaires)"]="Oui, d'une bourse du Crous (Centre regional des oeuvres universitaires et scolaires)"
d$I_BOURSE_RC[d$I_S_BOURSE_RC=="Oui, d'une autre bourse (hors Crous)" | d$I_U_BOURSE_RC=="Oui, d'une autre bourse (hors Crous)"]="Oui, d'une autre bourse (hors Crous)"
d$I_BOURSE_RC[d$I_S_BOURSE_RC=="Non" | d$I_U_BOURSE_RC=="Non"]="Non"

#On cree la variable I_ECHELON_BOURSE

d$I_ECHELON_BOURSE_RC <- NA
d$I_ECHELON_BOURSE_RC[d$I_S_ECHELON_BOURSE_RC=="Echelon 0 bis"|d$I_U_ECHELON_BOURSE_RC=="Echelon 0 bis"]="Echelon 0 bis"
d$I_ECHELON_BOURSE_RC[d$I_S_ECHELON_BOURSE_RC=="Echelon 1 a 7 (inclus)"|d$I_U_ECHELON_BOURSE_RC=="Echelon 1 a 7 (inclus)"]="Echelon 1 a 7 (inclus)"

d$I_ECHELON_BOURSE_RC <- factor(d$I_ECHELON_BOURSE, levels=c("Echelon 0 bis",
                                                             "Echelon 1 a 7 (inclus)",
                                                             NA))

#On cree la variable I_AUTRE_ETAB

d$I_AUTRE_ETAB_RC <- NA
d$I_AUTRE_ETAB_RC[d$I_S_AUTRE_ETAB_RC=="Oui"|d$I_U_AUTRE_ETAB_RC=="Oui"]="Oui"
d$I_AUTRE_ETAB_RC[d$I_S_AUTRE_ETAB_RC=="Non"|d$I_U_AUTRE_ETAB_RC=="Non"]="Non"

d$I_AUTRE_ETAB_RC <- factor(d$I_AUTRE_ETAB_RC, levels=c("Oui",
                                                        "Non",
                                                        NA))

#On cree une succession de variables dichotomiques pour le type d'etablissement precedent

d$I_AUTRE_ETAB_UNIV_FR[d$I_S_AUTRE_ETAB_TYPE_1==1 |d$I_U_AUTRE_ETAB_TYPE_1==1]="Oui"
d$I_AUTRE_ETAB_UNIV_FR[d$I_S_AUTRE_ETAB_TYPE_1==0 |d$I_U_AUTRE_ETAB_TYPE_1==0]="Non"

d$I_AUTRE_ETAB_IUT[d$I_S_AUTRE_ETAB_TYPE_2==1 |d$I_U_AUTRE_ETAB_TYPE_2==1]="Oui"
d$I_AUTRE_ETAB_IUT[d$I_S_AUTRE_ETAB_TYPE_2==0 |d$I_U_AUTRE_ETAB_TYPE_2==0]="Non"

d$I_AUTRE_ETAB_UNIV_ETR[d$I_S_AUTRE_ETAB_TYPE_3==1 |d$I_U_AUTRE_ETAB_TYPE_3==1]="Oui"
d$I_AUTRE_ETAB_UNIV_ETR[d$I_S_AUTRE_ETAB_TYPE_3==0 |d$I_U_AUTRE_ETAB_TYPE_3==0]="Non"

d$I_AUTRE_ETAB_PREP[d$I_S_AUTRE_ETAB_TYPE_4==1 |d$I_U_AUTRE_ETAB_TYPE_4==1]="Oui"
d$I_AUTRE_ETAB_PREP[d$I_S_AUTRE_ETAB_TYPE_4==0 |d$I_U_AUTRE_ETAB_TYPE_4==0]="Non"

d$I_AUTRE_ETAB_ECOM[d$I_S_AUTRE_ETAB_TYPE_5==1 |d$I_U_AUTRE_ETAB_TYPE_5==1]="Oui"
d$I_AUTRE_ETAB_ECOM[d$I_S_AUTRE_ETAB_TYPE_5==0 |d$I_U_AUTRE_ETAB_TYPE_5==0]="Non"

d$I_AUTRE_ETAB_EING[d$I_S_AUTRE_ETAB_TYPE_6==1 |d$I_U_AUTRE_ETAB_TYPE_6==1]="Oui"
d$I_AUTRE_ETAB_EING[d$I_S_AUTRE_ETAB_TYPE_6==0 |d$I_U_AUTRE_ETAB_TYPE_6==0]="Non"

d$I_AUTRE_ETAB_BTS[d$I_S_AUTRE_ETAB_TYPE_7==1 |d$I_U_AUTRE_ETAB_TYPE_7==1]="Oui"
d$I_AUTRE_ETAB_BTS[d$I_S_AUTRE_ETAB_TYPE_7==0 |d$I_U_AUTRE_ETAB_TYPE_7==0]="Non"

d$I_AUTRE_ETAB_AUTRE[d$I_S_AUTRE_ETAB_TYPE_8==1 |d$I_U_AUTRE_ETAB_TYPE_8==1]="Oui"
d$I_AUTRE_ETAB_AUTRE[d$I_S_AUTRE_ETAB_TYPE_8==0 |d$I_U_AUTRE_ETAB_TYPE_8==0]="Non"

d$I_AUTRE_ETAB_NSP[d$I_S_AUTRE_ETAB_TYPE_99==1 |d$I_U_AUTRE_ETAB_TYPE_99==1]="Oui"
d$I_AUTRE_ETAB_NSP[d$I_S_AUTRE_ETAB_TYPE_99==0 |d$I_U_AUTRE_ETAB_TYPE_99==0]="Non"

d$I_AUTRE_ETAB_REFUS[d$I_S_AUTRE_ETAB_TYPE_98==1 |d$I_U_AUTRE_ETAB_TYPE_98==1]="Oui"
d$I_AUTRE_ETAB_REFUS[d$I_S_AUTRE_ETAB_TYPE_98==0 |d$I_U_AUTRE_ETAB_TYPE_98==0]="Non"

#On cree la variable C_HABIT_RC

d$C_HABIT_RC <- NA
d$C_HABIT_RC <- d$C_HABIT

d$C_HABIT_RC[d$C_HABIT==1]="Chez un de vos parents ou chez vos deux parents"
d$C_HABIT_RC[d$C_HABIT==2]="Chez quelqu'un de votre entourage"
d$C_HABIT_RC[d$C_HABIT==3]="Dans un logement independant seul ou seule"
d$C_HABIT_RC[d$C_HABIT==4]="Dans un logement independant en couple"
d$C_HABIT_RC[d$C_HABIT==5]="Dans un logement independant en colocation"
d$C_HABIT_RC[d$C_HABIT==6]="Dans une residence collective (foyer, internat, residence universitaire...)"
d$C_HABIT_RC[d$C_HABIT==7]="Dans une chambre louee chez un particulier"
d$C_HABIT_RC[d$C_HABIT==8]="Vous n'avez pas de domicile fixe"
d$C_HABIT_RC[d$C_HABIT==9]="Autre"
d$C_HABIT_RC[d$C_HABIT==98]="Ne souhaite pas repondre"


d$C_HABIT_RC <- factor(d$C_HABIT, levels=c("Chez un de vos parents ou chez vos deux parents",
                                           "Chez quelqu'un de votre entourage",
                                           "Dans un logement independant seul ou seule",
                                           "Dans un logement independant en couple",
                                           "Dans un logement independant en colocation",
                                           "Dans une residence collective (foyer, internat, residence universitaire...)",
                                           "Dans une chambre louee chez un particulier",
                                           "Vous n'avez pas de domicile fixe",
                                           "Autre",
                                           NA))

#On cree la variable C_FREQ_FAMILLE_RC

d$C_FREQ_FAMILLE_RC <- NA
d$C_FREQ_FAMILLE_RC <- d$C_FREQ_FAMILLE[d$C_FREQ_FAMILLE==1]="Je rentre tous les week-ends ou plus"
d$C_FREQ_FAMILLE_RC <- d$C_FREQ_FAMILLE[d$C_FREQ_FAMILLE==4]="Je rentre au moins une fois par mois"
d$C_FREQ_FAMILLE_RC <- d$C_FREQ_FAMILLE[d$C_FREQ_FAMILLE==5]="Je rentre a chaque vacances ou presque"
d$C_FREQ_FAMILLE_RC <- d$C_FREQ_FAMILLE[d$C_FREQ_FAMILLE==6]="Je rentre environ une ou deux fois par an"
d$C_FREQ_FAMILLE_RC <- d$C_FREQ_FAMILLE[d$C_FREQ_FAMILLE==7]="Plus rarement"
d$C_FREQ_FAMILLE_RC <- d$C_FREQ_FAMILLE[d$C_FREQ_FAMILLE==8]="Non, jamais"
d$C_FREQ_FAMILLE_RC <- d$C_FREQ_FAMILLE[d$C_FREQ_FAMILLE==98]="Ne souhaite pas repondre"

#MODIFICATION si correction du recodage
#d$C_FREQ_FAMILLE_RC <- NA
#d$C_FREQ_FAMILLE_RC <- d$C_FREQ_FAMILLE[d$C_FREQ_FAMILLE==1]="Je rentre tous les week-ends ou plus"
#d$C_FREQ_FAMILLE_RC <- d$C_FREQ_FAMILLE[d$C_FREQ_FAMILLE==2]="Je rentre au moins une fois par mois"
#d$C_FREQ_FAMILLE_RC <- d$C_FREQ_FAMILLE[d$C_FREQ_FAMILLE==3]="Je rentre a chaque vacances ou presque"
#d$C_FREQ_FAMILLE_RC <- d$C_FREQ_FAMILLE[d$C_FREQ_FAMILLE==4]="Je rentre environ une ou deux fois par an"
#d$C_FREQ_FAMILLE_RC <- d$C_FREQ_FAMILLE[d$C_FREQ_FAMILLE==5]="Plus rarement"
#d$C_FREQ_FAMILLE_RC <- d$C_FREQ_FAMILLE[d$C_FREQ_FAMILLE==6]="Non, jamais"
#d$C_FREQ_FAMILLE_RC <- d$C_FREQ_FAMILLE[d$C_FREQ_FAMILLE==98]="Ne souhaite pas repondre"

d$C_FREQ_FAMILLE_RC <- factor(d$C_FREQ_FAMILLE_RC, levels = c("Je rentre tous les week-ends ou plus",
                                                              "Je rentre au moins une fois par mois",
                                                              "Je rentre a chaque vacances ou presque",
                                                              "Je rentre environ une ou deux fois par an",
                                                              "Plus rarement",
                                                              "Non, jamais",
                                                              NA))

#On cree une variable C_FREQ_PRESETA_RC

d$C_FREQ_PRESETA_RC <- NA
d$C_FREQ_PRESETA_RC <- d$C_FREQ_PRESETA

d$C_FREQ_PRESETA_RC[d$C_FREQ_PRESETA_RC==1]="Tous les jours ouvres ou presque"
d$C_FREQ_PRESETA_RC[d$C_FREQ_PRESETA_RC==4]="Au moins une fois par semaine"
d$C_FREQ_PRESETA_RC[d$C_FREQ_PRESETA_RC==5]="Moins frequemment"
d$C_FREQ_PRESETA_RC[d$C_FREQ_PRESETA_RC==98]="Ne souhaite pas repondre"

#MODIFICATION si correction du recodage
#d$C_FREQ_PRESETA_RC <- NA
#d$C_FREQ_PRESETA_RC <- d$C_FREQ_PRESETA
#d$C_FREQ_PRESETA_RC[d$C_FREQ_PRESETA_RC==1]="Tous les jours ouvres ou presque"
#d$C_FREQ_PRESETA_RC[d$C_FREQ_PRESETA_RC==2]="Au moins une fois par semaine"
#d$C_FREQ_PRESETA_RC[d$C_FREQ_PRESETA_RC==3]="Moins frequemment"
#d$C_FREQ_PRESETA_RC[d$C_FREQ_PRESETA_RC==98]="Ne souhaite pas repondre"

d$C_FREQ_PRESETA_RC <- factor(d$C_FREQ_PRESETA_RC,levels=c("Tous les jours ouvres ou presque",
                                                           "Au moins une fois par semaine",
                                                           "Moins frequemment",
                                                           NA))

#On cree la base C_FREQ_EVETU_RC

d$C_FREQ_EVETU_RC <- NA
d$C_FREQ_EVETU_RC <- d$C_FREQ_EVETU

#MODIFICATION si correction du recodage

#d$C_FREQ_EVETU_RC[d$C_FREQ_EVETU==1]="Plusieurs fois par semaine"
#d$C_FREQ_EVETU_RC[d$C_FREQ_EVETU==2]="Environ une fois par semaine"
#d$C_FREQ_EVETU_RC[d$C_FREQ_EVETU==3]="Une a deux fois pendant le mois"
#d$C_FREQ_EVETU_RC[d$C_FREQ_EVETU==4]="Moins frequemment"
#d$C_FREQ_EVETU_RC[d$C_FREQ_EVETU==98]="Ne souhaite pas repondre"

d$C_FREQ_EVETU_RC[d$C_FREQ_EVETU==1]="Plusieurs fois par semaine"
d$C_FREQ_EVETU_RC[d$C_FREQ_EVETU==4]="Environ une fois par semaine"
d$C_FREQ_EVETU_RC[d$C_FREQ_EVETU==5]="Une a deux fois pendant le mois"
d$C_FREQ_EVETU_RC[d$C_FREQ_EVETU==6]="Moins frequemment"
d$C_FREQ_EVETU_RC[d$C_FREQ_EVETU==98]="Ne souhaite pas repondre"

d$C_FREQ_EVETU_RC <- factor(d$C_FREQ_EVETU_RC, levels=c("Plusieurs fois par semaine",
                                                        "Environ une fois par semaine",
                                                        "Une a deux fois pendant le mois",
                                                        "Moins frequemment",
                                                        NA))

#On cree la variable C_OS

d$C_OS_RC <- NA
d$C_OS_RC <- d$C_OS

#MODIFICATION si correction du recodage

#d$C_OS_RC[d$C_OS==1]="Bisexuel/bisexuelle"
#d$C_OS_RC[d$C_OS==2]="Heterosexuel/heterosexuelle"
#d$C_OS_RC[d$C_OS==3]="Gay/Lesbienne/Homosexuel/homosexuelle"
#d$C_OS_RC[d$C_OS==4]="Vous utilisez un autre terme"
#d$C_OS_RC[d$C_OS==99]="Ne sait pas"
#d$C_OS_RC[d$C_OS==98]="Refus"

d$C_OS_RC[d$C_OS==1]="Bisexuel/bisexuelle"
d$C_OS_RC[d$C_OS==4]="Heterosexuel/heterosexuelle"
d$C_OS_RC[d$C_OS==5]="Gay/Lesbienne/Homosexuel/homosexuelle"
d$C_OS_RC[d$C_OS==6]="Vous utilisez un autre terme"
d$C_OS_RC[d$C_OS==99]="Ne sait pas"
d$C_OS_RC[d$C_OS==98]="Refus"

d$C_OS_RC <- factor(d$C_OS_RC,levels=c("Bisexuel/bisexuelle",
                                       "Heterosexuel/heterosexuelle",
                                       "Gay/Lesbienne/Homosexuel/homosexuelle",
                                       "Vous utilisez un autre terme",
                                       NA))
#On cree la variable C_ACTI_UNI_RC

d$C_ACTI_UNI_ALTERSTAGE <- NA
d$C_ACTI_UNI_ALTERSTAGE <- d$C_ACTI_UNI_4
d$C_ACTI_UNI_ALTERSTAGE[d$C_ACTI_UNI_4==1]="Oui"
d$C_ACTI_UNI_ALTERSTAGE[d$C_ACTI_UNI_4==0]="Non"

d$C_ACTI_UNI_ATER<- NA
d$C_ACTI_UNI_ATER <- d$C_ACTI_UNI_5
d$C_ACTI_UNI_ATER[d$C_ACTI_UNI_5==1]="Oui"
d$C_ACTI_UNI_ATER[d$C_ACTI_UNI_5==0]="Non"

d$C_ACTI_UNI_CONTRA_VACATAIRE <- NA
d$C_ACTI_UNI_CONTRA_VACATAIRE <- d$C_ACTI_UNI_6
d$C_ACTI_UNI_CONTRA_VACATAIRE[d$C_ACTI_UNI_6==1]="Oui"
d$C_ACTI_UNI_CONTRA_VACATAIRE[d$C_ACTI_UNI_6==0]="Non"

d$C_ACTI_UNI_AUTRE_ACT <- NA
d$C_ACTI_UNI_AUTRE_ACT <- d$C_ACTI_UNI_7
d$C_ACTI_UNI_AUTRE_ACT[d$C_ACTI_UNI_7==1]="Oui"
d$C_ACTI_UNI_AUTRE_ACT[d$C_ACTI_UNI_7==0]="Non"

d$C_ACTI_UNI_NON <- NA
d$C_ACTI_UNI_NON <- d$C_ACTI_UNI_8
d$C_ACTI_UNI_NON[d$C_ACTI_UNI_8==1]="Oui"
d$C_ACTI_UNI_NON[d$C_ACTI_UNI_8==0]="Non"

d$C_ACTI_UNI_NSP <- NA
d$C_ACTI_UNI_NSP <- d$C_ACTI_UNI_99
d$C_ACTI_UNI_NSP[d$C_ACTI_UNI_99==1]="Oui"
d$C_ACTI_UNI_NSP[d$C_ACTI_UNI_99==0]="Non"

d$C_ACTI_UNI_REFUS <- NA
d$C_ACTI_UNI_REFUS <- d$C_ACTI_UNI_98
d$C_ACTI_UNI_REFUS[d$C_ACTI_UNI_98==1]="Oui"
d$C_ACTI_UNI_REFUS[d$C_ACTI_UNI_98==0]="Non"

#MODIFICATION si correction du recodage

#d$C_ACTI_UNI_ALTERSTAGE <- NA
#d$C_ACTI_UNI_ALTERSTAGE <- d$C_ACTI_UNI_1
#d$C_ACTI_UNI_ALTERSTAGE[d$C_ACTI_UNI_1==1]="Oui"
#d$C_ACTI_UNI_ALTERSTAGE[d$C_ACTI_UNI_1==0]="Non"

#d$C_ACTI_UNI_ATER<- NA
#d$C_ACTI_UNI_ATER <- d$C_ACTI_UNI_2
#d$C_ACTI_UNI_ATER[d$C_ACTI_UNI_2==1]="Oui"
#d$C_ACTI_UNI_ATER[d$C_ACTI_UNI_2==0]="Non"

#d$C_ACTI_UNI_CONTRA_VACATAIRE <- NA
#d$C_ACTI_UNI_CONTRA_VACATAIRE <- d$C_ACTI_UNI_3
#d$C_ACTI_UNI_CONTRA_VACATAIRE[d$C_ACTI_UNI_3==1]="Oui"
#d$C_ACTI_UNI_CONTRA_VACATAIRE[d$C_ACTI_UNI_3==0]="Non"

#d$C_ACTI_UNI_AUTRE_ACT <- NA
#d$C_ACTI_UNI_AUTRE_ACT <- d$C_ACTI_UNI_4
#d$C_ACTI_UNI_AUTRE_ACT[d$C_ACTI_UNI_4==1]="Oui"
#d$C_ACTI_UNI_AUTRE_ACT[d$C_ACTI_UNI_4==0]="Non"

#d$C_ACTI_UNI_NON <- NA
#d$C_ACTI_UNI_NON <- d$C_ACTI_UNI_5
#d$C_ACTI_UNI_NON[d$C_ACTI_UNI_5==1]="Oui"
#d$C_ACTI_UNI_NON[d$C_ACTI_UNI_5==0]="Non"

#d$C_ACTI_UNI_NSP <- NA
#d$C_ACTI_UNI_NSP <- d$C_ACTI_UNI_99
#d$C_ACTI_UNI_NSP[d$C_ACTI_UNI_99==1]="Oui"
#d$C_ACTI_UNI_NSP[d$C_ACTI_UNI_99==0]="Non"

#d$C_ACTI_UNI_REFUS <- NA
#d$C_ACTI_UNI_REFUS <- d$C_ACTI_UNI_98
#d$C_ACTI_UNI_REFUS[d$C_ACTI_UNI_98==1]="Oui"
#d$C_ACTI_UNI_REFUS[d$C_ACTI_UNI_98==0]="Non"


#On cree la serie de variable C_ACTI_HORS_RC

d$C_ACTI_HORS_SOINS <- NA
d$C_ACTI_HORS_SOINS <- d$C_ACTI_HORS_1
d$C_ACTI_HORS_SOINS[d$C_ACTI_HORS_1==1]="Oui"
d$C_ACTI_HORS_SOINS[d$C_ACTI_HORS_1==0]="Non"

d$C_ACTI_HORS_TRDOM_AIDE<- NA
d$C_ACTI_HORS_TRDOM_AIDE <- d$C_ACTI_HORS_4
d$C_ACTI_HORS_TRDOM_AIDE[d$C_ACTI_HORS_4==1]="Oui"
d$C_ACTI_HORS_TRDOM_AIDE[d$C_ACTI_HORS_4==0]="Non"

d$C_ACTI_HORS_ACC_ADM_EVE <- NA
d$C_ACTI_HORS_ACC_ADM_EVE <- d$C_ACTI_HORS_5
d$C_ACTI_HORS_ACC_ADM_EVE[d$C_ACTI_HORS_5==1]="Oui"
d$C_ACTI_HORS_ACC_ADM_EVE[d$C_ACTI_HORS_5==0]="Non"

d$C_ACTI_HORS_HOTELREST <- NA
d$C_ACTI_HORS_HOTELREST <- d$C_ACTI_HORS_6
d$C_ACTI_HORS_HOTELREST[d$C_ACTI_HORS_6==1]="Oui"
d$C_ACTI_HORS_HOTELREST[d$C_ACTI_HORS_6==0]="Non"

d$C_ACTI_HORS_COMMERCE_DISTRIB <- NA
d$C_ACTI_HORS_COMMERCE_DISTRIB <- d$C_ACTI_HORS_8
d$C_ACTI_HORS_COMMERCE_DISTRIB[d$C_ACTI_HORS_8==1]="Oui"
d$C_ACTI_HORS_COMMERCE_DISTRIB[d$C_ACTI_HORS_8==0]="Non"

d$C_ACTI_HORS_AUTRE <- NA
d$C_ACTI_HORS_AUTRE <- d$C_ACTI_HORS_8
d$C_ACTI_HORS_AUTRE[d$C_ACTI_HORS_8==1]="Oui"
d$C_ACTI_HORS_AUTRE[d$C_ACTI_HORS_8==0]="Non"

d$C_ACTI_HORS_NON <- NA
d$C_ACTI_HORS_NON <- d$C_ACTI_HORS_9
d$C_ACTI_HORS_NON[d$C_ACTI_HORS_9==1]="Oui"
d$C_ACTI_HORS_NON[d$C_ACTI_HORS_9==0]="Non"

d$C_ACTI_HORS_NSP <- NA
d$C_ACTI_HORS_NSP <- d$C_ACTI_HORS_99
d$C_ACTI_HORS_NSP[d$C_ACTI_HORS_99==1]="Oui"
d$C_ACTI_HORS_NSP[d$C_ACTI_HORS_99==0]="Non"

d$C_ACTI_HORS_REFUS <- NA
d$C_ACTI_HORS_REFUS <- d$C_ACTI_HORS_98
d$C_ACTI_HORS_REFUS[d$C_ACTI_HORS_98==1]="Oui"
d$C_ACTI_HORS_REFUS[d$C_ACTI_HORS_98==0]="Non"

#MODIFICATION si correction du recodage

#d$C_ACTI_HORS_SOINS <- NA
#d$C_ACTI_HORS_SOINS <- d$C_ACTI_HORS_1
#d$C_ACTI_HORS_SOINS[d$C_ACTI_HORS_1==1]="Oui"
#d$C_ACTI_HORS_SOINS[d$C_ACTI_HORS_1==0]="Non"

#d$C_ACTI_HORS_TRDOM_AIDE<- NA
#d$C_ACTI_HORS_TRDOM_AIDE <- d$C_ACTI_HORS_2
#d$C_ACTI_HORS_TRDOM_AIDE[d$C_ACTI_HORS_2==1]="Oui"
#d$C_ACTI_HORS_TRDOM_AIDE[d$C_ACTI_HORS_2==0]="Non"

#d$C_ACTI_HORS_ACC_ADM_EVE <- NA
#d$C_ACTI_HORS_ACC_ADM_EVE <- d$C_ACTI_HORS_3
#d$C_ACTI_HORS_ACC_ADM_EVE[d$C_ACTI_HORS_3==1]="Oui"
#d$C_ACTI_HORS_ACC_ADM_EVE[d$C_ACTI_HORS_3==0]="Non"

#d$C_ACTI_HORS_HOTELREST <- NA
#d$C_ACTI_HORS_HOTELREST <- d$C_ACTI_HORS_4
#d$C_ACTI_HORS_HOTELREST[d$C_ACTI_HORS_4==1]="Oui"
#d$C_ACTI_HORS_HOTELREST[d$C_ACTI_HORS_4==0]="Non"

#d$C_ACTI_HORS_COMMERCE_DISTRIB <- NA
#d$C_ACTI_HORS_COMMERCE_DISTRIB <- d$C_ACTI_HORS_5
#d$C_ACTI_HORS_COMMERCE_DISTRIB[d$C_ACTI_HORS_5==1]="Oui"
#d$C_ACTI_HORS_COMMERCE_DISTRIB[d$C_ACTI_HORS_5==0]="Non"

#d$C_ACTI_HORS_AUTRE <- NA
#d$C_ACTI_HORS_AUTRE <- d$C_ACTI_HORS_6
#d$C_ACTI_HORS_AUTRE[d$C_ACTI_HORS_6==1]="Oui"
#d$C_ACTI_HORS_AUTRE[d$C_ACTI_HORS_6==0]="Non"

#d$C_ACTI_HORS_NON <- NA
#d$C_ACTI_HORS_NON <- d$C_ACTI_HORS_7
#d$C_ACTI_HORS_NON[d$C_ACTI_HORS_7==1]="Oui"
#d$C_ACTI_HORS_NON[d$C_ACTI_HORS_7==0]="Non"


#On cree la variable C_ACTI_SEX_RC

d$C_ACTI_SEX_RC <- NA
d$C_ACTI_SEX_RC <- d$C_ACTI_SEX

d$C_ACTI_SEX_RC[d$C_ACTI_SEX==1]="Oui, de maniere reguliere"
d$C_ACTI_SEX_RC[d$C_ACTI_SEX==7]="Oui, quelques fois"
d$C_ACTI_SEX_RC[d$C_ACTI_SEX==8]="Oui, une fois"

#MODIFICATION si correction du recodage
#d$C_ACTI_SEX_RC[d$C_ACTI_SEX==1]="Oui, de maniere reguliere"
#d$C_ACTI_SEX_RC[d$C_ACTI_SEX==2]="Oui, quelques fois"
#d$C_ACTI_SEX_RC[d$C_ACTI_SEX==3]="Oui, une fois"

d$C_ACTI_SEX_RC[d$C_ACTI_SEX==4]="Non"
d$C_ACTI_SEX_RC[d$C_ACTI_SEX==99]="NSP"
d$C_ACTI_SEX_RC[d$C_ACTI_SEX==98]="Ne souhaite pas repondre"

d$C_ACTI_SEX_RC <- factor(d$C_ACTI_SEX, levels=c("Oui, de maniere reguliere",
                                                 "Oui, quelques fois",
                                                 "Oui, une fois",
                                                 "Non",
                                                 NA))

#On cree la variable C_ACTI_NCR_RC

d$C_ACTI_NCR_RC <- NA
d$C_ACTI_NCR_RC <- d$C_ACTI_NCR

d$C_ACTI_NCR_RC[d$C_ACTI_NCR_RC==1]="Tout a fait indispensables"
d$C_ACTI_NCR_RC[d$C_ACTI_NCR_RC==2]="Plutot indispensables"
d$C_ACTI_NCR_RC[d$C_ACTI_NCR_RC==3]="Plutot pas indispensables"
d$C_ACTI_NCR_RC[d$C_ACTI_NCR_RC==4]="Pas du tout indispensables"
d$C_ACTI_NCR_RC[d$C_ACTI_NCR_RC==99]="NSP"
d$C_ACTI_NCR_RC[d$C_ACTI_NCR_RC==98]="Refus"

d$C_ACTI_NCR_RC <-
  factor(d$C_ACTI_NCR_RC,levels = c(
    "Tout a fait indispensables",
    "Plutot indispensables",
    "Plutot pas indispensables",
    "Pas du tout indispensables",
    NA))

#On cree la serie de variables pour C_INDIC_INTE

#d$C_INDIC_INTE_1 pour une version recodee

d$C_INDIC_INTE_1_RC <- NA
d$C_INDIC_INTE_1_RC <- d$C_INDIC_INTE_1

d$C_INDIC_INTE_1_RC[d$C_INDIC_INTE_1==1]="Oui"
d$C_INDIC_INTE_1_RC[d$C_INDIC_INTE_1==0]="Non"
d$C_INDIC_INTE_1_RC[d$C_INDIC_INTE_1==98]="Refus"
d$C_INDIC_INTE_1_RC[d$C_INDIC_INTE_1==99]="NSP"

d$C_INDIC_INTE_1_RC <- factor(d$C_INDIC_INTE_1_RC, levels=c("Oui",
                                                            "Non",
                                                            NA))
# #d$C_INDIC_INTE_1_BIN pour la variable binomiale
# 
# d$C_INDIC_INTE_1_BIN <- NA
# d$C_INDIC_INTE_1_BIN <- d$C_INDIC_INTE_1
# d$C_INDIC_INTE_1_BIN <- as.integer(d$C_INDIC_INTE_1)
# d$C_INDIC_INTE_1_BIN[d$C_INDIC_INTE_1==98]=NA
# d$C_INDIC_INTE_1_BIN[d$C_INDIC_INTE_1==99]=NA

#d$C_INDIC_INTE_2 pour une version recodee

d$C_INDIC_INTE_2_RC <- NA
d$C_INDIC_INTE_2_RC <- d$C_INDIC_INTE_2

d$C_INDIC_INTE_2_RC[d$C_INDIC_INTE_2==1]="Oui"
d$C_INDIC_INTE_2_RC[d$C_INDIC_INTE_2==0]="Non"
d$C_INDIC_INTE_2_RC[d$C_INDIC_INTE_2==98]="Refus"
d$C_INDIC_INTE_2_RC[d$C_INDIC_INTE_2==99]="NSP"

d$C_INDIC_INTE_2_RC <- factor(d$C_INDIC_INTE_2_RC, levels=c("Oui",
                                                            "Non",
                                                            NA))
# #d$C_INDIC_INTE_2_BIN pour la variable binaire
# 
# d$C_INDIC_INTE_2_BIN <- NA
# d$C_INDIC_INTE_2_BIN <- d$C_INDIC_INTE_2
# d$C_INDIC_INTE_2_BIN <- as.integer(d$C_INDIC_INTE_2)
# d$C_INDIC_INTE_2_BIN[d$C_INDIC_INTE_2==98]=NA
# d$C_INDIC_INTE_2_BIN[d$C_INDIC_INTE_2==99]=NA

#d$C_INDIC_INTE_3 pour une version recodee

d$C_INDIC_INTE_3_RC <- NA
d$C_INDIC_INTE_3_RC <- d$C_INDIC_INTE_3

d$C_INDIC_INTE_3_RC[d$C_INDIC_INTE_3==1]="Oui"
d$C_INDIC_INTE_3_RC[d$C_INDIC_INTE_3==0]="Non"
d$C_INDIC_INTE_3_RC[d$C_INDIC_INTE_3==98]="Refus"
d$C_INDIC_INTE_3_RC[d$C_INDIC_INTE_3==99]="NSP"

d$C_INDIC_INTE_3_RC <- factor(d$C_INDIC_INTE_3_RC, levels=c("Oui",
                                                            "Non",
                                                            NA))
#d$C_INDIC_INTE_3_BIN pour la variable binaire 
# 
# d$C_INDIC_INTE_3_BIN <- NA
# d$C_INDIC_INTE_3_BIN <- d$C_INDIC_INTE_3
# d$C_INDIC_INTE_3_BIN <- as.integer(d$C_INDIC_INTE_3)
# d$C_INDIC_INTE_3_BIN[d$C_INDIC_INTE_3==98]=NA
# d$C_INDIC_INTE_3_BIN[d$C_INDIC_INTE_3==99]=NA
# 

#d$C_INDIC_INTE_4 pour une version recodee

d$C_INDIC_INTE_4_RC <- NA
d$C_INDIC_INTE_4_RC <- d$C_INDIC_INTE_4

d$C_INDIC_INTE_4_RC[d$C_INDIC_INTE_4==1]="Oui"
d$C_INDIC_INTE_4_RC[d$C_INDIC_INTE_4==0]="Non"
d$C_INDIC_INTE_4_RC[d$C_INDIC_INTE_4==98]="Refus"
d$C_INDIC_INTE_4_RC[d$C_INDIC_INTE_4==99]="NSP"

d$C_INDIC_INTE_4_RC <- factor(d$C_INDIC_INTE_4_RC, levels=c("Oui",
                                                            "Non",
                                                            NA))
# #d$C_INDIC_INTE_4_BIN pour la variable binaire
# 
# d$C_INDIC_INTE_4_BIN <- NA
# d$C_INDIC_INTE_4_BIN <- d$C_INDIC_INTE_4
# d$C_INDIC_INTE_4_BIN <- as.integer(d$C_INDIC_INTE_4)
# d$C_INDIC_INTE_4_BIN[d$C_INDIC_INTE_4==98]=NA
# d$C_INDIC_INTE_4_BIN[d$C_INDIC_INTE_4==99]=NA

#On cree la serie de variables pour C_INDIC_EGA
#d$C_INDIC_EGA_1_RC

d$C_INDIC_EGA_1_RC <- NA
d$C_INDIC_EGA_1_RC <- d$C_INDIC_EGA_1

d$C_INDIC_EGA_1_RC[d$C_INDIC_EGA_1==1]="Tout a fait vrai"
d$C_INDIC_EGA_1_RC[d$C_INDIC_EGA_1==3]="Plutot vrai"
d$C_INDIC_EGA_1_RC[d$C_INDIC_EGA_1==4]="Ni vrai, ni faux"
d$C_INDIC_EGA_1_RC[d$C_INDIC_EGA_1==5]="Plutot faux"
d$C_INDIC_EGA_1_RC[d$C_INDIC_EGA_1==7]="Tout a fait faux"
d$C_INDIC_EGA_1_RC[d$C_INDIC_EGA_1==99]="NSP"
d$C_INDIC_EGA_1_RC[d$C_INDIC_EGA_1==99]="Refus"

d$C_INDIC_EGA_1_RC <- factor(d$C_INDIC_EGA_1_RC, levels=c("Tout a fait vrai",
                                                          "Plutot vrai",
                                                          "Ni vrai, ni faux",
                                                          "Plutot faux",
                                                          "Tout a fait faux",
                                                          NA))
#d$C_INDIC_EGA_2_RC

d$C_INDIC_EGA_2_RC <- NA
d$C_INDIC_EGA_2_RC <- d$C_INDIC_EGA_2

d$C_INDIC_EGA_2_RC[d$C_INDIC_EGA_2==1]="Tout a fait vrai"
d$C_INDIC_EGA_2_RC[d$C_INDIC_EGA_2==3]="Plutot vrai"
d$C_INDIC_EGA_2_RC[d$C_INDIC_EGA_2==4]="Ni vrai, ni faux"
d$C_INDIC_EGA_2_RC[d$C_INDIC_EGA_2==5]="Plutot faux"
d$C_INDIC_EGA_2_RC[d$C_INDIC_EGA_2==7]="Tout a fait faux"
d$C_INDIC_EGA_2_RC[d$C_INDIC_EGA_2==99]="NSP"
d$C_INDIC_EGA_2_RC[d$C_INDIC_EGA_2==99]="Refus"

d$C_INDIC_EGA_2_RC <- factor(d$C_INDIC_EGA_2_RC, levels=c("Tout a fait vrai",
                                                          "Plutot vrai",
                                                          "Ni vrai, ni faux",
                                                          "Plutot faux",
                                                          "Tout a fait faux",
                                                          NA))
#d$C_INDIC_EGA_3_RC

d$C_INDIC_EGA_3_RC <- NA
d$C_INDIC_EGA_3_RC <- d$C_INDIC_EGA_3

d$C_INDIC_EGA_3_RC[d$C_INDIC_EGA_3==1]="Tout a fait vrai"
d$C_INDIC_EGA_3_RC[d$C_INDIC_EGA_3==3]="Plutot vrai"
d$C_INDIC_EGA_3_RC[d$C_INDIC_EGA_3==4]="Ni vrai, ni faux"
d$C_INDIC_EGA_3_RC[d$C_INDIC_EGA_3==5]="Plutot faux"
d$C_INDIC_EGA_3_RC[d$C_INDIC_EGA_3==7]="Tout a fait faux"
d$C_INDIC_EGA_3_RC[d$C_INDIC_EGA_3==99]="NSP"
d$C_INDIC_EGA_3_RC[d$C_INDIC_EGA_3==99]="Refus"

d$C_INDIC_EGA_3_RC <- factor(d$C_INDIC_EGA_3_RC, levels=c("Tout a fait vrai",
                                                          "Plutot vrai",
                                                          "Ni vrai, ni faux",
                                                          "Plutot faux",
                                                          "Tout a fait faux",
                                                          NA))
#d$C_INDIC_EGA_4_RC

d$C_INDIC_EGA_4_RC <- NA
d$C_INDIC_EGA_4_RC <- d$C_INDIC_EGA_4

d$C_INDIC_EGA_4_RC[d$C_INDIC_EGA_4==1]="Tout a fait vrai"
d$C_INDIC_EGA_4_RC[d$C_INDIC_EGA_4==3]="Plutot vrai"
d$C_INDIC_EGA_4_RC[d$C_INDIC_EGA_4==4]="Ni vrai, ni faux"
d$C_INDIC_EGA_4_RC[d$C_INDIC_EGA_4==5]="Plutot faux"
d$C_INDIC_EGA_4_RC[d$C_INDIC_EGA_4==7]="Tout a fait faux"
d$C_INDIC_EGA_4_RC[d$C_INDIC_EGA_4==99]="NSP"
d$C_INDIC_EGA_4_RC[d$C_INDIC_EGA_4==99]="Refus"

d$C_INDIC_EGA_4_RC <- factor(d$C_INDIC_EGA_4_RC, levels=c("Tout a fait vrai",
                                                          "Plutot vrai",
                                                          "Ni vrai, ni faux",
                                                          "Plutot faux",
                                                          "Tout a fait faux",
                                                          NA))
#d$C_INDIC_EGA_5_RC

d$C_INDIC_EGA_5_RC <- NA
d$C_INDIC_EGA_5_RC <- d$C_INDIC_EGA_5

d$C_INDIC_EGA_5_RC[d$C_INDIC_EGA_5==1]="Tout a fait vrai"
d$C_INDIC_EGA_5_RC[d$C_INDIC_EGA_5==3]="Plutot vrai"
d$C_INDIC_EGA_5_RC[d$C_INDIC_EGA_5==4]="Ni vrai, ni faux"
d$C_INDIC_EGA_5_RC[d$C_INDIC_EGA_5==5]="Plutot faux"
d$C_INDIC_EGA_5_RC[d$C_INDIC_EGA_5==7]="Tout a fait faux"
d$C_INDIC_EGA_5_RC[d$C_INDIC_EGA_5==99]="NSP"
d$C_INDIC_EGA_5_RC[d$C_INDIC_EGA_5==99]="Refus"

d$C_INDIC_EGA_5_RC <- factor(d$C_INDIC_EGA_5_RC, levels=c("Tout a fait vrai",
                                                          "Plutot vrai",
                                                          "Ni vrai, ni faux",
                                                          "Plutot faux",
                                                          "Tout a fait faux",
                                                          NA))
#d$C_INDIC_EGA_6_RC

d$C_INDIC_EGA_6_RC <- NA
d$C_INDIC_EGA_6_RC <- d$C_INDIC_EGA_6

d$C_INDIC_EGA_6_RC[d$C_INDIC_EGA_6==1]="Tout a fait vrai"
d$C_INDIC_EGA_6_RC[d$C_INDIC_EGA_6==3]="Plutot vrai"
d$C_INDIC_EGA_6_RC[d$C_INDIC_EGA_6==4]="Ni vrai, ni faux"
d$C_INDIC_EGA_6_RC[d$C_INDIC_EGA_6==5]="Plutot faux"
d$C_INDIC_EGA_6_RC[d$C_INDIC_EGA_6==7]="Tout a fait faux"
d$C_INDIC_EGA_6_RC[d$C_INDIC_EGA_6==99]="NSP"
d$C_INDIC_EGA_6_RC[d$C_INDIC_EGA_6==99]="Refus"

d$C_INDIC_EGA_6_RC <- factor(d$C_INDIC_EGA_6_RC, levels=c("Tout a fait vrai",
                                                          "Plutot vrai",
                                                          "Ni vrai, ni faux",
                                                          "Plutot faux",
                                                          "Tout a fait faux",
                                                          NA))

#On cree la serie de variables C_INDIC_BIEN

#C_INDIC_BIEN_1
#C_INDIC_BIEN_1_RC

d$C_INDIC_BIEN_1_RC <- NA
d$C_INDIC_BIEN_1_RC <- d$C_INDIC_BIEN_1

d$C_INDIC_BIEN_1_RC[d$C_INDIC_BIEN_1==1]="Tres souvent"
d$C_INDIC_BIEN_1_RC[d$C_INDIC_BIEN_1==2]="Souvent"
d$C_INDIC_BIEN_1_RC[d$C_INDIC_BIEN_1==3]="Quelques fois"
d$C_INDIC_BIEN_1_RC[d$C_INDIC_BIEN_1==4]="Jamais"
d$C_INDIC_BIEN_1_RC[d$C_INDIC_BIEN_1==99]="NSP"
d$C_INDIC_BIEN_1_RC[d$C_INDIC_BIEN_1==99]="Refus"

d$C_INDIC_BIEN_1_RC <- factor(d$C_INDIC_BIEN_1_RC, levels=c("Tres souvent",
                                                            "Souvent",
                                                            "Quelques fois",
                                                            "Jamais",
                                                            NA))

#C_INDIC_BIEN_1_CL pour la construction de l'indicateur synthetique

d$C_INDIC_BIEN_1_CL <- NA
d$C_INDIC_BIEN_1_CL <- d$C_INDIC_BIEN_1
d$C_INDIC_BIEN_1_CL <- as.integer(d$C_INDIC_BIEN_1)
d$C_INDIC_BIEN_1_CL[d$C_INDIC_BIEN_1==99]=NA
d$C_INDIC_BIEN_1_CL[d$C_INDIC_BIEN_1==99]=NA

#C_INDIC_BIEN_2
#C_INDIC_BIEN_2_RC

d$C_INDIC_BIEN_2_RC <- NA
d$C_INDIC_BIEN_2_RC <- d$C_INDIC_BIEN_2

d$C_INDIC_BIEN_2_RC[d$C_INDIC_BIEN_2==1]="Tres souvent"
d$C_INDIC_BIEN_2_RC[d$C_INDIC_BIEN_2==2]="Souvent"
d$C_INDIC_BIEN_2_RC[d$C_INDIC_BIEN_2==3]="Quelques fois"
d$C_INDIC_BIEN_2_RC[d$C_INDIC_BIEN_2==4]="Jamais"
d$C_INDIC_BIEN_2_RC[d$C_INDIC_BIEN_2==99]="NSP"
d$C_INDIC_BIEN_2_RC[d$C_INDIC_BIEN_2==99]="Refus"

d$C_INDIC_BIEN_2_RC <- factor(d$C_INDIC_BIEN_2_RC, levels=c("Tres souvent",
                                                            "Souvent",
                                                            "Quelques fois",
                                                            "Jamais",
                                                            NA))

#C_INDIC_BIEN_2_CL pour la construction de l'indicateur synthetique

# d$C_INDIC_BIEN_2_CL <- NA
# d$C_INDIC_BIEN_2_CL <- d$C_INDIC_BIEN_2
# d$C_INDIC_BIEN_2_CL <- as.integer(d$C_INDIC_BIEN_2)
# d$C_INDIC_BIEN_2_CL[d$C_INDIC_BIEN_2==98]=NA
# d$C_INDIC_BIEN_2_CL[d$C_INDIC_BIEN_2==99]=NA


#C_INDIC_BIEN_3
#C_INDIC_BIEN_3_RC

d$C_INDIC_BIEN_3_RC <- NA
d$C_INDIC_BIEN_3_RC <- d$C_INDIC_BIEN_3

d$C_INDIC_BIEN_3_RC[d$C_INDIC_BIEN_3==1]="Tres souvent"
d$C_INDIC_BIEN_3_RC[d$C_INDIC_BIEN_3==2]="Souvent"
d$C_INDIC_BIEN_3_RC[d$C_INDIC_BIEN_3==3]="Quelques fois"
d$C_INDIC_BIEN_3_RC[d$C_INDIC_BIEN_3==4]="Jamais"
d$C_INDIC_BIEN_3_RC[d$C_INDIC_BIEN_3==99]="NSP"
d$C_INDIC_BIEN_3_RC[d$C_INDIC_BIEN_3==99]="Refus"

d$C_INDIC_BIEN_3_RC <- factor(d$C_INDIC_BIEN_3_RC, levels=c("Tres souvent",
                                                            "Souvent",
                                                            "Quelques fois",
                                                            "Jamais",
                                                            NA))

#C_INDIC_BIEN_3_CL pour la construction de l'indicateur synthetique

# d$C_INDIC_BIEN_3_CL <- NA
# d$C_INDIC_BIEN_3_CL <- d$C_INDIC_BIEN_3
# d$C_INDIC_BIEN_3_CL <- as.integer(d$C_INDIC_BIEN_3)
# d$C_INDIC_BIEN_3_CL[d$C_INDIC_BIEN_3==99]=NA
# d$C_INDIC_BIEN_3_CL[d$C_INDIC_BIEN_3==99]=NA

#C_INDIC_BIEN_4
#C_INDIC_BIEN_4_RC

d$C_INDIC_BIEN_4_RC <- NA
d$C_INDIC_BIEN_4_RC <- d$C_INDIC_BIEN_4

d$C_INDIC_BIEN_4_RC[d$C_INDIC_BIEN_4==1]="Tres souvent"
d$C_INDIC_BIEN_4_RC[d$C_INDIC_BIEN_4==2]="Souvent"
d$C_INDIC_BIEN_4_RC[d$C_INDIC_BIEN_4==3]="Quelques fois"
d$C_INDIC_BIEN_4_RC[d$C_INDIC_BIEN_4==4]="Jamais"
d$C_INDIC_BIEN_4_RC[d$C_INDIC_BIEN_4==99]="NSP"
d$C_INDIC_BIEN_4_RC[d$C_INDIC_BIEN_4==99]="Refus"

d$C_INDIC_BIEN_4_RC <- factor(d$C_INDIC_BIEN_4_RC, levels=c("Tres souvent",
                                                            "Souvent",
                                                            "Quelques fois",
                                                            "Jamais",
                                                            NA))

#C_INDIC_BIEN_4_CL pour la construction de l'indicateur synthetique

# d$C_INDIC_BIEN_4_CL <- NA
# d$C_INDIC_BIEN_4_CL <- d$C_INDIC_BIEN_4
# d$C_INDIC_BIEN_4_CL <- as.integer(d$C_INDIC_BIEN_4)
# d$C_INDIC_BIEN_4_CL[d$C_INDIC_BIEN_4==99]=NA
# d$C_INDIC_BIEN_4_CL[d$C_INDIC_BIEN_4==99]=NA

#C_INDIC_BIEN_5
#C_INDIC_BIEN_5_RC

d$C_INDIC_BIEN_5_RC <- NA
d$C_INDIC_BIEN_5_RC <- d$C_INDIC_BIEN_5

d$C_INDIC_BIEN_5_RC[d$C_INDIC_BIEN_5==1]="Tres souvent"
d$C_INDIC_BIEN_5_RC[d$C_INDIC_BIEN_5==2]="Souvent"
d$C_INDIC_BIEN_5_RC[d$C_INDIC_BIEN_5==3]="Quelques fois"
d$C_INDIC_BIEN_5_RC[d$C_INDIC_BIEN_5==4]="Jamais"
d$C_INDIC_BIEN_5_RC[d$C_INDIC_BIEN_5==99]="NSP"
d$C_INDIC_BIEN_5_RC[d$C_INDIC_BIEN_5==99]="Refus"

d$C_INDIC_BIEN_5_RC <- factor(d$C_INDIC_BIEN_5_RC, levels=c("Tres souvent",
                                                            "Souvent",
                                                            "Quelques fois",
                                                            "Jamais",
                                                            NA))

#C_INDIC_BIEN_5_CL pour la construction de l'indicateur synthetique

# d$C_INDIC_BIEN_5_CL <- NA
# d$C_INDIC_BIEN_5_CL <- d$C_INDIC_BIEN_5
# d$C_INDIC_BIEN_5_CL <- as.integer(d$C_INDIC_BIEN_5)
# d$C_INDIC_BIEN_5_CL[d$C_INDIC_BIEN_5==99]=NA
# d$C_INDIC_BIEN_5_CL[d$C_INDIC_BIEN_5==99]=NA

#C_INDIC_BIEN_6
#C_INDIC_BIEN_6_RC

d$C_INDIC_BIEN_6_RC <- NA
d$C_INDIC_BIEN_6_RC <- d$C_INDIC_BIEN_6

d$C_INDIC_BIEN_6_RC[d$C_INDIC_BIEN_6==1]="Tres souvent"
d$C_INDIC_BIEN_6_RC[d$C_INDIC_BIEN_6==2]="Souvent"
d$C_INDIC_BIEN_6_RC[d$C_INDIC_BIEN_6==3]="Quelques fois"
d$C_INDIC_BIEN_6_RC[d$C_INDIC_BIEN_6==4]="Jamais"
d$C_INDIC_BIEN_6_RC[d$C_INDIC_BIEN_6==99]="NSP"
d$C_INDIC_BIEN_6_RC[d$C_INDIC_BIEN_6==99]="Refus"

d$C_INDIC_BIEN_6_RC <- factor(d$C_INDIC_BIEN_6_RC, levels=c("Tres souvent",
                                                            "Souvent",
                                                            "Quelques fois",
                                                            "Jamais",
                                                            NA))

#C_INDIC_BIEN_6_CL pour la construction de l'indicateur synthetique

# d$C_INDIC_BIEN_6_CL <- NA
# d$C_INDIC_BIEN_6_CL <- d$C_INDIC_BIEN_6
# d$C_INDIC_BIEN_6_CL <- as.integer(d$C_INDIC_BIEN_6)
# d$C_INDIC_BIEN_6_CL[d$C_INDIC_BIEN_6==99]=NA
# d$C_INDIC_BIEN_6_CL[d$C_INDIC_BIEN_6==99]=NA

#C_INDIC_BIEN_7
#C_INDIC_BIEN_7_RC

d$C_INDIC_BIEN_7_RC <- NA
d$C_INDIC_BIEN_7_RC <- d$C_INDIC_BIEN_7

d$C_INDIC_BIEN_7_RC[d$C_INDIC_BIEN_7==1]="Tres souvent"
d$C_INDIC_BIEN_7_RC[d$C_INDIC_BIEN_7==2]="Souvent"
d$C_INDIC_BIEN_7_RC[d$C_INDIC_BIEN_7==3]="Quelques fois"
d$C_INDIC_BIEN_7_RC[d$C_INDIC_BIEN_7==4]="Jamais"
d$C_INDIC_BIEN_7_RC[d$C_INDIC_BIEN_7==99]="NSP"
d$C_INDIC_BIEN_7_RC[d$C_INDIC_BIEN_7==99]="Refus"

d$C_INDIC_BIEN_7_RC <- factor(d$C_INDIC_BIEN_7_RC, levels=c("Tres souvent",
                                                            "Souvent",
                                                            "Quelques fois",
                                                            "Jamais",
                                                            NA))

#C_INDIC_BIEN_7_CL pour la construction de l'indicateur synthetique

# d$C_INDIC_BIEN_7_CL <- NA
# d$C_INDIC_BIEN_7_CL <- d$C_INDIC_BIEN_7
# d$C_INDIC_BIEN_7_CL <- as.integer(d$C_INDIC_BIEN_7)
# d$C_INDIC_BIEN_7_CL[d$C_INDIC_BIEN_7==99]=NA
# d$C_INDIC_BIEN_7_CL[d$C_INDIC_BIEN_7==99]=NA

#On cree la serie de variable P_FAITS_PSY_RC
#P_FAITS_PSY_1
#d$P_FAITS_PSY_1_RC

d$P_FAITS_PSY_1_RC <- NA
d$P_FAITS_PSY_1_RC <- d$P_FAITS_PSY_1

d$P_FAITS_PSY_1_RC[d$P_FAITS_PSY_1==1]="Plusieurs fois"
d$P_FAITS_PSY_1_RC[d$P_FAITS_PSY_1==2]="Une fois"
d$P_FAITS_PSY_1_RC[d$P_FAITS_PSY_1==3]="Jamais"
d$P_FAITS_PSY_1_RC[d$P_FAITS_PSY_1==99]="Je ne suis pas sur, pas sure"
d$P_FAITS_PSY_1_RC[d$P_FAITS_PSY_1==98]="Refus"

d$P_FAITS_PSY_1_RC<-factor(d$P_FAITS_PSY_1_RC, levels=c("Plusieurs fois",
                                                        "Une fois",
                                                        "Jamais",
                                                        "Je ne suis pas sur, pas sure",
                                                        "Refus"))
#d$P_FAITS_PSY_1_BIN

d$P_FAITS_PSY_1_BIN[d$P_FAITS_PSY_1==1 | d$P_FAITS_PSY_1==2]=1
d$P_FAITS_PSY_1_BIN[d$P_FAITS_PSY_1==3]=0
d$P_FAITS_PSY_1_BIN[d$P_FAITS_PSY_1==99 | d$P_FAITS_PSY_1==98]=NA

#P_FAITS_PSY_2
#d$P_FAITS_PSY_2_RC

d$P_FAITS_PSY_2_RC <- NA
d$P_FAITS_PSY_2_RC <- d$P_FAITS_PSY_2

d$P_FAITS_PSY_2_RC[d$P_FAITS_PSY_2==1]="Plusieurs fois"
d$P_FAITS_PSY_2_RC[d$P_FAITS_PSY_2==2]="Une fois"
d$P_FAITS_PSY_2_RC[d$P_FAITS_PSY_2==3]="Jamais"
d$P_FAITS_PSY_2_RC[d$P_FAITS_PSY_2==99]="Je ne suis pas sur, pas sure"
d$P_FAITS_PSY_2_RC[d$P_FAITS_PSY_2==98]="Refus"

d$P_FAITS_PSY_2_RC<-factor(d$P_FAITS_PSY_2_RC, levels=c("Plusieurs fois",
                                                        "Une fois",
                                                        "Jamais",
                                                        "Je ne suis pas sur, pas sure",
                                                        "Refus"))
#d$P_FAITS_PSY_2_BIN

d$P_FAITS_PSY_2_BIN[d$P_FAITS_PSY_2==1 | d$P_FAITS_PSY_2==2]=1
d$P_FAITS_PSY_2_BIN[d$P_FAITS_PSY_2==3]=0
d$P_FAITS_PSY_2_BIN[d$P_FAITS_PSY_2==99 | d$P_FAITS_PSY_2==98]=NA

#P_FAITS_PSY_3
#d$P_FAITS_PSY_3_RC

d$P_FAITS_PSY_3_RC <- NA
d$P_FAITS_PSY_3_RC <- d$P_FAITS_PSY_3

d$P_FAITS_PSY_3_RC[d$P_FAITS_PSY_3==1]="Plusieurs fois"
d$P_FAITS_PSY_3_RC[d$P_FAITS_PSY_3==2]="Une fois"
d$P_FAITS_PSY_3_RC[d$P_FAITS_PSY_3==3]="Jamais"
d$P_FAITS_PSY_3_RC[d$P_FAITS_PSY_3==99]="Je ne suis pas sur, pas sure"
d$P_FAITS_PSY_3_RC[d$P_FAITS_PSY_3==98]="Refus"

d$P_FAITS_PSY_3_RC<-factor(d$P_FAITS_PSY_3_RC, levels=c("Plusieurs fois",
                                                        "Une fois",
                                                        "Jamais",
                                                        "Je ne suis pas sur, pas sure",
                                                        "Refus"))
#d$P_FAITS_PSY_3_BIN

d$P_FAITS_PSY_3_BIN[d$P_FAITS_PSY_3==1 | d$P_FAITS_PSY_3==2]=1
d$P_FAITS_PSY_3_BIN[d$P_FAITS_PSY_3==3]=0
d$P_FAITS_PSY_3_BIN[d$P_FAITS_PSY_3==99 | d$P_FAITS_PSY_3==98]=NA

#P_FAITS_PSY_4
#d$P_FAITS_PSY_4_RC

d$P_FAITS_PSY_4_RC <- NA
d$P_FAITS_PSY_4_RC <- d$P_FAITS_PSY_4

d$P_FAITS_PSY_4_RC[d$P_FAITS_PSY_4==1]="Plusieurs fois"
d$P_FAITS_PSY_4_RC[d$P_FAITS_PSY_4==2]="Une fois"
d$P_FAITS_PSY_4_RC[d$P_FAITS_PSY_4==3]="Jamais"
d$P_FAITS_PSY_4_RC[d$P_FAITS_PSY_4==99]="Je ne suis pas sur, pas sure"
d$P_FAITS_PSY_4_RC[d$P_FAITS_PSY_4==98]="Refus"

d$P_FAITS_PSY_4_RC<-factor(d$P_FAITS_PSY_4_RC, levels=c("Plusieurs fois",
                                                        "Une fois",
                                                        "Jamais",
                                                        "Je ne suis pas sur, pas sure",
                                                        "Refus"))
#d$P_FAITS_PSY_4_BIN

d$P_FAITS_PSY_4_BIN[d$P_FAITS_PSY_4==1 | d$P_FAITS_PSY_4==2]=1
d$P_FAITS_PSY_4_BIN[d$P_FAITS_PSY_4==3]=0
d$P_FAITS_PSY_4_BIN[d$P_FAITS_PSY_4==99 | d$P_FAITS_PSY_4==98]=NA

#P_FAITS_PSY_5
#d$P_FAITS_PSY_5_RC

d$P_FAITS_PSY_5_RC <- NA
d$P_FAITS_PSY_5_RC <- d$P_FAITS_PSY_5

d$P_FAITS_PSY_5_RC[d$P_FAITS_PSY_5==1]="Plusieurs fois"
d$P_FAITS_PSY_5_RC[d$P_FAITS_PSY_5==2]="Une fois"
d$P_FAITS_PSY_5_RC[d$P_FAITS_PSY_5==3]="Jamais"
d$P_FAITS_PSY_5_RC[d$P_FAITS_PSY_5==99]="Je ne suis pas sur, pas sure"
d$P_FAITS_PSY_5_RC[d$P_FAITS_PSY_5==98]="Refus"

d$P_FAITS_PSY_5_RC<-factor(d$P_FAITS_PSY_5_RC, levels=c("Plusieurs fois",
                                                        "Une fois",
                                                        "Jamais",
                                                        "Je ne suis pas sur, pas sure",
                                                        "Refus"))
#d$P_FAITS_PSY_5_BIN

d$P_FAITS_PSY_5_BIN[d$P_FAITS_PSY_5==1 | d$P_FAITS_PSY_5==2]=1
d$P_FAITS_PSY_5_BIN[d$P_FAITS_PSY_5==3]=0
d$P_FAITS_PSY_5_BIN[d$P_FAITS_PSY_5==99 | d$P_FAITS_PSY_5==98]=NA

#On cree une variable P_FAITS_PSY_GEN, qui comptabilise le nombre de faits de violence psychologiques declares par les repondant(e)s

d$P_FAITS_PSY_GEN= rowSums(d[,c("P_FAITS_PSY_1_BIN","P_FAITS_PSY_2_BIN","P_FAITS_PSY_3_BIN","P_FAITS_PSY_4_BIN","P_FAITS_PSY_5_BIN")],na.rm=T)


#On cree une variable P_FAITS_PSY_GEN_BIN, qui indique si l'individu a declare au moins un fait de violence psychologique

d$P_FAITS_PSY_GEN_BIN[d$P_FAITS_PSY_GEN==0]=0
d$P_FAITS_PSY_GEN_BIN[d$P_FAITS_PSY_GEN>=1]=1

#P_FAITS_PHYS
#P_FAITS_PHYS_1_RC

d$P_FAITS_PHYS_1_RC <- NA
d$P_FAITS_PHYS_1_RC <- d$P_FAITS_PHYS_1

d$P_FAITS_PHYS_1_RC[d$P_FAITS_PHYS_1==1]="Plusieurs fois"
d$P_FAITS_PHYS_1_RC[d$P_FAITS_PHYS_1==2]="Une fois"
d$P_FAITS_PHYS_1_RC[d$P_FAITS_PHYS_1==3]="Jamais"
d$P_FAITS_PHYS_1_RC[d$P_FAITS_PHYS_1==99]="Je ne suis pas sur, pas sure"
d$P_FAITS_PHYS_1_RC[d$P_FAITS_PHYS_1==98]="Refus"

d$P_FAITS_PHYS_1_RC<-factor(d$P_FAITS_PHYS_1_RC, levels=c("Plusieurs fois",
                                                          "Une fois",
                                                          "Jamais",
                                                          "Je ne suis pas sur, pas sure",
                                                          "Refus"))
#d$P_FAITS_PHYS_1_BIN

d$P_FAITS_PHYS_1_BIN[d$P_FAITS_PHYS_1==1 | d$P_FAITS_PHYS_1==2]=1
d$P_FAITS_PHYS_1_BIN[d$P_FAITS_PHYS_1==3]=0
d$P_FAITS_PHYS_1_BIN[d$P_FAITS_PHYS_1==99 | d$P_FAITS_PHYS_1==98]=NA

#On cree la serie de variable P_FAITS_SEX

#On cree la serie de variable P_FAITS_SEX_RC
#P_FAITS_SEX_1
#d$P_FAITS_SEX1_RC

d$P_FAITS_SEX_1_RC <- NA
d$P_FAITS_SEX_1_RC <- d$P_FAITS_SEX_1

d$P_FAITS_SEX_1_RC[d$P_FAITS_SEX_1==1]="Plusieurs fois"
d$P_FAITS_SEX_1_RC[d$P_FAITS_SEX_1==2]="Une fois"
d$P_FAITS_SEX_1_RC[d$P_FAITS_SEX_1==3]="Jamais"
d$P_FAITS_SEX_1_RC[d$P_FAITS_SEX_1==99]="Je ne suis pas sur, pas sure"
d$P_FAITS_SEX_1_RC[d$P_FAITS_SEX_1==98]="Refus"

d$P_FAITS_SEX_1_RC<-factor(d$P_FAITS_SEX_1_RC, levels=c("Plusieurs fois",
                                                        "Une fois",
                                                        "Jamais",
                                                        "Je ne suis pas sur, pas sure",
                                                        "Refus"))
#d$P_FAITS_SEX_1_BIN

d$P_FAITS_SEX_1_BIN[d$P_FAITS_SEX_1==1 | d$P_FAITS_SEX_1==2]=1
d$P_FAITS_SEX_1_BIN[d$P_FAITS_SEX_1==3]=0
d$P_FAITS_SEX_1_BIN[d$P_FAITS_SEX_1==99 | d$P_FAITS_SEX_1==98]=NA

#P_FAITS_SEX_2
#d$P_FAITS_SEX_2_RC

d$P_FAITS_SEX_2_RC <- NA
d$P_FAITS_SEX_2_RC <- d$P_FAITS_SEX_2

d$P_FAITS_SEX_2_RC[d$P_FAITS_SEX_2==1]="Plusieurs fois"
d$P_FAITS_SEX_2_RC[d$P_FAITS_SEX_2==2]="Une fois"
d$P_FAITS_SEX_2_RC[d$P_FAITS_SEX_2==3]="Jamais"
d$P_FAITS_SEX_2_RC[d$P_FAITS_SEX_2==99]="Je ne suis pas sur, pas sure"
d$P_FAITS_SEX_2_RC[d$P_FAITS_SEX_2==98]="Refus"

d$P_FAITS_SEX_2_RC<-factor(d$P_FAITS_SEX_2_RC, levels=c("Plusieurs fois",
                                                        "Une fois",
                                                        "Jamais",
                                                        "Je ne suis pas sur, pas sure",
                                                        "Refus"))
#d$P_FAITS_SEX_2_BIN

d$P_FAITS_SEX_2_BIN[d$P_FAITS_SEX_2==1 | d$P_FAITS_SEX_2==2]=1
d$P_FAITS_SEX_2_BIN[d$P_FAITS_SEX_2==3]=0
d$P_FAITS_SEX_2_BIN[d$P_FAITS_SEX_2==99 | d$P_FAITS_SEX_2==98]=NA

#P_FAITS_SEX_3
#d$P_FAITS_SEX_3_RC

d$P_FAITS_SEX_3_RC <- NA
d$P_FAITS_SEX_3_RC <- d$P_FAITS_SEX_3

d$P_FAITS_SEX_3_RC[d$P_FAITS_SEX_3==1]="Plusieurs fois"
d$P_FAITS_SEX_3_RC[d$P_FAITS_SEX_3==2]="Une fois"
d$P_FAITS_SEX_3_RC[d$P_FAITS_SEX_3==3]="Jamais"
d$P_FAITS_SEX_3_RC[d$P_FAITS_SEX_3==99]="Je ne suis pas sur, pas sure"
d$P_FAITS_SEX_3_RC[d$P_FAITS_SEX_3==98]="Refus"

d$P_FAITS_SEX_3_RC<-factor(d$P_FAITS_SEX_3_RC, levels=c("Plusieurs fois",
                                                        "Une fois",
                                                        "Jamais",
                                                        "Je ne suis pas sur, pas sure",
                                                        "Refus"))
#d$P_FAITS_SEX_3_BIN

d$P_FAITS_SEX_3_BIN[d$P_FAITS_SEX_3==1 | d$P_FAITS_SEX_3==2]=1
d$P_FAITS_SEX_3_BIN[d$P_FAITS_SEX_3==3]=0
d$P_FAITS_SEX_3_BIN[d$P_FAITS_SEX_3==99 | d$P_FAITS_SEX_3==98]=NA

#P_FAITS_SEX_4
#d$P_FAITS_SEX_4_RC

d$P_FAITS_SEX_4_RC <- NA
d$P_FAITS_SEX_4_RC <- d$P_FAITS_SEX_4

d$P_FAITS_SEX_4_RC[d$P_FAITS_SEX_4==1]="Plusieurs fois"
d$P_FAITS_SEX_4_RC[d$P_FAITS_SEX_4==2]="Une fois"
d$P_FAITS_SEX_4_RC[d$P_FAITS_SEX_4==3]="Jamais"
d$P_FAITS_SEX_4_RC[d$P_FAITS_SEX_4==99]="Je ne suis pas sur, pas sure"
d$P_FAITS_SEX_4_RC[d$P_FAITS_SEX_4==98]="Refus"

d$P_FAITS_SEX_4_RC<-factor(d$P_FAITS_SEX_4_RC, levels=c("Plusieurs fois",
                                                        "Une fois",
                                                        "Jamais",
                                                        "Je ne suis pas sur, pas sure",
                                                        "Refus"))
#d$P_FAITS_SEX_4_BIN

d$P_FAITS_SEX_4_BIN[d$P_FAITS_SEX_4==1 | d$P_FAITS_SEX_4==2]=1
d$P_FAITS_SEX_4_BIN[d$P_FAITS_SEX_4==3]=0
d$P_FAITS_SEX_4_BIN[d$P_FAITS_SEX_4==99 | d$P_FAITS_SEX_4==98]=NA

#P_FAITS_SEX_5
#d$P_FAITS_SEX_5_RC

d$P_FAITS_SEX_5_RC <- NA
d$P_FAITS_SEX_5_RC <- d$P_FAITS_SEX_5

d$P_FAITS_SEX_5_RC[d$P_FAITS_SEX_5==1]="Plusieurs fois"
d$P_FAITS_SEX_5_RC[d$P_FAITS_SEX_5==2]="Une fois"
d$P_FAITS_SEX_5_RC[d$P_FAITS_SEX_5==3]="Jamais"
d$P_FAITS_SEX_5_RC[d$P_FAITS_SEX_5==99]="Je ne suis pas sur, pas sure"
d$P_FAITS_SEX_5_RC[d$P_FAITS_SEX_5==98]="Refus"

d$P_FAITS_SEX_5_RC<-factor(d$P_FAITS_SEX_5_RC, levels=c("Plusieurs fois",
                                                        "Une fois",
                                                        "Jamais",
                                                        "Je ne suis pas sur, pas sure",
                                                        "Refus"))
#d$P_FAITS_SEX_5_BIN

d$P_FAITS_SEX_5_BIN[d$P_FAITS_SEX_5==1 | d$P_FAITS_SEX_5==2]=1
d$P_FAITS_SEX_5_BIN[d$P_FAITS_SEX_5==3]=0
d$P_FAITS_SEX_5_BIN[d$P_FAITS_SEX_5==99 | d$P_FAITS_SEX_5==98]=NA

#P_FAITS_SEX_6
#d$P_FAITS_SEX_6_RC

d$P_FAITS_SEX_6_RC <- NA
d$P_FAITS_SEX_6_RC <- d$P_FAITS_SEX_6

d$P_FAITS_SEX_6_RC[d$P_FAITS_SEX_6==1]="Plusieurs fois"
d$P_FAITS_SEX_6_RC[d$P_FAITS_SEX_6==2]="Une fois"
d$P_FAITS_SEX_6_RC[d$P_FAITS_SEX_6==3]="Jamais"
d$P_FAITS_SEX_6_RC[d$P_FAITS_SEX_6==99]="Je ne suis pas sur, pas sure"
d$P_FAITS_SEX_6_RC[d$P_FAITS_SEX_6==98]="Refus"

d$P_FAITS_SEX_6_RC<-factor(d$P_FAITS_SEX_6_RC, levels=c("Plusieurs fois",
                                                        "Une fois",
                                                        "Jamais",
                                                        "Je ne suis pas sur, pas sure",
                                                        "Refus"))
#d$P_FAITS_SEX_6_BIN

d$P_FAITS_SEX_6_BIN[d$P_FAITS_SEX_6==1 | d$P_FAITS_SEX_6==2]=1
d$P_FAITS_SEX_6_BIN[d$P_FAITS_SEX_6==3]=0
d$P_FAITS_SEX_6_BIN[d$P_FAITS_SEX_6==99 | d$P_FAITS_SEX_6==98]=NA

#P_FAITS_SEX_7
#d$P_FAITS_SEX_7_RC

d$P_FAITS_SEX_7_RC <- NA
d$P_FAITS_SEX_7_RC <- d$P_FAITS_SEX_7

d$P_FAITS_SEX_7_RC[d$P_FAITS_SEX_7==1]="Plusieurs fois"
d$P_FAITS_SEX_7_RC[d$P_FAITS_SEX_7==2]="Une fois"
d$P_FAITS_SEX_7_RC[d$P_FAITS_SEX_7==3]="Jamais"
d$P_FAITS_SEX_7_RC[d$P_FAITS_SEX_7==99]="Je ne suis pas sur, pas sure"
d$P_FAITS_SEX_7_RC[d$P_FAITS_SEX_7==98]="Refus"

d$P_FAITS_SEX_7_RC<-factor(d$P_FAITS_SEX_7_RC, levels=c("Plusieurs fois",
                                                        "Une fois",
                                                        "Jamais",
                                                        "Je ne suis pas sur, pas sure",
                                                        "Refus"))
#d$P_FAITS_SEX_7_BIN

d$P_FAITS_SEX_7_BIN[d$P_FAITS_SEX_7==1 | d$P_FAITS_SEX_7==2]=1
d$P_FAITS_SEX_7_BIN[d$P_FAITS_SEX_7==3]=0
d$P_FAITS_SEX_7_BIN[d$P_FAITS_SEX_7==99 | d$P_FAITS_SEX_7==98]=NA

#P_FAITS_SEX_8
#d$P_FAITS_SEX_8_RC

d$P_FAITS_SEX_8_RC <- NA
d$P_FAITS_SEX_8_RC <- d$P_FAITS_SEX_8

d$P_FAITS_SEX_8_RC[d$P_FAITS_SEX_8==1]="Plusieurs fois"
d$P_FAITS_SEX_8_RC[d$P_FAITS_SEX_8==2]="Une fois"
d$P_FAITS_SEX_8_RC[d$P_FAITS_SEX_8==3]="Jamais"
d$P_FAITS_SEX_8_RC[d$P_FAITS_SEX_8==99]="Je ne suis pas sur, pas sure"
d$P_FAITS_SEX_8_RC[d$P_FAITS_SEX_8==98]="Refus"

d$P_FAITS_SEX_8_RC<-factor(d$P_FAITS_SEX_8_RC, levels=c("Plusieurs fois",
                                                        "Une fois",
                                                        "Jamais",
                                                        "Je ne suis pas sur, pas sure",
                                                        "Refus"))
#d$P_FAITS_SEX_8_BIN

d$P_FAITS_SEX_8_BIN[d$P_FAITS_SEX_8==1 | d$P_FAITS_SEX_8==2]=1
d$P_FAITS_SEX_8_BIN[d$P_FAITS_SEX_8==3]=0
d$P_FAITS_SEX_8_BIN[d$P_FAITS_SEX_8==99 | d$P_FAITS_SEX_8==98]=NA

#On cree une variable P_FAITS_SEX_GEN, qui comptabilise le nombre de faits de violence sexuelle declares par les repondant(e)s

d$P_FAITS_SEX_GEN= rowSums(d[,c("P_FAITS_SEX_1_BIN","P_FAITS_SEX_2_BIN","P_FAITS_SEX_3_BIN","P_FAITS_SEX_4_BIN","P_FAITS_SEX_5_BIN","P_FAITS_SEX_6_BIN","P_FAITS_SEX_7_BIN","P_FAITS_SEX_8_BIN")],na.rm=T)

#On cree une variable P_FAITS_SEX_GEN_BIN, qui indique si l'individu a declare au moins un fait de violence psychologique

d$P_FAITS_SEX_GEN_BIN[d$P_FAITS_SEX_GEN==0]=0
d$P_FAITS_SEX_GEN_BIN[d$P_FAITS_SEX_GEN>=1]=1

#On cree la serie de variables V_PSY_DOUZE

#V_PSY_DOUZE_1
#V_PSY_DOUZE_1_RC pour recodage en toutes lettres
d$V_PSY_DOUZE_1_RC[d$V_PSY_DOUZE_1==1]="Oui"
d$V_PSY_DOUZE_1_RC[d$V_PSY_DOUZE_1==0]="Non"
d$V_PSY_DOUZE_1_RC[d$V_PSY_DOUZE_1==""]=NA
d$V_PSY_DOUZE_1_RC[is.na(d$V_PSY_DOUZE_1)]=NA

#V_PSY_DOUZE_1_BIN
# d$V_PSY_DOUZE_1_BIN[d$V_PSY_DOUZE_1==1]=1
# d$V_PSY_DOUZE_1_BIN[d$V_PSY_DOUZE_1==0]=0

#V_PSY_DOUZE_2
#V_PSY_DOUZE_2_RC pour recodage en toutes lettres
d$V_PSY_DOUZE_2_RC[d$V_PSY_DOUZE_2==1]="Oui"
d$V_PSY_DOUZE_2_RC[d$V_PSY_DOUZE_2==0]="Non"
d$V_PSY_DOUZE_2_RC[d$V_PSY_DOUZE_2==""]=NA
d$V_PSY_DOUZE_2_RC[is.na(d$V_PSY_DOUZE_2)]=NA

# #V_PSY_DOUZE_2_BIN
# d$V_PSY_DOUZE_2_BIN[d$V_PSY_DOUZE_2==1]=1
# d$V_PSY_DOUZE_2_BIN[d$V_PSY_DOUZE_2==0]=0

#V_PSY_DOUZE_3
#V_PSY_DOUZE_3_RC pour recodage en toutes lettres
d$V_PSY_DOUZE_3_RC[d$V_PSY_DOUZE_3==1]="Oui"
d$V_PSY_DOUZE_3_RC[d$V_PSY_DOUZE_3==0]="Non"
d$V_PSY_DOUZE_3_RC[d$V_PSY_DOUZE_3==""]=NA
d$V_PSY_DOUZE_3_RC[is.na(d$V_PSY_DOUZE_3)]=NA

# #V_PSY_DOUZE_3_BIN
# d$V_PSY_DOUZE_3_BIN[d$V_PSY_DOUZE_3==1]=1
# d$V_PSY_DOUZE_3_BIN[d$V_PSY_DOUZE_3==0]=0

#V_PSY_DOUZE_4
#V_PSY_DOUZE_4_RC pour recodage en toutes lettres
d$V_PSY_DOUZE_4_RC[d$V_PSY_DOUZE_4==1]="Oui"
d$V_PSY_DOUZE_4_RC[d$V_PSY_DOUZE_4==0]="Non"
d$V_PSY_DOUZE_4_RC[d$V_PSY_DOUZE_4==""]=NA
d$V_PSY_DOUZE_4_RC[is.na(d$V_PSY_DOUZE_4)]=NA

# #V_PSY_DOUZE_4_BIN
# d$V_PSY_DOUZE_4_BIN[d$V_PSY_DOUZE_4==1]=1
# d$V_PSY_DOUZE_4_BIN[d$V_PSY_DOUZE_4==0]=0

#V_PSY_DOUZE_5
#V_PSY_DOUZE_5_RC pour recodage en toutes lettres
d$V_PSY_DOUZE_5_RC[d$V_PSY_DOUZE_5==1]="Oui"
d$V_PSY_DOUZE_5_RC[d$V_PSY_DOUZE_5==0]="Non"
d$V_PSY_DOUZE_5_RC[d$V_PSY_DOUZE_5==""]=NA
d$V_PSY_DOUZE_5_RC[is.na(d$V_PSY_DOUZE_5)]=NA

# #V_PSY_DOUZE_5_BIN
# d$V_PSY_DOUZE_5_BIN[d$V_PSY_DOUZE_5==1]=1
# d$V_PSY_DOUZE_5_BIN[d$V_PSY_DOUZE_5==0]=0

#V_PSY_DOUZE_6
#V_PSY_DOUZE_6_RC pour recodage en toutes lettres
d$V_PSY_DOUZE_6_RC[d$V_PSY_DOUZE_6==1]="Oui"
d$V_PSY_DOUZE_6_RC[d$V_PSY_DOUZE_6==0]="Non"
d$V_PSY_DOUZE_6_RC[d$V_PSY_DOUZE_6==""]=NA
d$V_PSY_DOUZE_6_RC[is.na(d$V_PSY_DOUZE_6)]=NA

# #V_PSY_DOUZE_6_BIN
# d$V_PSY_DOUZE_6_BIN[d$V_PSY_DOUZE_6==1]=1
# d$V_PSY_DOUZE_6_BIN[d$V_PSY_DOUZE_6==0]=0

#V_PSY_DOUZE_98
#V_PSY_DOUZE_98_RC pour recodage en toutes lettres
d$V_PSY_DOUZE_98_RC[d$V_PSY_DOUZE_98==1]="Oui"
d$V_PSY_DOUZE_98_RC[d$V_PSY_DOUZE_98==0]="Non"
d$V_PSY_DOUZE_98_RC[d$V_PSY_DOUZE_98==""]=NA
d$V_PSY_DOUZE_98_RC[is.na(d$V_PSY_DOUZE_98)]=NA

# #V_PSY_DOUZE_98_BIN
# d$V_PSY_DOUZE_98_BIN[d$V_PSY_DOUZE_98==1]=1
# d$V_PSY_DOUZE_98_BIN[d$V_PSY_DOUZE_98==0]=0

#On cree une variable V_PSY_DOUZE_GEN qui synthetise le nombre de faits declares ayant eu lieu les 12 derniers mois

d$V_PSY_DOUZE_GEN <- rowSums(d[,c("V_PSY_DOUZE_1_BIN","V_PSY_DOUZE_2_BIN","V_PSY_DOUZE_3_BIN","V_PSY_DOUZE_4_BIN","V_PSY_DOUZE_5_BIN")], na.rm=T)
d$V_PSY_DOUZE_GEN[d$P_FAITS_PSY_GEN_BIN==0]=NA #A verifier

#On cree une variable V_PSY_DOUZE_GEN_BIN qui indique si l'individu a declare au moins un fait de violence psychologique au cours des 12 derniers mois

d$V_PSY_DOUZE_GEN_BIN[d$V_PSY_DOUZE_GEN==0]=0
d$V_PSY_DOUZE_GEN_BIN[is.na(d$V_PSY_DOUZE_GEN)]=NA
d$V_PSY_DOUZE_GEN_BIN[d$V_PSY_DOUZE_GEN>=1]=1

#On cree une variable V_PSY_MARQ

d$V_PSY_MARQ_RC <- NA
d$V_PSY_MARQ_RC <- d$V_PSY_MARQ

d$V_PSY_MARQ_RC[d$V_PSY_MARQ==1]="Les moqueries, les propos degradants ou humiliants" 
d$V_PSY_MARQ_RC[d$V_PSY_MARQ==2]="Le fait que quelqu'un a porte atteinte a votre reputation ou a tente de le faire, en repandant des rumeurs par exemple."
d$V_PSY_MARQ_RC[d$V_PSY_MARQ==3]="Le fait que quelqu'un a porte atteinte a votre image ou menace de le faire (diffusion de videos ou photos intimes ou prise a votre insu, montage photo...)."
d$V_PSY_MARQ_RC[d$V_PSY_MARQ==4]="Le fait que quelqu'un vous a tenu volontairement a l'ecart des activites etudiantes, collectives ou festives."
d$V_PSY_MARQ_RC[d$V_PSY_MARQ==5]="Le fait que quelqu'un se soit approprie voter travail, l'a fait disparaitre ; ou vous ait force a faire votre travail a sa place"
d$V_PSY_MARQ_RC[d$V_PSY_MARQ==98]="Refus"

d$V_PSY_MARQ_RC <- factor(d$V_PSY_MARQ_RC, levels=c("Les moqueries, les propos degradants ou humiliants",
                                                    "Le fait que quelqu'un a porte atteinte a votre reputation ou a tente de le faire, en repandant des rumeurs par exemple",
                                                    "Le fait que quelqu'un a porte atteinte a votre image ou menace de le faire (diffusion de videos ou photos intimes ou prise a votre insu, montage photo...)",
                                                    "Le fait que quelqu'un vous a tenu volontairement a l'ecart des activites etudiantes, collectives ou festives.",
                                                    "Le fait que quelqu'un se soit approprie voter travail, l'a fait disparaitre ; ou vous ait force a faire votre travail a sa place",
                                                    NA))

#On cree la serie de variables

#V_PSY_ETAB_1
#V_PSY_ETAB_1_RC
d$V_PSY_ETAB_1_RC <- NA
d$V_PSY_ETAB_1_RC <- d$V_PSY_ETAB_1
d$V_PSY_ETAB_1_RC[d$V_PSY_ETAB_1==1]="Vous etudiiez deja dans votre etablissement actuel"
d$V_PSY_ETAB_1_RC[d$V_PSY_ETAB_1==0]="Autre situation"

d$V_PSY_ETAB_1_RC <- factor(d$V_PSY_ETAB_1_RC,levels=c("Vous etudiiez deja dans votre etablissement actuel",
                                                       "Autre situation"))

# #V_PSY_ETAB_1_BIN
# d$V_PSY_ETAB_1_BIN <- NA
# d$V_PSY_ETAB_1_BIN <- d$V_PSY_ETAB_1
# d$V_PSY_ETAB_1_BIN[d$V_PSY_ETAB_1==1]=1
# d$V_PSY_ETAB_1_BIN[d$V_PSY_ETAB_1==0]=0

#V_PSY_ETAB_2
#V_PSY_ETAB_2_RC
d$V_PSY_ETAB_2_RC <- NA
d$V_PSY_ETAB_2_RC <- d$V_PSY_ETAB_2
d$V_PSY_ETAB_2_RC[d$V_PSY_ETAB_2==1]="vous etudiiez dans un autre etablissement dans le cadre d'un echange"
d$V_PSY_ETAB_2_RC[d$V_PSY_ETAB_2==0]="Autre situation"

d$V_PSY_ETAB_2_RC <- factor(d$V_PSY_ETAB_2_RC,levels=c("vous etudiiez dans un autre etablissement dans le cadre d'un echange",
                                                       "Autre situation"))
# #V_PSY_ETAB_2_BIN
# d$V_PSY_ETAB_2_BIN <- NA
# d$V_PSY_ETAB_2_BIN <- d$V_PSY_ETAB_2
# d$V_PSY_ETAB_2_BIN[d$V_PSY_ETAB_2==1]=1
# d$V_PSY_ETAB_2_BIN[d$V_PSY_ETAB_2==0]=0

#V_PSY_ETAB_3
#V_PSY_ETAB_3_RC
d$V_PSY_ETAB_3_RC <- NA
d$V_PSY_ETAB_3_RC <- d$V_PSY_ETAB_3
d$V_PSY_ETAB_3_RC[d$V_PSY_ETAB_3==1]="vous etudiiez dans un autre etablissement hors echange (etablissement precedent)"
d$V_PSY_ETAB_3_RC[d$V_PSY_ETAB_3==0]="Autre situation"

d$V_PSY_ETAB_3_RC <- factor(d$V_PSY_ETAB_3_RC,levels=c("vous etudiiez dans un autre etablissement hors echange (etablissement precedent)",
                                                       "Autre situation"))
# #V_PSY_ETAB_3_BIN
# d$V_PSY_ETAB_3_BIN <- NA
# d$V_PSY_ETAB_3_BIN <- d$V_PSY_ETAB_3
# d$V_PSY_ETAB_3_BIN[d$V_PSY_ETAB_3==1]=1
# d$V_PSY_ETAB_3_BIN[d$V_PSY_ETAB_3==0]=0

#V_PSY_ETAB_4
#V_PSY_ETAB_4_RC
d$V_PSY_ETAB_4_RC <- NA
d$V_PSY_ETAB_4_RC <- d$V_PSY_ETAB_4
d$V_PSY_ETAB_4_RC[d$V_PSY_ETAB_4==1]="Ne souhaite pas repondre"
d$V_PSY_ETAB_4_RC[d$V_PSY_ETAB_4==0]="Autre situation"

d$V_PSY_ETAB_4_RC <- factor(d$V_PSY_ETAB_3_RC,levels=c("Ne souhaite pas repondre",
                                                       "Autre situation"))  
# #V_PSY_ETAB_4_BIN
# d$V_PSY_ETAB_4_BIN <- NA
# d$V_PSY_ETAB_4_BIN <- d$V_PSY_ETAB_4
# d$V_PSY_ETAB_4_BIN[d$V_PSY_ETAB_4==1]=1
# d$V_PSY_ETAB_4_BIN[d$V_PSY_ETAB_4==0]=0

#MODIFICATION si correction du recodage
##V_PSY_ETAB_98
##V_PSY_ETAB_98_RC
#d$V_PSY_ETAB_98_RC <- NA
#d$V_PSY_ETAB_98_RC <- d$V_PSY_ETAB_98
#d$V_PSY_ETAB_98_RC[d$V_PSY_ETAB_98==1]="Ne souhaite pas repondre"
#d$V_PSY_ETAB_98_RC[d$V_PSY_ETAB_98==0]="Autre situation"
##V_PSY_ETAB_98_BIN
#d$V_PSY_ETAB_98_BIN <- NA
#d$V_PSY_ETAB_98_BIN <- d$V_PSY_ETAB_98
#d$V_PSY_ETAB_98_BIN[d$V_PSY_ETAB_98==1]=1
#d$V_PSY_ETAB_98_BIN[d$V_PSY_ETAB_98==0]=0


#On cree la serie de variables V_PSY_LIEU
#V_PSY_LIEU_1
#V_PSY_LIEU_1_RC
d$V_PSY_LIEU_1_RC <- NA
d$V_PSY_LIEU_1_RC <- d$V_PSY_LIEU_1

d$V_PSY_LIEU_1_RC[d$V_PSY_LIEU_1==1]="En salle de cours, amphi"
d$V_PSY_LIEU_1_RC[d$V_PSY_LIEU_1==0]="Autre situation"

d$V_PSY_LIEU_1_RC <- factor(d$V_PSY_LIEU_1_RC, factor("En salle de cours, amphi",
                                                      "Autre situation"))
# #V_PSY_LIEU_1_BIN
# d$V_PSY_LIEU_1_BIN <- NA
# d$V_PSY_LIEU_1_BIN <- d$V_PSY_LIEU_1
# 
# d$V_PSY_LIEU_1_BIN[d$V_PSY_LIEU_1==1]=1
# d$V_PSY_LIEU_1_BIN[d$V_PSY_LIEU_1==0]=0

#V_PSY_LIEU_2
#V_PSY_LIEU_2_RC
d$V_PSY_LIEU_2_RC <- NA
d$V_PSY_LIEU_2_RC <- d$V_PSY_LIEU_2

d$V_PSY_LIEU_2_RC[d$V_PSY_LIEU_2==1]="Sur un terrain ou equipement sportif"
d$V_PSY_LIEU_2_RC[d$V_PSY_LIEU_2==0]="Autre situation"

d$V_PSY_LIEU_2_RC <- factor(d$V_PSY_LIEU_2_RC, factor("Sur un terrain ou equipement sportif",
                                                      "Autre situation"))
# #V_PSY_LIEU_2_BIN
# d$V_PSY_LIEU_2_BIN <- NA
# d$V_PSY_LIEU_2_BIN <- d$V_PSY_LIEU_2
# 
# d$V_PSY_LIEU_2_BIN[d$V_PSY_LIEU_2==1]=1
# d$V_PSY_LIEU_2_BIN[d$V_PSY_LIEU_2==0]=0

#V_PSY_LIEU_3
#V_PSY_LIEU_3_RC
d$V_PSY_LIEU_3_RC <- NA
d$V_PSY_LIEU_3_RC <- d$V_PSY_LIEU_3

d$V_PSY_LIEU_3_RC[d$V_PSY_LIEU_3==1]="Dans un bureau (enseignant, administration)"
d$V_PSY_LIEU_3_RC[d$V_PSY_LIEU_3==0]="Autre situation"

d$V_PSY_LIEU_3_RC <- factor(d$V_PSY_LIEU_3_RC, factor("Dans un bureau (enseignant, administration)",
                                                      "Autre situation"))
# #V_PSY_LIEU_3_BIN
# d$V_PSY_LIEU_3_BIN <- NA
# d$V_PSY_LIEU_3_BIN <- d$V_PSY_LIEU_3
# 
# d$V_PSY_LIEU_3_BIN[d$V_PSY_LIEU_3==1]=1
# d$V_PSY_LIEU_3_BIN[d$V_PSY_LIEU_3==0]=0

#V_PSY_LIEU_4
#V_PSY_LIEU_4_RC
d$V_PSY_LIEU_4_RC <- NA
d$V_PSY_LIEU_4_RC <- d$V_PSY_LIEU_4

d$V_PSY_LIEU_4_RC[d$V_PSY_LIEU_4==1]="Dans un autre espace collectif de la fac/ecole (cafeteria, salles communes, jardinsâ€¦)"
d$V_PSY_LIEU_4_RC[d$V_PSY_LIEU_4==0]="Autre situation"

d$V_PSY_LIEU_4_RC <- factor(d$V_PSY_LIEU_4_RC, factor("Dans un autre espace collectif de la fac/ecole (cafeteria, salles communes, jardinsâ€¦)",
                                                      "Autre situation"))
# #V_PSY_LIEU_4_BIN
# d$V_PSY_LIEU_4_BIN <- NA
# d$V_PSY_LIEU_4_BIN <- d$V_PSY_LIEU_4
# 
# d$V_PSY_LIEU_4_BIN[d$V_PSY_LIEU_4==1]=1
# d$V_PSY_LIEU_4_BIN[d$V_PSY_LIEU_4==0]=0

#V_PSY_LIEU_5
#V_PSY_LIEU_5_RC
d$V_PSY_LIEU_5_RC <- NA
d$V_PSY_LIEU_5_RC <- d$V_PSY_LIEU_5

d$V_PSY_LIEU_5_RC[d$V_PSY_LIEU_5==1]="Sur votre lieu de travail (alternance/job etudiant)"
d$V_PSY_LIEU_5_RC[d$V_PSY_LIEU_5==0]="Autre situation"

d$V_PSY_LIEU_5_RC <- factor(d$V_PSY_LIEU_5_RC, factor("Sur votre lieu de travail (alternance/job etudiant)",
                                                      "Autre situation"))
# #V_PSY_LIEU_5_BIN
# d$V_PSY_LIEU_5_BIN <- NA
# d$V_PSY_LIEU_5_BIN <- d$V_PSY_LIEU_5
# 
# d$V_PSY_LIEU_5_BIN[d$V_PSY_LIEU_5==1]=1
# d$V_PSY_LIEU_5_BIN[d$V_PSY_LIEU_5==0]=0

#V_PSY_LIEU_6
#V_PSY_LIEU_6_RC
d$V_PSY_LIEU_6_RC <- NA
d$V_PSY_LIEU_6_RC <- d$V_PSY_LIEU_6

d$V_PSY_LIEU_6_RC[d$V_PSY_LIEU_6==1]="Sur votre lieu de stage"
d$V_PSY_LIEU_6_RC[d$V_PSY_LIEU_6==0]="Autre situation"

d$V_PSY_LIEU_6_RC <- factor(d$V_PSY_LIEU_6_RC, factor("Sur votre lieu de stage",
                                                      "Autre situation"))
# #V_PSY_LIEU_6_BIN
# d$V_PSY_LIEU_6_BIN <- NA
# d$V_PSY_LIEU_6_BIN <- d$V_PSY_LIEU_6
# 
# d$V_PSY_LIEU_6_BIN[d$V_PSY_LIEU_6==1]=1
# d$V_PSY_LIEU_6_BIN[d$V_PSY_LIEU_6==0]=0

#V_PSY_LIEU_7
#V_PSY_LIEU_7_RC
d$V_PSY_LIEU_7_RC <- NA
d$V_PSY_LIEU_7_RC <- d$V_PSY_LIEU_7

d$V_PSY_LIEU_7_RC[d$V_PSY_LIEU_7==1]="Sur votre lieu de stage"
d$V_PSY_LIEU_7_RC[d$V_PSY_LIEU_7==0]="Autre situation"

d$V_PSY_LIEU_7_RC <- factor(d$V_PSY_LIEU_7_RC, factor("Sur votre lieu de stage",
                                                      "Autre situation"))
# #V_PSY_LIEU_7_BIN
# d$V_PSY_LIEU_7_BIN <- NA
# d$V_PSY_LIEU_7_BIN <- d$V_PSY_LIEU_7
# 
# d$V_PSY_LIEU_7_BIN[d$V_PSY_LIEU_7==1]=1
# d$V_PSY_LIEU_7_BIN[d$V_PSY_LIEU_7==0]=0

#V_PSY_LIEU_8
#V_PSY_LIEU_8_RC
d$V_PSY_LIEU_8_RC <- NA
d$V_PSY_LIEU_8_RC <- d$V_PSY_LIEU_8

d$V_PSY_LIEU_8_RC[d$V_PSY_LIEU_8==1]="A votre domicile ou voiture"
d$V_PSY_LIEU_8_RC[d$V_PSY_LIEU_8==0]="Autre situation"

d$V_PSY_LIEU_8_RC <- factor(d$V_PSY_LIEU_8_RC, factor("A votre domicile ou voiture",
                                                      "Autre situation"))
# #V_PSY_LIEU_8_BIN
# d$V_PSY_LIEU_8_BIN <- NA
# d$V_PSY_LIEU_8_BIN <- d$V_PSY_LIEU_8
# 
# d$V_PSY_LIEU_8_BIN[d$V_PSY_LIEU_8==1]=1
# d$V_PSY_LIEU_8_BIN[d$V_PSY_LIEU_8==0]=0

#V_PSY_LIEU_9
#V_PSY_LIEU_9_RC
d$V_PSY_LIEU_9_RC <- NA
d$V_PSY_LIEU_9_RC <- d$V_PSY_LIEU_9

d$V_PSY_LIEU_9_RC[d$V_PSY_LIEU_9==1]="Au domicile ou dans la voiture de la/les personne(s) ayant commis les faits"
d$V_PSY_LIEU_9_RC[d$V_PSY_LIEU_9==0]="Autre situation"

d$V_PSY_LIEU_9_RC <- factor(d$V_PSY_LIEU_9_RC, factor("Au domicile ou dans la voiture de la/les personne(s) ayant commis les faits",
                                                      "Autre situation"))
# #V_PSY_LIEU_9_BIN
# d$V_PSY_LIEU_9_BIN <- NA
# d$V_PSY_LIEU_9_BIN <- d$V_PSY_LIEU_9
# 
# d$V_PSY_LIEU_9_BIN[d$V_PSY_LIEU_9==1]=1
# d$V_PSY_LIEU_9_BIN[d$V_PSY_LIEU_9==0]=0

#V_PSY_LIEU_10
#V_PSY_LIEU_10_RC
d$V_PSY_LIEU_10_RC <- NA
d$V_PSY_LIEU_10_RC <- d$V_PSY_LIEU_10

d$V_PSY_LIEU_10_RC[d$V_PSY_LIEU_10==1]="Dans le cadre d'une soiree entre etudiants, etudiantes ou d'un evenement organise dans un cadre prive"
d$V_PSY_LIEU_10_RC[d$V_PSY_LIEU_10==0]="Autre situation"

d$V_PSY_LIEU_10_RC <- factor(d$V_PSY_LIEU_10_RC, factor("Dans le cadre d'une soiree entre etudiants, etudiantes ou d'un evenement organise dans un cadre prive",
                                                        "Autre situation"))
# #V_PSY_LIEU_10_BIN
# d$V_PSY_LIEU_10_BIN <- NA
# d$V_PSY_LIEU_10_BIN <- d$V_PSY_LIEU_10
# 
# d$V_PSY_LIEU_10_BIN[d$V_PSY_LIEU_10==1]=1
# d$V_PSY_LIEU_10_BIN[d$V_PSY_LIEU_10==0]=0

#V_PSY_LIEU_11
#V_PSY_LIEU_11_RC
d$V_PSY_LIEU_11_RC <- NA
d$V_PSY_LIEU_11_RC <- d$V_PSY_LIEU_11

d$V_PSY_LIEU_11_RC[d$V_PSY_LIEU_11==1]="Dans le cadre d'un week-end/voyage entre etudiants, etudiantes (hors de tout cadre associatif ou universitaire)"
d$V_PSY_LIEU_11_RC[d$V_PSY_LIEU_11==0]="Autre situation"

d$V_PSY_LIEU_11_RC <- factor(d$V_PSY_LIEU_11_RC, factor("Dans le cadre d'un week-end/voyage entre etudiants, etudiantes (hors de tout cadre associatif ou universitaire)",
                                                        "Autre situation"))
# #V_PSY_LIEU_11_BIN
# d$V_PSY_LIEU_11_BIN <- NA
# d$V_PSY_LIEU_11_BIN <- d$V_PSY_LIEU_11
# 
# d$V_PSY_LIEU_11_BIN[d$V_PSY_LIEU_11==1]=1
# d$V_PSY_LIEU_11_BIN[d$V_PSY_LIEU_11==0]=0

#V_PSY_LIEU_12
#V_PSY_LIEU_12_RC
d$V_PSY_LIEU_12_RC <- NA
d$V_PSY_LIEU_12_RC <- d$V_PSY_LIEU_12

d$V_PSY_LIEU_12_RC[d$V_PSY_LIEU_12==1]="Dans le cadre d'un week-end/voyage scolaire organise par l'administration"
d$V_PSY_LIEU_12_RC[d$V_PSY_LIEU_12==0]="Autre situation"

d$V_PSY_LIEU_12_RC <- factor(d$V_PSY_LIEU_12_RC, factor("Dans le cadre d'un week-end/voyage scolaire organise par l'administration",
                                                        "Autre situation"))
# #V_PSY_LIEU_12_BIN
# d$V_PSY_LIEU_12_BIN <- NA
# d$V_PSY_LIEU_12_BIN <- d$V_PSY_LIEU_12
# 
# d$V_PSY_LIEU_12_BIN[d$V_PSY_LIEU_12==1]=1
# d$V_PSY_LIEU_12_BIN[d$V_PSY_LIEU_12==0]=0

#V_PSY_LIEU_13
#V_PSY_LIEU_13_RC
d$V_PSY_LIEU_13_RC <- NA
d$V_PSY_LIEU_13_RC <- d$V_PSY_LIEU_13

d$V_PSY_LIEU_13_RC[d$V_PSY_LIEU_13==1]="Dans le cadre d'une soiree ou d'un evenement organise par une association etudiante"
d$V_PSY_LIEU_13_RC[d$V_PSY_LIEU_13==0]="Autre situation"

d$V_PSY_LIEU_13_RC <- factor(d$V_PSY_LIEU_13_RC, factor("Dans le cadre d'une soiree ou d'un evenement organise par une association etudiante",
                                                        "Autre situation"))
# #V_PSY_LIEU_13_BIN
# d$V_PSY_LIEU_13_BIN <- NA
# d$V_PSY_LIEU_13_BIN <- d$V_PSY_LIEU_13
# 
# d$V_PSY_LIEU_13_BIN[d$V_PSY_LIEU_13==1]=1
# d$V_PSY_LIEU_13_BIN[d$V_PSY_LIEU_13==0]=0

#V_PSY_LIEU_14
#V_PSY_LIEU_14_RC
d$V_PSY_LIEU_14_RC <- NA
d$V_PSY_LIEU_14_RC <- d$V_PSY_LIEU_14

d$V_PSY_LIEU_14_RC[d$V_PSY_LIEU_14==1]="Dans le cadre d'une soiree ou d'un evenement organise par l'administration de votre ecole/universite/etablissement"
d$V_PSY_LIEU_14_RC[d$V_PSY_LIEU_14==0]="Autre situation"

d$V_PSY_LIEU_14_RC <- factor(d$V_PSY_LIEU_14_RC, factor("Dans le cadre d'une soiree ou d'un evenement organise par l'administration de votre ecole/universite/etablissement",
                                                        "Autre situation"))
# #V_PSY_LIEU_14_BIN
# d$V_PSY_LIEU_14_BIN <- NA
# d$V_PSY_LIEU_14_BIN <- d$V_PSY_LIEU_14
# 
# d$V_PSY_LIEU_14_BIN[d$V_PSY_LIEU_14==1]=1
# d$V_PSY_LIEU_14_BIN[d$V_PSY_LIEU_14==0]=0

#V_PSY_LIEU_15
#V_PSY_LIEU_15_RC
d$V_PSY_LIEU_15_RC <- NA
d$V_PSY_LIEU_15_RC <- d$V_PSY_LIEU_15

d$V_PSY_LIEU_15_RC[d$V_PSY_LIEU_15==1]="Dans le cadre d'un week-end/voyage organise par une association etudiante"
d$V_PSY_LIEU_15_RC[d$V_PSY_LIEU_15==0]="Autre situation"

d$V_PSY_LIEU_15_RC <- factor(d$V_PSY_LIEU_15_RC, factor("Dans le cadre d'un week-end/voyage organise par une association etudiante",
                                                        "Autre situation"))
# #V_PSY_LIEU_15_BIN
# d$V_PSY_LIEU_15_BIN <- NA
# d$V_PSY_LIEU_15_BIN <- d$V_PSY_LIEU_15
# 
# d$V_PSY_LIEU_15_BIN[d$V_PSY_LIEU_15==1]=1
# d$V_PSY_LIEU_15_BIN[d$V_PSY_LIEU_15==0]=0

#V_PSY_LIEU_16
#V_PSY_LIEU_16_RC
d$V_PSY_LIEU_16_RC <- NA
d$V_PSY_LIEU_16_RC <- d$V_PSY_LIEU_16

d$V_PSY_LIEU_16_RC[d$V_PSY_LIEU_16==1]="Dans le cadre d'une soiree ou d'un evenement organise par une autre institution (universite, entreprise, etc.)"
d$V_PSY_LIEU_16_RC[d$V_PSY_LIEU_16==0]="Autre situation"

d$V_PSY_LIEU_16_RC <- factor(d$V_PSY_LIEU_16_RC, factor("Dans le cadre d'une soiree ou d'un evenement organise par une autre institution (universite, entreprise, etc.)",
                                                        "Autre situation"))
# #V_PSY_LIEU_16_BIN
# d$V_PSY_LIEU_16_BIN <- NA
# d$V_PSY_LIEU_16_BIN <- d$V_PSY_LIEU_16
# 
# d$V_PSY_LIEU_16_BIN[d$V_PSY_LIEU_16==1]=1
# d$V_PSY_LIEU_16_BIN[d$V_PSY_LIEU_16==0]=0

#V_PSY_LIEU_17
#V_PSY_LIEU_17_RC
d$V_PSY_LIEU_17_RC <- NA
d$V_PSY_LIEU_17_RC <- d$V_PSY_LIEU_17

d$V_PSY_LIEU_17_RC[d$V_PSY_LIEU_17==1]="Lors d'un evenement sportif ou une rencontre universitaire"
d$V_PSY_LIEU_17_RC[d$V_PSY_LIEU_17==0]="Autre situation"

d$V_PSY_LIEU_17_RC <- factor(d$V_PSY_LIEU_17_RC, factor("Lors d'un evenement sportif ou une rencontre universitaire",
                                                        "Autre situation"))
# #V_PSY_LIEU_17_BIN
# d$V_PSY_LIEU_17_BIN <- NA
# d$V_PSY_LIEU_17_BIN <- d$V_PSY_LIEU_17
# 
# d$V_PSY_LIEU_17_BIN[d$V_PSY_LIEU_17==1]=1
# d$V_PSY_LIEU_17_BIN[d$V_PSY_LIEU_17==0]=0

#V_PSY_LIEU_18
#V_PSY_LIEU_18_RC
d$V_PSY_LIEU_18_RC <- NA
d$V_PSY_LIEU_18_RC <- d$V_PSY_LIEU_18

d$V_PSY_LIEU_18_RC[d$V_PSY_LIEU_18==1]="Dans un espace public en dehors des lieux d'etudes"
d$V_PSY_LIEU_18_RC[d$V_PSY_LIEU_18==0]="Autre situation"

d$V_PSY_LIEU_18_RC <- factor(d$V_PSY_LIEU_18_RC, factor("Dans un espace public en dehors des lieux d'etudes",
                                                        "Autre situation"))
# #V_PSY_LIEU_18_BIN
# d$V_PSY_LIEU_18_BIN <- NA
# d$V_PSY_LIEU_18_BIN <- d$V_PSY_LIEU_18
# 
# d$V_PSY_LIEU_18_BIN[d$V_PSY_LIEU_18==1]=1
# d$V_PSY_LIEU_18_BIN[d$V_PSY_LIEU_18==0]=0

#V_PSY_LIEU_19
#V_PSY_LIEU_19_RC
d$V_PSY_LIEU_19_RC <- NA
d$V_PSY_LIEU_19_RC <- d$V_PSY_LIEU_19

d$V_PSY_LIEU_19_RC[d$V_PSY_LIEU_19==1]="Au telephone ou en ligne"
d$V_PSY_LIEU_19_RC[d$V_PSY_LIEU_19==0]="Autre situation"

d$V_PSY_LIEU_19_RC <- factor(d$V_PSY_LIEU_19_RC, factor("Au telephone ou en ligne",
                                                        "Autre situation"))
# #V_PSY_LIEU_19_BIN
# d$V_PSY_LIEU_19_BIN <- NA
# d$V_PSY_LIEU_19_BIN <- d$V_PSY_LIEU_19
# 
# d$V_PSY_LIEU_19_BIN[d$V_PSY_LIEU_19==1]=1
# d$V_PSY_LIEU_19_BIN[d$V_PSY_LIEU_19==0]=0

#V_PSY_LIEU_20
#V_PSY_LIEU_20_RC
d$V_PSY_LIEU_20_RC <- NA
d$V_PSY_LIEU_20_RC <- d$V_PSY_LIEU_20

d$V_PSY_LIEU_20_RC[d$V_PSY_LIEU_20==1]="Autre"
d$V_PSY_LIEU_20_RC[d$V_PSY_LIEU_20==0]="Autre situation"

d$V_PSY_LIEU_20_RC <- factor(d$V_PSY_LIEU_20_RC, factor("Autre",
                                                        "Autre situation"))
# #V_PSY_LIEU_20_BIN
# d$V_PSY_LIEU_20_BIN <- NA
# d$V_PSY_LIEU_20_BIN <- d$V_PSY_LIEU_20
# 
# d$V_PSY_LIEU_20_BIN[d$V_PSY_LIEU_20==1]=1
# d$V_PSY_LIEU_20_BIN[d$V_PSY_LIEU_20==0]=0

#V_PSY_LIEU_99
#V_PSY_LIEU_99_RC
d$V_PSY_LIEU_99_RC <- NA
d$V_PSY_LIEU_99_RC <- d$V_PSY_LIEU_99

d$V_PSY_LIEU_99_RC[d$V_PSY_LIEU_99==1]="NSP"
d$V_PSY_LIEU_99_RC[d$V_PSY_LIEU_99==0]="Autre situation"

d$V_PSY_LIEU_99_RC <- factor(d$V_PSY_LIEU_99_RC, factor("NSP",
                                                        "Autre situation"))
# #V_PSY_LIEU_99_BIN
# d$V_PSY_LIEU_99_BIN <- NA
# d$V_PSY_LIEU_99_BIN <- d$V_PSY_LIEU_99
# 
# d$V_PSY_LIEU_99_BIN[d$V_PSY_LIEU_99==1]=1
# d$V_PSY_LIEU_99_BIN[d$V_PSY_LIEU_99==0]=0

#V_PSY_LIEU_98
#V_PSY_LIEU_98_RC
d$V_PSY_LIEU_98_RC <- NA
d$V_PSY_LIEU_98_RC <- d$V_PSY_LIEU_98

d$V_PSY_LIEU_98_RC[d$V_PSY_LIEU_98==1]="Refus"
d$V_PSY_LIEU_98_RC[d$V_PSY_LIEU_98==0]="Autre situation"

d$V_PSY_LIEU_98_RC <- factor(d$V_PSY_LIEU_98_RC, factor("Refus",
                                                        "Autre situation"))
# #V_PSY_LIEU_98_BIN
# d$V_PSY_LIEU_98_BIN <- NA
# d$V_PSY_LIEU_98_BIN <- d$V_PSY_LIEU_98
# 
# d$V_PSY_LIEU_98_BIN[d$V_PSY_LIEU_98==1]=1
# d$V_PSY_LIEU_98_BIN[d$V_PSY_LIEU_98==0]=0



#On cree la variable V_PSY_LIEU_NB

d$V_PSY_LIEU_NB <-  rowSums(d[,c("V_PSY_LIEU_1",
                                 "V_PSY_LIEU_2",
                                 "V_PSY_LIEU_3",
                                 "V_PSY_LIEU_4",
                                 "V_PSY_LIEU_5",
                                 "V_PSY_LIEU_6",
                                 "V_PSY_LIEU_7",
                                 "V_PSY_LIEU_8",
                                 "V_PSY_LIEU_9",
                                 "V_PSY_LIEU_10",
                                 "V_PSY_LIEU_11",
                                 "V_PSY_LIEU_12",
                                 "V_PSY_LIEU_13",
                                 "V_PSY_LIEU_14",
                                 "V_PSY_LIEU_15",
                                 "V_PSY_LIEU_16",
                                 "V_PSY_LIEU_17",
                                 "V_PSY_LIEU_18",
                                 "V_PSY_LIEU_19",
                                 "V_PSY_LIEU_20")], na.rm=T)

#On cree la serie de variables V_PSY_INTERNET
#V_PSY_INTERNET_1
#V_PSY_INTERNET_1_RC  
d$V_PSY_INTERNET_1_RC <- NA
d$V_PSY_INTERNET_1_RC <- d$V_PSY_INTERNET_1

d$V_PSY_INTERNET_1_RC[d$V_PSY_INTERNET_1==1]="En message ou dans une conversation privee"
d$V_PSY_INTERNET_1_RC[d$V_PSY_INTERNET_1==0]="Autres reponses"

d$V_PSY_INTERNET_1_RC <- factor(d$V_PSY_INTERNET_1_RC,levels=c("En message ou dans une conversation privee",
                                                               "Autres reponses"))
# #V_PSY_INTERNET_1_BIN
# 
# d$V_PSY_INTERNET_1_RC <- NA
# d$V_PSY_INTERNET_1_RC <- d$V_PSY_INTERNET_1
# 
# d$V_PSY_INTERNET_1_RC[d$V_PSY_INTERNET_1==1]=1
# d$V_PSY_INTERNET_1_RC[d$V_PSY_INTERNET_1==0]=0

#V_PSY_INTERNET_2
#V_PSY_INTERNET_2_RC  
d$V_PSY_INTERNET_2_RC <- NA
d$V_PSY_INTERNET_2_RC <- d$V_PSY_INTERNET_2

d$V_PSY_INTERNET_2_RC[d$V_PSY_INTERNET_2==1]="Dans une discussion de groupe privee"
d$V_PSY_INTERNET_2_RC[d$V_PSY_INTERNET_2==0]="Autres reponses"

d$V_PSY_INTERNET_2_RC <- factor(d$V_PSY_INTERNET_2_RC,levels=c("Dans une discussion de groupe privee",
                                                               "Autres reponses"))
# #V_PSY_INTERNET_2_BIN
# 
# d$V_PSY_INTERNET_2_RC <- NA
# d$V_PSY_INTERNET_2_RC <- d$V_PSY_INTERNET_2
# 
# d$V_PSY_INTERNET_2_RC[d$V_PSY_INTERNET_2==1]=1
# d$V_PSY_INTERNET_2_RC[d$V_PSY_INTERNET_2==0]=0

#V_PSY_INTERNET_3
#V_PSY_INTERNET_3_RC  
d$V_PSY_INTERNET_3_RC <- NA
d$V_PSY_INTERNET_3_RC <- d$V_PSY_INTERNET_3

d$V_PSY_INTERNET_3_RC[d$V_PSY_INTERNET_3==1]="Sur une plateforme ou groupe public"
d$V_PSY_INTERNET_3_RC[d$V_PSY_INTERNET_3==0]="Autres reponses"

d$V_PSY_INTERNET_3_RC <- factor(d$V_PSY_INTERNET_3_RC,levels=c("Sur une plateforme ou groupe public",
                                                               "Autres reponses"))
# #V_PSY_INTERNET_3_BIN
# 
# d$V_PSY_INTERNET_3_RC <- NA
# d$V_PSY_INTERNET_3_RC <- d$V_PSY_INTERNET_3
# 
# d$V_PSY_INTERNET_3_RC[d$V_PSY_INTERNET_3==1]=1
# d$V_PSY_INTERNET_3_RC[d$V_PSY_INTERNET_3==0]=0

#V_PSY_INTERNET_99
#V_PSY_INTERNET_99_RC  
d$V_PSY_INTERNET_99_RC <- NA
d$V_PSY_INTERNET_99_RC <- d$V_PSY_INTERNET_99

d$V_PSY_INTERNET_99_RC[d$V_PSY_INTERNET_99==1]="NSP"
d$V_PSY_INTERNET_99_RC[d$V_PSY_INTERNET_99==0]="Autres reponses"

d$V_PSY_INTERNET_99_RC <- factor(d$V_PSY_INTERNET_99_RC,levels=c("NSP",
                                                                 "Autres reponses"))
# #V_PSY_INTERNET_99_BIN
# 
# d$V_PSY_INTERNET_99_RC <- NA
# d$V_PSY_INTERNET_99_RC <- d$V_PSY_INTERNET_99
# 
# d$V_PSY_INTERNET_99_RC[d$V_PSY_INTERNET_99==1]=1
# d$V_PSY_INTERNET_99_RC[d$V_PSY_INTERNET_99==0]=0

#V_PSY_INTERNET_98
#V_PSY_INTERNET_98_RC  
d$V_PSY_INTERNET_98_RC <- NA
d$V_PSY_INTERNET_98_RC <- d$V_PSY_INTERNET_98

d$V_PSY_INTERNET_98_RC[d$V_PSY_INTERNET_98==1]="Refus"
d$V_PSY_INTERNET_98_RC[d$V_PSY_INTERNET_98==0]="Autres reponses"

d$V_PSY_INTERNET_98_RC <- factor(d$V_PSY_INTERNET_98_RC,levels=c("Refus",
                                                                 "Autres reponses"))
# #V_PSY_INTERNET_98_BIN
# 
# d$V_PSY_INTERNET_98_RC <- NA
# d$V_PSY_INTERNET_98_RC <- d$V_PSY_INTERNET_98
# 
# d$V_PSY_INTERNET_98_RC[d$V_PSY_INTERNET_98==1]=1
# d$V_PSY_INTERNET_98_RC[d$V_PSY_INTERNET_98==0]=0


#On cree la variable V_PSY_INTERNET_NB

d$V_PSY_INTERNET_GEN <-  rowSums(d[,c("V_PSY_INTERNET_1",
                                     "V_PSY_INTERNET_2",
                                     "V_PSY_INTERNET_3")], na.rm=T)

#On cree la serie de variables V_PSY_AUTEUR_GENRE
#V_PSY_AUTEUR_GENRE_1
#V_PSY_AUTEUR_GENRE_1_RC
d$V_PSY_AUTEUR_GENRE_1_RC <- NA
d$V_PSY_AUTEUR_GENRE_1_RC <- d$V_PSY_AUTEUR_GENRE_1

d$V_PSY_AUTEUR_GENRE_1_RC[d$V_PSY_AUTEUR_GENRE_1==1]="Une femme (seule)"
d$V_PSY_AUTEUR_GENRE_1_RC[d$V_PSY_AUTEUR_GENRE_1==0]="Autres reponses"

# #V_PSY_AUTEUR_GENRE_1_BIN
# d$V_PSY_AUTEUR_GENRE_1_BIN <- NA
# d$V_PSY_AUTEUR_GENRE_1_BIN <- d$V_PSY_AUTEUR_GENRE_1
# 
# d$V_PSY_AUTEUR_GENRE_1_BIN[d$V_PSY_AUTEUR_GENRE_1==1]=1
# d$V_PSY_AUTEUR_GENRE_1_BIN[d$V_PSY_AUTEUR_GENRE_1==0]=0

#V_PSY_AUTEUR_GENRE_2
#V_PSY_AUTEUR_GENRE_2_RC
d$V_PSY_AUTEUR_GENRE_2_RC <- NA
d$V_PSY_AUTEUR_GENRE_2_RC <- d$V_PSY_AUTEUR_GENRE_2

d$V_PSY_AUTEUR_GENRE_2_RC[d$V_PSY_AUTEUR_GENRE_2==1]="Un homme (seul)"
d$V_PSY_AUTEUR_GENRE_2_RC[d$V_PSY_AUTEUR_GENRE_2==0]="Autres reponses"

# #V_PSY_AUTEUR_GENRE_2_BIN
# d$V_PSY_AUTEUR_GENRE_2_BIN <- NA
# d$V_PSY_AUTEUR_GENRE_2_BIN <- d$V_PSY_AUTEUR_GENRE_2
# 
# d$V_PSY_AUTEUR_GENRE_2_BIN[d$V_PSY_AUTEUR_GENRE_2==1]=1
# d$V_PSY_AUTEUR_GENRE_2_BIN[d$V_PSY_AUTEUR_GENRE_2==0]=0  

#V_PSY_AUTEUR_GENRE_3
#V_PSY_AUTEUR_GENRE_3_RC
d$V_PSY_AUTEUR_GENRE_3_RC <- NA
d$V_PSY_AUTEUR_GENRE_3_RC <- d$V_PSY_AUTEUR_GENRE_3

d$V_PSY_AUTEUR_GENRE_3_RC[d$V_PSY_AUTEUR_GENRE_3==1]="Plusieurs femmes (en groupe)"
d$V_PSY_AUTEUR_GENRE_3_RC[d$V_PSY_AUTEUR_GENRE_3==0]="Autres reponses"

# #V_PSY_AUTEUR_GENRE_3_BIN
# d$V_PSY_AUTEUR_GENRE_3_BIN <- NA
# d$V_PSY_AUTEUR_GENRE_3_BIN <- d$V_PSY_AUTEUR_GENRE_3
# 
# d$V_PSY_AUTEUR_GENRE_3_BIN[d$V_PSY_AUTEUR_GENRE_3==1]=1
# d$V_PSY_AUTEUR_GENRE_3_BIN[d$V_PSY_AUTEUR_GENRE_3==0]=0

#V_PSY_AUTEUR_GENRE_4
#V_PSY_AUTEUR_GENRE_4_RC
d$V_PSY_AUTEUR_GENRE_4_RC <- NA
d$V_PSY_AUTEUR_GENRE_4_RC <- d$V_PSY_AUTEUR_GENRE_4

d$V_PSY_AUTEUR_GENRE_4_RC[d$V_PSY_AUTEUR_GENRE_4==1]="Plusieurs hommes (en groupe)"
d$V_PSY_AUTEUR_GENRE_4_RC[d$V_PSY_AUTEUR_GENRE_4==0]="Autres reponses"

# #V_PSY_AUTEUR_GENRE_4_BIN
# d$V_PSY_AUTEUR_GENRE_4_BIN <- NA
# d$V_PSY_AUTEUR_GENRE_4_BIN <- d$V_PSY_AUTEUR_GENRE_4
# 
# d$V_PSY_AUTEUR_GENRE_4_BIN[d$V_PSY_AUTEUR_GENRE_4==1]=1
# d$V_PSY_AUTEUR_GENRE_4_BIN[d$V_PSY_AUTEUR_GENRE_4==0]=0   

#V_PSY_AUTEUR_GENRE_5
#V_PSY_AUTEUR_GENRE_5_RC
d$V_PSY_AUTEUR_GENRE_5_RC <- NA
d$V_PSY_AUTEUR_GENRE_5_RC <- d$V_PSY_AUTEUR_GENRE_5

d$V_PSY_AUTEUR_GENRE_5_RC[d$V_PSY_AUTEUR_GENRE_5==1]="Plusieurs hommes et femmes (en groupe)"
d$V_PSY_AUTEUR_GENRE_5_RC[d$V_PSY_AUTEUR_GENRE_5==0]="Autres reponses"

# #V_PSY_AUTEUR_GENRE_5_BIN
# d$V_PSY_AUTEUR_GENRE_5_BIN <- NA
# d$V_PSY_AUTEUR_GENRE_5_BIN <- d$V_PSY_AUTEUR_GENRE_5
# 
# d$V_PSY_AUTEUR_GENRE_5_BIN[d$V_PSY_AUTEUR_GENRE_5==1]=1
# d$V_PSY_AUTEUR_GENRE_5_BIN[d$V_PSY_AUTEUR_GENRE_5==0]=0

#V_PSY_AUTEUR_GENRE_6
#V_PSY_AUTEUR_GENRE_6_RC
d$V_PSY_AUTEUR_GENRE_6_RC <- NA
d$V_PSY_AUTEUR_GENRE_6_RC <- d$V_PSY_AUTEUR_GENRE_6

d$V_PSY_AUTEUR_GENRE_6_RC[d$V_PSY_AUTEUR_GENRE_6==1]="Autre"
d$V_PSY_AUTEUR_GENRE_6_RC[d$V_PSY_AUTEUR_GENRE_6==0]="Autres reponses"

# #V_PSY_AUTEUR_GENRE_6_BIN
# d$V_PSY_AUTEUR_GENRE_6_BIN <- NA
# d$V_PSY_AUTEUR_GENRE_6_BIN <- d$V_PSY_AUTEUR_GENRE_6
# 
# d$V_PSY_AUTEUR_GENRE_6_BIN[d$V_PSY_AUTEUR_GENRE_6==1]=1
# d$V_PSY_AUTEUR_GENRE_6_BIN[d$V_PSY_AUTEUR_GENRE_6==0]=0

#V_PSY_AUTEUR_GENRE_99
#V_PSY_AUTEUR_GENRE_99_RC
d$V_PSY_AUTEUR_GENRE_99_RC <- NA
d$V_PSY_AUTEUR_GENRE_99_RC <- d$V_PSY_AUTEUR_GENRE_99

d$V_PSY_AUTEUR_GENRE_99_RC[d$V_PSY_AUTEUR_GENRE_99==1]="NSP"
d$V_PSY_AUTEUR_GENRE_99_RC[d$V_PSY_AUTEUR_GENRE_99==0]="Autres reponses"

# #V_PSY_AUTEUR_GENRE_99_BIN
# d$V_PSY_AUTEUR_GENRE_99_BIN <- NA
# d$V_PSY_AUTEUR_GENRE_99_BIN <- d$V_PSY_AUTEUR_GENRE_99
# 
# d$V_PSY_AUTEUR_GENRE_99_BIN[d$V_PSY_AUTEUR_GENRE_99==1]=1
# d$V_PSY_AUTEUR_GENRE_99_BIN[d$V_PSY_AUTEUR_GENRE_99==0]=0

#V_PSY_AUTEUR_GENRE_98
#V_PSY_AUTEUR_GENRE_98_RC
d$V_PSY_AUTEUR_GENRE_98_RC <- NA
d$V_PSY_AUTEUR_GENRE_98_RC <- d$V_PSY_AUTEUR_GENRE_98

d$V_PSY_AUTEUR_GENRE_98_RC[d$V_PSY_AUTEUR_GENRE_98==1]="Refus"
d$V_PSY_AUTEUR_GENRE_98_RC[d$V_PSY_AUTEUR_GENRE_98==0]="Autres reponses"

# #V_PSY_AUTEUR_GENRE_98_BIN
# d$V_PSY_AUTEUR_GENRE_98_BIN <- NA
# d$V_PSY_AUTEUR_GENRE_98_BIN <- d$V_PSY_AUTEUR_GENRE_98
# 
# d$V_PSY_AUTEUR_GENRE_98_BIN[d$V_PSY_AUTEUR_GENRE_98==1]=1
# d$V_PSY_AUTEUR_GENRE_98_BIN[d$V_PSY_AUTEUR_GENRE_98==0]=0

#On cree la variable V_PSY_AUTEUR_GENRE_NB

d$V_PSY_AUTEUR_GENRE_NB <-  rowSums(d[,c("V_PSY_AUTEUR_GENRE_1",
                                         "V_PSY_AUTEUR_GENRE_2",
                                         "V_PSY_AUTEUR_GENRE_3",
                                         "V_PSY_AUTEUR_GENRE_4",
                                         "V_PSY_AUTEUR_GENRE_5",
                                         "V_PSY_AUTEUR_GENRE_6")], na.rm=T)

#On cree la serie de variables V_PSY_AUTEUR_STATUT
#V_PSY_AUTEUR_STATUT_1
#V_PSY_AUTEUR_STATUT_1_RC

d$V_PSY_AUTEUR_STATUT_1_RC <- NA
d$V_PSY_AUTEUR_STATUT_1_RC <- d$V_PSY_AUTEUR_STATUT_1

d$V_PSY_AUTEUR_STATUT_1_RC[d$V_PSY_AUTEUR_STATUT_1==1]="Etudiant ou etudiante"
d$V_PSY_AUTEUR_STATUT_1_RC[d$V_PSY_AUTEUR_STATUT_1==0]="Autres reponses"

d$V_PSY_AUTEUR_STATUT_1_RC <- factor(d$V_PSY_AUTEUR_STATUT_1, levels=c("Etudiant ou etudiante",
                                                                       "Autres reponses"))
# #V_PSY_AUTEUR_STATUT_1_BIN
# 
# d$V_PSY_AUTEUR_STATUT_1_BIN <- NA
# d$V_PSY_AUTEUR_STATUT_1_BIN <- d$V_PSY_AUTEUR_STATUT_1
# 
# d$V_PSY_AUTEUR_STATUT_1_RC[d$V_PSY_AUTEUR_STATUT_1==1]=1
# d$V_PSY_AUTEUR_STATUT_1_RC[d$V_PSY_AUTEUR_STATUT_1==0]=0 

#V_PSY_AUTEUR_STATUT_2
#V_PSY_AUTEUR_STATUT_2_RC

d$V_PSY_AUTEUR_STATUT_2_RC <- NA
d$V_PSY_AUTEUR_STATUT_2_RC <- d$V_PSY_AUTEUR_STATUT_2

d$V_PSY_AUTEUR_STATUT_2_RC[d$V_PSY_AUTEUR_STATUT_2==1]="Enseignant ou enseignante que vous avez eu en cours (amphi, TD, seminaire, etc.)"
d$V_PSY_AUTEUR_STATUT_2_RC[d$V_PSY_AUTEUR_STATUT_2==0]="Autres reponses"

d$V_PSY_AUTEUR_STATUT_2_RC <- factor(d$V_PSY_AUTEUR_STATUT_2, levels=c("Enseignant ou enseignante que vous avez eu en cours (amphi, TD, seminaire, etc.)",
                                                                       "Autres reponses"))
# #V_PSY_AUTEUR_STATUT_2_BIN
# 
# d$V_PSY_AUTEUR_STATUT_2_BIN <- NA
# d$V_PSY_AUTEUR_STATUT_2_BIN <- d$V_PSY_AUTEUR_STATUT_2
# 
# d$V_PSY_AUTEUR_STATUT_2_RC[d$V_PSY_AUTEUR_STATUT_2==1]=1
# d$V_PSY_AUTEUR_STATUT_2_RC[d$V_PSY_AUTEUR_STATUT_2==0]=0  

#V_PSY_AUTEUR_STATUT_3
#V_PSY_AUTEUR_STATUT_3_RC

d$V_PSY_AUTEUR_STATUT_3_RC <- NA
d$V_PSY_AUTEUR_STATUT_3_RC <- d$V_PSY_AUTEUR_STATUT_3

d$V_PSY_AUTEUR_STATUT_3_RC[d$V_PSY_AUTEUR_STATUT_3==1]="Autre enseignant ou enseignante"
d$V_PSY_AUTEUR_STATUT_3_RC[d$V_PSY_AUTEUR_STATUT_3==0]="Autres reponses"

d$V_PSY_AUTEUR_STATUT_3_RC <- factor(d$V_PSY_AUTEUR_STATUT_3, levels=c("Autre enseignant ou enseignante",
                                                                       "Autres reponses"))
# #V_PSY_AUTEUR_STATUT_3_BIN
# 
# d$V_PSY_AUTEUR_STATUT_3_BIN <- NA
# d$V_PSY_AUTEUR_STATUT_3_BIN <- d$V_PSY_AUTEUR_STATUT_3
# 
# d$V_PSY_AUTEUR_STATUT_3_RC[d$V_PSY_AUTEUR_STATUT_3==1]=1
# d$V_PSY_AUTEUR_STATUT_3_RC[d$V_PSY_AUTEUR_STATUT_3==0]=0

#V_PSY_AUTEUR_STATUT_4
#V_PSY_AUTEUR_STATUT_4_RC

d$V_PSY_AUTEUR_STATUT_4_RC <- NA
d$V_PSY_AUTEUR_STATUT_4_RC <- d$V_PSY_AUTEUR_STATUT_4

d$V_PSY_AUTEUR_STATUT_4_RC[d$V_PSY_AUTEUR_STATUT_4==1]="Directeur ou directrice de memoire ou de these"
d$V_PSY_AUTEUR_STATUT_4_RC[d$V_PSY_AUTEUR_STATUT_4==0]="Autres reponses"

d$V_PSY_AUTEUR_STATUT_4_RC <- factor(d$V_PSY_AUTEUR_STATUT_4, levels=c("Directeur ou directrice de memoire ou de these",
                                                                       "Autres reponses"))
# #V_PSY_AUTEUR_STATUT_4_BIN
# 
# d$V_PSY_AUTEUR_STATUT_4_BIN <- NA
# d$V_PSY_AUTEUR_STATUT_4_BIN <- d$V_PSY_AUTEUR_STATUT_4
# 
# d$V_PSY_AUTEUR_STATUT_4_RC[d$V_PSY_AUTEUR_STATUT_4==1]=1
# d$V_PSY_AUTEUR_STATUT_4_RC[d$V_PSY_AUTEUR_STATUT_4==0]=0

#V_PSY_AUTEUR_STATUT_5
#V_PSY_AUTEUR_STATUT_5_RC

d$V_PSY_AUTEUR_STATUT_5_RC <- NA
d$V_PSY_AUTEUR_STATUT_5_RC <- d$V_PSY_AUTEUR_STATUT_5

d$V_PSY_AUTEUR_STATUT_5_RC[d$V_PSY_AUTEUR_STATUT_5==1]="Personnel administratif technique/d'entretien"
d$V_PSY_AUTEUR_STATUT_5_RC[d$V_PSY_AUTEUR_STATUT_5==0]="Autres reponses"

d$V_PSY_AUTEUR_STATUT_5_RC <- factor(d$V_PSY_AUTEUR_STATUT_5, levels=c("Personnel administratif technique/d'entretien",
                                                                       "Autres reponses"))
#V_PSY_AUTEUR_STATUT_5_BIN

# d$V_PSY_AUTEUR_STATUT_5_BIN <- NA
# d$V_PSY_AUTEUR_STATUT_5_BIN <- d$V_PSY_AUTEUR_STATUT_5
# 
# d$V_PSY_AUTEUR_STATUT_5_RC[d$V_PSY_AUTEUR_STATUT_5==1]=1
# d$V_PSY_AUTEUR_STATUT_5_RC[d$V_PSY_AUTEUR_STATUT_5==0]=0

#MODIFICATION si correction du recodage
#V_PSY_AUTEUR_STATUT_6
#V_PSY_AUTEUR_STATUT_6_RC

#d$V_PSY_AUTEUR_STATUT_6_RC <- NA
#d$V_PSY_AUTEUR_STATUT_6_RC <- d$V_PSY_AUTEUR_STATUT_6

#d$V_PSY_AUTEUR_STATUT_6_RC[d$V_PSY_AUTEUR_STATUT_6==1]="Autre personne du lieu d'etude"
#d$V_PSY_AUTEUR_STATUT_6_RC[d$V_PSY_AUTEUR_STATUT_6==0]="Autres reponses"

#d$V_PSY_AUTEUR_STATUT_6_RC <- factor(d$V_PSY_AUTEUR_STATUT_6, levels=c("Autre personne du lieu d'etude","Autres reponses"))
#V_PSY_AUTEUR_STATUT_6_BIN

#d$V_PSY_AUTEUR_STATUT_6_BIN <- NA
#d$V_PSY_AUTEUR_STATUT_6_BIN <- d$V_PSY_AUTEUR_STATUT_6

#d$V_PSY_AUTEUR_STATUT_6_RC[d$V_PSY_AUTEUR_STATUT_6==1]=1
#d$V_PSY_AUTEUR_STATUT_6_RC[d$V_PSY_AUTEUR_STATUT_6==0]=0

#V_PSY_AUTEUR_STATUT_7 #A SUPPRIMER SI CORRECTION DU RECODAGE
#V_PSY_AUTEUR_STATUT_7_RC

d$V_PSY_AUTEUR_STATUT_7_RC <- NA
d$V_PSY_AUTEUR_STATUT_7_RC <- d$V_PSY_AUTEUR_STATUT_7

d$V_PSY_AUTEUR_STATUT_7_RC[d$V_PSY_AUTEUR_STATUT_7==1]="Autre personne du lieu d'etude"
d$V_PSY_AUTEUR_STATUT_7_RC[d$V_PSY_AUTEUR_STATUT_7==0]="Autres reponses"

d$V_PSY_AUTEUR_STATUT_7_RC <- factor(d$V_PSY_AUTEUR_STATUT_7, levels=c("Autre personne du lieu d'etude",
                                                                       "Autres reponses"))
# #V_PSY_AUTEUR_STATUT_7_BIN
# 
# d$V_PSY_AUTEUR_STATUT_7_BIN <- NA
# d$V_PSY_AUTEUR_STATUT_7_BIN <- d$V_PSY_AUTEUR_STATUT_7
# 
# d$V_PSY_AUTEUR_STATUT_7_RC[d$V_PSY_AUTEUR_STATUT_7==1]=1
# d$V_PSY_AUTEUR_STATUT_7_RC[d$V_PSY_AUTEUR_STATUT_7==0]=0

#V_PSY_AUTEUR_STATUT_100 #A SUPPRIMER SI CORRECTION DU RECODAGE
#V_PSY_AUTEUR_STATUT_100_RC

d$V_PSY_AUTEUR_STATUT_100_RC <- NA
d$V_PSY_AUTEUR_STATUT_100_RC <- d$V_PSY_AUTEUR_STATUT_100

d$V_PSY_AUTEUR_STATUT_100_RC[d$V_PSY_AUTEUR_STATUT_100==1]="Collegue"
d$V_PSY_AUTEUR_STATUT_100_RC[d$V_PSY_AUTEUR_STATUT_100==0]="Autres reponses"

d$V_PSY_AUTEUR_STATUT_100_RC <- factor(d$V_PSY_AUTEUR_STATUT_100, levels=c("Collegue",
                                                                           "Autres reponses"))
# #V_PSY_AUTEUR_STATUT_100_BIN
# 
# d$V_PSY_AUTEUR_STATUT_100_BIN <- NA
# d$V_PSY_AUTEUR_STATUT_100_BIN <- d$V_PSY_AUTEUR_STATUT_100
# 
# d$V_PSY_AUTEUR_STATUT_100_RC[d$V_PSY_AUTEUR_STATUT_100==1]=1
# d$V_PSY_AUTEUR_STATUT_100_RC[d$V_PSY_AUTEUR_STATUT_100==0]=0

#MODIFICATION si correction du recodage
##V_PSY_AUTEUR_STATUT_7
##V_PSY_AUTEUR_STATUT_7_RC

#d$V_PSY_AUTEUR_STATUT_7_RC <- NA
#d$V_PSY_AUTEUR_STATUT_7_RC <- d$V_PSY_AUTEUR_STATUT_7

#d$V_PSY_AUTEUR_STATUT_7_RC[d$V_PSY_AUTEUR_STATUT_7==1]="Collegue"
#d$V_PSY_AUTEUR_STATUT_7_RC[d$V_PSY_AUTEUR_STATUT_7==0]="Autres reponses"

#d$V_PSY_AUTEUR_STATUT_7_RC <- factor(d$V_PSY_AUTEUR_STATUT_7, levels=c("Collegue","Autres reponses"))
##V_PSY_AUTEUR_STATUT_7_BIN

#d$V_PSY_AUTEUR_STATUT_7_BIN <- NA
#d$V_PSY_AUTEUR_STATUT_7_BIN <- d$V_PSY_AUTEUR_STATUT_7

#d$V_PSY_AUTEUR_STATUT_7_RC[d$V_PSY_AUTEUR_STATUT_7==1]=1
#d$V_PSY_AUTEUR_STATUT_7_RC[d$V_PSY_AUTEUR_STATUT_7==0]=0

#V_PSY_AUTEUR_STATUT_8
#V_PSY_AUTEUR_STATUT_8_RC

d$V_PSY_AUTEUR_STATUT_8_RC <- NA
d$V_PSY_AUTEUR_STATUT_8_RC <- d$V_PSY_AUTEUR_STATUT_8

d$V_PSY_AUTEUR_STATUT_8_RC[d$V_PSY_AUTEUR_STATUT_8==1]="Employeur ou employeuse"
d$V_PSY_AUTEUR_STATUT_8_RC[d$V_PSY_AUTEUR_STATUT_8==0]="Autres reponses"

d$V_PSY_AUTEUR_STATUT_8_RC <- factor(d$V_PSY_AUTEUR_STATUT_8, levels=c("Employeur ou employeuse","Autres reponses"))
# #V_PSY_AUTEUR_STATUT_8_BIN
# 
# d$V_PSY_AUTEUR_STATUT_8_BIN <- NA
# d$V_PSY_AUTEUR_STATUT_8_BIN <- d$V_PSY_AUTEUR_STATUT_8
# 
# d$V_PSY_AUTEUR_STATUT_8_RC[d$V_PSY_AUTEUR_STATUT_8==1]=1
# d$V_PSY_AUTEUR_STATUT_8_RC[d$V_PSY_AUTEUR_STATUT_8==0]=0

#V_PSY_AUTEUR_STATUT_9
#V_PSY_AUTEUR_STATUT_9_RC

d$V_PSY_AUTEUR_STATUT_9_RC <- NA
d$V_PSY_AUTEUR_STATUT_9_RC <- d$V_PSY_AUTEUR_STATUT_9

d$V_PSY_AUTEUR_STATUT_9_RC[d$V_PSY_AUTEUR_STATUT_9==1]="Tuteur ou tutrice de stage, maitre ou maitresse de stage"
d$V_PSY_AUTEUR_STATUT_9_RC[d$V_PSY_AUTEUR_STATUT_9==0]="Autres reponses"

d$V_PSY_AUTEUR_STATUT_9_RC <- factor(d$V_PSY_AUTEUR_STATUT_9, levels=c("Tuteur ou tutrice de stage, maitre ou maitresse de stage","Autres reponses"))
#V_PSY_AUTEUR_STATUT_9_BIN

# d$V_PSY_AUTEUR_STATUT_9_BIN <- NA
# d$V_PSY_AUTEUR_STATUT_9_BIN <- d$V_PSY_AUTEUR_STATUT_9
# 
# d$V_PSY_AUTEUR_STATUT_9_RC[d$V_PSY_AUTEUR_STATUT_9==1]=1
# d$V_PSY_AUTEUR_STATUT_9_RC[d$V_PSY_AUTEUR_STATUT_9==0]=0

#V_PSY_AUTEUR_STATUT_10
#V_PSY_AUTEUR_STATUT_10_RC

d$V_PSY_AUTEUR_STATUT_10_RC <- NA
d$V_PSY_AUTEUR_STATUT_10_RC <- d$V_PSY_AUTEUR_STATUT_10

d$V_PSY_AUTEUR_STATUT_10_RC[d$V_PSY_AUTEUR_STATUT_10==1]="Client ou cliente, patient ou patiente, autre personne que vous pouvez rencontrer dans le cadre d'un stage ou d'un emploi"
d$V_PSY_AUTEUR_STATUT_10_RC[d$V_PSY_AUTEUR_STATUT_10==0]="Autres reponses"

d$V_PSY_AUTEUR_STATUT_10_RC <- factor(d$V_PSY_AUTEUR_STATUT_10, levels=c("Client ou cliente, patient ou patiente, autre personne que vous pouvez rencontrer dans le cadre d'un stage ou d'un emploi","Autres reponses"))
#V_PSY_AUTEUR_STATUT_10_BIN

# d$V_PSY_AUTEUR_STATUT_10_BIN <- NA
# d$V_PSY_AUTEUR_STATUT_10_BIN <- d$V_PSY_AUTEUR_STATUT_10
# 
# d$V_PSY_AUTEUR_STATUT_10_RC[d$V_PSY_AUTEUR_STATUT_10==1]=1
# d$V_PSY_AUTEUR_STATUT_10_RC[d$V_PSY_AUTEUR_STATUT_10==0]=0

#V_PSY_AUTEUR_STATUT_11
#V_PSY_AUTEUR_STATUT_11_RC

d$V_PSY_AUTEUR_STATUT_11_RC <- NA
d$V_PSY_AUTEUR_STATUT_11_RC <- d$V_PSY_AUTEUR_STATUT_11

d$V_PSY_AUTEUR_STATUT_11_RC[d$V_PSY_AUTEUR_STATUT_11==1]="Autre relation professionnelle"
d$V_PSY_AUTEUR_STATUT_11_RC[d$V_PSY_AUTEUR_STATUT_11==0]="Autres reponses"

d$V_PSY_AUTEUR_STATUT_11_RC <- factor(d$V_PSY_AUTEUR_STATUT_11, levels=c("Autre relation professionnelle","Autres reponses"))

#V_PSY_AUTEUR_STATUT_11_BIN

# d$V_PSY_AUTEUR_STATUT_11_BIN <- NA
# d$V_PSY_AUTEUR_STATUT_11_BIN <- d$V_PSY_AUTEUR_STATUT_11
# 
# d$V_PSY_AUTEUR_STATUT_11_RC[d$V_PSY_AUTEUR_STATUT_11==1]=1
# d$V_PSY_AUTEUR_STATUT_11_RC[d$V_PSY_AUTEUR_STATUT_11==0]=0

#V_PSY_AUTEUR_STATUT_12
#V_PSY_AUTEUR_STATUT_12_RC

d$V_PSY_AUTEUR_STATUT_12_RC <- NA
d$V_PSY_AUTEUR_STATUT_12_RC <- d$V_PSY_AUTEUR_STATUT_12

d$V_PSY_AUTEUR_STATUT_12_RC[d$V_PSY_AUTEUR_STATUT_12==1]="Un ou une inconnue"
d$V_PSY_AUTEUR_STATUT_12_RC[d$V_PSY_AUTEUR_STATUT_12==0]="Autres reponses"

d$V_PSY_AUTEUR_STATUT_12_RC <- factor(d$V_PSY_AUTEUR_STATUT_12, levels=c("Un ou une inconnue","Autres reponses"))

#V_PSY_AUTEUR_STATUT_12_BIN

# d$V_PSY_AUTEUR_STATUT_12_BIN <- NA
# d$V_PSY_AUTEUR_STATUT_12_BIN <- d$V_PSY_AUTEUR_STATUT_12
# 
# d$V_PSY_AUTEUR_STATUT_12_RC[d$V_PSY_AUTEUR_STATUT_12==1]=1
# d$V_PSY_AUTEUR_STATUT_12_RC[d$V_PSY_AUTEUR_STATUT_12==0]=0

#V_PSY_AUTEUR_STATUT_99
#V_PSY_AUTEUR_STATUT_99_RC

d$V_PSY_AUTEUR_STATUT_99_RC <- NA
d$V_PSY_AUTEUR_STATUT_99_RC <- d$V_PSY_AUTEUR_STATUT_99

d$V_PSY_AUTEUR_STATUT_99_RC[d$V_PSY_AUTEUR_STATUT_99==1]="NSP"
d$V_PSY_AUTEUR_STATUT_99_RC[d$V_PSY_AUTEUR_STATUT_99==0]="Autres reponses"

d$V_PSY_AUTEUR_STATUT_99_RC <- factor(d$V_PSY_AUTEUR_STATUT_99, levels=c("NSP","Autres reponses"))

#V_PSY_AUTEUR_STATUT_99_BIN
# 
# d$V_PSY_AUTEUR_STATUT_99_BIN <- NA
# d$V_PSY_AUTEUR_STATUT_99_BIN <- d$V_PSY_AUTEUR_STATUT_99
# 
# d$V_PSY_AUTEUR_STATUT_99_RC[d$V_PSY_AUTEUR_STATUT_99==1]=1
# d$V_PSY_AUTEUR_STATUT_99_RC[d$V_PSY_AUTEUR_STATUT_99==0]=0

#V_PSY_AUTEUR_STATUT_98
#V_PSY_AUTEUR_STATUT_98_RC

d$V_PSY_AUTEUR_STATUT_98_RC <- NA
d$V_PSY_AUTEUR_STATUT_98_RC <- d$V_PSY_AUTEUR_STATUT_98

d$V_PSY_AUTEUR_STATUT_98_RC[d$V_PSY_AUTEUR_STATUT_98==1]="Refus"
d$V_PSY_AUTEUR_STATUT_98_RC[d$V_PSY_AUTEUR_STATUT_98==0]="Autres reponses"

d$V_PSY_AUTEUR_STATUT_98_RC <- factor(d$V_PSY_AUTEUR_STATUT_98, levels=c("NSP","Autres reponses"))
# 
# #V_PSY_AUTEUR_STATUT_98_BIN
# 
# d$V_PSY_AUTEUR_STATUT_98_BIN <- NA
# d$V_PSY_AUTEUR_STATUT_98_BIN <- d$V_PSY_AUTEUR_STATUT_98
# 
# d$V_PSY_AUTEUR_STATUT_98_RC[d$V_PSY_AUTEUR_STATUT_98==1]=1
# d$V_PSY_AUTEUR_STATUT_98_RC[d$V_PSY_AUTEUR_STATUT_98==0]=0

#On cree la variable V_PSY_AUTEUR_STATUT_NB

d$V_PSY_AUTEUR_STATUT_NB <-
  rowSums(d[, c(
    "V_PSY_AUTEUR_STATUT_1",
    "V_PSY_AUTEUR_STATUT_2",
    "V_PSY_AUTEUR_STATUT_3",
    "V_PSY_AUTEUR_STATUT_4",
    "V_PSY_AUTEUR_STATUT_5",
    "V_PSY_AUTEUR_STATUT_7",
    "V_PSY_AUTEUR_STATUT_100",
    "V_PSY_AUTEUR_STATUT_8",
    "V_PSY_AUTEUR_STATUT_9",
    "V_PSY_AUTEUR_STATUT_10",
    "V_PSY_AUTEUR_STATUT_11",
    "V_PSY_AUTEUR_STATUT_12",
  )], na.rm = T)

#MODIFICATION si correction du recodage
#d$V_PSY_AUTEUR_STATUT_NB <-  rowSums(d[,c("V_PSY_AUTEUR_STATUT_1_BIN","V_PSY_AUTEUR_STATUT_2_BIN","V_PSY_AUTEUR_STATUT_3_BIN","V_PSY_AUTEUR_STATUT_4_BIN","V_PSY_AUTEUR_STATUT_5_BIN","V_PSY_AUTEUR_STATUT_6_BIN","V_PSY_AUTEUR_STATUT_7_BIN","V_PSY_AUTEUR_STATUT_8_BIN","V_PSY_AUTEUR_STATUT_9_BIN","V_PSY_AUTEUR_STATUT_10_BIN","V_PSY_AUTEUR_STATUT_11_BIN","V_PSY_AUTEUR_STATUT_12_BIN",)], na.rm=T)

#[ICI]

#On cree P_FAITS_PHYS_1_BIN, une variable dichotomique pour permettre de mesurer si oui ou non les individus ont declare avoir subi le fait de violences physiques

d$P_FAITS_PHYS_1_BIN[d$P_FAITS_PHYS_1==1 | d$P_FAITS_PHYS_1==2]=1
d$P_FAITS_PHYS_1_BIN[d$P_FAITS_PHYS_1==3 | d$P_FAITS_PHYS_1==99 | d$P_FAITS_PHYS_1==98 ]=0

#On cree une variable P_FAITS_PSY_GEN, qui comptabilise le nombre de faits de violence phychologiques declares par les repondant(e)s (un seul fait, mais plus simple pour construction de la variable generale)

d$P_FAITS_PHYS_GEN = (d$P_FAITS_PHYS_1_BIN)

#On amende au besoin la variable V_PHYS_DOUZE pour s'assurer que les valeurs vides sont considerees comme des NA et non comme des 0

d$V_PHYS_DOUZE[d$V_PHYS_DOUZE==""]=NA

#####VIOLENCES SEXUELLES

#On cree 8 variables dichotomiques, pour permettre de mesurer si oui ou non les individus ont declare avoir subi les 8 faits de violences sexuelles 

d$P_FAITS_SEX_1_BIN[d$P_FAITS_SEX_1==1 | d$P_FAITS_SEX_1==2]=1
d$P_FAITS_SEX_1_BIN[d$P_FAITS_SEX_1==3 | d$P_FAITS_SEX_1==99 | d$P_FAITS_SEX_1==98 ]=0

d$P_FAITS_SEX_2_BIN[d$P_FAITS_SEX_2==1 | d$P_FAITS_SEX_2==2]=1
d$P_FAITS_SEX_2_BIN[d$P_FAITS_SEX_2==3 | d$P_FAITS_SEX_2==99 | d$P_FAITS_SEX_2==98 ]=0

d$P_FAITS_SEX_3_BIN[d$P_FAITS_SEX_3==1 | d$P_FAITS_SEX_3==2]=1
d$P_FAITS_SEX_3_BIN[d$P_FAITS_SEX_3==3 | d$P_FAITS_SEX_3==99 | d$P_FAITS_SEX_3==98 ]=0

d$P_FAITS_SEX_4_BIN[d$P_FAITS_SEX_4==1 | d$P_FAITS_SEX_4==2]=1
d$P_FAITS_SEX_4_BIN[d$P_FAITS_SEX_4==3 | d$P_FAITS_SEX_4==99 | d$P_FAITS_SEX_4==98 ]=0

d$P_FAITS_SEX_5_BIN[d$P_FAITS_SEX_5==1 | d$P_FAITS_SEX_5==2]=1
d$P_FAITS_SEX_5_BIN[d$P_FAITS_SEX_5==3 | d$P_FAITS_SEX_5==99 | d$P_FAITS_SEX_5==98 ]=0

d$P_FAITS_SEX_6_BIN[d$P_FAITS_SEX_6==1 | d$P_FAITS_SEX_6==2]=1
d$P_FAITS_SEX_6_BIN[d$P_FAITS_SEX_6==3 | d$P_FAITS_SEX_6==99 | d$P_FAITS_SEX_6==98 ]=0

d$P_FAITS_SEX_7_BIN[d$P_FAITS_SEX_7==1 | d$P_FAITS_SEX_7==2]=1
d$P_FAITS_SEX_7_BIN[d$P_FAITS_SEX_7==3 | d$P_FAITS_SEX_7==99 | d$P_FAITS_SEX_7==98 ]=0

#On cree une autre variable dichotomique pour la variable des faits de signalements (pour l'instant, viols)

# d$P_FAITS_SEX_8_BIN [d$P_FAITS_SIGN_MAJ_1==1 | d$P_FAITS_SIGN_MAJ_1==2]=1
# d$P_FAITS_SEX_8_BIN[d$P_FAITS_SIGN_MAJ_1==3 | d$P_FAITS_SIGN_MAJ_1==99 | d$P_FAITS_SIGN_MAJ_1==98 ]=0
# d$P_FAITS_SEX_8_BIN [d$P_FAITS_SIGN_MIN_1==1 | d$P_FAITS_SIGN_MIN_1==2]=1
# d$P_FAITS_SEX_8_BIN[d$P_FAITS_SIGN_MIN_1==3 | d$P_FAITS_SIGN_MIN_1==99 | d$P_FAITS_SIGN_MIN_1==98 ]=0

#On creer une variable P_FAITS_SEX_GEN, qui comptabilise le nombre de faits de violence sexuelle declares par les repondant(e)s

d$P_FAITS_SEX_GEN= rowSums(d[,c("P_FAITS_SEX_1_BIN","P_FAITS_SEX_2_BIN","P_FAITS_SEX_3_BIN","P_FAITS_SEX_4_BIN","P_FAITS_SEX_5_BIN","P_FAITS_SEX_6_BIN","P_FAITS_SEX_7_BIN","P_FAITS_SEX_8_BIN")],na.rm=T)


#On cree une variable P_FAITS_SEX_GEN_BIN, qui indique si l'individu a declare au moins un fait de violence sexuelle

d$P_FAITS_SEX_GEN_BIN[d$P_FAITS_SEX_GEN==0]=0
d$P_FAITS_SEX_GEN_BIN[d$P_FAITS_SEX_GEN>=1]=1

#On cree une variable V_SEX_DOUZE_GEN

d$V_SEX_DOUZE_1[d$V_SEX_DOUZE_1==""]=NA
d$V_SEX_DOUZE_2[d$V_SEX_DOUZE_2==""]=NA
d$V_SEX_DOUZE_3[d$V_SEX_DOUZE_3==""]=NA
d$V_SEX_DOUZE_4[d$V_SEX_DOUZE_4==""]=NA
d$V_SEX_DOUZE_5[d$V_SEX_DOUZE_5==""]=NA
d$V_SEX_DOUZE_6[d$V_SEX_DOUZE_6==""]=NA
d$V_SEX_DOUZE_7[d$V_SEX_DOUZE_6==""]=NA
d$V_SEX_DOUZE_7[d$V_SEX_DOUZE_7==""]=NA
d$V_SEX_DOUZE_8[d$V_SEX_DOUZE_8==""]=NA
d$V_SEX_DOUZE_9[d$V_SEX_DOUZE_8==""]=NA


# d$V_SEX_DOUZE_1 <- as.integer(d$V_SEX_DOUZE_1)
# d$V_SEX_DOUZE_2 <- as.integer(d$V_SEX_DOUZE_2)
# d$V_SEX_DOUZE_3 <- as.integer(d$V_SEX_DOUZE_3)
# d$V_SEX_DOUZE_4 <- as.integer(d$V_SEX_DOUZE_4)
# d$V_SEX_DOUZE_5 <- as.integer(d$V_SEX_DOUZE_5)
# d$V_SEX_DOUZE_6 <- as.integer(d$V_SEX_DOUZE_6)
# d$V_SEX_DOUZE_7 <- as.integer(d$V_SEX_DOUZE_7)
# d$V_SEX_DOUZE_8 <- as.integer(d$V_SEX_DOUZE_8)
# d$V_SEX_DOUZE_9 <- as.integer(d$V_SEX_DOUZE_9)

#On cree une variable V_SEX_DOUZE_GEN qui synthetise le nombre de faits declares ayant eu lieu les 12 derniers mois

d$V_SEX_DOUZE_GEN <- rowSums(d[,c("V_SEX_DOUZE_1","V_SEX_DOUZE_2","V_SEX_DOUZE_3","V_SEX_DOUZE_4","V_SEX_DOUZE_5","V_SEX_DOUZE_6","V_SEX_DOUZE_7","V_SEX_DOUZE_8")], na.rm=T)
d$V_SEX_DOUZE_GEN[d$V_SEX_DOUZE_GEN_BIN==0]=NA
d$V_SEX_DOUZE_GEN[is.na(d$V_SEX_DOUZE_GEN_BIN)]=NA

#On cree une variable V_SEX_DOUZE_GEN_BIN qui indique si l'individu a declare au moins un fait de violence psychologique au cours des 12 derniers mois

d$V_SEX_DOUZE_GEN_BIN[d$V_SEX_DOUZE_GEN==0]=0
d$V_SEX_DOUZE_GEN_BIN[is.na(d$V_SEX_DOUZE_GEN)]=NA
d$V_SEX_DOUZE_GEN_BIN[d$V_SEX_DOUZE_GEN>=1]=1


d$V_SEX_MARQ[d$V_SEX_MARQ==""]=NA


#On cree une variable P_FAITS_GEN, qui recapitule le nombre de faits declares

d$P_FAITS_GEN <- rowSums(d[,c("P_FAITS_PSY_GEN", "P_FAITS_PHYS_GEN", "P_FAITS_SEX_GEN")], na.rm=T)

#On cree une variable P_FAITS_GEN_BIN, qui indique si l'individu a declare au moins un fait de violence au total

d$P_FAITS_GEN_BIN[d$P_FAITS_GEN==0]=0
d$P_FAITS_GEN_BIN[d$P_FAITS_GEN>=1]=1






