#### Fusionner safeduc_num et safeduc_let afin de labelliser les variables #### 

#d_label <- merge_with_labels(safeduc_num, safeduc_let)
#import packages 
source("packages.R")

#import fonctions
source("fonctions.R")

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
###### Création de nouvelles variables recodées ######



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


#Variable de synthèse du niveau d'étude

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
