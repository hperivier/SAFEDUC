#import packages 
source("packages.R")

#import fonctions
source("fonctions.R")

##### Nombre de r�ponses ######

d #tableau de toutes les r�ponses

filter_complete(d) #tableau des r�ponses compl�tes

filter_exploitable(d) #tableau des r�ponses exploitables (progr�s>54%, possible de changer ce taux dans fonctions.R)

dim(filter_complete(d))

dim(filter_exploitable(d))


##### Profil des r�pondants #####

CrossTable(filter_complete(d)$I_ETAB_RC, filter_complete(d)$I_IDGENRE, prop.c=F, prop.chisq = F)

CrossTable(filter_complete(d)$I_ETAB_RC, filter_complete(d)$I_NATIONALITE_1, prop.c=F, prop.chisq = F)
