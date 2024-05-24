#import packages 
source("packages.R")

#import fonctions
source("fonctions.R")

##### Nombre de réponses ######

d #tableau de toutes les réponses

filter_complete(d) #tableau des réponses complètes

filter_exploitable(d) #tableau des réponses exploitables (progrès>54%, possible de changer ce taux dans fonctions.R)

dim(filter_complete(d))

dim(filter_exploitable(d))


##### Profil des répondants #####

CrossTable(filter_complete(d)$I_ETAB_RC, filter_complete(d)$I_IDGENRE, prop.c=F, prop.chisq = F)

CrossTable(filter_complete(d)$I_ETAB_RC, filter_complete(d)$I_NATIONALITE_1, prop.c=F, prop.chisq = F)
