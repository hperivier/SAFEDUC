#import packages 
source("packages.R")

#import fonctions
source("fonctions.R")

d #tableau de toutes les r�ponses

filter_complete(d) #tableau des r�ponses compl�tes

filter_exploitable(d) #tableau des r�ponses exploitables (progr�s>54%, possible de changer ce taux dans fonctions.R)