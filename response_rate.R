#import packages 
source("packages.R")

#import fonctions
source("fonctions.R")

d #tableau de toutes les réponses

filter_complete(d) #tableau des réponses complètes

filter_exploitable(d) #tableau des réponses exploitables (progrès>54%, possible de changer ce taux dans fonctions.R)
