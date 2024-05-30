#import packages 
source("packages.R")

#import fonctions
source("fonctions.R")

#script de recodages
source("codebook.R")

safeduc_finished <- d[d$Progress==100 & !is.na(d$I_ETAB), ]

safeduc_all <- d[!is.na(d$I_ETAB), ]

write.csv(safeduc_finished, "safeduc_finished.csv")
write.csv(safeduc_all, "safeduc_all.csv")





