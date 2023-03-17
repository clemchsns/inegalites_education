
library(readxl)
library(readr)

import_epe<-  read.csv("data/Enseignant_par_eleves.csv")
epe_reduit <- import_epe[,c(1,6:7)]
epe_10l <- epe_reduit[10,]
