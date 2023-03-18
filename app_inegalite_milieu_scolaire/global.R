library(shinydashboard)
library(readxl)
library(readr)
library(tidyverse)

# -- Importation --

# Enseignants par élèves
import_epe<-  read.csv("data/Enseignant_par_eleves.csv",sep=";",header=TRUE,stringsAsFactor=TRUE)
epe_reduit <- import_epe[,c(1,6:7)]

colnames(epe_reduit) <- c("LOCATION","TIME","VALUE")
epe_filtre <- epe_reduit |> 
  dplyr::filter(TIME>=2014,TIME<=2021)

epe_filtre$ACRONYME_PAYS <- epe_filtre$LOCATION
epe_filtre$ACRONYME_PAYS <- as.factor(epe_filtre$ACRONYME_PAYS)
epe_filtre$LOCATION <- as.factor(epe_filtre$LOCATION)
levels(epe_filtre$LOCATION) <- c("Australie","Autriche","Belgique","Brésil","Canada","Suisse","Chili","Colombie","Costa Rica","République Tchèque","Allemagne","Danemark","Espagne","Estonie","Finlande",
                                 "France","G20","Royaume-Uni","Grèce","Hongrie","Irlande","Islande","Israël","Italie","Japon","Corée du Sud","Lituanie","Luxembourg","Lettonie","Mexique",
                                 "Pays-Bas","Norvège","Nouvelle-Zélande","OAVG","Pologne","Portugal","Slovaquie","Slovénie","Suède","Turquie","USA")

epe_filtre_10 <- epe_filtre[1:10,]


# Taux optention diplôme
import_tod <- read_excel("data/Taux_optention_diplome.xlsx",sheet=1)
tod_reduit <- import_tod[,c(2,4,6,8:9,12,15)]
tod_reduit_bis <- na.omit(tod_reduit)
tod_10l <- tod_reduit_bis[1:10,]
tod_filtre <- tod_10l |> 
  dplyr::filter(YEAR>=2014,YEAR<=2021) |> 
  dplyr::filter(Indicateur == "Taux d’obtention d’un diplôme")

summary(tod_filtre)

# France : indicateur de ségrégation sociale

import_segreg <- read.csv("data/Fr_indicateur_segregation_sociale_colleges.csv",sep=";",dec=".",header=TRUE,stringsAsFactor=TRUE)
segreg_reduit <- import_segreg[,c(2,4:6,8,9,11:25,27,28)]
colnames(segreg_reduit) <- c("annee","nom_academie","dep","nom_dep",
                             "nb_college_PU","nb_college_PR",
                             "proportion_tfav","proportion_fav","proportion_moy","proportion_defav",
                             "proportion_tfav_PU","proportion_fav_PU","proportion_moy_PU","proportion_defav_PU",
                             "proportion_tfav_PR","proportion_fav_PR","proportion_moy_PR","proportion_defav_PR",
                             "indice_entropie_total","indice_entropie_PU","indice_entropie_PR",
                             "contrib_college_PU","contrib_college_PR")
segreg_reduit$nom_dep <- as.factor(segreg_reduit$nom_dep)
segreg_reduit_10l <- segreg_reduit[1:10,]
summary(segreg_reduit_10l)

# Taux de scolarisation

import_ts<- read.csv("data/Taux_scolarisation_petite_enfance.csv",sep=";",header=TRUE,dec=".",stringsAsFactors = T)
ts_reduit <- bind_cols(import_ts[,c(1,6:7)])
ts_reduit_10l <- ts_reduit[1:10,]
summary(ts_reduit_10l)

# Etudiant en mobilité internationale
import_em <- read.csv("data/Pct_etudiants_en_mobilite.csv",sep=";",header=TRUE,dec=".",stringsAsFactors = T)
em_reduit <- bind_cols(import_em[,c(1,6:7)])
colnames(em_reduit) <- c("LOCATION","TIME","VALUE")
em_filtre <- em_reduit |> 
  dplyr::filter(TIME>=2014,TIME<=2021)
levels(em_filtre$LOCATION) <- c("Australie","Autriche","Belgique","Brésil","Canada","Suisse","Chili","Colombie","Costa Rica","République Tchèque","Allemagne","Danemark","Espagne","Estonie","Finlande",
                                "France","Royaume-Uni","Grèce","Hongrie","Irlande","Islande","Israël","Italie","Japon","Corée du Sud","Lituanie","Luxembourg","Lettonie","Mexique",
                                "Pays-Bas","Norvège","Nouvelle-Zélande","OAVG","OEU","Pologne","Portugal","Slovaquie","Slovénie","Suède","Turquie","USA")
em_filtre_10l <- em_filtre[1:10,]
summary(em_filtre_10l)

# France : Taux des scolarisations par département
importfr_ts_dpt <- read_excel("data/Fr-taux_scolarisation.xlsx",sheet=1)
importfr_ts_dpt[,c("Numéro département","Libellé département")] <- lapply(importfr_ts_dpt[,c("Numéro département","Libellé département")],factor)
importfr_ts_dpt_10l <- importfr_ts_dpt[1:10,]
summary(importfr_ts_dpt_10l)

#France : Taux des scolarisations par région
importfr_ts_rg <- read_excel("data/Fr-taux_scolarisation.xlsx",sheet=2)
importfr_ts_rg$Numero <- factor(importfr_ts_rg$Numero)
importfr_ts_rg$Region <- factor(importfr_ts_rg$Region)
summary(importfr_ts_rg_10l)

# France : réussite bac 

import_fr_rb <- read.csv("data/Fr-reussite_bac_origine_sociale.csv",sep=";",dec=".",header=TRUE)
colnames(import_fr_rb) <- c("Annee","Origine_sociale","Nombre d'admis au baccalaureat general","Pourcentage d'admis au baccalaureat general",
                            "Nombre d'admis au baccalaureat technologique","Pourcentage d'admis au baccalaureat technologique",
                            "Nombre d'admis au baccalaureat professionnel","Pourcentage d'admis au baccalaureat professionnel",
                            "Nombre_admis_baccalaureat" , "Pourcentage d'admis au baccalaureat")
import_fr_rb$Origine_sociale <- factor(import_fr_rb$Origine_sociale)
fr_rb_filtre <- import_fr_rb|> 
  dplyr::filter(Annee>=2014,Annee<=2021) |> 
  dplyr::filter(Origine_sociale!="Professions intermediaires : instituteurs et assimiles")

fr_rb <- fr_rb_filtre[!grepl("^dont", fr_rb_filtre$Origine_sociale), ]
summary(fr_rb)

