library(shinydashboard)
library(readxl)
library(readr)
library(tidyverse)
library(treemap)
library(shinycssloaders) # pour le withspinner
library(rAmCharts)
library(sf)
library(leaflet)
library(DT)
library(rmapshaper)
library(rnaturalearthdata)
library(rnaturalearth)
library(BH)
library(shinyWidgets)

# -- Importation --

# Enseignants par élèves
import_epe<-  read.csv("data/Enseignant_par_eleves.csv",sep=";",header=TRUE,stringsAsFactor=TRUE)
epe_reduit <- import_epe[,c(1,6:7)]

colnames(epe_reduit) <- c("LOCATION","TIME","VALUE")
enseignant_par_eleves <- epe_reduit |> 
  dplyr::filter(TIME>=2014,TIME<=2021)

enseignant_par_eleves$ACRONYME_PAYS <- enseignant_par_eleves$LOCATION
enseignant_par_eleves$ACRONYME_PAYS <- as.factor(enseignant_par_eleves$ACRONYME_PAYS)
enseignant_par_eleves$LOCATION <- as.factor(enseignant_par_eleves$LOCATION)
levels(enseignant_par_eleves$LOCATION) <- c("Australie","Autriche","Belgique","Brésil","Canada","Suisse","Chili","Colombie","Costa Rica","République Tchèque","Allemagne","Danemark","Espagne","Estonie","Finlande",
                                 "France","G20","Royaume-Uni","Grèce","Hongrie","Irlande","Islande","Israël","Italie","Japon","Corée du Sud","Lituanie","Luxembourg","Lettonie","Mexique",
                                 "Pays-Bas","Norvège","Nouvelle-Zélande","OAVG","Pologne","Portugal","Slovaquie","Slovénie","Suède","Turquie","USA")



# Taux obtention diplôme
import_tod <- read_excel("data/Taux_obtention_diplome.xlsx",sheet=1)
tod_reduit <- import_tod[,c(2,4,6,8:9,12,15)]
tod_reduit_bis <- na.omit(tod_reduit)
tod_filtre <- tod_reduit_bis |> 
  dplyr::filter(YEAR>=2014,YEAR<=2021) |> 
  dplyr::filter(Indicateur == "Taux d’obtention d’un diplôme")

summary(tod_filtre)

# France : indicateur de ségrégation sociale

import_segreg <- read.csv("data/Fr_indicateur_segregation_sociale_colleges.csv",sep=";",dec=".",header=TRUE,stringsAsFactor=TRUE)
fr_indicateur_segreg_college <- import_segreg[,c(2,4:6,8,9,11:25,27,28)]
colnames(fr_indicateur_segreg_college) <- c("annee","nom_academie","dep","nom_dep",
                             "nb_college_PU","nb_college_PR",
                             "proportion_tfav","proportion_fav","proportion_moy","proportion_defav",
                             "proportion_tfav_PU","proportion_fav_PU","proportion_moy_PU","proportion_defav_PU",
                             "proportion_tfav_PR","proportion_fav_PR","proportion_moy_PR","proportion_defav_PR",
                             "indice_entropie_total","indice_entropie_PU","indice_entropie_PR",
                             "contrib_college_PU","contrib_college_PR")
fr_indicateur_segreg_college$nom_dep <- as.factor(fr_indicateur_segreg_college$nom_dep)
summary(fr_indicateur_segreg_college)

# Taux de scolarisation

import_ts<- read.csv("data/Taux_scolarisation_petite_enfance.csv",sep=";",header=TRUE,dec=".",stringsAsFactors = T)
taux_scolarisation <- bind_cols(import_ts[,c(1,6:7)])
summary(taux_scolarisation)

# Etudiant en mobilité internationale
import_em <- read.csv("data/Pct_etudiants_en_mobilite.csv",sep=";",header=TRUE,dec=".",stringsAsFactors = T)
em_reduit <- bind_cols(import_em[,c(1,6:7)])
colnames(em_reduit) <- c("LOCATION","TIME","VALUE")
etud_mobilite <- em_reduit |> 
  dplyr::filter(TIME>=2014,TIME<=2021)
levels(etud_mobilite$LOCATION) <- c("Australie","Autriche","Belgique","Brésil","Canada","Suisse","Chili","Colombie","Costa Rica","République Tchèque","Allemagne","Danemark","Espagne","Estonie","Finlande",
                                "France","Royaume-Uni","Grèce","Hongrie","Irlande","Islande","Israël","Italie","Japon","Corée du Sud","Lituanie","Luxembourg","Lettonie","Mexique",
                                "Pays-Bas","Norvège","Nouvelle-Zélande","OAVG","OEU","Pologne","Portugal","Slovaquie","Slovénie","Suède","Turquie","USA")

summary(etud_mobilite)

# France : Taux des scolarisations par département
fr_taux_scolarisation_dpt <- read_excel("data/Fr-taux_scolarisation.xlsx",sheet=1)
fr_taux_scolarisation_dpt[,c("Numéro département","Libellé département")] <- lapply(fr_taux_scolarisation_dpt[,c("Numéro département","Libellé département")],factor)
summary(fr_taux_scolarisation_dpt)

#France : Taux des scolarisations par région
fr_taux_scolarisation_reg <- read_excel("data/Fr-taux_scolarisation.xlsx",sheet=2)
fr_taux_scolarisation_reg$Numero <- factor(fr_taux_scolarisation_reg$Numero)
fr_taux_scolarisation_reg$Region <- factor(fr_taux_scolarisation_reg$Region)

summary(fr_taux_scolarisation_reg)

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

fr_reussite_bac <- fr_rb_filtre[!grepl("^dont", fr_rb_filtre$Origine_sociale), ]
summary(fr_reussite_bac)

# France : Brevet par établissement
importfr_dnb <- read.csv("data/Fr-dnb-par-etablissement.csv",sep=";",header=TRUE)
colnames(importfr_dnb) <- c("Session","Numero d'etablissement","Type d'etablissement","Patronyme",
                            "Secteur d'enseignement","Commune","Libellé commune","Code département",
                            "Libellé_département","Code académie","Libellé académie", "Code région",
                            "Libellé région","Inscrits","Presents", "Admis",
                            "Admis sans mention","Nombre d admis Mention AB","Admis Mention bien","Admis Mention très bien","Taux de réussite")
fr_dnb_reduit <- importfr_dnb[,c(1,3,5,8:9,12:20)]
fr_dnb_reduit[,c(2:7)] <- lapply(fr_dnb_reduit[,c(2:7)],factor)
fr_dnb_etablissement <- fr_dnb_reduit|> 
  dplyr::filter(Session>=2014,Session<=2021)
summary(fr_dnb_etablissement)


# France : boursier par établissement
importfr_boursiers_dpt <- read.csv("data/Fr-boursiers-par-departement.csv",sep=";",header=TRUE)
colnames(importfr_boursiers_dpt) <- c("Rentrée scolaire","Libellé formation","X_Type étab",
                                      "Secteur","Numéro département","Libellé département",
                                      "Nb boursiers", "Commentaire_Nb boursiers","Nb dernier échelon","Commentaire_Nb dernier échelon")
fr_boursiers_dpt <- importfr_boursiers_dpt[,c(1,4:7)]
fr_boursiers_dpt[,c(2:4)] <- lapply(fr_boursiers_dpt[,c(2:4)],factor)
summary(fr_boursiers_dpt)


# France : Bac par académie
importfr_bac_academie <- read.csv("data/Fr-bac_par_academie.csv",sep=";",header=TRUE)
colnames(importfr_bac_academie) <- c("Session" , "Académie","Sexe","Statut du candidat","Voie","Série",
                                     "Diplôme spécialité","Nombre d inscrits","Nombre de présents","Nombre d admis au 1er groupe",
                                     "Nombre de refusés au 1er groupe","Nombre d ajournés  passant les épreuves du 2nd groupe",
                                     "Nombre d admis à l issue du 2nd groupe","Nombre de refusés à l issue du 2nd groupe","Nombre d admis totaux",
                                     "Nombre d admis avec mention TB avec les félicitations du jury","Nombre d admis avec mention TB sans les félicitations du jury",
                                     "Nombre d admis avec mention B","Nombre d admis avec mention AB","Nombre d admis sans mention","Nombre de refusés totaux")
fr_bac_academie <- importfr_bac_academie[,c(1,3,5,8:21)]
fr_bac_academie[,c(2:3)] <- lapply(fr_bac_academie[,c(2:3)],factor)
summary(fr_bac_academie)



liste_df = list("OCDE : Enseignants par élèves"=enseignant_par_eleves,
                "OCDE : Taux d'obtention d'un diplôme"=tod_filtre,
                "France : Indicateur de ségrégation sociale"=fr_indicateur_segreg_college,
                "OCDE : Taux de scolarisation "=taux_scolarisation,
                "OCDE : Etudiants en mobilité internationale"=etud_mobilite,
                "France : Taux de scolarisation par département"=fr_taux_scolarisation_dpt,
                "France : Taux de scolarisation par région"=fr_taux_scolarisation_reg,
                'France : Réussite par baccalauréat'=fr_reussite_bac,
                "France : Obtention du brevet par établissement"=fr_dnb_etablissement,
                "France : Nombre de boursiers par établissement"=fr_boursiers_dpt,
                "France : Obtention du baccalauréat par académie"=fr_bac_academie)

### Acceuil --- 
# Value-box

education_nationale <- valueBox(
  "1932" , "L'instruction publique devient l'éducation nationale (renommé par le gouvernement d'Edouard Herriot",
  icon = icon("school"), color = "green"
)

Loi_Falloux_Box <- valueBox(
  1850, "Loi Falloux incite à ouvrir des écoles pour filles",
  icon = icon("list"),color = "purple"
)

Progres_filles <- valueBox(
  1880, "Les filles ont le droit d'aller au collège et au lycée", 
  icon = icon("list"), color ="green"
)

Gratuite <- valueBox(
  "1881-1882", "Gratuité de l'enseignement public par la loi Jules FERRY. L'Enseignement devient laïque et obligatoire",
  icon = icon("thumbs-up", lib = "glyphicon"), color = "yellow"
)

Separation_eglise_etat <- valueBox(
  "1905", "Séparation de l'Eglise et de l'Etat",
  icon = icon("bolt"), color = "red"
)

Bac_filles <- valueBox(
  "1923", "Les filles ont le droit de passer le baccalauréat",
  icon = icon("briefcase"), color = "maroon"
)


Prog_identiques <- valueBox(
  "1924", "Programmes du collège et du lycée identiques pour les filles et les garçons",
  icon = icon("briefcase"), color = "blue"
)

Ecole_obligatoire_16 <- valueBox(
  "1959", "Ecole obligatoire jusqu'à 16 ans",
  icon = icon("school"), color = "fuchsia"
)

Mixite_filles_garcons <- valueBox(
  "1969", "Mixité : Garçons et filles réunis au sein des mêmes établissements",
  icon = icon("children"), color = "teal"
)

Creations_bacs_pros <- valueBox(
  "1992", "Création des bacs professionnels",
  icon = icon("graduation-cap"), color = "olive"
)



### Inégalités socio-économiques

# value-box

# Moyenne du pct d'admis au baccalauréat selon la PCS 
df_PCS <- data.frame(
  value = aggregate(`Pourcentage d'admis au baccalaureat`~ Origine_sociale,data=fr_reussite_bac,mean)
)
colnames(df_PCS) <- c("Origine_sociale","Pct_admis_baccalaureat")
df_PCS_filtre <- df_PCS |> dplyr::filter(Origine_sociale!="Ensemble")
df_PCS

baccalaureat_cadre <- df_PCS |> 
  dplyr::filter(Origine_sociale=='Cadres, professions intellectuelles superieures')
baccalaureat_cadre

baccalaureat_sans_emploi <-df_PCS |> 
  dplyr::filter(Origine_sociale=='Autres personnes sans activite professionnelle')
baccalaureat_sans_emploi

baccalaureat <- round(sum(df_PCS$Pct_admis_baccalaureat)/nrow(df_PCS))
baccalaureat


# Taux de réussite DNB selon le secteur (etablissement)
taux_reussite_public <- fr_dnb_etablissement |> 
  select(`Secteur d'enseignement`,Admis,Inscrits) |> 
  dplyr::filter(`Secteur d'enseignement`=="PUBLIC") |> 
  summarise(moy_reussite_public = round(mean(Admis/Inscrits),3))
colnames(taux_reussite_public) <- "Collèges publics"

taux_reussite_prive <- fr_dnb_etablissement |> 
  select(`Secteur d'enseignement`,Admis,Inscrits) |> 
  dplyr::filter(`Secteur d'enseignement`=="PRIVE") |> 
  summarise(moy_reussite_prive= round(mean(Admis/Inscrits),3))
colnames(taux_reussite_prive) <- "Collèges privés"

taux_reussite_secteur <- cbind(taux_reussite_prive,taux_reussite_public)
taux_reussite_secteur


# Comparaison des PCS entre le college prive et public
PCS <- c("Tres favorise","favorise","moyenne","defavorise")
val_college_PU <- c(mean(fr_indicateur_segreg_college$proportion_tfav_PU),
                    mean(fr_indicateur_segreg_college$proportion_fav_PU),
                    mean(fr_indicateur_segreg_college$proportion_moy_PU),
                    mean(fr_indicateur_segreg_college$proportion_defav_PU))
val_college_PR <- c(mean(fr_indicateur_segreg_college$proportion_tfav_PR),
                    mean(fr_indicateur_segreg_college$proportion_fav_PR),
                    mean(fr_indicateur_segreg_college$proportion_moy_PR),
                    mean(fr_indicateur_segreg_college$proportion_defav_PR))

df_comparaison_pcs_PU_PR <- data.frame(PCS, college_prive=val_college_PR,college_public=val_college_PU)
comp_college_PU_PR <- amBarplot(x = "PCS", y = c("college_prive", "college_public"),groups_color = c("#87cefa", "#c7158"), legend=TRUE,data = df_comparaison_pcs_PU_PR,title="Comparaison des PCS entre le collège prive et public")





### Inégalité territoriale

# ---- Carte : Taux de réussite DNB par département -----
dpt <- sf::read_sf("data/dpt")
# Jointure entre dpt et fr_dnb_etablissement pour récupérer les multipolygons associés aux dpt
dpt2 <- merge(x=fr_dnb_etablissement,y=dpt,by.x="Libellé_département",by.y="NOM_DEPT")
dpt2
# VOIR AVEC INNER JOIN
dpt3 <- dpt2 |>
  select(Session,`Code département`,Inscrits,Admis,geometry) |>
  mutate(reussite = Admis/Inscrits*100)


# ---- Carte : PCS majoritaire selon le département ------
# fichier fr_segregation_sociale
# colone proportiontfav, proportion_fav, proportion_fav et proportion_defav
# Essai avec une seule année (pas de choix année)
# pour chaque dpt :
# max entre proportiontfav proportion_fav proportion_fav proportion_defav
# représenter 
# Carte interactive leaflet
dpt_pcs_maj <- merge(x=fr_indicateur_segreg_college,y=dpt, by.x = "nom_dep", by.y = "NOM_DEPT")

dpt_pcs_maj2 <- dpt_pcs_maj |> 
  select(nom_dep,annee,proportion_tfav,proportion_fav,proportion_moy, proportion_defav,geometry) |> 
  group_by(nom_dep,annee) |> 
  pivot_longer(cols=c(proportion_tfav,proportion_fav,proportion_moy, proportion_defav),
               names_to = "Classe_sociale",
               values_to = "Valeur") |> 
  filter(Valeur == max(Valeur, na.rm=TRUE))

carte_pcs <- leaflet() |> 
  addTiles() |> 
  setView(lat = 46.2276, lng = 2.2137, zoom = 5)


# Carte Taux de scolarisation en france
regions <- read_sf("data/regions-20180101-shp/")
regions1 <- ms_simplify(regions)
regions2 <- merge(x=regions1,y=fr_taux_scolarisation_reg,by.x="code_insee",by.y = "Numero")
format(object.size(regions2),units="Mb")

carte_tx_scolarisation <- ggplot(regions2)+geom_sf()+
  geom_sf(aes(fill=`Total premier degre`))+
  coord_sf(xlim = c(-5.5,10),ylim=c(41,51))+
  scale_fill_continuous(low="yellow",high="red")+
  labs(title = "Taux de scolarisation (en maternelle et primaire) en France")+
  theme_void()


# Voies professionnelles des élèves
df_voies <- data.frame(Sexe=fr_bac_academie$Sexe,Voie=fr_bac_academie$Voie)
table(df_voies)
df <- data.frame(
  Sexe=c("Filles","Garcons"),
  Bac_General = c(89,89),
  Bac_Professionnel = c(2847,3894),
  Bac_Technologique = c(714,774)
)

df_voies_professionnelles <- df |> pivot_longer(cols=c(Bac_General, Bac_Professionnel,Bac_Technologique),
                          names_to = "Bac",
                          values_to = "Valeur")
voies <- ggplot(df_voies_professionnelles) +
  aes(x=Sexe, y=Valeur, fill=Bac) +
  geom_bar(stat="identity", position = "dodge")+
  labs(x="Sexe",y="Nombre d'eleves",title="Nombre d'eleves selon la voie professionnelle en 2021")+
  scale_fill_brewer(palette="Blues")+
  theme(plot.title = element_text(hjust = 0.45))


# Commentaires graphiques ---

# Reussite bac selon PCS selon secteur au college
commg_amchartComparaisonPCS <- HTML("Ce graphique nous permet de distinguer la répartition des PCS selon le secteur d'enseignement.
Nous pouvons directement nous rendre compte des disparités sociales entre les collèges puisque la classe sociale majoritaire dans les collèges publics est défavorisée alors que dans ceux privés, elle correspond à une classe aisée.")

global_comparaison_evol_enseignant_eleves <- HTML("Ces deux graphiques permettent la comparaison entre deux pays.")
