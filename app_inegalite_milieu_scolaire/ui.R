# library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(plotly)

shinyUI(
  dashboardPage(
    dashboardHeader(title=HTML("Les inégalités dans le milieu scolaire"),# Titre de l'application
                    disable= FALSE,
                    titleWidth = 380,
                    tags$li(class="dropdown",tags$a(href="https://twitter.com/education_gouv", icon("twitter"), "Twitter", style="color:white")),
                    tags$li(class="dropdown",tags$a(href="https://www.education.gouv.fr/",  "Site Officiel", style="color:white")),
                    tags$li(class="dropdown",tags$a(href="https://www.instagram.com/education_gouv/?hl=fr",icon("instagram") , "Instagram", style="color:white")),
                    dropdownMenu(type="message", messageItem(from="Notification",message="Bienvenue sur notre application WEB!",icon=icon("envelope-open")))
    ), 
    dashboardSidebar( # Menu de l'application
      width = 220,
      sidebarMenu(
        menuItem("Accueil", tabName = "Accueil", icon = icon("home")),
        menuItem(HTML("Inégalités \n socio-économiques"), tabName = "social", icon = icon("money-bills")),
        menuItem("Inégalités territoriales",tabName = "geo", icon = icon("flag")),
        menuItem("Inégalités de genre", tabName = "genre",icon=icon("venus-mars")),
        menuItem("Bases de données",tabName = "BDD",icon = icon("database"))
      )),
    
    dashboardBody(
      shinyDashboardThemes(
        theme = "grey_light"
      ),
      tabItems(
        tabItem(tabName = "Accueil",
                tabsetPanel(
                  tabPanel("Informations générales",
                           fluidRow(style = 'margin:6px;'),
                           fluidRow(
                             box(title = "L'éducation nationale", status = "primary", solidHeader = TRUE,
                                 p("L'Éducation nationale en France est le ministère chargé de l'organisation et de la gestion du système éducatif national. 
                                        Il est responsable de la politique éducative, de la mise en œuvre des programmes scolaires, de la formation des enseignants et de la gestion des établissements scolaires publics.
                                        Le ministère de l'Éducation nationale est dirigé par un ministre nommé par le Président de la République, sur proposition du Premier ministre. 
                                        Le ministre est assisté d'un secrétaire d'État chargé de l'Enseignement supérieur et de la Recherche.
                                        L'Éducation nationale en France est chargée de garantir l'égalité des chances pour tous les élèves et de promouvoir la réussite scolaire."
                                 )),
                             valueBoxOutput("educ_nationale")
                             ), # ferme le fluidRox
                           fluidRow(
                             box(title = "Les inégalités dans le milieu scolaire",status = "warning",solidHeader = TRUE,collapsible = TRUE,
                                 HTML("Ces inégalités peuvent prendre différentes formes : 
                                    <ul><li>Inégalités socio-économiques : Les élèves issus de milieux défavorisés ont souvent moins accès aux ressources et aux opportunités éducatives que les élèves issus de milieux plus aisés.</li>
                                    <li>Inégalités territoriales : Les établissements scolaires ne sont pas tous égaux en termes de qualité des infrastructures, de la taille des classes, des programmes éducatifs proposés, de la qualité des enseignants.</li>
                                    <li>Inégalités de genre : Les filles et les garçons ne bénéficient pas toujours des mêmes opportunités éducatives. </li>
                                    <li>Inégalités liées aux origines culturelles et linguistiques : Les élèves issus de l'immigration peuvent rencontrer des difficultés linguistiques et culturelles, qui peuvent nuire à leur réussite scolaire.</li></ul>
                                    ")
                             ),
                             box(title = "Qu'est ce que l'OCDE?", status = "info", solidHeader = TRUE, collapsible = TRUE,
                                 p("L'OCDE (Organisation de Coopération et de Développement Économiques) est une organisation internationale qui rassemble 38 pays membres, dont la plupart sont des pays développés.
                                      Les pays membres de l'OCDE représentent ensemble plus de 60% de l'économie mondiale et ils collaborent sur de nombreux sujets économiques, sociaux et environnementaux pour favoriser la croissance et le développement durable.
                                      ")
                             )
                             ) # ferme le fluidRow
                             ), # ferme le tabPanel Informations générales
                  tabPanel("Les grandes lois sur l'école",
                           fluidRow(style='margin:6px;'),
                           fluidRow(
                             valueBoxOutput("loi_Falloux"),
                             valueBoxOutput("progres_filles"),
                             valueBoxOutput("gratuite")
                           ),
                           fluidRow(
                             valueBoxOutput("sep_eglise_etat"),
                             valueBoxOutput("filles_bac"),
                             valueBoxOutput("prog_identique_F_G")
                           ),
                           fluidRow(
                             valueBoxOutput("ecole_obl_16"),
                             valueBoxOutput("mixite"),
                             valueBoxOutput("creation_bac_pro")
                           )
                           ),# ferme le tabPanel les grandes lois sur l'écoles
                  tabPanel("Personnages importants (dans les pays de l'OCDE)",
                           fluidRow(style='margin:6px;'),
                           fluidRow(
                             box(title="En France",h1("Jules FERRY"),HTML("Il était ministre de l'instruction Publique en 1879.\n Il a permis la gratuité de l'enseignement public"),fluidRow(style="margin:6px;"),imageOutput("Jules_FERRY"),status = "primary"),
                             box(title="En Allemagne",h1("Martin LUTHER"),HTML("Martin Luther a grandement contribué à l'éducation en Allemagne en promouvant l'enseignement primaire et en créant des écoles pour tous les enfants, surtout les filles"),fluidRow(style="margin:6px;"),imageOutput("Martin_LUTHER"),status="success")
                           ), # ferme le fluidRow
                           fluidRow(
                             box(title ="En Espagne",h1("Francisco Giner de los Ríos"),HTML("Francisco Giner de los Ríos était un pédagogue et intellectuel espagnol du XIXe siècle, connu pour être le fondateur de l'Institution Libre d'Enseignement (ILE), une institution éducative novatrice qui a eu une grande influence sur l'éducation en Espagne."),
                                 fluidRow(style="margin:6px;"),imageOutput("Francisco_Giner_de_los_Ríos"),status="warning"),
                             box(title= "En Turquie", h1("Mustafa Kemal Atatürk"),HTML("Mustafa Kemal Atatürk était un dirigeant politique turc qui a joué un rôle crucial dans la fondation de la République de Turquie en 1923. 
                                                                                       Il a dirigé le pays pendant près de 20 ans en tant que président et a permis de nombreux avancement dans de nombreux domaines, notamment l'éducation, l'économie, la politique et la culture."),
                                 fluidRow(style="margin:6px;"),imageOutput("Mustafa_Kemal_Atatürk"),status = "info")
                           ),
                           fluidRow(
                             box(title = "Au Japon", HTML("<u>Deux principaux acteurs :</u>"), h2("Gouvernement Japonais"),HTML("Le gouvernement japonais a fait de l'éducation une priorité nationale et a mis en place un système éducatif solide pour les générations futures."),
                                h2("Mori Arinori"),HTML("Mori Arinori a été l'un des premiers réformateurs de l'éducation sous le gouvernement Meiji. 
                                                                                                                                                                                                                                                                                                   Il a mis en place des réformes éducatives majeures, notamment la mise en place d'un système d'éducation nationale obligatoire pour tous les enfants, l'expansion de l'éducation pour les filles et la création de nombreuses écoles professionnelles. 
                                                                                                                                                                                                                                                                                                   Il a également joué un rôle clé dans la création de l'Université impériale de Tokyo, l'une des premières universités de recherche modernes du Japon."),
                                 fluidRow(style="margin:6px;"),
                                 imageOutput("Mori_Arinori"),status = "danger"),
                             box(title = "Au Canada",p("L'éducation au Canada est gérée au niveau provincial et territorial plutôt qu'au niveau fédéral. Cela signifie que chaque province et territoire a mis en place son propre système d'éducation, et que l'histoire et l'évolution de l'éducation nationale varient d'une région à l'autre."))
                           )
                           ), # ferme le tabPanel Personnages importants
                  tabPanel("Pour en savoir plus",
                           fluidPage(HTML("<h1 style=\"color : white ; background-color : lightgrey ; text-align : center ; border-radius : 10px\"> Une petite vidéo sur le sujet des inégalités </h1>"), 
                                     HTML('<p align="center"><iframe width="560" height="315" src="https://www.youtube.com/embed/od5i5s3ghPw" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe></p>'),
                                     HTML("<h1 style=\"color : white ; background-color : lightgrey ; text-align : center ; border-radius : 10px\"> Contribuez à résoudre les inégalités dans le milieu scolaire en faisant un don à cette <a href='https://afev.org/'>association</a> ! </h1>")
                           )
                    
                  ) #ferme le tabPanel pour en savoir plus
                  ) # ferme le tabSetPanel
                ), # ferme le tabItem Accueil
                
                tabItem("social",
                        sidebarLayout(
                          sidebarPanel(
                            radioButtons(inputId="annee",label = "Choisissez une année :",choices=c(2014,2015,2016,2017,2018,2019,2020,2021)),
                            selectizeInput("origine_sociale",label="Origine Sociale",
                                           choices = list("Agriculteurs exploitants","Artisans, commerçants, chefs d'entreprise","Autres personnes sans activité professionnelle",
                                                          "Cadres, professions intellectuelles supérieures","Cadres, professions intellectuelles supérieures : professeurs et assimilés",
                                                          "Employés","Indéterminé","Ouvriers","Professions intermédiaires","Retraités")),
                            width = 3
                          ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Origines sociales",
                                     style='margin:6px;',
                                     HTML("<h1 style=\"color : white ; background-color : lightgrey ; text-align : center ; border-radius : 10px\">L'impact de l'origine sociale sur la scolarité</h1>"),
                                     box(comm_onglet_socio_economique,width = 300),
                                     
                                     HTML("<h2 style=\"color : white ; background-color : lightgrey ; text-align : center ; border-radius : 10px\">Des chiffres clés</h2>"),
                                     fluidRow(
                                       valueBoxOutput("bac_origine_sociale") ,
                                       valueBoxOutput("bac_sans_emploi") ,
                                       valueBoxOutput("bac_cadre")
                                     ),
                                     selectizeInput("nom_departement",label="Choisissez un département :",
                                                    choices=list("AIN","AISNE","ALLIER","ALPES-DE-HTE-PROVENCE","ALPES-MARITIMES","ARDECHE","ARDENNES","ARIEGE","AUBE","AUDE","AVEYRON","BAS-RHIN","BOUCHES-DU-RHONE","CALVADOS","CANTAL","CHARENTE","CHARENTE-MARITIME",
                                                                 "CHER","CORREZE","CORSE-DU-SUD", "COTE D'OR","COTES D'ARMOR","CREUSE","DEUX-SEVRES","DORDOGNE","DOUBS","DROME","Ensemble de l'acadÃ©mie","ESSONNE",
                                                                 "EURE","EURE-ET-LOIR","FINISTERE","GARD","GERS" ,"GIRONDE","GUADELOUPE","GUYANE","HAUT-RHIN","HAUTE-CORSE" ,"HAUTE-GARONNE","HAUTE-LOIRE","HAUTE-MARNE","HAUTE-SAONE","HAUTE-VIENNE","HAUTE SAVOIE","HAUTES-ALPES","HAUTES-PYRENEES","HAUTS-DE-SEINE","HERAULT","ILLE-ET-VILAINE" ,"INDRE","INDRE-ET-LOIRE","ISERE","JURA","LA REUNION","LANDES","LOIR-ET-CHER","LOIRE","LOIRE-ATLANTIQUE","LOIRET","LOT","LOT-ET-GARONNE","LOZERE","MAINE-ET-LOIRE","MANCHE","MARNE","MARTINIQUE", "MAYENNE","MAYOTTE","MEURTHE-ET-MOSELLE","MEUSE","MORBIHAN","MOSELLE","NIEVRE","NORD","OISE","ORNE","PARIS","PAS-DE-CALAIS","PUY-DE-DOME","PYRENEES-ATLANTIQUES","PYRENEES-ORIENTALES","RHONE","SAONE-ET-LOIRE","SARTHE","SAVOIE","SEINE-ET-MARNE","SEINE-SAINT-DENIS","SEINE MARITIME","SOMME","TARN","TARN-ET-GARONNE","TERRITOIRE DE BELFORT","VAL-D'OISE","VAL-DE-MARNE","VAR","VAUCLUSE","VENDEE","VIENNE","VOSGES","YONNE","YVELINES")
                                     ),
                                     fluidRow(style="margin:6px;",
                                              withSpinner(
                                                plotOutput("treemap_college"),
                                                type = 1)),
                                     fluidRow(style="margin:6px;",
                                              box(commg_treemap_college,width=300)
                                              ),
                                     fluidRow(style="margin:6px;",
                                              withSpinner(
                                                plotlyOutput("camembert_lycee"),
                                                type = 1)),
                                    fluidRow(style="margin:6px;",
                                             box(commg_camembert_lycee,width=300)
                                              ),
                                     # Modifier la mise en page de ce graphique : 
                                     fluidRow(style="margin:6px;",
                                              withSpinner(
                                                plotOutput("reussite_bac_PCS"), 
                                                type = 1)),
                                    fluidRow(style="margin:6px;",
                                             box(commg_reussite_bac_PCS,width=300)
                                    )
                                     
                            ), # ferme tabPanel Origine sociale
                            tabPanel("Privé ou public ?",
                                     style='margin:6px;',
                                     HTML("<h1 style=\"color : white ; background-color : lightgrey ; text-align : center ; border-radius : 10px\">L'impact du privé et du public sur la scolarité</h1>"),
                                     
                                     HTML("<h2 style=\"color : white ; background-color : lightgrey ; text-align : center ; border-radius : 10px\">Taux de réussite au Diplôme National du Brevet</h2>"),
                                     fluidRow(style="margin:6px;",
                                              withSpinner(
                                                dataTableOutput("reussite_secteur"),
                                                type = 1)),
                                     fluidRow(style="margin:6px;",
                                              box(commg_reussite_secteur,width = 300)),
                                     
                                     HTML("<h2 style=\"color : white ; background-color : lightgrey ; text-align : center ; border-radius : 10px\">Professions et Catégories Sociales selon le secteur d'enseignement</h2>"),
                                     fluidRow(style="margin:6px;",
                                              withSpinner(
                                                amChartsOutput("amchartComparaisonPCS"),
                                                type = 1)),
                                     fluidRow(style="margin:6px;",
                                              box(commg_amchartComparaisonPCS,width = 300))
                            )
                            
                          )
                        ) # ferme le mainPanel
                        ) # ferme le sidebarLayout
                        ), # ferme le tabItem Accueil
        
        ### Inégalités territoriales
                tabItem("geo",
                        fluidPage(
                          HTML("<h1 style=\"color : white ; background-color : lightgrey ; text-align : center ; border-radius : 10px\"> Les inégalités territoriales </h1>"), 
                          tabsetPanel(
                            tabPanel("Condition d'apprentissage", # evolution du nombre d'enseignant par élève et le taux reussite
                                     fluidRow(style='margin:6px;',
                                              column(width = 6,selectizeInput("Pays_enseignant",label="Pays",
                                                      choices = list("Australie","Autriche","Belgique","Brésil","Canada","Suisse","Chili","Colombie","Costa Rica","République Tchèque","Allemagne","Danemark","Espagne","Estonie","Finlande",
                                                                     "France","G20","Royaume-Uni","Grèce","Hongrie","Irlande","Islande","Israël","Italie","Japon","Corée du Sud","Lituanie","Luxembourg","Lettonie","Mexique",
                                                                     "Pays-Bas","Norvège","Nouvelle-Zélande","OAVG","Pologne","Portugal","Slovaquie","Slovénie","Suède","Turquie","USA")
                                                      )),
                                              column(width = 6,selectizeInput("Pays_enseignant2",label="Pays",
                                                      choices = list("Australie","Autriche","Belgique","Brésil","Canada","Suisse","Chili","Colombie","Costa Rica","République Tchèque","Allemagne","Danemark","Espagne","Estonie","Finlande",
                                                                     "France","G20","Royaume-Uni","Grèce","Hongrie","Irlande","Islande","Israël","Italie","Japon","Corée du Sud","Lituanie","Luxembourg","Lettonie","Mexique",
                                                                     "Pays-Bas","Norvège","Nouvelle-Zélande","OAVG","Pologne","Portugal","Slovaquie","Slovénie","Suède","Turquie","USA")
                                                      ))
                                       
                                     ),
                                     fluidRow(style='margin:6px;',
                                              box(comm_onglet_territoriales,width=300)),
                                     fluidRow(style='margin:6px;',
                                       verbatimTextOutput("comparaison_evol_enseignant_eleves"),
                                       box(title="Evolution nombre d'enseignant par élèves",
                                           withSpinner(
                                             plotOutput("evol_enseignant_eleves"),
                                             type = 1)
                                       ),
                                       box(title = "Evolution du nombre d'enseignant par élèves",
                                           withSpinner(
                                             plotOutput("evol_enseignant_eleves_2"),
                                             type = 1)
                                       )
                                     ),
                                     fluidRow(style='margin:6px;',
                                              box(commg_evol_enseignant_eleves,width=300)),
                                              
                                     box(title = "Carte du nombre d'élèves",status = "primary",width ="50000px",solidheader = TRUE,
                                         radioButtons(inputId="annee_carte",label = "Choisissez une année :",choices=c(2014,2015,2016,2017,2018,2019,2020,2021)),
                                         fluidRow(style="margin:6px;")
                                         # plotOutput("carte_evol_enseignant")
                                     )
                            ), # ferme tabPanel
                            tabPanel("Taux de réussite DNB par département",
                                     fluidRow(style="margin:6px;",
                                              radioButtons(inputId="annee_geo",label = "Choisissez une année",inline = TRUE,choices=c(2014,2015,2016,2017,2018,2019,2020,2021)),
                                              withSpinner(
                                                plotOutput("carte_reussite_DNB"),
                                                type = 1)
                                              ),
                                     fluidRow(style="margin:6px",
                                              box(commg_carte_reussite_DNB,width=300))
                            ),
                            
                            tabPanel("PCS majoritaire par département",
                                     fluidRow(style="margin:6px;",
                                              withSpinner(
                                                leafletOutput("map_pcs_dpt"),
                                                type = 1)
                                     )
                                     # fluidRow(style="margin:6px;",
                                     #          box(commg_map_pcs_dpt,width=300))
                            ),
                            
                            tabPanel("Répartition des élèves en France",
                                     fluidRow(style="margin:6px;",
                                              HTML("<h2 style=\"color : white ; background-color : lightgrey ; text-align : center ; border-radius : 10px\">Répartition des enfants scolarisés selon les régions en France</h2>"),
                                              box(status = "primary",width ="50000px",solidheader = TRUE, 
                                                  withSpinner(
                                                    plotOutput("taux_scolarisation_FR"),
                                                    type = 1))
                                     ),
                                     fluidRow(style="margin:6px;",
                                              box(commg_taux_scolarisation_FR,width=300))
                            ),
                            
                            tabPanel("Etudiants en mobilité internationale",
                                     selectizeInput("Pays_mobilite",label="Pays",
                                                    choices = list("Australie","Autriche","Belgique","Brésil","Canada","Suisse","Chili","Colombie","Costa Rica","République Tchèque","Allemagne","Danemark","Espagne","Estonie","Finlande",
                                                                   "France","Royaume-Uni","Grèce","Hongrie","Irlande","Islande","Israël","Italie","Japon","Corée du Sud","Lituanie","Luxembourg","Lettonie","Mexique",
                                                                   "Pays-Bas","Norvège","Nouvelle-Zélande","OAVG","OEU","Pologne","Portugal","Slovaquie","Slovénie","Suède","Turquie","USA")),
                            fluidRow(style="margin:6px;",
                                     withSpinner(
                                       # plotOutput("mobilite"),
                                       amChartsOutput("mobilite"),
                                       type = 1)),
                            fluidRow(style="margin:6px;",
                                     box(commg_mobilite,width=300))
                            )
                          
                          )
                        )
                        ), # ferme le tabItem géo 
        # Inégalités de genre
        tabItem(tabName = "genre",
                fluidPage(
                  HTML("<h1 style=\"color : white ; background-color : lightgrey ; text-align : center ; border-radius : 10px\"> Les inégalités de genre </h1>"), 
                  tabsetPanel(
                    tabPanel("Répartition des bacs",
                             fluidRow(style="margin:6px;",
                                      box(comm_onglet_genre,width=300)),
                             fluidRow(style='margin:6px;',
                             box(title = "Répartition des baccalauréats selon le genre des élèves",status = "primary",width ="50000px",solidheader = TRUE, 
                                 withSpinner(
                                   plotOutput("repartition_bac"),
                                   type = 1))
                    ),
                    fluidRow(style="margin:6px;",
                             box(commg_repartition_bac,width=300))
                  )
                )
                )
                
        ), # ferme tabItem genre
        tabItem(tabName = "BDD",
                fluidPage(
                  HTML("<h1 style=\"color : white ; background-color : lightgrey ; text-align : center ; border-radius : 10px\"> Nos bases de données </h1>"),
                  selectizeInput(inputId = "affichage_table", label = "Choisissez une table à afficher",
                                 choices = names(liste_df)),
                  dataTableOutput("table")
                )
        )
        )
      )
  ) # ferme le dashboarpage
) #ferme le ui