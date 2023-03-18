# library(shiny)
library(shinydashboard)

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
                           )# ferme le tabPanel les grandes lois sur l'écoles
                  ) # ferme le tabSetPanel
                ), # ferme le tabItem Acceuil
                
                tabItem("social"),
                tabItem("geo"),
                tabItem("genre"),
                tabItem("BDD"))
      )
  ) # ferme le dashboarpage
) #ferme le ui