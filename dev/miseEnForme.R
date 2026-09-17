pasDeBase <-   fluidPage(   
h4("Aucune base n'a été chargée en mémoire, cet onglet n'est pas accessible." ),
p("Pour charger une base de données, rendez-vous sur l'onglet « Base de Données » dans la barre latérale.")
)


###################################################
#####   PAGE 3      ###############################
###################################################
univarie <- fluidPage(navbarPage(title = NULL,id="descriptif",
            
            tabPanel("Informations BDD",
                     fluidPage(
                       titlePanel("Informations sur la base de données"),
                       
                       navlistPanel(
                         "Menu",
                         tabPanel("Informations brutes", 

                                  fluidRow(
                                    splitLayout(cellWidths = c("30%","70%"),
                                                downloadButton(ns('PDFdescriptif1o1'),label="AIDE et Détails",class = "butt")
                                    )
                                  ),#finFluidRow
                                  tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
                                  verbatimTextOutput (ns("tableauBASE")),
                                  plotOutput(ns('plotNAbase1'))),
                         tabPanel("Données manquantes cumulées par variable",
                                  fluidRow(
                                    splitLayout(cellWidths = c("30%","70%"),
                                                downloadButton(ns('PDFdescriptif1o2'),label="AIDE et Détails",class = "butt"),
                                            h4("Faites attention s'il y a un filtre")
                                    )
                                  ),#finFluidRow
                                  
                                  tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
                                  h4("Descriptif cumulé des données manquantes par variable",align="center"),
                                  p("On représente ci-dessous les données manquantes en proportions par variable étudiée."),
                                  plotOutput(ns('plotNAbase2')),
                                  tableOutput(ns("tableNAbase2"))),
                         tabPanel("Données manquantes cumulées par sujet",
                                  fluidRow(
                                    splitLayout(cellWidths = c("30%","70%"),
                                                downloadButton(ns('PDFdescriptif1o3'),label="AIDE et Détails",class = "butt"),
                                            h4("Faites attention s'il y a un filtre")
                                    )
                                  ),#finFluidRow
                                  
                                  tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
                                  h4("Descriptif cumulé des données manquantes par sujet",align="center"),
                                  p("On représente ci-dessous les données manquantes en proportions par sujet d'étude."),
                                  plotOutput(ns('plotNAbase3')),
                                  tableOutput(ns("tableNAbase3")))
                       )# fin navlistpanel
                       
                     ) # fin fluipage        
                     
            ),# fin tabpanel
            
            
            
            
            ###################################################
            #####   PAGE 3.2     ###############################
            ###################################################
            
            
            
            tabPanel("Descriptif univarié",
                     fluidPage(    
                       titlePanel("Analyses descriptives"),
                       sidebarLayout( 
                         sidebarPanel(                              
                           uiOutput(ns("propositions")),
                           radioButtons(ns('qualiquanti'), "Nature de la variable",
                                        c(Quantitative='quant', Qualitative='qual'),'qual'
                           )
                         ),
                         # Create a spot for the barplot
                         mainPanel( 
                           # fluidRow(
                           #   # splitLayout(cellWidths = c("30%","70%"), 
                           #   #             downloadButton('PDFdescriptif2',label="AIDE et Détails",class = "butt"),
                           #   #         h4("Faites attention s'il y a un filtre")  
                           #   # )
                           # ),#finFluidRow
                           
                           tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
                           fluidRow(
                             column(6,   textOutput(ns("descriptifUni")),br(),  tableOutput(ns("descvar"))),
                             column(6,     plotOutput(ns('plot1')) , plotOutput(ns('plot2')) )
                           )# fin fluid row du main panel 
                           
                         )# fin MainPanel
                         
                       )# fin sidebarlayout
                     )# fin fluidpage
            )# fin tabPanel 2
            
)# fin navbarPage

)

#######################################################################
#####   PAGE 4 CROISEMENTS INFERENCE   ###############################
####################################################################### 


CroisementsInference<-
         fluidPage(navbarPage(id="Panel 2.x",title = NULL,
                     
                     tabPanel("Croisement 2 à 2",
  
  
  fluidPage( includeCSS("./www/tables.css"),   
    titlePanel("Analyses descriptives croisées"),
    sidebarLayout( 
      sidebarPanel(
        
        uiOutput(ns("propositionsCROISE1")),
        radioButtons(ns('qualiquantiCROISE1'), 
                     "Nature de la variable",
                     c(Quantitative='quant', Qualitative='qual'),
                     'quant'),
        uiOutput(ns("propositionsCROISE2")),
        radioButtons(ns('qualiquantiCROISE2'), 
                     "Nature de la variable",
                     c(Quantitative='quant', Qualitative='qual'),
                     'quant'),
        conditionalPanel(
          condition = "input.qualiquantiCROISE1 == 'qual' && input.qualiquantiCROISE2 == 'qual'",
          radioButtons(ns('NATableau'), 
                       "Afficher les données manquante",
                       c(Non="no", Oui='always'),
                      "no"))
       
        
        
      ),
      mainPanel(  
        fluidRow(
        splitLayout(cellWidths = c("30%","70%"), 
                    downloadButton(ns('PDFcroisements'),label="AIDE et Détails",class = "butt"),
                h4("Faites attention s'il y a un filtre")  
        )
      ),#finFluidRow
      
      tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
      h3("Représentation graphique du lien entre les deux variables"),
      plotOutput(ns('plotCROISE' )),
      # debut conditionnal panel QualiQuali
      conditionalPanel(
        condition = "input.qualiquantiCROISE1 == 'qual' && input.qualiquantiCROISE2 == 'qual'",
        h3("Tableau croisé",align = "left",style = "color:#08088A"),
        tableOutput(ns("montableauCroisAUTO")),br(),
        tableOutput(ns("montableauCroise2AUTO")),
        tableOutput(ns("montableauCroise3AUTO")),
        h3("Tests d'association / Comparaison des proportions",align = "left",style = "color:#08088A"),
        fluidRow(
          splitLayout(cellWidths = c("50%","50%"), 
                      tableOutput(ns('AUTOtableCHI2')), 
                      tableOutput(ns('AUTOtableFISHER'))
          )
        ),
        textOutput(ns('AUTOCHI2conditions')),
        h3("Rapport de cotes",align = "left",style = "color:#08088A"),
        tableOutput(ns('oddratioAUTO'))
      ),# fin panelQualiQuali,
      # debut conditionnal panel QuantiQuali
      conditionalPanel(
        condition = "input.qualiquantiCROISE1 != input.qualiquantiCROISE2",
        h3("Descriptif complet",align = "left",style = "color:#08088A"),
        tableOutput(ns('descr3DESCRIPTIF')),
        h3("Tests de comparaisons:",align = "left",style = "color:#08088A"),
        verbatimTextOutput (ns("descr3TestNormalite")),
        verbatimTextOutput (ns("descr3Testpv")),
        verbatimTextOutput (ns("descr3TestsNPv")),
        verbatimTextOutput (ns("descr3Tests_de_Student")),
        verbatimTextOutput(ns("descr3TestsMANN")),
        verbatimTextOutput (ns("ChoixSortieCROISE"))
      ), # fin Panel Quali Quanti
      # debut conditionnal panel QuantiQuanti
      conditionalPanel(
        condition = "input.qualiquantiCROISE1 == 'quant' && input.qualiquantiCROISE2 == 'quant'",
        h3("Corrélation entre deux variables quantitatives",align = "left",style = "color:#08088A"),
        verbatimTextOutput (ns("CorrelationCROISE"))
      ),# fin panelQuantiQuali,
      plotOutput(ns('plotCROISE2'))
      )# fin MainPanel
      
    )# fin sidebarlayout
  ))# fin fluidpage
  ,
  
  tabPanel("Tableau croisement",

           
           
           fluidPage(    
             titlePanel("Analyses descriptives croisées"),
             sidebarLayout( 
               sidebarPanel(
                 # 
                 uiOutput(ns("propositionsTableauCROISE")),
                 uiOutput(ns("selectionVariablesCroisees1")),
                 uiOutput(ns("selectionVariablesCroisees3")),
                 uiOutput(ns("selectionVariablesCroisees2")),
                 radioButtons(ns("tableauCroiseSimpli"),"Tableau avec abréviation :"
                              , c( Oui = 1, Non = 0),0),
                 sliderInput(ns("nbDec"), "Nombre de decimales : ", min =0,
                             max = 5, value= 3, step = 1),
                 downloadButton(ns('downloadData'), 'Télécharger la table')
                 
                 
               ),
               mainPanel(    
               #   fluidRow(
               # 
               #                 downloadButton('PDFcroisements',label="AIDE et Détails",class = "butt")
               #  
               #      
               # ),#finFluidRow
               
               # tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
               h3("Tableau de comparaison de population"),
               conditionalPanel(condition = "!is.null(input$VariableCroisees)", tableOutput(ns('tableauCroisement')))
               
               )# fin MainPanel
               
             )# fin sidebarlayout
           )# fin fluidpage
  ) # fin tabPanel tableau Croisement                  
                     ) # fin tabset
         )

############################################################
#####  page 5 survie              ##########################
############################################################  

analyseDeSurvie<- 
  fluidPage(    
    titlePanel("Analyses de survie"),
    sidebarLayout( 
      sidebarPanel(
        p("Sélectionnez la variable quantitative représentant le délai jusqu'à survenue de l'évènement ou de censure."),
        uiOutput(ns("propositionsSURVIE1")),
        p("Sélectionnez la variable qualitative codée 0 si censure ou 1 si survenue de l'évènement."),
        uiOutput(ns("propositionsSURVIE2")),
        br(),
        br(),
        checkboxInput(ns("SURVIEcompar"), "Faire une comparaison inter-groupes"),
        conditionalPanel(
          condition = "input.SURVIEcompar",
          ns=ns,
          p("Sélectionnez la variable qualitative représentant les différents sous-groupes:"),
          uiOutput(ns("propositionsSURVIE3"))
        )# fin condi
        # if(!is.null(golem::get_golem_options("input$SURVIEcompar"))){
        #   #p("Sélectionnez la variable qualitative représentant les différents sous-groupes:")
        #   uiOutput(ns("propositionsSURVIE3"))
        # }
      )# fin sidebar panel
      ,
      
      mainPanel(   
        fluidRow(
          splitLayout(cellWidths = c("30%","70%"), 
                      downloadButton(ns('PDFsurvie'),label="AIDE et Détails",class = "butt"),
                     
                  h4("Faites attention s'il y a un filtre")  

          )
        ),#finFluidRow
        
        tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
        h3("Courbe(s) de Kaplan-Meier"),
        p("La courbe de survie associée aux variables selectionnées est présentée ci-dessous. Si aucune comparaison entre groupes n'est
                                                        effectuée, la courbe est présentée dans son intervalle de confiance à 95%. Si une comparaison est demandée, le graphique présente
                                                        la courbe de Kaplan-Meier dans chacun des groupes."),
        plotOutput(ns('plotSURVIE')),
        tags$br(),
        p("Le détail des données utilisées pour la construction de cette ou ces courbes est présenté ci-dessous. Dans le cas
                                                        d'une comparaison entre plusieurs groupes, le détail est présenté par groupes, un test d'égalité de l'ensemble des courbes est 
                                                        présenté (Test du Log-Rank) et les résultats sont affichés au bas de cette page."),
        h3("Valeurs numériques de survie: analyses détaillées"),
        verbatimTextOutput (ns("sortieSURVIE2")))# fin MainPanel
      
    )# fin sidebarlayout
  )# fin fluidpage


############################################################
#####  page 6 tests diagnostique  ##########################
############################################################  


testsDiagnostiques<- 
  fluidPage(    

navbarPage("",
            
            

            
            
            tabPanel("Réalisation d'un test diagnostique univarié",
                     
                     
                     
                     fluidPage(
                       title = 'Examples of DataTables',
                       sidebarLayout(
                         sidebarPanel(
                           p("Sélectionnez la variable qualitative codée 0 ou 1 à expliquer."),
                           uiOutput(ns("propositionsLOGIT1")),
                           p("Sélectionnez la variable quantitative explicative."),
                           uiOutput(ns("propositionsLOGIT2")),
                           br(),
                           br()                              
                           
                           
                         ), # fin sidebar panel
                        mainPanel(
                           fluidRow(
                             splitLayout(cellWidths = c("30%","70%"), 
                                         downloadButton(ns('PDFdiag'),label="AIDE et Détails",class = "butt"),
                                     h4("Faites attention s'il y a un filtre")  
                             )
                           ),#finFluidRow
                           
                           tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
                           navbarPage(title=NULL,
                              id='datasetlogit',
                             tabPanel('Variables sélectionnées',
                                      h3("Variables sélectionnées"),
                                      p("Les variables sélectionnées pour la réalisation du test diagnostique sont :"),
                                      tableOutput(ns('mytableLOGIT1'))
                             ),
                             tabPanel('Courbe ROC',
                                      p("La courbe ROC réalisée à partir des variables sélectionnées est présentée ci-dessous."),
                                      checkboxInput(ns("LOGIToptionsGRAPHIQUES"), "Je souhaite ajouter des options graphiques", FALSE),
                                      conditionalPanel(
                                        condition = "input.LOGIToptionsGRAPHIQUES",
                                        ns=ns,
                                        checkboxInput(ns("LOGIToptionsAUC"), "Afficher Aire sous la courbe sur le graphique", FALSE),
                                        checkboxInput(ns("LOGIToptionsSEUIL"), "Afficher seuil optimal sur le graphique", FALSE),
                                        checkboxInput(ns("LOGIToptionsIntervalle"), "Afficher intervalle de confiance courbe ROC", FALSE)
                                      ),
                                      h3("Courbe ROC associée"),
                                      plotOutput(ns('LogitROC')),
                                      br(),br(),
                                      h3("Meilleur seuil estimé par maximisation de l'indice de Youden"),
                                      tableOutput(ns('LogitROCtableauBEST')),
                                      br(),br(),
                                      h3("Détails des seuils utilisés pour construire la courbe"),
                                      tableOutput(ns('LogitROCtableau'))
                                      
                             ), # fin tab panel
                             tabPanel('Performances diagnostiques',
                                      h4("Le meilleur seuil (au sens défini précédemment) est estimé à:"),
                                      tableOutput(ns('LogitPERFtableauBEST')),br(),
                                      h4("Pour un tel seuil le tableau croisé devient:"),
                                      tableOutput(ns('LogitPERF3')),br(),
                                      h4("La sensibilité et la specificité sont:"),
                                      tableOutput(ns('LogitPERF1')),br(),
                                      h4("La critères de performances sont alors"),
                                      tableOutput(ns('LogitPERF2')),
                                      p("Attention, si l'évènement est associé à une mesure inférieure au cut, la lecture des VP, VN, FP, FN, VPP et VPN est inversée dans ce dernier tableau. 
                                                                             Il faut alors se référer au tableau à 4 cases sur le haut de cette page."),br()
                                      
                                      
                                      
                             )  
                           )# fin tab set panel
                         ) # fin main panel
                         ) # fin sidebarlayout
                       ) # fin fluipage   )     
                     ) # fin tabpanel
                     
            )
)




############################################################
#####  page 7 tests diagnostique    ########################
############################################################  
concordanceAvecBase<- 
fluidPage(    
  titlePanel("Analyse de concordance entre 2 lecteurs"),
  sidebarLayout( 
    sidebarPanel(
      # si saisie par choix des variables
      conditionalPanel(
        condition = "input.CONCORsaisie == false",
        ns=ns,
        p("Sélectionnez la variable associée à la mesure du premier lecteur"),
        uiOutput(ns("CONCORDANCElecture1")),
        br(),
        br(),
        p("Sélectionnez la variable associée à la mesure du deuxième lecteur"),
        uiOutput(ns("CONCORDANCElecture2")),
        br(),
        br()),
      # quel type de saisie souhaité   
      checkboxInput(ns("CONCORsaisie"), "Saisir les données des deux lecteurs manuellement"),
      # ajout de l'intervalle de confiance   
      checkboxInput(ns("CONCORinter"), "Ajouter l'intervalle de confiance (simulations, le calcul peut prendre plusieurs minutes)"),
      # si saisie manuelle   
      conditionalPanel(
        condition = "input.CONCORsaisie == true",
        ns=ns,
        p("Entrez les valeurs du lecteur 1, séparées par un espace."),
        textInput(ns("Concoman1"), label = "Valeurs du lecteur 1", value = ""),
        p("Entrez les valeurs du lecteur 2, séparées par un espace."),
        textInput(ns("Concoman2"), label = "Valeurs du lecteur 2", value = "")
      )# fin condi
    )# fin sidebar panel
    ,
    
    mainPanel(   
      fluidRow(
        splitLayout(cellWidths = c("30%","70%"), 
                    downloadButton(ns('PDFconcordance'),label="AIDE et Détails",class = "butt"),
                h4("Faites attention s'il y a un filtre")  
        )
      ),#finFluidRow
      
      tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
      h3("Tableau croisé"),
      p("On présente ci-dessous le tableau croisé des lectures réalisées :"),
      tableOutput(ns('mytableCONCORDANCE1')),br(),
      
      
      
      conditionalPanel(
        condition = "input.CONCORinter == true",
        ns=ns,
        verbatimTextOutput (ns("ConcordanceManuelleINTERV"))
      ),# fin condi 1
      conditionalPanel(
        condition = "input.CONCORinter == false",
        ns=ns,
        verbatimTextOutput (ns("ConcordanceManuelleSimple"))
      ),# fin condi 2
      p("Landis et Koch proposent l'interprétation suivante du coefficient Kappa de Cohen :"),
      tableOutput(ns('LandisEtKoch2'))
      
      
      
    )# fin MainPanel
    
  )# fin sidebarlayout
)# fin fluidpage



concordanceSansBase<- 
  fluidPage(    
    titlePanel("Analyse de concordance entre 2 lecteurs"),
    sidebarLayout( 
      sidebarPanel(
        p("Entrez les valeurs du lecteur 1, séparées par un espace"),
        textInput(ns("Concoman1"), label = "Valeurs du lecteur 1", value = ""),
        p("Entrez les valeurs du lecteur 2, séparées par un espace"),
        textInput(ns("Concoman2"), label = "Valeurs du lecteur 2", value = ""),
        checkboxInput(ns("CONCORinter"), "Ajouter l'intervalle de confiance (simulations, le calcul peut prendre plusieurs minutes)")       # si saisie manuelle   


      )# fin sidebar panel
      ,
      
      mainPanel(   
        fluidRow(
          splitLayout(cellWidths = c("30%","70%"), 
                      downloadButton(ns('PDFconcordance'),label="AIDE et Détails",class = "butt"),
                  h4("Faites attention s'il y a un filtre")  
          )
        ),#finFluidRow
        
        tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
        h3("Tableau croisé"),
        p("On présente ci-dessous le tableau croisé des lectures réalisées:"),
        tableOutput(ns('mytableCONCORDANCE2')),br(),
        
        
        
        conditionalPanel(
          condition = "input.CONCORinter == true",
          ns=ns,
          verbatimTextOutput (ns("ConcordanceManuelleINTERV2"))
        ),

        

        conditionalPanel(
          condition = "input.CONCORinter == false",
          ns=ns,
          verbatimTextOutput (ns("ConcordanceManuelleSimple2"))
        ),# fin condi 2
        p("Landis et Koch proposent l'interprétation suivante du coefficient Kappa de Cohen:"),
        tableOutput(ns('LandisEtKoch2'))
        
        
        
      )# fin MainPanel
      
    )# fin sidebarlayout
  )# fin fluidpage






