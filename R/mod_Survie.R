#' Survie UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Survie_ui <- function(id){
  ns <- NS(id)
  tagList(
  )
  uiOutput(ns('analyseDeSurvie'))
}
    
#' Survie Server Functions
#'
#' @noRd 
mod_Survie_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    pasDeBase <-   fluidPage(   
      h4("Aucune base n'a été chargée en mémoire, cet onglet n'est pas accessible." ),
      p("Pour charger une base de données, rendez-vous sur l'onglet « Base de Données » dans la barre latérale.")
    )
    
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
            

            h3("Courbe(s) de Kaplan-Meier"),
            p("La courbe de survie associée aux variables selectionnées est présentée ci-dessous. Si aucune comparaison entre groupes n'est
                                                        effectuée, la courbe est présentée dans son intervalle de confiance à 95%. Si une comparaison est demandée, le graphique présente
                                                        la courbe de Kaplan-Meier dans chacun des groupes."),
            plotOutput(ns('plotSURVIE')),
            downloadButton(ns('pngPLOT_SURVIE'), label = "Télécharger la courbe (PNG)", class = "butt"),
            tags$br(),
            p("Le détail des données utilisées pour la construction de cette ou ces courbes est présenté ci-dessous. Dans le cas
                                                        d'une comparaison entre plusieurs groupes, le détail est présenté par groupes, un test d'égalité de l'ensemble des courbes est 
                                                        présenté (Test du Log-Rank) et les résultats sont affichés au bas de cette page."),
            h3("Valeurs numériques de survie: analyses détaillées"),
            tableOutput(ns("sortieSURVIE2")),
            downloadButton(ns('csvSURVIE'), label = "Télécharger le tableau de survie (CSV)", class = "butt"))# fin MainPanel
          
        )# fin sidebarlayout
      )# fin fluidpage
    
    #source("./CodeSansDependance.R", local = TRUE)
    #source("./fonctions.R", local = TRUE)
    #source("./miseEnForme.R", local = TRUE)
    #eval(parse("./miseEnForme.R", encoding="UTF-8"))
    
    output$PDFsurvie = downloadHandler(
      filename    = '4_Survie.pdf',
      content     = function(file) file.copy(system.file("app/www/4_Survie.pdf", package = 'GmrcShinyStats'), file, overwrite = TRUE),
      contentType = 'application/pdf'
    ) 

      
      observe({
    output$analyseDeSurvie = renderUI({
      if(!r$BASEchargee) pasDeBase
      else analyseDeSurvie
      
      
    })
    output$propositionsSURVIE1 <- renderUI({
      
      selectInput(ns("variablesurvie1"), "Variable délai",   choices=r$noms) 
    })
    
    output$propositionsSURVIE2 <- renderUI({
      
      selectInput(ns("variablesurvie2"), "Variable évenement 0/1:",   choices=r$noms[r$nbModeVariable==2]) 
    })
    
    output$propositionsSURVIE3 <- renderUI({
      noms2    <-r$noms[r$nbModeVariable<30]
      selectInput(ns("variablesurvie3"), "Variable groupe",   choices=noms2) 
    })
    
    dessinerPLOT_SURVIE <- function() {
      base            <- r$BDD
      variablesurvie1 <- base[, colnames(base) == input$variablesurvie1]
      variablesurvie2 <- base[, colnames(base) == input$variablesurvie2]
      variablesurvie3 <- base[, colnames(base) == input$variablesurvie3]
      if (input$SURVIEcompar) {
        ggsurvie(variablesurvie1, variablesurvie2, variablesurvie3)
      } else {
        ggsurvie(variablesurvie1, variablesurvie2)
      }
    }
    output$plotSURVIE <- renderPlot({
      dessinerPLOT_SURVIE()
    })
    
    
    output$sortieSURVIE2<- renderTable({
      base    <-r$BDD
      variablesurvie1 <-base[,colnames(base)==input$variablesurvie1]
      variablesurvie2 <-base[,colnames(base)==input$variablesurvie2]
      variablesurvie3 <-base[,colnames(base)==input$variablesurvie3]
      if(input$SURVIEcompar){    tab_survie(variablesurvie1,variablesurvie2,variablesurvie3) }
      else{                       tab_survie(variablesurvie1,variablesurvie2) }
    }, rownames=FALSE)

    output$pngPLOT_SURVIE <- downloadHandler(
      filename = function() paste0("survie_", input$variablesurvie1, ".png"),
      content = function(file) export_png(file, dessinerPLOT_SURVIE)
    )

    output$csvSURVIE <- downloadHandler(
      filename = function() paste0("survie_", input$variablesurvie1, ".csv"),
      content = function(file) {
        base <- r$BDD
        variablesurvie1 <- base[, colnames(base) == input$variablesurvie1]
        variablesurvie2 <- base[, colnames(base) == input$variablesurvie2]
        if (input$SURVIEcompar) {
          variablesurvie3 <- base[, colnames(base) == input$variablesurvie3]
          DETAIL <- tab_survie(variablesurvie1, variablesurvie2, variablesurvie3)
        } else {
          DETAIL <- tab_survie(variablesurvie1, variablesurvie2)
        }
        write.csv(DETAIL, file, row.names = FALSE)
      }
    )
    
    })
    
    
  })
}
    
## To be copied in the UI
# mod_Survie_ui("Survie_1")
    
## To be copied in the server
# mod_Survie_server("Survie_1")
