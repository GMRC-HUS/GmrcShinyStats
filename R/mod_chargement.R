#' chargement UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shinyFiles
mod_chargement_ui <- function(id){
  ns <- NS(id)
  tagList(
 h1("Hello chargement !")
  )
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file1"), "Choose CSV File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        tags$hr(),
        checkboxInput(ns('header'), 'Titre       (Votre fichier contient-il des titres de colonnes ?)', TRUE),
        radioButtons(ns('sep'), 'Séparateur',
                     c(Virgule=',',
                       `Point Virgule`=';',
                       Tabulation='\t'),
                     ';'),
        radioButtons(ns('manquants'), 'Manquants (Quel symbole est utilisé pour les données manquantes ?)',
                     c(Slash='/',
                       Etoile='*',
                       `Lettres NA`='NA'),
                     '*'),
        radioButtons(ns('decimale'), 'Symbole de décimale',
                     c(Virgule=',',
                       Point='.'),
                     ','),
        tags$br(),
        #actionButton(ns("upload"), "Charger/actualiser la base",class = "btn-success") ,
        radioButtons(ns('encodage'), "Si vous avez des problèmes d'import de la base de données, potentiellement liés à l'encodage",
                     c(`Windows/Excel`='windows-1252',
                       `Linux/LibreOffice` ='utf-8'),
                     'windows-1252'),
        
        tags$br(),tags$br(),tags$br(),
        "Les lignes entièrement vides seront retirées pour la suite des analyses."
      ),
      mainPanel(
        tableOutput(ns("contents"))
      )
    )
  )
}

#' chargement Server Functions
#'
#' @noRd
mod_chargement_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    output$contents <- renderTable({
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
      lire_bdd_csv(inFile$datapath, header=input$header, sep=input$sep,
                   manquants=input$manquants, decimale=input$decimale,
                   encodage=input$encodage)
      })
    
    # BDD      <- reactive({
    #   inFile  <- input$file1
    #   if (is.null(inFile))
    #     return(NULL)
    #   D<-read.csv(inFile$datapath, header=input$header,sep=input$sep, na.string=c("",input$manquants),dec=input$decimale)
    #   # D<-D[input$contents2_rows_all,]
    #   lignesVides<-apply(D,1,function(x){sum(is.na(x))})==dim(D)[2]
    #   D<-D[!lignesVides,]
    #   D
    # })
    

    
    observeEvent(input$file1,ignoreInit = T,{
      inFile <- input$file1
      DD<-lire_bdd_csv(inFile$datapath, header=input$header, sep=input$sep,
                       manquants=input$manquants, decimale=input$decimale,
                       encodage=input$encodage)
      if(is.null(DD)) return()
      r$BDD<-DD
      r$contentInput<-DD
    })
    
    
    # BASEchargee<-reactive({
    #   CHARGEE<- tryCatch(dim(r$BDD)[1]>0,warning= function(e) F,error= function(e) F)
    #   
    #   CHARGEE
    #   
    # })
    
    observe({
      #r$BASEchargee<-tryCatch(dim(r$BDD)[1]>0,warning= function(e) F,error= function(e) F)
      r$BASEchargee <- !is.null(input$file1)

    })
    
    # noms<- reactive({
    #   base<- r$BDD
    #   colnames(base) 
    # })
    # 
    # nbSujet<-reactive({
    #   res<-dim(r$contentInput)[1]
    #   print(res)
    #   res
    #   
    # })
    
    #Recherche du nombre de modalités par variable
    # nbModeVariable <- reactive({
    #   D <- r$BDD
    #   ret <- NULL
    #   for(i in 1 : (dim(D)[2])){
    #     ret[i] <- nlevels(as.factor(D[,i]))
    #   }
    #   ret
    # })
    
    observeEvent(r$BDD,ignoreInit = T,{
      r$noms<-colnames(r$BDD)
      r$nbSujet<-dim(r$contentInput)[1]
      D <- r$BDD
      Y<-as.data.frame(lapply(D, factor))
      z<-as.numeric(lapply(Y, nlevels))
      r$nbModeVariable<-z
    })

    # for(i in 1 : (dim(D)[2])){
    #   ret[i] <- nlevels(as.factor(D[,i]))
    # }
    # 
    
    # observe({
    #   D <- r$BDD
    # 
    #   #r$nbModeVariable<-ret[1]
    # })
    
    
    # variableNum <- reactive({
    #   D <- r$BDD
    #   ret <- NULL
    #   for(i in 1 : (dim(D)[2])){
    #     ret[i] <- is.numeric(D[,i])
    #   }
    #   ret
    # })
    # 
    observeEvent(r$BDD,ignoreInit = T,{
      D <- r$BDD
      r$variableNum<-as.logical(lapply(D, is.numeric))
    })
    # 
    # # variableNormale <- reactive({
    # #   D <- r$BDD
    # #   ret <- rep(NA, dim(D)[2])
    # #   
    # #   for(i in which(variableNum())){
    # #     ret[i] <- desctable::is.normal(D[,i])
    # #   }
    # #   ret
    # # })
    # 
    
    # X<-isolate(r$BDD)
    # Y<-isolate(r$variableNum)
    # ret<-reactive({rep(NA, ncol(X))})
    # ret[which(isolate(r$variableNum))] <- as.logical(lapply(as.data.frame(D[,which(isolate(r$variableNum))]), desctable::is.normal))
    # #r$variableNormale<-ret
    
    
    observeEvent(r$BDD,ignoreInit = T,{
      D <- r$BDD
      ret<-rep(NA, ncol(D))
      ret[which(r$variableNum)] <- as.logical(lapply(as.data.frame(D[,which(r$variableNum)]), desctable::is.normal))
      r$variableNormale<-ret
    })
    
    
  })
}

## To be copied in the UI
# mod_chargement_ui("chargement_1")

## To be copied in the server
# mod_chargement_server("chargement_1")
