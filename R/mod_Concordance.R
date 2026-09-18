#' Concordance UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @import irr
#' @import boot

mod_Concordance_ui <- function(id){
  ns <- NS(id)
  tagList(
  )
  uiOutput(ns('concordance'))
}
    
#' Concordance Server Functions
#'
#' @noRd 
mod_Concordance_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    pasDeBase <-   fluidPage(   
      h4("Aucune base n'a été chargée en mémoire, cet onglet n'est pas accessible." ),
      p("Pour charger une base de données, rendez-vous sur l'onglet « Base de Données » dans la barre latérale.")
    )
    
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
    
    #source("./CodeSansDependance.R", local = TRUE)
    #source("./fonctions.R", local = TRUE)
    #source("./miseEnForme.R", local = TRUE)
    #eval(parse("./miseEnForme.R", encoding="UTF-8"))
    
    output$PDFconcordance = downloadHandler(
      filename    = '6_Concordance.pdf',
      content     = function(file) file.copy(system.file("app/www/6_Concordance.pdf", package = 'GmrcShinyStats'), file, overwrite = TRUE),
      contentType = 'application/pdf'
    ) 
    
    
    observe({
      output$concordance = renderUI({
        if(!r$BASEchargee) pasDeBase
        else concordanceAvecBase
      })
    })
      
      observeEvent(r$BDD,ignoreInit = T,{
      output$CONCORDANCElecture1 <- renderUI({
        
        selectInput(ns("CONCORDANCElecture1"), "Variable qualitative : Lecteur 1",   choices=r$noms) 
      })
      
      output$CONCORDANCElecture2 <- renderUI({
        
        selectInput(ns("CONCORDANCElecture2"), "Variable qualitative : Lecteur 2",   choices=r$noms) 
      })
      
      
      output$mytableCONCORDANCE1 <- renderTable({
        
        base<-r$BDD
        
        if(input$CONCORsaisie){
          if(input$Concoman1!=""&input$Concoman2!=""){
          variableCONCORDANCE1 <-as.factor(strsplit(input$Concoman1," ")[[1]])
          variableCONCORDANCE2 <-as.factor(strsplit(input$Concoman2," ")[[1]])}else{
            variableCONCORDANCE1<-""
            variableCONCORDANCE2<-""}
        }else{
          variableCONCORDANCE1 <-base[,colnames(base)==input$CONCORDANCElecture1]
          variableCONCORDANCE2 <-base[,colnames(base)==input$CONCORDANCElecture2]  
          
        }
        if(variableCONCORDANCE1!="" & variableCONCORDANCE2!=""){
        as.data.frame.matrix(addmargins(table(variableCONCORDANCE1,variableCONCORDANCE2)))}else{}
      },rownames=TRUE)
      
      
      output$ConcordanceManuelleINTERV <- renderPrint({
        
        base<-r$BDD
        if(input$CONCORsaisie){
          if(input$Concoman1!=""&input$Concoman2!=""){
          x <-as.factor(strsplit(input$Concoman1," ")[[1]])
          y <-as.factor(strsplit(input$Concoman2," ")[[1]])}else{
            x<-""
            y<-""}
        }else{
          
          
          x <-base[,colnames(base)==input$CONCORDANCElecture1]
          y <-base[,colnames(base)==input$CONCORDANCElecture2]  
        }
        if(x!="" & y!=""){
        # creation matrice
        Mat2<-cbind(x,y)
        Mat2<-Mat2[complete.cases(Mat2),]
        
        # partage des facteurs
        Mat2						<-as.data.frame(Mat2)
        LEV						<-sort(unique(c(as.character(levels(as.factor(Mat2[,1]))),as.character(levels(as.factor(Mat2[,2]))))))
        Mat2[,1]				<-as.factor(Mat2[,1])
        Mat2[,2]				<-as.factor(Mat2[,2])
        levels(Mat2[,1])	<- c(levels(Mat2[,1]),LEV[!is.element(LEV,levels(Mat2[,1]))]      )
        levels(Mat2[,2])	<- c(levels(Mat2[,2]),LEV[!is.element(LEV,levels(Mat2[,2]))]      )
        Mat2[,1]<-reorder_factor_levels(Mat2[,1], new.order=levels(Mat2[,2]))
        
        if(all(Mat2[,1]==Mat2[,2])){RESULTAT<-c(1,1,1)}else{
          lkappa.boot <- function(data,x) {kappa2(data[x,])$value}
          res <- boot(Mat2,lkappa.boot,10000)
          RESULTAT<-c(lkappa.boot(Mat2),boot.ci(res,type="bca")$ bca[,4:5])
        }
        cat("Le coefficient de concordance Kappa de Cohen est estimé à",RESULTAT[1],
            "dans l'intervalle à 95% [",RESULTAT[2],";",RESULTAT[3],"]\nTest\nLe test de nullité de ce coefficient peut être réalisé et la p.valeur associée est",round(kappa2(cbind(x,y))$p.value,3), "\n")
      }})
      
      output$ConcordanceManuelleSimple <- renderPrint({ 
        
        base<-r$BDD
        if(input$CONCORsaisie){
          if(input$Concoman1!=""&input$Concoman2!=""){
          x <-as.factor(strsplit(input$Concoman1," ")[[1]])
          y <-as.factor(strsplit(input$Concoman2," ")[[1]])}else{
            x<-""
            y<-""}
        }else{
          
          x <-base[,colnames(base)==input$CONCORDANCElecture1]
          y <-base[,colnames(base)==input$CONCORDANCElecture2]  
        }
        if(x!="" & y!=""){
        cat("Estimation\nLe coefficient de concordance Kappa de Cohen est estimé à",round(kappa2(cbind(x,y))$value,3),".
        \n\nTest\nLe test de nullité de ce coefficient peut être réalisé et la p.valeur associée est",round(kappa2(cbind(x,y))$p.value,3), "\n")
      }else{cat("Veuillez saisir les réponses de chaque lecteur.")}})
      
      output$LandisEtKoch2 <- renderTable({
        data.frame(Kappa=c("0-0.2","0.21-0.40","0.41-0.60","0.61-0.80","0.81-1"),
                   interpretation=c("très faible","faible","modéré","fort","presque parfait"))
        
      },rownames=TRUE)   
      
    })
  })
}
    
## To be copied in the UI
# mod_Concordance_ui("Concordance_1")
    
## To be copied in the server
# mod_Concordance_server("Concordance_1")
