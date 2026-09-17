#' Tests UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import pROC

mod_Tests_ui <- function(id){
  ns <- NS(id)
  tagList(
  )
  uiOutput(ns('testsDiagnostiques'))
}
    
#' Tests Server Functions
#'
#' @noRd 
mod_Tests_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    pasDeBase <-   fluidPage(   
      h4("Aucune base n'a été chargée en mémoire, cet onglet n'est pas accessible." ),
      p("Pour charger une base de données, rendez-vous sur l'onglet « Base de Données » dans la barre latérale.")
    )
 
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
    
    #source("./CodeSansDependance.R", local = TRUE)
    #source("./fonctions.R", local = TRUE)
    #source("./miseEnForme.R", local = TRUE)
    #eval(parse("./miseEnForme.R", encoding="UTF-8"))
    
    output$PDFdiag = downloadHandler(
      filename    = '5_Diagnostiques.pdf',
      content     = function(file) file.copy(system.file("app/www/5_Diagnostiques.pdf", package = 'GmrcShinyStats'), file, overwrite = TRUE),
      contentType = 'application/pdf'
    ) 
    
    observe({
    output$testsDiagnostiques = renderUI({
      if(!r$BASEchargee) do.call(tabPanel,pasDeBase)
      else do.call(tabPanel,testsDiagnostiques)
    })
    })
    
    observeEvent(r$BDD,ignoreInit = T,{
      
    output$propositionsLOGIT1 <- renderUI({
      
      selectInput(ns("variableLogit1"), "Variable d'intérêt 0/1",   choices=r$noms) 
    })
    
    output$propositionsLOGIT2 <- renderUI({
      
      selectInput(ns("variableLogit2"), "Variable quantitative explicative",   choices=r$noms) 
    })
    
    output$mytableLOGIT1 <- renderTable({

      base<-r$BDD
      #variablesurvie1 <-base[,colnames(base)==input$variableLogit1]
      variablesurvie1 <-base %>% select(input$variableLogit1)
      variablesurvie1 <- variablesurvie1[[1]]
      #variablesurvie2 <-base[,colnames(base)==input$variableLogit2]
      variablesurvie2 <-base %>% select(input$variableLogit2)
      variablesurvie2 <- variablesurvie2[[1]]
      data.frame(Reponse=variablesurvie1, Facteur=variablesurvie2)
      },rownames=TRUE)
#     
# 
# 
# 
# 
output$LogitROC <- renderPlot({
  base    <-r$BDD
  D       <-base

  #variablesurvie1 <-base[,colnames(base)==input$variableLogit1]
  variablesurvie1 <-base %>% select(input$variableLogit1)
  variablesurvie1 <- variablesurvie1[[1]]
  #variablesurvie2 <-base[,colnames(base)==input$variableLogit2]
  variablesurvie2 <-base %>% select(input$variableLogit2)
  variablesurvie2 <- variablesurvie2[[1]]


  rocobj<-plot.roc(variablesurvie1,variablesurvie2, percent=TRUE,ci=TRUE,print.auc=input$LOGIToptionsAUC)


  if(input$LOGIToptionsSEUIL){
    optimums       <-ci(rocobj, of="thresholds", thresholds="best")
    plot(optimums) }

  if(input$LOGIToptionsIntervalle){
    ciobj           <- ci.se(rocobj, specificities=seq(0, 100, 5))
    plot(ciobj, type="shape", col="#1c61b6AA") }
})
})
 
 
# ## Creation des perf
    observeEvent(input$variableLogit1,ignoreInit = T,{

      base    <-r$BDD

      D       <-base
      #variablesurvie1 <-base[,colnames(base)==input$variableLogit1]
      variablesurvie1 <-base %>% select(input$variableLogit1)
      variablesurvie1 <- variablesurvie1[[1]]
      #variablesurvie2 <-base[,colnames(base)==input$variableLogit2]
       variablesurvie2 <-base %>% select(input$variableLogit2)
       variablesurvie2 <- variablesurvie2[[1]]

   rocobj<-roc(variablesurvie1,variablesurvie2, percent=TRUE,ci=TRUE,print.auc=input$LOGIToptionsAUC)
  x<-ci.thresholds(rocobj)

  MatriceSEUILS<-cbind(
    attr(x, "thresholds"),
    x$ sensitivity,
    x$ specificity,
    x$ sensitivity[,2]+x$ specificity[,2]-100)
  colnames(MatriceSEUILS)<-c("Seuils","2.5% Sensibilité","Sensibilité","97.5% Sensibilité","2.5% Spécificité","Spécificité","97.5% Spécificité","Indice de Youden")
  r$MatriceReactiveSeuils<-MatriceSEUILS

})

observe({


    output$LogitROCtableau <- renderTable({

      base    <-r$BDD

      D       <-base
            #variablesurvie1 <-base[,colnames(base)==input$variableLogit1]
      variablesurvie1 <-base %>% select(input$variableLogit1)
      variablesurvie1 <- variablesurvie1[[1]]
      #variablesurvie2 <-base[,colnames(base)==input$variableLogit2]
      variablesurvie2 <-base %>% select(input$variableLogit2)
      variablesurvie2 <- variablesurvie2[[1]]

      rocobj<-roc(variablesurvie1,variablesurvie2, percent=TRUE,ci=TRUE,print.auc=input$LOGIToptionsAUC)
      x<-ci.thresholds(rocobj)

      MatriceSEUILS<-cbind(
        attr(x, "thresholds"),
        x$ sensitivity,
        x$ specificity,
        x$ sensitivity[,2]+x$ specificity[,2]-100)
      colnames(MatriceSEUILS)<-c("Seuils","2.5% Sensibilité","Sensibilité","97.5% Sensibilité","2.5% Spécificité","Spécificité","97.5% Spécificité","Indice de Youden")
      MatriceSEUILS
    },rownames=TRUE)


    output$LogitROCtableauBEST <- renderTable({
      base    <-r$BDD

      D       <-base
      #variablesurvie1 <-base[,colnames(base)==input$variableLogit1]
      variablesurvie1 <-base %>% select(input$variableLogit1)
      variablesurvie1 <- variablesurvie1[[1]]
      #variablesurvie2 <-base[,colnames(base)==input$variableLogit2]
      variablesurvie2 <-base %>% select(input$variableLogit2)
      variablesurvie2 <- variablesurvie2[[1]]

      rocobj<-roc(variablesurvie1,variablesurvie2, percent=TRUE,ci=TRUE,print.auc=input$LOGIToptionsAUC)
      MatriceSEUILS<-r$MatriceReactiveSeuils
      #x<-ci.thresholds(rocobj)

      #MatriceSEUILS<-cbind(
      #  attr(x, "thresholds"),
      #  x$ sensitivity,
      #  x$ specificity,
      #  x$ sensitivity[,2]+x$ specificity[,2]-100)
      #colnames(MatriceSEUILS)<-c("Seuils","2.5% Se","Se","97.5% Se","2.5% Sp","Sp","97.5% Sp","Indice de Youden")

      t(MatriceSEUILS[which(MatriceSEUILS[,8]==max(MatriceSEUILS[,8])),])
    },rownames=TRUE)




    output$LogitPERFtableauBEST <- renderTable({
      base    <-r$BDD

      D       <-base
      #variablesurvie1 <-base[,colnames(base)==input$variableLogit1]
      variablesurvie1 <-base %>% select(input$variableLogit1)
      variablesurvie1 <- variablesurvie1[[1]]
      #variablesurvie2 <-base[,colnames(base)==input$variableLogit2]
      variablesurvie2 <-base %>% select(input$variableLogit2)
      variablesurvie2 <- variablesurvie2[[1]]

      rocobj<-roc(variablesurvie1,variablesurvie2, percent=TRUE,ci=TRUE,print.auc=input$LOGIToptionsAUC)
      x<-ci.thresholds(rocobj)

      MatriceSEUILS<-cbind(
        attr(x, "thresholds"),
        x$ sensitivity,
        x$ specificity,
        x$ sensitivity[,2]+x$ specificity[,2]-100)
      colnames(MatriceSEUILS)<-c("Seuils","2.5% Se","Se","97.5% Se","2.5% Sp","Sp","97.5% Sp","Indice de Youden")
      t(MatriceSEUILS[which(MatriceSEUILS[,8]==max(MatriceSEUILS[,8])),1])
    },rownames=TRUE)


    output$LogitPERF1 <- renderTable({
      base    <-r$BDD

      D       <-base
     #y <-base[,colnames(base)==input$variableLogit1]
     y<-base %>% select(input$variableLogit1)
     y<-y[[1]]
     #x <-base[,colnames(base)==input$variableLogit2]
     x<-base %>% select(input$variableLogit2)
     x<-x[[1]]

      rocobj                  <-roc(y,x,main=input$variableLogit2, percent=TRUE,ci=TRUE,print.auc=TRUE)
      optimums                <-ci(rocobj, of="thresholds", thresholds="best")

      AUC                     <-c( round(rocobj$ci[1],2),  round(rocobj$ci[2],2),  round(rocobj$ci[3],2) )
      best.cut                <-as.numeric(rownames(round(optimums$sensitivity,2)))
      best.sen                <-c(round(optimums$sensitivity,2))
      best.spe                <-c(round(optimums$specificity,2))

      SeSp                    <-rbind(best.sen,best.spe,AUC)
      rownames(SeSp)  =c("Sensibilité","Spécificité","AUC (Aire sous la courbe)")
      colnames(SeSp)  =c("2.5%","Val","97.5%")
      SeSp
    },rownames=TRUE)



    output$LogitPERF2 <- renderTable({
      base<-r$BDD

      D<-base
      #y <-base[,colnames(base)==input$variableLogit1]
      y<-base %>% select(input$variableLogit1)
      y<-y[[1]]
      #x <-base[,colnames(base)==input$variableLogit2]
      x<-base %>% select(input$variableLogit2)
      x<-x[[1]]
      
      rocobj<-roc(y,x,main=input$variableLogit2, percent=TRUE,ci=TRUE,print.auc=TRUE)
      optimums<-ci(rocobj, of="thresholds", thresholds="best")

      best.cut<-as.numeric(rownames(round(optimums$sensitivity,2)))

      y2 <-y
      x2 <-ifelse(x>best.cut,1,0)
      T <-table(x2,y2)
      VP<-T[2,2]
      VN<-T[1,1]
      FP<-T[2,1]
      FN<-T[1,2]
      V1<-cbind(VP,VN,FP,FN);colnames(V1)=c("VP","VN","FP","FN")

      VPP<-round(VP/(VP+FP),2)
      VPN<-round(VN/(VN+FN),2)
      Exact<-round((VP+VN)/(VP+VN+FP+FN),2)
      Erreur<-round((FP+FN)/(VP+VN+FP+FN),2)
      V2<-cbind(VPP,VPN,Exact,Erreur);colnames(V2)=c("VPP","VPN","Exactitude","Taux d'erreur")

      cbind(V1,V2)
    },rownames=TRUE)


    output$LogitPERF3 <- renderTable({
      base    <-r$BDD

      D <-base
      #y <-base[,colnames(base)==input$variableLogit1]
      y<-base %>% select(input$variableLogit1)
      y<-y[[1]]
      #x <-base[,colnames(base)==input$variableLogit2]
      x<-base %>% select(input$variableLogit2)
      x<-x[[1]]
      
      rocobj<-roc(y,x,main=input$variableLogit2, percent=TRUE,ci=TRUE,print.auc=TRUE)
      optimums<-ci(rocobj, of="thresholds", thresholds="best")

      best.cut<-as.numeric(rownames(round(optimums$sensitivity,2)))

      y2<-y
      x2<-ifelse(x>best.cut,1,0)
      T<-table(x2,y2)
      rownames(T)<-c("x<cut","x>cut")
      colnames(T)<-c("0","1")
      T
    },rownames=TRUE)
  })
  })
  
}
    
## To be copied in the UI
# mod_Tests_ui("Tests_1")
    
## To be copied in the server
# mod_Tests_server("Tests_1")
