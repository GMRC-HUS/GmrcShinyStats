#' Descriptifs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @import ggplot2
#' @import xtable
#' @import ggthemes


mod_Descriptifs_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
  uiOutput(ns('univarie'))
}
    
#' Descriptifs Server Functions
#'
#' @noRd 
#' 
mod_Descriptifs_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    diagrammeBarre <- function(base){
      data<- tablePourcent(base)
      bp<-ggplot(data=data, aes(x=nom ,y=pourcent*100, fill=reorder_factor_levels(factor(nom), nom[order(-pourcent)])))
      
      maxPourcent<- max(data$pourcent, na.rm = T)
      label<-  paste(round(data$pourcent,3)*100,"%")
      vjust<- unlist(as.list(ifelse(data$pourcent< maxPourcent/5, -1.6, 1.6)), use.names = F)
      
      barre <- bp +
        labs(title="Diagramme en barre", 
             x="", y = "pourcentage")+
        geom_bar(stat="identity", color='black')+
        guides(fill=guide_legend(override.aes=list(colour=NULL)))+
        
        
        geom_text(aes( label = label), vjust=vjust, color="black", size=5) +
        theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=17))
      
      barre$labels$fill <- ""
      return(barre)
      
    }
    
    tablePourcent<- function(base){
      pourcent <-  prop.table(table(base)) 
      pourcent<- pourcent[order(pourcent)]
      
      data<- data.frame(pourcent = as.numeric(pourcent), nom = names(pourcent))
      
      
      
      return(data)
      
    }
    
    pasDeBase <-   fluidPage(   
      h4("Aucune base n'a été chargée en mémoire, cet onglet n'est pas accessible." ),
      p("Pour charger une base de données, rendez-vous sur l'onglet « Base de Données » dans la barre latérale.")
    )
    
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

                                                           verbatimTextOutput (ns("tableauBASE")),
                                                           plotOutput(ns('plotNAbase1'))),
                                                  tabPanel("Données manquantes cumulées par variable",
                                                           fluidRow(
                                                             splitLayout(cellWidths = c("30%","70%"),
                                                                         downloadButton(ns('PDFdescriptif1o2'),label="AIDE et Détails",class = "butt"),
                                                                         h4("Faites attention s'il y a un filtre")
                                                             )
                                                           ),#finFluidRow
                                                           

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
    
    #source("./CodeSansDependance.R", local = TRUE)
    #source("./fonctions.R", local = TRUE)
    #source("./miseEnForme.R", local = TRUE)
    #eval(parse("./miseEnForme.R", encoding="UTF-8"))
    
    output$PDFdescriptif1o1 = downloadHandler(
      filename    = '2_Descriptif.pdf',
      content     = function(file) file.copy(system.file("app/www/2_Descriptif.pdf", package = 'GmrcShinyStats'), file, overwrite = TRUE),
      contentType = 'application/pdf'
    )
    
    output$PDFdescriptif1o2 = downloadHandler(
      filename    = '2_Descriptif.pdf',
      content     = function(file) file.copy(system.file("app/www/2_Descriptif.pdf", package = 'GmrcShinyStats'), file, overwrite = TRUE),
      contentType = 'application/pdf'
    )
    output$PDFdescriptif1o3 = downloadHandler(
      filename    = '2_Descriptif.pdf',
      content     = function(file) file.copy(system.file("app/www/2_Descriptif.pdf", package = 'GmrcShinyStats'), file, overwrite = TRUE),
      contentType = 'application/pdf'
    )

    observe({
    output$univarie = renderUI({
      if(!r$BASEchargee) pasDeBase
      else univarie
    })
    })

    observe({ 
    output$plotNAbase1 <- renderPlot({
      plot.na(r$BDD)
      #DataExplorer::plot_missing(r$BDD)+theme_clean()
    })

    output$plotNAbase2 <- renderPlot({
      barplot(apply(is.na(r$BDD),2,sum),xlab="", col = "palegreen3")
    })

    output$plotNAbase3 <- renderPlot({
      barplot(apply(is.na(r$BDD),1,sum),xlab="",  col ="lightblue1")
    })

    output$tableauBASE <- renderPrint({
      descd(r$BDD)

    })
    
    output$tableNAbase2 <- renderTable({
      D           <-r$BDD
      NbVariables <-dim(D)[2]
      matriceNA   <-matrix(NA,nrow=NbVariables,ncol=3)
      for(i in 1:NbVariables){
        matriceNA[i,1]<-round(sum   (is.na(D[,i])))
        matriceNA[i,2]<-round(length(is.na(D[,i])))
        matriceNA[i,3]<-round(sum   (100*is.na(D[,i]))/length(is.na(D[,i])),2)
      }
      colnames(matriceNA)<-c("Nb.manquants","Nb.données","%")
      rownames(matriceNA)<-colnames(D)
      matriceNA
    },rownames=TRUE)

    output$tableNAbase3 <- renderTable({
      D           <-t(r$BDD)
      NbVariables <-dim(D)[2]
      matriceNA   <-matrix(NA,nrow=NbVariables,ncol=3)
      for(i in 1:NbVariables){
        matriceNA[i,1]<-round(sum               (is.na(D[,i])))
        matriceNA[i,2]<-round(length            (is.na(D[,i])))
        matriceNA[i,3]<-round(sum               (100*is.na(D[,i]))/length       (is.na(D[,i])),2)
      }
      colnames(matriceNA)<-c("Nb.manquants","Nb.données","%")
      rownames(matriceNA)<-colnames(D)
      matriceNA
    },rownames=TRUE)
    
    
    ########################################################################################################################
    ####    OUTPUT page 3 : Descriptifs univaries          
    ########################################################################################################################
    
    
    output$propositions <- renderUI({

      selectInput(ns("variable"), "Variable:",   choices=r$noms)
    })

    # output$summary <- renderPrint({
    #   summary(r$BDD)
    # })

    output$descriptifUni <- renderText(paste("Descriptif de la variable ",input$variable, sep = ""))
    
    output$descvar <- renderTable({
      base    <-r$BDD
      variable<-base[,colnames(base)==input$variable]
      print(input$variable)
      if(input$qualiquanti=="quant"){res<-data.frame(descr1(variable)$Descriptif)
      colnames(res) <- c("Descriptif")}
      if(input$qualiquanti=="qual") {res<-data.frame(desql(variable))
      colnames(res) <- c("Effectifs", "Proportions")}
      xtable(res, "essai")
    },hover = T,rownames=TRUE)

    output$plot1 <- renderPlot({
      base    <-r$BDD
      variable<-base[,input$variable]
      if(input$qualiquanti=="quant"){
        print(   hist(variable,
                      xlab = input$variable,
                      ylab = "Effectif",
                      main= "Histogramme",
                      col = "#75AADB", border = "white") )
        #g<-ggplot(base, aes_string(x=input$variable))+geom_histogram(fill="#75AADB", color="white")+theme_minimal()+xlab(input$variable)+ylab("Effectif")+ggtitle("Histogramme"); print(g)


      }
      if(input$qualiquanti=="qual") {variable<-as.character(variable);print( diagrammeBarre(variable)  )}
    })

    output$plot2 <- renderPlot({
      base    <-r$BDD
      variable<-base[,colnames(base)==input$variable]
      #variable<-base[,input$variable]
      #if(input$qualiquanti=="quant"){boxplot(x=variable,main="Diagramme boite", xlab = input$variable)}
      if(input$qualiquanti=="quant"){g<-ggplot(base, aes_string(y=input$variable))+geom_boxplot(width=0.2)+theme_minimal(); print(g)}
      #if(input$qualiquanti=="qual"){print(graphics::pie(as.vector(table(variable))))}
      if(input$qualiquanti=="qual"){print(as.data.frame(table(variable))); g<-ggplot(as.data.frame(table(variable)), aes(x="", y=Freq, fill=variable)) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0)+theme_void()+ggtitle("Diagramme circulaire")+
        geom_text(aes(label = variable),
                  position = position_stack(vjust = 0.5)) +theme(legend.position="none"); print(g)}
    })
    
    # 
    # quali
    # 
    
    
    
    
    
    
    })
    

  })
}
    
## To be copied in the UI
# mod_Descriptifs_ui("Descriptifs_1")
    
## To be copied in the server
# mod_Descriptifs_server("Descriptifs_1")


