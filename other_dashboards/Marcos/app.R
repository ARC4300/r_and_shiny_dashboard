library(shiny)
#Para cargar el tema
library(bslib)
library(thematic)
library(ggplot2)
library(dplyr)
miTema <- bs_theme(4,'solar', base_font= font_google("Fira Sans"))
tema_luz <- bs_theme(4,'solar',bg = '#558AC5', fg = '#F9B02D')

thematic_shiny(font = "auto")

ui <- fluidPage(
  theme = miTema, 
    #bs_theme(
    #bootswatch = "solar",
  #), 
  #titulo <- "Proyecto BEDU:La Liga Española",
  #titlePanel(titulo),
  fluidRow(
    column(2, align="center", img( src = "logo.png", 
                  height = 90, width = 72)),
    column(2, align="center", imageOutput(outputId = "ligaLogo", 
                                          height = "100px", width = "100px", 
                                          inline = T)),
    column(6, titlePanel("Proyecto BEDU: La Liga Española")),
    column(2, radioButtons("tema_actual", "Modo:", 
                        choiceNames = list(icon("moon"), icon("sun")),
                        choiceValues = list("Oscuro", "Luz"),
                        inline = T),
    ),
  ),
  fluidRow(
    column(8,
      navlistPanel(
      tabPanel("Barras", 
             titlePanel("Número de partidos por número de goles de visitante o casa"), 
             selectInput("x", "Seleccione el valor de X",
                         choices = c("home.score", "away.score")),
             plotOutput("plotGoles", height = 450, width = 750)
        ),
      tabPanel("Ganancias",
               titlePanel(h3("Ganancias estimadas")),
               selectInput("tipo_momios", "Momios",
                           c("Máximos" = "maximo",
                             "Promedio" = "promedio")) ,
               conditionalPanel(condition = "input.tipo_momios == 'maximo'",
                  column(12, h3("Factor de ganancia Máximo"),
                            imageOutput(outputId = "maxMom",
                                            inline = T)
                         ),
               ),
               conditionalPanel(condition = "input.tipo_momios == 'promedio'",
                  column(12, h3("Factor de ganancia Promedio"),
                                imageOutput(outputId = "proMom",
                                            inline = T)
                        )
               ),
        ),
      #Pestania de Data frame. Nota: Puede tomar algo de tiempo en cargar
      tabPanel("Data Table", 
             dataTableOutput("data_table")
        ),
      )
    )
  )
)
    


server <- function(input, output, session) {
  
  datosLiga <- read.csv("match.data.csv", header = T)
  
  #Gráficas                       <----------
  
  output$plotGoles <- renderPlot({
    data <-  read.csv("match.data.csv", header = T)
    #Los codigos de ganador: 
    #H <- equipo de casa
    #A <- equipo visitante
    #D <- empate
    data <- mutate(data, FTR = ifelse(home.score > away.score, "H", 
                                      ifelse(home.score < away.score, 
                                             "A", "D")))
    x <- data[,"home.score"]
  
    data %>% ggplot(aes(x, fill = FTR)) + 
      geom_bar() + 
      facet_wrap("away.team") +
      labs(x =input$x, y = "Partidos") + 
      ylim(0,50)
  })
  
  #Para cargar el data frame.
  output$data_table <- renderDataTable({datosLiga}, 
                                       options = list(aLengthMenu = c(5,25,50),
                                                      iDisplayLength = 10)
  )
    
  
    
  
  #Para el switch de modo oscuro
  observe({
    # El 4 indica la version de bootstrap
    session$setCurrentTheme(
      if(input$tema_actual == "Luz"){
        bs_theme_update(miTema, bootswatch = 'solar', 
                        bg = "#FFFFFF", fg = "#FF6A39")
      } else {
        bs_theme_update(miTema, bootswatch = 'solar')
      }
    )
  })
  
  #Elegir una imagen distinta segun el tema
  #Imagenes condicionales
  output$maxMom <- renderImage({
    if(input$tema_actual=="Oscuro") maxImg <-"www/momMaxO.png"
    else maxImg <-"www/momMaxL.png"
    list(src = maxImg)
  }, deleteFile = FALSE)
  
  output$proMom <- renderImage({                                                                                                                         
    if(input$tema_actual=="Oscuro") proImg <-"www/momMedO.png"
    else proImg<-"www/momMedL.png"
    list(src = proImg)
  }, deleteFile = FALSE)
  
  output$ligaLogo <- renderImage({
    if(input$tema_actual=="Oscuro") logoL <-"www/laligaOs.png"
    else logoL<-"www/laligas.png"
    list(src = logoL)
  }, deleteFile = FALSE)
  
}

shinyApp(ui = ui, server = server)
